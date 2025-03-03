// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

struct test_environment : public lug::environment
{
	int reset_count{0};
	int drain_count{0};
	int accept_count{0};
	std::unordered_map<std::string, int> action_counts;

	void on_reset() override
	{
		++reset_count;
		drain_count = 0;
		accept_count = 0;
		action_counts.clear();
	}

	void on_drain() override
	{
		++drain_count;
	}

	void on_accept_started() override
	{
		++accept_count;
	}
};

void test_accept_choice()
{
	namespace ll = lug::language;
	using namespace ll::operators;

	test_environment E;

	// Test accept behavior in: e1 > cut > e2 | e3
	ll::rule A = ll::noskip["a"_sx < [&]{ E.action_counts["accept"]++; } > ll::accept > "b"_sx];
	ll::rule B = ll::noskip["c"_sx > "d"_sx];
	ll::rule S = ll::noskip[(A | B) > ll::eoi];
	ll::grammar G = start(S);

	// Should succeed with complete match
	assert(lug::parse("ab", G, E));
	assert(E.reset_count == 1);
	assert(E.drain_count == 0);
	assert(E.accept_count == 2);
	assert(E.action_counts["accept"] == 1);
	assert(E.match() == "ab");
	assert(E.subject().empty());

	// Should succeed with complete match
	assert(lug::parse("cd", G, E));
	assert(E.reset_count == 2);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["accept"] == 0);
	assert(E.match() == "cd");
	assert(E.subject().empty());

	// Should fail - after matching 'a', cut prevents backtracking to try B.
	// However, the cut is deferred, so the callback is called.
	assert(!lug::parse("ac", G, E));
	assert(E.reset_count == 3);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["accept"] == 1);
	assert(E.match() == "a");
	assert(E.subject() == "c");
}

void test_accept_repetition()
{
	namespace ll = lug::language;
	using namespace ll::operators;

	test_environment E;
	
	// Test cut behavior in *(e1 > cut > e2)
	ll::rule A = ll::noskip["a"_sx < [&]{ E.action_counts["accept"]++; } > ll::accept > "b"_sx];
	ll::rule S = ll::noskip[*A > ll::eoi];
	ll::grammar G = start(S);

	// Empty input should succeed with zero repetitions
	assert(lug::parse("", G, E));
	assert(E.reset_count == 1);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["accept"] == 0);
	assert(E.match() == "");
	assert(E.subject().empty());

	// Single 'ab' should match once
	assert(lug::parse("ab", G, E));
	assert(E.reset_count == 2);
	assert(E.drain_count == 0);
	assert(E.accept_count == 2);
	assert(E.action_counts["accept"] == 1);
	assert(E.match() == "ab");
	assert(E.subject().empty());

	// 'ac' should fail after matching 'a' due to cut preventing backtracking
	assert(!lug::parse("ac", G, E));
	assert(E.reset_count == 3);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["accept"] == 1); // Callback still executed on cut
	assert(E.match() == "a");
	assert(E.subject() == "c");

	// 'cd' should fail immediately without executing callback
	assert(!lug::parse("cd", G, E));
	assert(E.reset_count == 4);
	assert(E.drain_count == 0);
	assert(E.accept_count == 0);
	assert(E.action_counts["accept"] == 0);
	assert(E.match().empty());
	assert(E.subject() == "cd");

	// Two repetitions of 'ab' should match successfully
	assert(lug::parse("abab", G, E));
	assert(E.reset_count == 5);
	assert(E.drain_count == 0);
	assert(E.accept_count == 3);
	assert(E.action_counts["accept"] == 2);
	assert(E.match() == "abab");
	assert(E.subject().empty());

	// 'abac' should fail after matching 'aba' due to cut
	assert(!lug::parse("abac", G, E));
	assert(E.reset_count == 6);
	assert(E.drain_count == 0);
	assert(E.accept_count == 2);
	assert(E.action_counts["accept"] == 2);  // Callback executed twice before failure
	assert(E.match() == "aba");
	assert(E.subject() == "c");

	// Three repetitions of 'ab' should match successfully
	assert(lug::parse("ababab", G, E));
	assert(E.reset_count == 7);
	assert(E.drain_count == 0);
	assert(E.accept_count == 4);
	assert(E.action_counts["accept"] == 3);
	assert(E.match() == "ababab");
	assert(E.subject().empty());

	// 'ababa' should fail after matching 'ababa' due to missing 'b'
	assert(!lug::parse("ababa", G, E));
	assert(E.reset_count == 8);
	assert(E.drain_count == 0);
	assert(E.accept_count == 3);
	assert(E.action_counts["accept"] == 3);  // Callback executed three times before failure
	assert(E.match() == "ababa");
	assert(E.subject().empty());
}

void test_cut_choice()
{
	using namespace lug::language;

	test_environment E;

	// Test cut behavior in: e1 > cut > e2 | e3
	rule A = noskip["a"_sx < [&]{ E.action_counts["cut"]++; } > cut > "b"_sx];
	rule B = noskip["c"_sx > "d"_sx];
	rule S = noskip[(A | B) > eoi];
	grammar G = start(S);

	// Should succeed with complete match
	assert(lug::parse("ab", G, E));
	assert(E.reset_count == 1);
	assert(E.drain_count == 1);
	assert(E.accept_count == 2);
	assert(E.action_counts["cut"] == 1);
	assert(E.match() == "b");
	assert(E.subject().empty());

	// Should succeed with complete match
	assert(lug::parse("cd", G, E));
	assert(E.reset_count == 2);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["cut"] == 0);
	assert(E.match() == "cd");
	assert(E.subject().empty());

	// Should fail - after matching 'a', cut prevents backtracking to try B.
	// However, the cut is deferred, so the callback is called.
	assert(!lug::parse("ac", G, E));
	assert(E.reset_count == 3);
	assert(E.drain_count == 1);
	assert(E.accept_count == 1);
	assert(E.action_counts["cut"] == 1);
	assert(E.match().empty());
	assert(E.subject() == "c");
}

void test_cut_repetition()
{
	using namespace lug::language;

	test_environment E;
	
	// Test cut behavior in *(e1 > cut > e2)
	rule A = noskip["a"_sx < [&]{ E.action_counts["cut"]++; } > cut > "b"_sx];
	rule S = noskip[*A > eoi];
	grammar G = start(S);

	// Empty input should succeed with zero repetitions
	assert(lug::parse("", G, E));
	assert(E.reset_count == 1);
	assert(E.drain_count == 0);
	assert(E.accept_count == 1);
	assert(E.action_counts["cut"] == 0);
	assert(E.match().empty());
	assert(E.subject().empty());

	// Single 'ab' should match once
	assert(lug::parse("ab", G, E));
	assert(E.reset_count == 2);
	assert(E.drain_count == 1);
	assert(E.accept_count == 2);
	assert(E.action_counts["cut"] == 1);
	assert(E.match() == "b");
	assert(E.subject().empty());

	// 'ac' should fail after matching 'a' due to cut preventing backtracking
	assert(!lug::parse("ac", G, E));
	assert(E.reset_count == 3);
	assert(E.drain_count == 1);
	assert(E.accept_count == 1);
	assert(E.action_counts["cut"] == 1); // Callback still executed on cut
	assert(E.match().empty());
	assert(E.subject() == "c");

	// 'cd' should fail immediately without executing callback
	assert(!lug::parse("cd", G, E));
	assert(E.reset_count == 4);
	assert(E.drain_count == 0);
	assert(E.accept_count == 0);
	assert(E.action_counts["cut"] == 0);
	assert(E.match().empty());
	assert(E.subject() == "cd");

	// Two repetitions of 'ab' should match successfully
	assert(lug::parse("abab", G, E));
	assert(E.reset_count == 5);
	assert(E.drain_count == 2);
	assert(E.accept_count == 3);
	assert(E.action_counts["cut"] == 2);
	assert(E.match() == "b");
	assert(E.subject().empty());

	// 'abac' should fail after matching 'aba' due to cut
	assert(!lug::parse("abac", G, E));
	assert(E.reset_count == 6);
	assert(E.drain_count == 2);
	assert(E.accept_count == 2);
	assert(E.action_counts["cut"] == 2);  // Callback executed twice before failure
	assert(E.match().empty());
	assert(E.subject() == "c");

	// Three repetitions of 'ab' should match successfully
	assert(lug::parse("ababab", G, E));
	assert(E.reset_count == 7);
	assert(E.drain_count == 3);
	assert(E.accept_count == 4);
	assert(E.action_counts["cut"] == 3);
	assert(E.match() == "b");
	assert(E.subject().empty());

	// 'ababa' should fail after matching 'ababa' due to missing 'b'
	assert(!lug::parse("ababa", G, E));
	assert(E.reset_count == 8);
	assert(E.drain_count == 3);
	assert(E.accept_count == 3);
	assert(E.action_counts["cut"] == 3);  // Callback executed three times before failure
	assert(E.match().empty());
	assert(E.subject().empty());
}

void test_cut_nested()
{
	using namespace lug::language;
	
	test_environment E;

	// Test cut with more complex grammar
	rule Expr, Term, Factor;
	
	Factor = lexeme[+digit]
	       | '(' > Expr <[&]{ E.action_counts["group"]++; } > cut > ')';

	Term   = Factor > *(
	             '*'  <[&]{ E.action_counts["product"]++; } > cut > Factor
	           | '/'  <[&]{ E.action_counts["product"]++; } > cut > Factor
	       );

	Expr   = Term > *(
	             '+'  <[&]{ E.action_counts["sum"]++; } > cut > Term
	           | '-'  <[&]{ E.action_counts["sum"]++; } > cut > Term
	       );

	grammar G = start(Expr > eoi);
	auto const evaluate = [&](std::string_view input) -> bool { return lug::parse(input, G, E); };

	// Valid expressions
	assert(evaluate("123"));
	assert(E.reset_count == 1 && E.drain_count == 0 && E.accept_count == 1);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 0);

	assert(evaluate("123+456"));
	assert(E.reset_count == 2 && E.drain_count == 1 && E.accept_count == 2);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 1);

	assert(evaluate("123*456"));
	assert(E.reset_count == 3 && E.drain_count == 1 && E.accept_count == 2);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 0);

	assert(evaluate("(123)"));
	assert(E.reset_count == 4 && E.drain_count == 1 && E.accept_count == 2);
	assert(E.action_counts["group"] == 1 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 0);

	assert(evaluate("(123+456)"));
	assert(E.reset_count == 5 && E.drain_count == 2 && E.accept_count == 3);
	assert(E.action_counts["group"] == 1 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 1);

	assert(evaluate("123+456/789"));
	assert(E.reset_count == 6 && E.drain_count == 2 && E.accept_count == 3);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 1);

	assert(evaluate("(123-(456*789))"));
	assert(E.reset_count == 7 && E.drain_count == 4 && E.accept_count == 5);
	assert(E.action_counts["group"] == 2 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 1);
	
	// Invalid expressions - cut prevents backtracking
	assert(!evaluate("123+"));      // Missing operand after cut
	assert(E.reset_count == 8 && E.drain_count == 1 && E.accept_count == 1);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 1);

	assert(!evaluate("123*"));      // Missing operand after cut
	assert(E.reset_count == 9 && E.drain_count == 1 && E.accept_count == 1);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 0);

	assert(!evaluate("(123"));      // Missing closing paren after cut
	assert(E.reset_count == 10 && E.drain_count == 1 && E.accept_count == 1);
	assert(E.action_counts["group"] == 1 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 0);

	assert(!evaluate("(123+)"));    // Missing operand after cut
	assert(E.reset_count == 11 && E.drain_count == 1 && E.accept_count == 1);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 0 && E.action_counts["sum"] == 1);

	assert(!evaluate("123-456*789+")); // Missing operand after cut
	assert(E.reset_count == 12 && E.drain_count == 3 && E.accept_count == 3);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 2);

	assert(!evaluate("123+456/789*")); // Missing operand after cut
	assert(E.reset_count == 13 && E.drain_count == 3 && E.accept_count == 3);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 2 && E.action_counts["sum"] == 1);

	assert(!evaluate("(123+(456*789)+)"));    // Missing operand after cut
	assert(E.reset_count == 14 && E.drain_count == 4 && E.accept_count == 4);
	assert(E.action_counts["group"] == 1 && E.action_counts["product"] == 1 && E.action_counts["sum"] == 2);

	assert(!evaluate("(123+(456/789))*"));    // Missing operand after cut
	assert(E.reset_count == 15 && E.drain_count == 5 && E.accept_count == 5);
	assert(E.action_counts["group"] == 2 && E.action_counts["product"] == 2 && E.action_counts["sum"] == 1);

	assert(!evaluate("(123-(456*789*)"));    // Missing operand after cut
	assert(E.reset_count == 16 && E.drain_count == 3 && E.accept_count == 3);
	assert(E.action_counts["group"] == 0 && E.action_counts["product"] == 2 && E.action_counts["sum"] == 1);
}

int main()
{
	try {
		test_accept_choice();
		test_accept_repetition();
		test_cut_choice();
		test_cut_repetition();
		test_cut_nested();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

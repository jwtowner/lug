// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_direct_left_recursion()
{
	using namespace lug::language;
	grammar::implicit_space = nop;
	rule R, S;
	R = R > chr('a') | chr('a');
	S = R > !chr('a');
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aab", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aaa2", G));
	assert(lug::parse("aaaa", G));
	assert(lug::parse("aaaak", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
}

void test_indirect_left_recursion()
{
	using namespace lug::language;
	grammar::implicit_space = nop;
	rule Q, R, S;
	Q = R > chr('a');
	R = Q | chr('a');
	S = R > !chr('a');
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aab", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aaa2", G));
	assert(lug::parse("aaaa", G));
	assert(lug::parse("aaaak", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
}

void test_association_and_precedence()
{
	using namespace lug::language;
	grammar::implicit_space = nop;
	std::string out;
	rule N, E, S;
	N = chr('1') | chr('2') | chr('3');
	E = E[1] > chr('+') > E[2] <[&out]{ out += '+'; }
	  | E[2] > chr('*') > E[3] <[&out]{ out += '*'; }
	  | N <[&out](csyntax& x){ out += x.capture(); };
	S = E > eoi;
	grammar G = start(S);
	out.clear();
	assert(lug::parse("1", G) && out == "1");
	out.clear();
	assert(lug::parse("1+2", G) && out == "12+");
	out.clear();
	assert(lug::parse("3*1", G) && out == "31*");
	out.clear();
	assert(lug::parse("1*2+3*2", G) && out == "12*32*+");
	out.clear();
	assert(lug::parse("2+2*3+1", G) && out == "223*+1+");
	out.clear();
	assert(lug::parse("2+2*3+1*2*3+1", G) && out == "223*+12*3*+1+");
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("1+", G));
}

/* NOTE: Medeiros' algorithm doesn't appear to support hidden left recursion
void test_hidden_left_recursion()
{
	using namespace lug::language;
	grammar::implicit_space = nop;
	rule R = *chr('b') > R > chr('a') | chr('a');
	rule S = R > !(chr('a') | chr('b'));
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("baa", G));
	assert(lug::parse("aag", G));
	assert(lug::parse("abah", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aabbba", G));
	assert(lug::parse("aaa7", G));
	assert(lug::parse("abbaa3", G));
	assert(lug::parse("aaaa", G));
	assert(lug::parse("bbaabbbaa", G));
	assert(lug::parse("aaaak", G));
	assert(lug::parse("bbababbbbaaaai", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("z", G));
}*/

int main()
{
	try {
		test_direct_left_recursion();
		test_indirect_left_recursion();
		test_association_and_precedence();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

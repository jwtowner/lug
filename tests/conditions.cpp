// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <iostream>

#undef NDEBUG
#include <cassert>

void test_condition()
{
	using namespace lug::language;

	rule S;

	// Define a rule that matches "ab" when condition "accept_ab" is true,
	// otherwise matches "a"
	S = when("accept_ab") > "ab"_sx
	  | unless("accept_ab") > "a"_sx;
	environment E;
	grammar G = start(S > eoi);
	
	// Initially, condition is false (default), so "a" should match
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
	
	// Set condition to true, now "ab" should match instead
	E.set_condition("accept_ab", true);
	assert(!lug::parse("a", G, E));
	assert(lug::parse("ab", G, E));
	
	// Set condition back to false, "a" should match again
	E.set_condition("accept_ab", false);
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
}

void test_condition_block()
{
	using namespace lug::language;

	rule S;

	// Define a recursive rule that:
	// - When "accept_ab" is true: matches "ab" and then turns off the condition for nested matches
	// - When "accept_ab" is false: matches "a" and then turns on the condition for nested matches
	S = when("accept_ab") > "ab"_sx > ~off("accept_ab")[ S ]
	  | unless("accept_ab") > "a"_sx > ~on("accept_ab")[ S ];
	grammar G = start(S > eoi);
	
	// This should match the alternating pattern: "a ab a ab"
	// Starting with "accept_ab" = false, matching "a", turning on "accept_ab"
	// Then matching "ab", turning off "accept_ab"
	// Then matching "a", turning on "accept_ab"
	// Finally matching "ab"
	assert(lug::parse("a ab a ab", G));
	
	// These patterns don't follow the alternating rule structure
	assert(!lug::parse("a a ab a ab", G));    // Can't have two "a"s in a row
	assert(!lug::parse("a ab ab a ab", G));   // Can't have two "ab"s in a row
	assert(!lug::parse("ab a ab a", G));      // Can't start with "ab" (condition starts false)
	assert(!lug::parse("ab ab a ab a", G));   // Can't start with "ab" and can't have two "ab"s in a row
}

int main()
try {
	test_condition();
	test_condition_block();
	return 0;
} catch (std::exception const& e) {
	std::cerr << "Error: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "Unknown Error\n";
	return 1;
}

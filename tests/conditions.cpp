// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_condition()
{
	using namespace lug::language;
	rule S;
	S = when("accept_ab") > "ab"_sx
	  | unless("accept_ab") > "a"_sx;
	environment E;
	grammar G = start(S > eoi);
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
	E.set_condition("accept_ab", true);
	assert(!lug::parse("a", G, E));
	assert(lug::parse("ab", G, E));
	E.set_condition("accept_ab", false);
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
}

void test_condition_block()
{
	using namespace lug::language;
	rule S;
	S = when("accept_ab") > "ab"_sx > ~off("accept_ab")[ S ]
	  | unless("accept_ab") > "a"_sx > ~on("accept_ab")[ S ];
	grammar G = start(S > eoi);
	assert(lug::parse("a ab a ab", G));
	assert(!lug::parse("a a ab a ab", G));
	assert(!lug::parse("a ab ab a ab", G));
	assert(!lug::parse("ab a ab a", G));
	assert(!lug::parse("ab ab a ab a", G));
}

int main()
{
	try {
		test_condition();
		test_condition_block();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

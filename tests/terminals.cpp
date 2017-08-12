// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cassert>

void test_any_terminal()
{
	using namespace lug::language;
	rule S = lug::any_terminal{} > !lug::any_terminal{};
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("2", G));
	assert(lug::parse("z", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("", G));
}

void test_empty_terminal()
{
	using namespace lug::language;
	rule S = lug::empty_terminal{} > !lug::any_terminal{};
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("z", G));
	assert(!lug::parse("aa", G));
	assert(lug::parse("", G));
}

void test_char_terminal()
{
	using namespace lug::language;
	rule S = lug::char_terminal{'a'} > !lug::any_terminal{};
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
}

void test_terminal_sequence()
{
	using namespace lug::language;
	constexpr auto A = lug::any_terminal{};
	using C = lug::char_terminal;
	rule S = C{'a'} > A > C{'b'} > !A;
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(lug::parse("a2b", G));
	assert(!lug::parse("aza", G));
	assert(lug::parse("azb", G));
	assert(!lug::parse("azb3", G));
	assert(!lug::parse("", G));
}

void test_terminal_choice()
{
	using namespace lug::language;
	constexpr auto A = lug::any_terminal{};
	using C = lug::char_terminal;
	rule S = (C{'a'} > A | C{'b'}) > !A;
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(lug::parse("a2", G));
	assert(lug::parse("b", G));
	assert(!lug::parse("azb", G));
	assert(!lug::parse("b3", G));
	assert(!lug::parse("", G));
}

int main()
{
	try {
		test_any_terminal();
		test_empty_terminal();
		test_char_terminal();
		test_terminal_sequence();
		test_terminal_choice();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug.hpp>
#include <cassert>

void test_any_terminal()
{
	using namespace lug::lang;
	rule Body = any_terminal{} > !any_terminal{};
	grammar Grammar = start(Body);
	assert(lug::parse("a", Grammar));
	assert(lug::parse("2", Grammar));
	assert(lug::parse("z", Grammar));
	assert(!lug::parse("aa", Grammar));
	assert(!lug::parse("", Grammar));
}

void test_char_terminal()
{
	using namespace lug::lang;
	rule Body = 'a' > !any_terminal{};
	grammar Grammar = start(Body);
	assert(lug::parse("a", Grammar));
	assert(!lug::parse("2", Grammar));
	assert(!lug::parse("aa", Grammar));
	assert(!lug::parse("", Grammar));
	assert(!lug::parse("b", Grammar));
}

void test_empty_terminal()
{
	using namespace lug::lang;
	rule Body = empty_terminal{} > !any_terminal{};
	grammar Grammar = start(Body);
	assert(!lug::parse("a", Grammar));
	assert(!lug::parse("2", Grammar));
	assert(!lug::parse("z", Grammar));
	assert(!lug::parse("aa", Grammar));
	assert(lug::parse("", Grammar));
}

void test_terminal_sequence()
{
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule Body = C{'a'} > A > C{'b'} > !A;
	grammar Grammar = start(Body);
	assert(!lug::parse("a", Grammar));
	assert(lug::parse("a2b", Grammar));
	assert(!lug::parse("aza", Grammar));
	assert(lug::parse("azb", Grammar));
	assert(!lug::parse("azb3", Grammar));
	assert(!lug::parse("", Grammar));
}

void test_terminal_choice()
{
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule Body = (C{'a'} > A | C{'b'}) > !A;
	grammar Grammar = start(Body);
	assert(!lug::parse("a", Grammar));
	assert(lug::parse("a2", Grammar));
	assert(lug::parse("b", Grammar));
	assert(!lug::parse("azb", Grammar));
	assert(!lug::parse("b3", Grammar));
	assert(!lug::parse("", Grammar));
}

int main(int argc, char** argv)
{
	try {
		test_any_terminal();
		test_char_terminal();
		test_empty_terminal();
		test_terminal_sequence();
		test_terminal_choice();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

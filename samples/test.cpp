// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug.hpp>
#include <cassert>

void test_any_terminals()
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

void test_char_terminals()
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

int main(int argc, char** argv)
{
	try {
		test_any_terminals();
		test_char_terminals();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

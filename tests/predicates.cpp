// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cassert>

void test_simple_predicates()
{
	using namespace lug::language;
	rule S = ("a"s > []{ return false; } | []{ return true; } > "ab");
	grammar G = start(S > !"."s);
	assert(!lug::parse("a", G));
	assert(lug::parse("ab", G));
}

void test_input_position_predicate()
{
	using namespace lug::language;
	rule S = +("a"s > [](parser& p){ return p.input_position() <= 4; });
	grammar G = start(S > !"."s);
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aaaa", G));
	assert(!lug::parse("aaaaa", G));
}

int main()
{
	try {
		test_simple_predicates();
		test_input_position_predicate();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

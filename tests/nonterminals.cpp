// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_sequence()
{
	using namespace lug::language;
	rule S = noskip[ chr('a') > any > chr('b') > eoi ];
	grammar G = start(S);
	assert(lug::parse("a2b", G));
	assert(lug::parse("azb", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("aza", G));
	assert(!lug::parse("azb3", G));
	assert(!lug::parse("a z b", G));
	assert(!lug::parse(" a z b", G));
}

void test_choice()
{
	using namespace lug::language;
	rule S = noskip[ (chr('a') | chr('b')) > eoi ];
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("b", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("ab", G));
	assert(!lug::parse("ba", G));
	assert(!lug::parse(" a", G));
}

void test_zero_or_one()
{
	using namespace lug::language;
	rule S = noskip[ ~chr('x') > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("x", G));
	assert(!lug::parse("xx", G));
	assert(!lug::parse("xxxxxxx", G));
	assert(!lug::parse("y", G));
	assert(!lug::parse("xy", G));
	assert(!lug::parse("xxxxxy", G));
}

void test_zero_or_many()
{
	using namespace lug::language;
	rule S = noskip[ *chr('x') > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("x", G));
	assert(lug::parse("xx", G));
	assert(lug::parse("xxxxxxx", G));
	assert(!lug::parse("y", G));
	assert(!lug::parse("xy", G));
	assert(!lug::parse("xxxxxy", G));
}

void test_one_or_many()
{
	using namespace lug::language;
	rule S = noskip[ +chr('x') > eoi ];
	grammar G = start(S);
	assert(lug::parse("x", G));
	assert(lug::parse("xx", G));
	assert(lug::parse("xxxxxxx", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("y", G));
	assert(!lug::parse("xy", G));
	assert(!lug::parse("xxxxxy", G));
	assert(!lug::parse(" xx", G));
	assert(!lug::parse("x x", G));
	assert(!lug::parse("xx ", G));
}

void test_not()
{
	using namespace lug::language;
	rule S = noskip[ !chr('x') > any > eoi ];
	grammar G = start(S);
	assert(lug::parse("y", G));
	assert(lug::parse("Z", G));
	assert(lug::parse("2", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("x", G));
	assert(!lug::parse("xx", G));
	assert(!lug::parse("y2", G));
	assert(!lug::parse("yx", G));
	assert(!lug::parse(" 2", G));
}

void test_predicate()
{
	using namespace lug::language;
	rule S = noskip[ &chr('x') > any > any > eoi ];
	grammar G = start(S);
	assert(lug::parse("xx", G));
	assert(lug::parse("xy", G));
	assert(lug::parse("xZ", G));
	assert(lug::parse("x2", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("y", G));
	assert(!lug::parse("Z", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("x", G));
	assert(!lug::parse("y2", G));
	assert(!lug::parse(" xx", G));
	assert(!lug::parse("x  x", G));
}

int main()
{
	try {
		test_sequence();
		test_choice();
		test_zero_or_one();
		test_zero_or_many();
		test_one_or_many();
		test_not();
		test_predicate();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

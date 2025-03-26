// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <iostream>

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
	assert(!lug::parse("azb ", G));
	assert(!lug::parse("azb3", G));
	assert(!lug::parse("a z b", G));
	assert(!lug::parse(" a z b", G));
}

void test_sequence_with_skip()
{
	using namespace lug::language;
	rule S = chr('a') > any > chr('b') > eoi;
	grammar G = start(S);
	assert(lug::parse("a2b", G));
	assert(lug::parse("azb", G));
	assert(lug::parse("ak b", G));
	assert(lug::parse("a z b", G));
	assert(lug::parse(" a g b ", G));
	assert(lug::parse("a  c  b", G));
	assert(lug::parse("\ta\nz\tb", G));
	assert(lug::parse("a\td\nb ", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("aza", G));
	assert(!lug::parse("azb3", G));
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

void test_choice_with_skip()
{
	using namespace lug::language;
	rule S = (chr('a') | chr('b')) > eoi;
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("b", G));
	assert(lug::parse(" a", G));
	assert(lug::parse("a ", G));
	assert(lug::parse(" a ", G));
	assert(lug::parse(" b", G));
	assert(lug::parse("b ", G));
	assert(lug::parse(" b ", G));
	assert(lug::parse("\ta\n", G));
	assert(lug::parse("\n\tb\t", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("ab", G));
	assert(!lug::parse("ba", G));
	assert(!lug::parse("c", G));
	assert(!lug::parse(" c ", G));
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

void test_zero_or_one_string()
{
	using namespace lug::language;
	rule S = noskip[ ~str("abc") > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("abc", G));
	assert(!lug::parse("abcabc", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("ab", G));
	assert(!lug::parse("abca", G));
	assert(!lug::parse("abcabz", G));
	assert(!lug::parse("xxx", G));
	assert(!lug::parse(" ", G));
	assert(!lug::parse(" abc", G));
	assert(!lug::parse("abc ", G));
	assert(!lug::parse("abc abc", G));
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
	assert(!lug::parse(" ", G));
	assert(!lug::parse(" xx", G));
	assert(!lug::parse("x x", G));
	assert(!lug::parse("xx ", G));
}

void test_zero_or_many_string()
{
	using namespace lug::language;
	rule S = noskip[ *str("abc") > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("abc", G));
	assert(lug::parse("abcabc", G));
	assert(lug::parse("abcabcabcabc", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("ab", G));
	assert(!lug::parse("abcab", G));
	assert(!lug::parse("abcabz", G));
	assert(!lug::parse("abcabcab", G));
	assert(!lug::parse(" ", G));
	assert(!lug::parse(" abc", G));
	assert(!lug::parse("abc ", G));
	assert(!lug::parse("abc abc", G));
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

void test_one_or_many_string()
{
	using namespace lug::language;
	rule S = noskip[ +str("abc") > eoi ];
	grammar G = start(S);
	assert(lug::parse("abc", G));
	assert(lug::parse("abcabc", G));
	assert(lug::parse("abcabcabcabc", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("ab", G));
	assert(!lug::parse("abcab", G));
	assert(!lug::parse("abcabx", G));
	assert(!lug::parse(" abc", G));
	assert(!lug::parse("abc ", G));
	assert(!lug::parse("abc abc", G));
}

void test_repeat()
{
	using namespace lug::language;
	rule S = noskip[ repeat<3, 5>[ chr('x') ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("xxx", G));
	assert(lug::parse("xxxx", G));
	assert(lug::parse("xxxxx", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("xx", G));
	assert(!lug::parse("xxxxxx", G));
	assert(!lug::parse("xxxxxxx", G));
	assert(!lug::parse("aaa", G));
	assert(!lug::parse("aaaa", G));
	assert(!lug::parse("aaaaa", G));
}

void test_repeat_string()
{
	using namespace lug::language;
	rule S = noskip[ repeat<3, 5>[ str("abc") ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("abcabcabc", G));
	assert(lug::parse("abcabcabcabc", G));
	assert(lug::parse("abcabcabcabcabc", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("abc", G));
	assert(!lug::parse("abcabc", G));
	assert(!lug::parse("abcabcab", G));
	assert(!lug::parse("abcabcabcaz", G));
	assert(!lug::parse(" abcabcabc", G));
	assert(!lug::parse("abcabcabc ", G));
	assert(!lug::parse("abcabcabc abc", G));
}

void test_at_least()
{
	using namespace lug::language;
	rule S = noskip[ at_least<2>[ chr('x') ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("xx", G));
	assert(lug::parse("xxx", G));
	assert(lug::parse("xxxx", G));
	assert(lug::parse("xxxxxxxxxxxxxxxx", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("x", G));
	assert(!lug::parse("xa", G));
	assert(!lug::parse("xxa", G));
	assert(!lug::parse("xx ", G));
	assert(!lug::parse(" xx", G));
	assert(!lug::parse("x x", G));
	assert(!lug::parse("xx abc", G));
}

void test_at_least_string()
{
	using namespace lug::language;
	rule S = noskip[ at_least<2>[ str("abc") ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("abcabc", G));
	assert(lug::parse("abcabcabcabc", G));
	assert(lug::parse("abcabcabcabcabc", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("abc", G));
	assert(!lug::parse("abcab", G));
	assert(!lug::parse("abcabz", G));
	assert(!lug::parse("abcabcabcabcabcab", G));
	assert(!lug::parse(" abcabcabcabc", G));
	assert(!lug::parse("abcabcabcabc ", G));
	assert(!lug::parse("abc abc abc abc", G));
}

void test_at_most()
{
	using namespace lug::language;
	rule S = noskip[ at_most<4>[ chr('x') ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("x", G));
	assert(lug::parse("xx", G));
	assert(lug::parse("xxx", G));
	assert(lug::parse("xxxx", G));
	assert(!lug::parse("xxxxx", G));
	assert(!lug::parse("xxxxxxxxxxxxxxxx", G));
	assert(!lug::parse("xa", G));
	assert(!lug::parse("xxa", G));
	assert(!lug::parse("xx ", G));
	assert(!lug::parse(" xx", G));
	assert(!lug::parse("x x", G));
	assert(!lug::parse("xx abc", G));
}

void test_at_most_string()
{
	using namespace lug::language;
	rule S = noskip[ at_most<4>[ str("abc") ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("", G));
	assert(lug::parse("abc", G));
	assert(lug::parse("abcabc", G));
	assert(lug::parse("abcabcabc", G));
	assert(lug::parse("abcabcabcabc", G));
	assert(!lug::parse("abcabcabcabcabc", G));
	assert(!lug::parse("abcabcabcabcabcabc", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("abcab", G));
	assert(!lug::parse("abcabz", G));
	assert(!lug::parse("abcabcabcabcabcab", G));
	assert(!lug::parse(" abcabcabcabc", G));
	assert(!lug::parse("abcabcabcabc ", G));
	assert(!lug::parse("abc abc abc abc", G));
}

void test_exactly()
{
	using namespace lug::language;
	rule S = noskip[ exactly<4>[ chr('x') ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("xxxx", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("xxx", G));
	assert(!lug::parse("xxxxx", G));
	assert(!lug::parse("aaaa", G));
}

void test_exactly_string()
{
	using namespace lug::language;
	rule S = noskip[ exactly<4>[ str("abc") ] > eoi ];
	grammar G = start(S);
	assert(lug::parse("abcabcabcabc", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(!lug::parse("abc", G));
	assert(!lug::parse("abcabc", G));
	assert(!lug::parse("abcabcabc", G));
	assert(!lug::parse("abcabcabcabcabc", G));
	assert(!lug::parse("abcabcabcabz", G));
	assert(!lug::parse("abcabcabca", G));
	assert(!lug::parse(" abcabcabcabc", G));
	assert(!lug::parse("abcabcabcabc ", G));
	assert(!lug::parse("abc abc abc abc", G));
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

void test_list()
{
	using namespace lug::language;
	rule S = noskip[ chr('a') >> "," > eoi ];
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("a,a", G));
	assert(lug::parse("a,a,a", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("a,", G));
	assert(!lug::parse("a,a,", G));
	assert(!lug::parse("a,b", G));
	assert(!lug::parse("azb3", G));
	assert(!lug::parse("a z b", G));
	assert(!lug::parse(" a z b", G));
	assert(!lug::parse("a, a, a", G));
}

int main()
try {
	test_sequence();
	test_sequence_with_skip();
	test_choice();
	test_choice_with_skip();
	test_zero_or_one();
	test_zero_or_one_string();
	test_zero_or_many();
	test_zero_or_many_string();
	test_one_or_many();
	test_one_or_many_string();
	test_repeat();
	test_repeat_string();
	test_at_least();
	test_at_least_string();
	test_at_most();
	test_at_most_string();
	test_exactly();
	test_exactly_string();
	test_not();
	test_predicate();
	test_list();
	return 0;
} catch (std::exception const& e) {
	std::cerr << "Error: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "Unknown Error\n";
	return 1;
}

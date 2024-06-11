// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_simple_predicates()
{
	using namespace lug::language;
	rule S = "a"_sx > [](environment&){ return false; } | [](environment&){ return true; } > "ab";
	grammar G = start(S > eoi);
	assert(!lug::parse("a", G));
	assert(lug::parse("ab", G));
}

void test_subject_index_predicate()
{
	using namespace lug::language;
	rule S = +("a"_sx > [](environment& e){ return e.parser().subject_index() <= 4; });
	grammar G = start(S > eoi);
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
		test_subject_index_predicate();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	}
	return 0;
}

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

void test_condition_by_bool_reference()
{
	using namespace lug::language;
	bool accept_ab = false;
	rule S = when(accept_ab) > "ab"_sx | unless(accept_ab) > "a"_sx;
	grammar G = start(S > eoi);
	assert(lug::parse("a", G));
	assert(!lug::parse("ab", G));
	accept_ab = true;
	assert(!lug::parse("a", G));
	assert(lug::parse("ab", G));
	accept_ab = false;
	assert(lug::parse("a", G));
	assert(!lug::parse("ab", G));
}

void test_condition_by_bool_reference_dynamic()
{
	using namespace lug::language;
    bool accept_ab = false;
	rule S = when(accept_ab) > "ab"_sx > unset(accept_ab)
	       | unless(accept_ab) > "a"_sx > set(accept_ab);
	grammar G = start(+S > eoi);
	assert(lug::parse("a ab a ab", G));
    accept_ab = false;
	assert(!lug::parse("a a ab a ab", G));
    accept_ab = false;
	assert(!lug::parse("a ab ab a ab", G));
    accept_ab = false;
	assert(!lug::parse("ab a ab a", G));
    accept_ab = false;
	assert(!lug::parse("ab ab a ab a", G));
}

void test_condition_by_name()
{
	using namespace lug::language;
	rule S = when("accept_ab") > "ab"_sx | unless("accept_ab") > "a"_sx;
	environment E;
	grammar G = start(S > eoi);
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
	E.set_condition("accept_ab");
	assert(!lug::parse("a", G, E));
	assert(lug::parse("ab", G, E));
	E.unset_condition("accept_ab");
	assert(lug::parse("a", G, E));
	assert(!lug::parse("ab", G, E));
}

void test_condition_by_name_dynamic()
{
	using namespace lug::language;
	rule S = when("accept_ab") > "ab"_sx > unset("accept_ab")
	       | unless("accept_ab") > "a"_sx > set("accept_ab");
	grammar G = start(+S > eoi);
	assert(lug::parse("a ab a ab", G));
	assert(!lug::parse("a a ab a ab", G));
	assert(!lug::parse("a ab ab a ab", G));
	assert(!lug::parse("ab a ab a", G));
	assert(!lug::parse("ab ab a ab a", G));
}

void test_condition_by_predicate()
{
	using namespace lug::language;
	rule S = "a"_sx > when([]{ return false; }) | unless([]{ return false; }) > "ab";
	grammar G = start(S > eoi);
	assert(lug::parse("ab", G));
	assert(!lug::parse("a", G));
}

int main()
{
	try {
		test_condition_by_bool_reference();
        test_condition_by_bool_reference_dynamic();
		test_condition_by_name();
		test_condition_by_name_dynamic();
		test_condition_by_predicate();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	}
	return 0;
}

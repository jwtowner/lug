// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug.hpp>
#include <cassert>

void test_any_terminal() {
	using namespace lug::lang;
	rule S = any_terminal{} > !any_terminal{};
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(lug::parse("2", G));
	assert(lug::parse("z", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("", G));
}

void test_char_terminal() {
	using namespace lug::lang;
	rule S = 'a' > !any_terminal{};
	grammar G = start(S);
	assert(lug::parse("a", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("aa", G));
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
}

void test_empty_terminal() {
	using namespace lug::lang;
	rule S = empty_terminal{} > !any_terminal{};
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(!lug::parse("2", G));
	assert(!lug::parse("z", G));
	assert(!lug::parse("aa", G));
	assert(lug::parse("", G));
}

void test_terminal_sequence() {
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule S = C{'a'} > A > C{'b'} > !A;
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(lug::parse("a2b", G));
	assert(!lug::parse("aza", G));
	assert(lug::parse("azb", G));
	assert(!lug::parse("azb3", G));
	assert(!lug::parse("", G));
}

void test_terminal_choice() {
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule S = (C{'a'} > A | C{'b'}) > !A;
	grammar G = start(S);
	assert(!lug::parse("a", G));
	assert(lug::parse("a2", G));
	assert(lug::parse("b", G));
	assert(!lug::parse("azb", G));
	assert(!lug::parse("b3", G));
	assert(!lug::parse("", G));
}

void test_direct_left_recursion() {
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule S = (S > C{'a'} | C{'a'}) ;
	grammar G = start(S > !C{'a'});
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aab", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aaa2", G));
	assert(lug::parse("aaaa", G));
	assert(lug::parse("aaaak", G));
}

void test_indirect_left_recursion() {
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule Q, S;
	Q = S > C{'a'};
	S = Q | C{'a'};
	grammar G = start(S > !C{'a'});
	assert(!lug::parse("", G));
	assert(!lug::parse("b", G));
	assert(lug::parse("a", G));
	assert(lug::parse("aa", G));
	assert(lug::parse("aab", G));
	assert(lug::parse("aaa", G));
	assert(lug::parse("aaa2", G));
	assert(lug::parse("aaaa", G));
	assert(lug::parse("aaaak", G));
}

void test_left_association() {
	using namespace lug::lang;
	constexpr auto A = any_terminal{};
	using C = char_terminal;
	rule N = C{'1'} | C{'2'} | C{'3'};
	rule E = E(1) > C{'+'} > E(2) | N;
	grammar G = start(E > !A);
	assert(!lug::parse("", G));
	assert(!lug::parse("a", G));
	assert(lug::parse("1", G));
	assert(lug::parse("1+2", G));
	assert(lug::parse("1+2+3", G));
	assert(lug::parse("1+2+3+2", G));
	assert(!lug::parse("1+a", G));
}

int main(int argc, char** argv) {
	try {
		test_any_terminal();
		test_char_terminal();
		test_empty_terminal();
		test_terminal_sequence();
		test_terminal_choice();
		test_direct_left_recursion();
		test_indirect_left_recursion();
		test_left_association();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

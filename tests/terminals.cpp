// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cassert>

namespace
{
	void test_empty()
	{
		using namespace lug::language;
		rule S = noskip[ eps > eoi ];
		grammar G = start(S);
		assert(lug::parse("", G));
		assert(!lug::parse("a", G));
		assert(!lug::parse("2", G));
		assert(!lug::parse("z", G));
		assert(!lug::parse("aa", G));
	}

	void test_any()
	{
		using namespace lug::language;
		rule S = noskip[ any > eoi ];
		grammar G = start(S);
		assert(lug::parse("a", G));
		assert(lug::parse("2", G));
		assert(lug::parse("z", G));
		assert(!lug::parse("aa", G));
		assert(!lug::parse("", G));
		assert(!lug::parse(" a", G));
	}

	void test_char()
	{
		using namespace lug::language;
		rule S = noskip[ chr('a') > eoi ];
		grammar G = start(S);
		assert(lug::parse("a", G));
		assert(!lug::parse("", G));
		assert(!lug::parse("2", G));
		assert(!lug::parse("aa", G));
		assert(!lug::parse("b", G));
		assert(!lug::parse(" a", G));
	}

	void test_char_range()
	{
		using namespace lug::language;

		rule S1 = noskip[ chr('d', 'g') > eoi ];
		grammar G1 = start(S1);
		assert(lug::parse("d", G1));
		assert(lug::parse("e", G1));
		assert(lug::parse("f", G1));
		assert(lug::parse("g", G1));
		assert(!lug::parse("", G1));
		assert(!lug::parse("c", G1));
		assert(!lug::parse("h", G1));
		assert(!lug::parse("dd", G1));
		assert(!lug::parse(" e", G1));
		assert(!lug::parse("f ", G1));
		assert(!lug::parse(" g ", G1));

		rule S2 = noskip[ chr('b', 'b') > eoi ];
		grammar G2 = start(S2);
		assert(lug::parse("b", G2));
		assert(!lug::parse("", G2));
		assert(!lug::parse("a", G2));
		assert(!lug::parse("c", G2));
		assert(!lug::parse("bb", G2));
		assert(!lug::parse(" b", G2));
		assert(!lug::parse("b ", G2));
	}

	void test_string()
	{
		using namespace lug::language;
		rule S = noskip[ str("hello world") > eoi ];
		grammar G = start(S);
		assert(lug::parse("hello world", G));
		assert(!lug::parse("hello world!", G));
		assert(!lug::parse("hello", G));
		assert(!lug::parse("h", G));
		assert(!lug::parse("", G));
	}

	void test_regular_expression()
	{
		using namespace lug::language;
		rule S = noskip[ bre("hello.w[oO]rld[[:digit:]]") > eoi ];
		grammar G = start(S);
		assert(lug::parse("hello world4", G));
		assert(lug::parse("hello_wOrld8", G));
		assert(!lug::parse("hello world!", G));
		assert(!lug::parse("hello", G));
		assert(!lug::parse("h", G));
		assert(!lug::parse("", G));
	}
}

int main()
{
	try {
		test_empty();
		test_any();
		test_char();
		test_char_range();
		test_string();
		test_regular_expression();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

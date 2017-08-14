// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cassert>

namespace
{
	void test_direct_left_recursion()
	{
		using namespace lug::language;
		rule R = R >> lug::chr('a') | lug::chr('a');
		rule S = R >> !lug::chr('a');
		grammar G = start(S);
		assert(lug::parse("a", G));
		assert(lug::parse("aa", G));
		assert(lug::parse("aab", G));
		assert(lug::parse("aaa", G));
		assert(lug::parse("aaa2", G));
		assert(lug::parse("aaaa", G));
		assert(lug::parse("aaaak", G));
		assert(!lug::parse("", G));
		assert(!lug::parse("b", G));
	}

	void test_indirect_left_recursion()
	{
		using namespace lug::language;
		rule Q, R, S;
		Q = R >> lug::chr('a');
		R = Q | lug::chr('a');
		S = R >> !lug::chr('a');
		grammar G = start(S);
		assert(lug::parse("a", G));
		assert(lug::parse("aa", G));
		assert(lug::parse("aab", G));
		assert(lug::parse("aaa", G));
		assert(lug::parse("aaa2", G));
		assert(lug::parse("aaaa", G));
		assert(lug::parse("aaaak", G));
		assert(!lug::parse("", G));
		assert(!lug::parse("b", G));
	}

	void test_association_and_precedence()
	{
		using namespace lug::language;
		std::string out;
		rule N	= lug::chr('1') | lug::chr('2') | lug::chr('3');
		rule E	= E[1] >> lug::chr('+') >> E[2] <[&out]{ out += '+'; }
				| E[2] >> lug::chr('*') >> E[3] <[&out]{ out += '*'; }
				| N <[&out](semantics&, syntax x){ out += x.capture; };
		rule S = E >> lug::eoi;
		grammar G = start(S);
		out.clear();
		assert(lug::parse("1", G) && out == "1");
		out.clear();
		assert(lug::parse("1+2", G) && out == "12+");
		out.clear();
		assert(lug::parse("3*1", G) && out == "31*");
		out.clear();
		assert(lug::parse("1*2+3*2", G) && out == "12*32*+");
		out.clear();
		assert(lug::parse("2+2*3+1", G) && out == "223*+1+");
		out.clear();
		assert(lug::parse("2+2*3+1*2*3+1", G) && out == "223*+12*3*+1+");
		assert(!lug::parse("", G));
		assert(!lug::parse("a", G));
		assert(!lug::parse("1+", G));
	}
}

int main()
{
	try {
		test_direct_left_recursion();
		test_indirect_left_recursion();
		test_association_and_precedence();
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug.hpp>

namespace calc
{
	using namespace lug::language;

	extern rule Expr;

	long long variables[26];
	long long e, l, n, r, s;
	int i;

	rule SPACE  = *" \t"_bracket;
	rule EOL    = "\n"_literal | "\r\n"_literal | "\r"_literal | ";"_literal;

	rule NUMBER = +"0-9"_bracket >= SPACE <= [](std::string t) { return std::stoll(t); };
	rule ID     = "a-z"_bracket >= SPACE <= [](std::string t) -> int { return t[0] - 'a'; };

	rule ASSIGN = "="_literal > SPACE;
	rule PLUS   = "+"_literal > SPACE;
	rule MINUS  = "-"_literal > SPACE;
	rule TIMES  = "*"_literal > SPACE;
	rule DIVIDE = "/"_literal > SPACE;
	rule OPEN   = "("_literal > SPACE;
	rule CLOSE  = ")"_literal > SPACE;

	rule Value =
		n%NUMBER                < [&]() { return n; }
		| i%ID > !ASSIGN        < [&]() { return variables[i]; }
		| OPEN > e%Expr > CLOSE < [&]() { return e; };

	rule Product =
		l%Value > *(
			TIMES > r%Value     < [&]() { l *= r; }
			| DIVIDE > r%Value  < [&]() { l /= r; }
		)                       < [&]() { return l; };

	rule Sum =
		l%Product > *(
			PLUS > r%Product    < [&]() { l += r; }
			| MINUS > r%Product < [&]() { l -= r; }
		)                       < [&]() { return l; };

	rule Expr =
		i%ID > ASSIGN > s%Sum   < [&]() { return variables[i] = s; }
		| s%Sum                 < [&]() { return s; };

	rule Stmt =
		SPACE > e%Expr > EOL    < [&]() { std::cout << e << std::endl; }
		| *(!EOL > _any) > EOL  < []() { std::cerr << "syntax error" << std::endl; };

	grammar Grammar = Stmt;
}

int main()
{
	try {
		while(lug::parse(calc::Grammar));
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

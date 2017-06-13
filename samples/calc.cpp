// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug.hpp>

namespace calc
{
	using namespace lug::lang;

	long long variables[26];
	long long e, l, n, r, s;
	int i;

	rule SPACE  = *"[ \t]"s;
	rule EOL    = "\n"s | "\r\n" | "\r" | ";";
	rule NUMBER = +"[0-9]"s >= SPACE <= [](auto t) { return std::stoll(std::string{t}); };
	rule ID     = "[a-z]"s >= SPACE <= [](auto t) -> int { return t[0] - 'a'; };
	rule ASSIGN = "=" > SPACE;
	rule PLUS   = "+" > SPACE;
	rule MINUS  = "-" > SPACE;
	rule TIMES  = "*" > SPACE;
	rule DIVIDE = "/" > SPACE;
	rule OPEN   = "(" > SPACE;
	rule CLOSE  = ")" > SPACE;

	extern rule Expr;

	rule Value =
		n%NUMBER                < []() { return n; }
		| i%ID > !ASSIGN        < []() { return variables[i]; }
		| OPEN > e%Expr > CLOSE < []() { return e; };

	rule Product =
		l%Value > *(
			TIMES > r%Value     < []() { l *= r; }
			| DIVIDE > r%Value  < []() { l /= r; }
		)                       < []() { return l; };

	rule Sum =
		l%Product > *(
			PLUS > r%Product    < []() { l += r; }
			| MINUS > r%Product < []() { l -= r; }
		)                       < []() { return l; };

	rule Expr =
		i%ID > ASSIGN > s%Sum   < []() { return variables[i] = s; }
		| s%Sum                 < []() { return s; };

	rule Stmt =
		SPACE > e%Expr > EOL    < []() { std::cout << e << std::endl; }
		| *(!EOL > ".") > EOL   < []() { std::cerr << "syntax error" << std::endl; };

	grammar Grammar = start(Stmt);
}

int main(int argc, char** argv)
{
	try {
		while(lug::parse(calc::Grammar));
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

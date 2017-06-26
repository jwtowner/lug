// lug - Embedded DSL for PE grammar parsers in C++
// Copyright (c) 2017 Jesse W. Towner

#include <lug/lug.hpp>
#include <cstdlib>

namespace samples::calc
{
	using namespace lug::language;

	double variables[26];
	double e, l, n, r, s;
	std::string_view m;
	int i;

	rule _		= *"[ \t]"s;

	rule EOL	= (("\n"s | "\r\n" | "\r") > lug::newline_action{}) | ";";

	rule ID		= m<< "[a-z]"s > _           <[]() -> int { return m[0] - 'a'; };

	rule NUMBER = m<< (~"[-+]"s > +"[0-9]"s
				> ~("[.]"s > +"[0-9]"s)) > _ <[]() { return std::stod(std::string{m}); };

	extern rule Expr;

	rule Value	= n%NUMBER                   <[]() { return n; }
				| i%ID > !( "=" > _ )        <[]() { return variables[i]; }
				| "(" > _ > e%Expr > ")" > _ <[]() { return e; };

	rule Prod	= l%Value > *(
				      "*" > _ > r%Value      <[]() { l *= r; }
				    | "/" > _ > r%Value      <[]() { l /= r; }
				)                            <[]() { return l; };

	rule Sum	= l%Prod > *(
					  "+" > _ > r%Prod       <[]() { l += r; }
					| "-" > _ > r%Prod       <[]() { l -= r; }
				)                            <[]() { return l; };

	rule Expr	= i%ID > "=" > _ > s%Sum     <[]() { return variables[i] = s; }
				| s%Sum                      <[]() { return s; };
	
	rule Stmt	= _ > (
					  "quit" > _             <[]() { std::exit(EXIT_SUCCESS); }
					| e%Expr                 <[]() { std::cout << e << std::endl; }
				) > EOL
				| *( !EOL > "." ) > EOL      <[]() { std::cerr << "syntax error" << std::endl; };

	grammar Grammar = start(Stmt);
}

int main()
{
	try {
		while(lug::parse(samples::calc::Grammar));
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

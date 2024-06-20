// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#include <cstdlib>
#include <iostream>

namespace samples::calc {

using namespace lug::language;

environment Env;
variable<std::string_view> m{Env};
variable<double> e{Env}, l{Env}, n{Env}, r{Env}, s{Env};
variable<int> i{Env};
double v[26];

extern rule Expr;

implicit_space_rule BLANK = lexeme[ *"[ \t]"_rx ];

rule EOL	= lexeme[ "[\n\r;]"_rx ];
rule ID		= lexeme[ capture(m)[ "[a-z]"_rx ] <[]() -> int { return m->at(0) - 'a'; } ];
rule NUMBER = lexeme[ capture(m)[ ~"[-+]"_rx > +"[0-9]"_rx > ~("."_sx > +"[0-9]"_rx) ]
			<[]{ return std::stod(std::string{*m}); } ];
rule Value	= n%NUMBER               <[]{ return *n; }
			| i%ID > !"="_sx         <[]{ return v[*i]; }
			| "(" > e%Expr > ")"     <[]{ return *e; };
rule Prod	= l%Value > *(
				  "*" > r%Value      <[]{ *l *= *r; }
				| "/" > r%Value      <[]{ *l /= *r; }
			)                        <[]{ return *l; };
rule Sum	= l%Prod > *(
				  "+" > r%Prod       <[]{ *l += *r; }
				| "-" > r%Prod       <[]{ *l -= *r; }
			)                        <[]{ return *l; };
rule Expr	= i%ID > "=" > s%Sum     <[]{ return v[*i] = *s; }
			| s%Sum                  <[]{ return *s; };
rule Stmt	= (   "quit"_isx         <[]{ std::exit(EXIT_SUCCESS); }
				| e%Expr             <[]{ std::cout << *e << "\n"; }
			) > EOL
			| *( !EOL > any ) > EOL  <[]{ std::cerr << "SYNTAX ERROR\n"; };

grammar Grammar = start(Stmt);

} // namespace samples::calc

int main()
{
	try {
		while (lug::parse(samples::calc::Grammar, samples::calc::Env)) ;
	} catch (std::exception const& e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "UNKNOWN ERROR\n";
		return -1;
	}
	return 0;
}

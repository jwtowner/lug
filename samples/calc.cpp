// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cstdlib>

namespace samples::calc
{
	using namespace lug::language;

	semantics Sema;
	variable<std::string_view> m{Sema};
	variable<double> e{Sema}, l{Sema}, n{Sema}, r{Sema}, s{Sema};
	variable<int> i{Sema};
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
				    | e%Expr             <[]{ std::cout << *e << std::endl; }
				) > EOL
				| *( !EOL > any ) > EOL  <[]{ std::cerr << "syntax error" << std::endl; };

	grammar Grammar = start(Stmt);
}

int main()
{
	try {
		while (lug::parse(samples::calc::Grammar, samples::calc::Sema)) ;
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

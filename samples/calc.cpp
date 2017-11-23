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

	implicit_space_rule BLANK = lexeme[ *"[ \t]"s ];

	rule EOL	= lexeme[ "[\n\r;]" ];

	rule ID		= lexeme[ capture(m)[ "[a-z]" ] <[]() -> int { return m->at(0) - 'a'; } ];

	rule NUMBER = lexeme[ capture(m)[ ~"[-+]"s > +"[0-9]"s > ~("[.]" > +"[0-9]"s) ]
				    <[]{ return std::stod(std::string{*m}); } ];

	extern rule Expr;

	rule Value	= n%NUMBER                   <[]{ return *n; }
				| i%ID > !"="s               <[]{ return v[*i]; }
				| "(" > e%Expr > ")"         <[]{ return *e; };

	rule Prod	= l%Value > *(
				      "*" > r%Value          <[]{ *l *= *r; }
				    | "/" > r%Value          <[]{ *l /= *r; }
				)                            <[]{ return *l; };

	rule Sum	= l%Prod > *(
				      "+" > r%Prod           <[]{ *l += *r; }
				    | "-" > r%Prod           <[]{ *l -= *r; }
				)                            <[]{ return *l; };

	rule Expr	= i%ID > "=" > s%Sum         <[]{ return v[*i] = *s; }
				| s%Sum                      <[]{ return *s; };
	
	rule Stmt	= (   "quit"                 <[]{ std::exit(EXIT_SUCCESS); }
				    | e%Expr                 <[]{ std::cout << *e << std::endl; }
				) > EOL
				| *( !EOL > "." ) > EOL      <[]{ std::cerr << "syntax error" << std::endl; };

	grammar Grammar = start(Stmt);
}

int main()
{
	try {
		while (lug::parse(samples::calc::Grammar, samples::calc::Sema)) ;
	} catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
	return 0;
}

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

#include <cctype>
#include <cstdlib>

namespace samples::calc {

using namespace lug::language;

int i;
double e, l, n, r, s;
double v[26];

extern rule Expr;

rule BLANK  = noskip[ *" \t"_bx ];
rule ID     = lexeme[ "a-zA-Z"_bx    <[](syntax m) -> int { return std::tolower(m.str().at(0)) - 'a'; } ];
rule NUMBER = lexeme[ ( ~"+-"_bx > +"0-9"_bx > ~('.' > +"0-9"_bx) )
                                     <[](syntax m) -> double { return std::stod(std::string{m}); } ];
rule Value  = n%NUMBER               <[]{ return n; }
            | i%ID > !"="_sx         <[]{ return v[i]; }
            | '(' > e%Expr > ')'     <[]{ return e; };
rule Prod   = l%Value > *(
                  '*' > r%Value      <[]{ l *= r; }
                | '/' > r%Value      <[]{ l /= r; }
            )                        <[]{ return l; };
rule Sum    = l%Prod > *(
                  '+' > r%Prod       <[]{ l += r; }
                | '-' > r%Prod       <[]{ l -= r; }
            )                        <[]{ return l; };
rule Expr   = i%ID > '=' > s%Sum     <[]{ return v[i] = s; }
            | s%Sum                  <[]{ return s; };
rule Cmnt   = ';' > *( !eol > any );
rule Stmt   = ( (   "exit"_isx
                  | "quit"_isx )     <[]{ std::exit(EXIT_SUCCESS); }
                | e%Expr             <[]{ std::cout << e << "\n"; }
            ) > ~Cmnt > eol
            | *( !eol > any ) > eol  <[]{ std::cout << "SYNTAX ERROR\n"; };

grammar Grammar = start(Stmt > eoi, BLANK);

} // namespace samples::calc

int main()
try {
    while (lug::parse(samples::calc::Grammar, lug::source_options::interactive)) ;
    return 0;
} catch (std::exception const& e) {
    std::cerr << "ERROR: " << e.what() << "\n";
    return 1;
} catch (...) {
    std::cerr << "UNKNOWN ERROR\n";
    return 1;
}

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

#include <cstdlib>

namespace samples::calc {

using namespace lug::language;

int i;
double e, l, n, r, s;
double v[26];

extern rule Expr;

implicit_space_rule BLANK = lexeme[ *"[ \t]"_rx ];

rule EOL    = lexeme[ "[\n\r;]"_rx ];
rule ID     = lexeme[ "[a-zA-Z]"_rx  <[](syntax m) -> int { return static_cast<int>(lug::unicode::tolower(static_cast<char32_t>(m.str().at(0))) - U'a'); } ];
rule NUMBER = lexeme[ ( ~"[-+]"_rx > +"[0-9]"_rx > ~('.' > +"[0-9]"_rx) )
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
rule Stmt   = ( (   "exit"_isx
                  | "quit"_isx )     <[]{ std::exit(EXIT_SUCCESS); }
                | e%Expr             <[]{ std::cout << e << "\n"; }
            ) > EOL
            | *( !EOL > any ) > EOL  <[]{ std::cerr << "SYNTAX ERROR\n"; };

grammar Grammar = start(Stmt > eoi);

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

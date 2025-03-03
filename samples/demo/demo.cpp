// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

// This sample demonstrates how to use the lug library to parse and evaluate a simple arithmetic expression.

// Include the lug library header file
#include <lug/lug.hpp>

int main() {
    // Import the namespace containing the embedded DSL operators and types
    using namespace lug::language;

    // Define attribute variables for the recursive rules
    int l = 0, r = 0;

    // Define a lexical rule that matches one or more digits and converts them to an integer
    auto Number = lexeme[+digit] <[](syntax s){ return std::stoi(std::string{s.str()}); };

    // Forward declaration for recursive rules
    rule Expr;

    // Define a rule that matches a number or a parenthesized expression
    rule Factor = Number | ('(' > Expr > ')');

    // Define a rule that matches a factor followed by zero or more '*' and a factor, and multiplies the factors
    rule Term = l%Factor > *('*' > r%Factor <[&]{ l *= r; }) <[&]{ return l; };

    // Define a rule that matches a term followed by zero or more '+' and a term, and adds the terms
    Expr = l%Term > *('+' > r%Term <[&]{ l += r; }) <[&]{ return l; };

    // Create grammar starting with an Expr and finishes by matching end-of-input
    auto grammar = start(Expr > eoi);

    // Parse and evaluate the sample input
    std::string input = "2 * (3 + 4)";
    lug::environment env;
    if (!lug::parse(input, grammar, env)) {
        std::cout << "Parse failed\n";
        return -1;
    }

    // Pop the result from the environment and display it to the console
    int result = env.pop_attribute<int>();
    std::cout << input << " = " << result << "\n"; // Outputs: 2 * (3 + 4) = 14
    return 0;
}

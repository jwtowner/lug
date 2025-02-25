// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>
#include <sstream>

using namespace std::string_literals;
using namespace std::string_view_literals;

void test_basic_error_recovery()
{
	using namespace lug::language;

	// Define recovery rule that accepts "XYZ" without skipping whitespace
	rule TermRecovery = noskip[str("XYZ") ^ error_response::accept];

	// Define term rule that matches 'A' followed by either "BCD" or raises error with recovery
	rule Term = lexeme[chr('A') > (str("BCD") | raise("Error", TermRecovery))];

	grammar G = start(Term > eoi);
	assert(lug::parse("ABCD", G)); // Test valid input "ABCD" - should succeed
	assert(lug::parse("AXYZ", G)); // Test error recovery - "AXYZ" should succeed by recovering with "XYZ"
	assert(!lug::parse("XYZA", G)); // Test invalid input starting with "XYZ" - should fail
	assert(!lug::parse("APQR", G)); // Test invalid input with no recovery - should fail
}

void test_basic_error_handling()
{
	using namespace lug::language;

	// Variables to capture error information
	syntax error_syntax;
	syntax_position error_pos;

	// Define failure label for invalid symbol characters
	failure const FSymbolChar{"symbol character"};
	
	// Define rule that matches any sequence of characters except '@'
	// and fails if '@' is encountered
	auto const Symbol = lexeme[+(!chr('@') > any) > (!chr('@'))[FSymbolChar]];

	// Define grammar rule with error handler that captures error details
	rule S = Symbol > eoi ^= [&](error_context& e) -> error_response {
			assert(e.label() == "symbol character"sv);
			error_syntax = e.syntax();
			error_pos = e.position_begin();
			return error_response::halt;
		};

	grammar G = start(S);

	// Test invalid input containing '@' -- should fail and capture error
	assert(!lug::parse("invalid@symbol", G));
	assert(error_syntax == "@symbol"sv);
	assert((error_pos == syntax_position{1, 8}));

	// Test valid input -- should succeed with no error
	error_syntax = syntax{};
	error_pos = syntax_position{};
	assert(lug::parse("valid_symbol", G));
	assert(error_syntax.empty());
	assert((error_pos == syntax_position{}));
}

void test_calculator_with_recovery()
{
	using namespace lug::language;

	// Define error handler that captures output
	std::ostringstream error_output;
	auto const error_handler = [&error_output](error_context& e) -> error_response {
		error_output << "Error at line " << e.position_begin().line
		             << ", column " << e.position_begin().column
		             << ": " << e.label() << "\n";
		return error_response::resume;
	};

	// Define failure labels
	failure FNumber{"expected number"};
	failure FRParen{"expected ')'"};
	failure FOperand{"expected operand"};

	rule Expr, Term, Factor, Number;

	// Basic number parsing with error handling
	Number = lexeme[+chr('0', '9')][FNumber];
	
	// Parenthesized expressions and atomic factors
	Factor = (chr('(') > Expr > chr(')')[FRParen])
	       | Number;
	
	// Multiplication with error recovery
	Term = Factor > *(chr('*') > Factor[FOperand]);
	
	// Addition with error recovery
	Expr = Term > *(chr('+') > Term[FOperand]);

	// Create grammar with error handling
	grammar G = start(Expr > eoi ^= error_handler);

	// Test valid input
	assert(lug::parse("123+456*789", G));
	assert(error_output.str().empty());

	// Test invalid input with recovery
	error_output.str(""s);
	assert(!lug::parse("123++456*789", G));  // Double plus should recover
	assert(error_output.str() == "Error at line 1, column 4: expected operand\n"s);

	error_output.str(""s);
	assert(!lug::parse("123+456**789", G));  // Double multiply should recover
	assert(error_output.str() == "Error at line 1, column 8: expected operand\n"s);

	error_output.str(""s);
	assert(!lug::parse("123+456)", G));      // Unmatched parenthesis should recover
	assert(error_output.str() == "Error at line 1, column 7: expected operand\n"s);

	error_output.str(""s);
	assert(!lug::parse("123+", G));          // Incomplete expression should fail
	assert(error_output.str() == "Error at line 1, column 4: expected number\n"s);
}

/*
void test_custom_error_handlers()
{
	rule S, ErrorRecovery;
	bool error_handler_called = false;

	// Define error recovery rule
	ErrorRecovery = lexeme[*(~chr('\n'))] > chr('\n') < [&](environment&, syntax) {
		error_handler_called = true;
	};

	S = (lexeme[+(~chr('\n'))] | ErrorRecovery) > chr('\n');

	environment E;
	grammar G = start(S > eoi);

	bool const result = lug::parse("This line has an error!\n", G, E);
	assert(!result);
	assert(error_handler_called);
}

void test_nested_recovery()
{
	rule S, Block;
	int recovery_count = 0;

	// Test nested structure recovery
	Block = chr('{') > lexeme[*(~chr('}'))] > chr('}') < [&](environment&, syntax) {
		recovery_count++;
	};

	S = *Block;

	environment E;
	grammar G = start(S > eoi);

	bool const result = lug::parse("{valid} {invalid", G, E);
	assert(!result);
	assert(recovery_count == 1); // Should have attempted recovery once
}*/

int main()
{
	try {
		test_basic_error_recovery();
		test_basic_error_handling();
		//test_calculator_with_recovery();
		//test_custom_error_handlers();
		//test_nested_recovery();
		return 0;
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Error: Unknown exception\n";
		return -1;
	}
}

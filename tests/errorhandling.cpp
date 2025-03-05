// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>
#include <sstream>

using namespace std::string_literals;
using namespace std::string_view_literals;

void test_simple_error_no_recovery()
{
	using namespace lug::language;

	// Define grammar rule that matches:
	// - Zero or more 'A's followed by 'B'
	// - Or a single 'C'
	// - Or raises an error
	rule Term = lexeme[((*chr('A') > chr('B')) | chr('C')) | raise("Error")];
	grammar G = start(Term > eoi);

	lug::environment E;
	assert(lug::parse("AB", G, E));      // Single A followed by B
	assert(E.match() == "AB");

	assert(lug::parse("AAAB", G, E));    // Multiple A's followed by B
	assert(E.match() == "AAAB");

	assert(!lug::parse("AAC", G, E));    // A's not followed by B should fail

	assert(lug::parse("B", G, E));       // Single B
	assert(E.match() == "B");

	assert(lug::parse("C", G, E));       // Single C
	assert(E.match() == "C");

	assert(!lug::parse("XXC", G, E));    // Invalid prefix should fail even with valid C
}

void test_simple_error_recovery()
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

void test_simple_error_handling()
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

void test_calculator_errors_no_recovery()
{
	using namespace lug::language;

	// Define error handler that captures output
	std::ostringstream error_output;
	auto const error_handler = [&error_output](error_context& e) -> error_response {
		error_output << "Error at line " << e.position_begin().line
		             << ", column " << e.position_begin().column
		             << ": " << e.label() << "\n";
		return error_response::halt;
	};

	// Define labeled failures
	failure FNoExpr{"missing expression"};
	failure FExpr{"expected an expression after opening parenthesis"};
	failure FOperand{"expected an operand after the operator"};
	failure FRParen{"expected a closing parenthesis after expression"};
	failure FLParen{"closing parenthesis with no matching opening parenthesis"};
	failure FInvalid{"invalid character"};

	// Define variables to capture results
	double n{0.0}, e{0.0}, l{0.0}, r{0.0};

	// Define NUMBER rule that converts captured string to double
	auto NUMBER = lexeme[ ( ~'-'_cx > +"[0-9]"_rx > ~('.' > +"[0-9]"_rx) )
			<[](syntax m) -> double { return std::stod(std::string{m}); } ];

	// Define Stmt, Expr, Prod, and Term rules
	rule Stmt, Expr, Prod, Term;

	Term = n%NUMBER                              <[&]{ return n; }
	     | '(' > e%Expr[FExpr] > ')'_cx[FRParen] <[&]{ return e; };
	Prod = l%Term > *(
	       '*' > r%Term[FOperand]   <[&]{ l *= r; }
	     | '/' > r%Term[FOperand]   <[&]{ l /= r; }
	     )                          <[&]{ return l; };
	Expr = l%Prod > *(
	       '+' > r%Prod[FOperand]   <[&]{ l += r; }
	     | '-' > r%Prod[FOperand]   <[&]{ l -= r; }
	     )                          <[&]{ return l; };
	Stmt = Expr[FNoExpr] > (
	       &')'_cx > raise(FLParen)
	     | eoi[FInvalid]
	     )
	     ^= error_handler;

	// Define grammar with error halting on failure
	grammar G = start(Stmt);

	// Define function to evaluate expression and return result
	auto const evaluate = [&](std::string_view input) -> std::optional<double> {
		n = 0.0; e = 0.0; l = 0.0; r = 0.0;
		error_output.str(""s);
		lug::environment E;
		if (!lug::parse(input, G, E))
			return std::nullopt;
		return E.pop_attribute<double>();
	};

	// Test valid input
	auto result = evaluate("123 + 456*(787 + 4/2)");
	assert(result.has_value() && (*result == 359907.0));
	assert(error_output.str().empty());

	// Test invalid input
	assert(!evaluate("123++456*789"));
	assert(error_output.str() == "Error at line 1, column 5: expected an operand after the operator\n");

	assert(!evaluate("123 + 456**789"));
	assert(error_output.str() == "Error at line 1, column 11: expected an operand after the operator\n");

	assert(!evaluate("123+"));
	assert(error_output.str() == "Error at line 1, column 5: expected an operand after the operator\n");

	assert(!evaluate("123+456 ) "));
	assert(error_output.str() == "Error at line 1, column 9: closing parenthesis with no matching opening parenthesis\n");

	assert(!evaluate("600 / \n(2*3"));
	assert(error_output.str() == "Error at line 2, column 5: expected a closing parenthesis after expression\n");

	assert(!evaluate(" "));
	assert(error_output.str() == "Error at line 1, column 2: missing expression\n");

	assert(!evaluate("456+22/4%22"));
	assert(error_output.str() == "Error at line 1, column 9: invalid character\n");
}

void test_calculator_errors_with_recovery_resume()
{
	using namespace lug::language;

	// Define error handler that captures output
	std::ostringstream error_output;
	auto const error_handler = [&error_output](error_context& e) -> error_response {
		e.environment().set_condition("evaluate-errors", true);
		error_output << "Error at line " << e.position_begin().line
		             << ", column " << e.position_begin().column
		             << ": " << e.label() << "\n";
		return error_response::resume;
	};

	// Define labeled failures with recovery rules
	failure FNoExpr{"missing expression", with_value("<MISSING-EXPRESSION>"s)};
	failure FExpr{"expected an expression after opening parenthesis", sync_with_value("[)+*/-]"_rx, "<BAD-EXPRESSION>"s)};
	failure FOperand{ "expected an operand after the operator", sync_with_value("[)+*/-]"_rx, "<BAD-OPERAND>"s)};
	failure FRParen{"expected a closing parenthesis after expression", sync(')'_cx)};
	failure FLParen{"closing parenthesis with no matching opening parenthesis"};
	failure FInvalid{"invalid character"};

	// Define lexical token rule that matches a number
	auto NUMBER = lexeme[~'-'_cx > +"[0-9]"_rx > ~('.' > +"[0-9]"_rx)];

	// Define attribute variables to capture results
	std::string_view number;
	std::string e, l, r;

	// Define Stmt, Expr, Prod, and Term rules
	rule Stmt, Expr, Prod, Term;

	Term = capture(number)[NUMBER]               <[&]{ return std::string{number}; }
	     | '(' > e%Expr[FExpr] > ')'_cx[FRParen] <[&]{ return '(' + e + ')'; };
	Prod = l%Term > *(
	       '*' > r%Term[FOperand]   <[&]{ l += " * " + r; }
	     | '/' > r%Term[FOperand]   <[&]{ l += " / " + r; }
	     )                          <[&]{ return l; };
	Expr = l%Prod > *(
	       '+' > r%Prod[FOperand]   <[&]{ l += " + " + r; }
	     | '-' > r%Prod[FOperand]   <[&]{ l += " - " + r; }
	     )                          <[&]{ return l; };
	Stmt = Expr[FNoExpr] > (
	       &')'_cx > raise(FLParen)
	     | eoi[FInvalid]
	     ) > ( when("evaluate-errors") > accept | nop ) // Force acceptance of semantic actions
	     ^= error_handler;

	// Define grammar with error halting on failure
	grammar G = start(Stmt);

	// Define function to evaluate expression and return result
	auto const evaluate = [&](std::string_view input) -> std::pair<std::string, bool> {
		// Reset attribute variables and error output buffer
		number = ""sv;
		e = ""s;
		l = ""s;
		r = ""s;
		error_output.str(""s);

		// Parse the input string and return evaluated output and parse result
		lug::environment E;
		bool const m = lug::parse(input, G, E);
		return std::pair{E.pop_attribute<std::string>(), m};
	};

	// Test valid input
	std::string output;
	bool success{false};

	std::tie(output, success) = evaluate("123 + 456 * (787 + 4/2)");
	assert(success && (output == "123 + 456 * (787 + 4 / 2)"));
	assert(error_output.str().empty());

	// Test invalid input
	std::tie(output, success) = evaluate("123+");
	assert(!success && output == "123 + <BAD-OPERAND>");
	assert(error_output.str() == "Error at line 1, column 5: expected an operand after the operator\n");

	std::tie(output, success) = evaluate(" 123++ 456 *789");
	assert(!success && output == "123 + <BAD-OPERAND> + 456 * 789");
	assert(error_output.str() == "Error at line 1, column 6: expected an operand after the operator\n");

	std::tie(output, success) = evaluate("123 + 456**789");
	assert(!success && output == "123 + 456 * <BAD-OPERAND> * 789");
	assert(error_output.str() == "Error at line 1, column 11: expected an operand after the operator\n");

	std::tie(output, success) = evaluate("123+456 ) ");
	assert(!success && output == "123 + 456");
	assert(error_output.str() == "Error at line 1, column 9: closing parenthesis with no matching opening parenthesis\n");

	std::tie(output, success) = evaluate("600 / \n(2*3");
	assert(!success && output == "600 / (2 * 3)");
	assert(error_output.str() == "Error at line 2, column 5: expected a closing parenthesis after expression\n");

	std::tie(output, success) = evaluate(" ");
	assert(!success && output == "<MISSING-EXPRESSION>");
	assert(error_output.str() == "Error at line 1, column 2: missing expression\n");

	std::tie(output, success) = evaluate("456+22/4%22");
	assert(!success && output == "456 + 22 / 4");
	assert(error_output.str() == "Error at line 1, column 9: invalid character\n");

	std::tie(output, success) = evaluate("456+()+");
	assert(!success && output == "456 + (<BAD-EXPRESSION>) + <BAD-OPERAND>");
	assert(error_output.str() == "Error at line 1, column 6: expected an expression after opening parenthesis\nError at line 1, column 8: expected an operand after the operator\n");
}

void test_calculator_errors_with_recovery_accept()
{
	using namespace lug::language;

	// Define error handler that captures output
	std::ostringstream error_output;
	auto const error_handler = [&error_output](error_context& e) {
		error_output << "Error at line " << e.position_begin().line
		             << ", column " << e.position_begin().column
		             << ": " << e.label() << "\n";
	};

	// Define labeled failures with recovery rules
	failure FNoExpr{"missing expression", with_value<error_response::accept>("<MISSING-EXPRESSION>"s)};
	failure FExpr{"expected an expression after opening parenthesis", sync_with_value<error_response::accept>("[)+*/-]"_rx, "<BAD-EXPRESSION>"s)};
	failure FOperand{ "expected an operand after the operator", sync_with_value<error_response::accept>("[)+*/-]"_rx, "<BAD-OPERAND>"s)};
	failure FRParen{"expected a closing parenthesis after expression", sync<error_response::accept>(')'_cx)};
	failure FLParen{"closing parenthesis with no matching opening parenthesis", with_response<error_response::accept>()};
	failure FInvalid{"invalid character", with_response<error_response::accept>()};

	// Define lexical token rule that matches a number
	auto NUMBER = lexeme[~'-'_cx > +"[0-9]"_rx > ~('.' > +"[0-9]"_rx)];

	// Define attribute variables to capture results
	std::string_view number;
	std::string e, l, r;

	// Define Stmt, Expr, Prod, and Term rules
	rule Stmt, Expr, Prod, Term;

	Term = capture(number)[NUMBER]               <[&]{ return std::string{number}; }
	     | '(' > e%Expr[FExpr] > ')'_cx[FRParen] <[&]{ return '(' + e + ')'; };
	Prod = l%Term > *(
	       '*' > r%Term[FOperand]   <[&]{ l += " * " + r; }
	     | '/' > r%Term[FOperand]   <[&]{ l += " / " + r; }
	     )                          <[&]{ return l; };
	Expr = l%Prod > *(
	       '+' > r%Prod[FOperand]   <[&]{ l += " + " + r; }
	     | '-' > r%Prod[FOperand]   <[&]{ l += " - " + r; }
	     )                          <[&]{ return l; };
	Stmt = Expr[FNoExpr] > (
	       &')'_cx > raise(FLParen)
	     | eoi[FInvalid]
	     )
	     ^= error_handler;

	// Define grammar with error halting on failure
	grammar G = start(Stmt);

	// Define function to evaluate expression and return result
	auto const evaluate = [&](std::string_view input) -> std::pair<std::string, bool> {
		// Reset attribute variables and error output buffer
		number = ""sv;
		e = ""s;
		l = ""s;
		r = ""s;
		error_output.str(""s);

		// Parse the input string and return evaluated output and parse result
		lug::environment E;
		bool const m = lug::parse(input, G, E);
		return std::pair{E.pop_attribute<std::string>(), m};
	};

	// Test valid input
	std::string output;
	bool success{false};

	std::tie(output, success) = evaluate("123 + 456 * (787 + 4/2)");
	assert(success && (output == "123 + 456 * (787 + 4 / 2)"));
	assert(error_output.str().empty());

	// Test invalid input
	std::tie(output, success) = evaluate("123+");
	assert(success && output == "123 + <BAD-OPERAND>");
	assert(error_output.str() == "Error at line 1, column 5: expected an operand after the operator\n");

	std::tie(output, success) = evaluate(" 123++ 456 *789");
	assert(success && output == "123 + <BAD-OPERAND> + 456 * 789");
	assert(error_output.str() == "Error at line 1, column 6: expected an operand after the operator\n");

	std::tie(output, success) = evaluate("123 + 456**789");
	assert(success && output == "123 + 456 * <BAD-OPERAND> * 789");
	assert(error_output.str() == "Error at line 1, column 11: expected an operand after the operator\n");

	std::tie(output, success) = evaluate("123+456 ) ");
	assert(success && output == "123 + 456");
	assert(error_output.str() == "Error at line 1, column 9: closing parenthesis with no matching opening parenthesis\n");

	std::tie(output, success) = evaluate("600 / \n(2*3");
	assert(success && output == "600 / (2 * 3)");
	assert(error_output.str() == "Error at line 2, column 5: expected a closing parenthesis after expression\n");

	std::tie(output, success) = evaluate(" ");
	assert(success && output == "<MISSING-EXPRESSION>");
	assert(error_output.str() == "Error at line 1, column 2: missing expression\n");

	std::tie(output, success) = evaluate("456+22/4%22");
	assert(success && output == "456 + 22 / 4");
	assert(error_output.str() == "Error at line 1, column 9: invalid character\n");

	std::tie(output, success) = evaluate("456+()+");
	assert(success && output == "456 + (<BAD-EXPRESSION>) + <BAD-OPERAND>");
	assert(error_output.str() == "Error at line 1, column 6: expected an expression after opening parenthesis\nError at line 1, column 8: expected an operand after the operator\n");
}

void test_error_suppression_in_negative_lookahead()
{
	using namespace lug::language;

	environment E;

	// Define a handler that will be called when the error is raised
	bool error_handler_called{false};
	auto const error_handler = [&error_handler_called](error_context&) { error_handler_called = true; };

	// Define a rule with a negative lookahead containing a raise
	// The raise should be suppressed and not trigger when the negative lookahead succeeds
	rule Term = lexeme[chr('A') > !raise("This error should be suppressed") > chr('B')];
	grammar G = start(Term > eoi ^= error_handler);

	assert(lug::parse("AB", G, E)); // Should succeed because the negative lookahead suppresses the error
	assert(E.match() == "AB");
	assert(!error_handler_called);

	// Define a rule where the negative lookahead fails with a raise
	rule Term2 = lexeme[chr('A') > !(chr('B') > raise("This error should be suppressed")) > chr('B')];
	grammar G2 = start(Term2 > eoi ^= error_handler);

	assert(lug::parse("AB", G2, E)); // Should succeed because the negative lookahead suppresses the error
	assert(E.match() == "AB");
	assert(!error_handler_called);
}

void test_error_suppression_in_positive_lookahead()
{
	using namespace lug::language;

	// Define a handler that will be called when the error is raised
	bool error_handler_called{false};
	auto const error_handler = [&error_handler_called](error_context&) { error_handler_called = true; };

	// Define a rule with a positive lookahead containing a raise
	// The raise should trigger when the positive lookahead is evaluated
	rule Term = lexeme[chr('A') > &raise("This error should be suppressed") > chr('B')];
	grammar G = start(Term > eoi ^= error_handler);

	assert(!lug::parse("AB", G)); // Should fail because the positive lookahead fails
	assert(!error_handler_called); // But the error is suppressed

	// Define a rule where the positive lookahead succeeds, causing error to be raised
	rule Term2 = lexeme[chr('A') > &(chr('B') > raise("This error should be suppressed")) > chr('B')];
	grammar G2 = start(Term2 > eoi ^= error_handler);

	assert(!lug::parse("AB", G2)); // Should fail because the positive lookahead fails
	assert(!error_handler_called); // But the error is suppressed
}

int main()
{
	try {
		test_simple_error_no_recovery();
		test_simple_error_recovery();
		test_simple_error_handling();
		test_calculator_errors_no_recovery();
		test_calculator_errors_with_recovery_resume();
		test_calculator_errors_with_recovery_accept();
		test_error_suppression_in_negative_lookahead();
		test_error_suppression_in_positive_lookahead();
		return 0;
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Error: Unknown exception\n";
		return -1;
	}
}

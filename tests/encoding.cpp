// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <iostream>

#undef NDEBUG
#include <cassert>

namespace {

template <class E>
auto compile_expression(E const& e)
{
	lug::program prog;
	lug::program_callees prog_callees;
	lug::encoder encoder{prog, prog_callees};
	(void)make_expression(e).evaluate(encoder);
	return prog;
}

auto make_repeat_instruction(lug::opcode op, std::uint_least8_t imm8, std::uint_least16_t imm16, std::size_t min, std::size_t max)
{
	return lug::instruction{op, imm8, imm16, lug::instruction::pack_min_max(min, max)};
}

template <class E>
void test_repetition_optimization_for(E const& expr, lug::opcode op, std::uint_least8_t imm8 = 0, std::uint_least16_t imm16 = 0)
{
	// Test zero-or-many repetition in lexeme
	auto const prog1 = [&]
	{
		using namespace lug::dsl;
		auto S = lexeme[*expr];
		return compile_expression(S);
	}();

	assert(prog1.instructions.size() == 1);
	assert(prog1.instructions[0] == make_repeat_instruction(op, imm8, imm16, 0, lug::forever));

	// Test one-or-many repetition in noskip
	auto const prog2 = [&]
	{
		using namespace lug::dsl;
		auto S = noskip[+expr];
		return compile_expression(S);
	}();

	assert(prog2.instructions.size() == 1);
	assert(prog2.instructions[0] == make_repeat_instruction(op, imm8, imm16, 1, lug::forever));

	// Test repeat<3,5> repetition in noskip
	auto const prog3 = [&]
	{
		using namespace lug::dsl;
		auto S = noskip[repeat<3, 5>[expr]];
		return compile_expression(S);
	}();

	assert(prog3.instructions.size() == 1);
	assert(prog3.instructions[0] == make_repeat_instruction(op, imm8, imm16, 3, 5));

	// Test that optimization is disabled when not in lexeme or noskip directive
	auto const prog4 = [&]
	{
		using namespace lug::dsl;
		auto S = *expr;
		return compile_expression(S);
	}();

	assert(prog4.instructions.size() > 1);

	// Test that skip directive disables optimization
	auto const prog5 = [&]
	{
		using namespace lug::dsl;
		auto S = noskip[skip[*expr]];
		return compile_expression(S);
	}();

	assert(prog5.instructions.size() > 1);
}

} // namespace

void test_repetition_optimization_for_char()
{
	test_repetition_optimization_for(lug::dsl::chr('a'), lug::opcode::repeat_unit, 'a');
}

void test_repetition_optimization_for_char_range()
{
	test_repetition_optimization_for(lug::dsl::chr('A', 'Z'), lug::opcode::repeat_set);
}

void test_repetition_optimization_for_bracket()
{
	test_repetition_optimization_for(lug::dsl::bkt("abcd"), lug::opcode::repeat_set);
}

void test_repetition_optimization_for_any()
{
	test_repetition_optimization_for(lug::dsl::any, lug::opcode::repeat_any);
}

void test_repetition_optimization_for_space()
{
	test_repetition_optimization_for(lug::dsl::space, lug::opcode::repeat_space);
}

void test_repetition_optimization_for_blank()
{
	test_repetition_optimization_for(lug::dsl::blank, lug::opcode::repeat_blank);
}

void test_repetition_optimization_for_ascii_ctype()
{
	test_repetition_optimization_for(lug::dsl::ascii::alpha, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::alnum, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::digit, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::xdigit, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::lower, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::upper, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::cntrl, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::graph, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::print, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::punct, lug::opcode::repeat_set);
	test_repetition_optimization_for(lug::dsl::ascii::word, lug::opcode::repeat_set);
}

void test_directives_stack_no_skip_operations()
{
	lug::program prog;
	lug::program_callees prog_callees;
	for (int i = 0; i < 256; ++i) {
		auto const dinitial = static_cast<lug::directive_traits>(static_cast<std::uint_least8_t>(i));
		lug::encoder enc{prog, prog_callees, dinitial};
		auto const old_dir = enc.directives_push(lug::directive_traits::none, lug::directive_traits::none);
		assert(enc.directives() == dinitial); // No change
		enc.directives(old_dir);
		assert(enc.directives() == dinitial); // No change
		assert(prog.instructions.empty()); // No instructions added
	}
}

int main()
{
	LUG_TRY {
		test_repetition_optimization_for_char();
		test_repetition_optimization_for_char_range();
		test_repetition_optimization_for_bracket();
		test_repetition_optimization_for_any();
		test_repetition_optimization_for_space();
		test_repetition_optimization_for_blank();
		test_repetition_optimization_for_ascii_ctype();
		test_directives_stack_no_skip_operations();
		return 0;
	} LUG_CATCH (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return 1;
	} LUG_CATCH_ANY {
		std::cerr << "Unknown Error\n";
		return 1;
	}
}

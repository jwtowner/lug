// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

using namespace std::string_view_literals;

constexpr auto sentences1 =
R"(The stranger officiates the meal.
She was too short to see over the fence.
This is the last random sentence I will be writing and I am going to stop mid-sent
It was getting dark, and we weren't there yet.
There were white out conditions in the town; subsequently, the roads were impassable.
I really want to go to work, but I am too sick to drive.
I am counting my calories, yet I really want dessert.
I checked to make sure that he was still alive.
I love eating toasted cheese and tuna sandwiches.
Everyone was busy, so I went to the movie alone.
He got a small piece of pie.
What was the person thinking when they discovered cow's milk was fine for human consumption... and why did they do it in the first place!?
Where do random thoughts come from?
If Purple People Eaters are real... where do they find purple people to eat?
He turned in the research paper on Friday; otherwise, he would have not passed the class.
How was the math test?
The mysterious diary records the voice.
She works two jobs to make ends meet; at least, that was her reason for not having time to join us.
I want more detailed information.
	He told us a very exciting adventure story.)"sv;

constexpr auto sentences2 =
R"(
There were white out conditions in the town; subsequently, the roads were impassable.
He told us a very exciting adventure story.
The sky is clear; the stars are twinkling.
Let me help you with your baggage.
The stranger officiates the meal.
The rocks are approaching at high velocity.
Abstraction is often one floor above you.
)"sv;

void test_line_column_tracking()
{
	// Array to store starting positions of specific words in the text
	std::array<lug::syntax_position, 4> startpos;
	startpos.fill({0, 0});

	// Array to store ending positions of specific words in the text
	std::array<lug::syntax_position, 4> endpos;
	endpos.fill({0, 0});

	// Create a grammar that tracks positions of specific words
	lug::grammar const G = [&]
	{
		using namespace lug::language;

		// Define a rule that matches words and captures positions of specific words
		rule Word = lexeme[
				  str("officiates") < [&](environment& e, syntax x) { startpos[0] = e.position_begin(x); endpos[0] = e.position_end(x); }
				| str("Everyone") < [&](environment& e, syntax x) { endpos[1] = e.position_end(x); startpos[1] = e.position_begin(x); }
				| str("Friday") < [&](environment& e, syntax x) { startpos[2] = e.position_begin(x); endpos[2] = e.position_end(x); }
				| str("story") < [&](environment& e, syntax x) { endpos[3] = e.position_end(x); startpos[3] = e.position_begin(x); }
				| +alpha
			];

		// Return a grammar that matches any number of words or punctuation until end of input
		return start(*(Word | punct) > eoi);
	}();

	// Create environment for parsing
	lug::environment E;
	lug::parser p{G, E};

	// Test parsing the first text sample
	bool const success1 = p.parse(std::begin(sentences1), std::end(sentences1));
	assert(success1);
	assert(p.match() == sentences1);

	// Verify starting positions of tracked words in first text
	assert(startpos[0].line == 1 && startpos[0].column == 14);
	assert(startpos[1].line == 10 && startpos[1].column == 1);
	assert(startpos[2].line == 15 && startpos[2].column == 36);
	assert(startpos[3].line == 20 && startpos[3].column == 46);

	// Verify ending positions of tracked words in first text
	assert(endpos[0].line == 1 && endpos[0].column == 24);
	assert(endpos[1].line == 10 && endpos[1].column == 9);
	assert(endpos[2].line == 15 && endpos[2].column == 42);
	assert(endpos[3].line == 20 && endpos[3].column == 51);

	// Verify maximum parsing position reached
	assert(p.max_subject_index() == sentences1.size());
	assert(p.max_subject_position().line == 20 && p.max_subject_position().column == 52);

	// Test parsing the second text sample
	bool const success2 = p.parse(std::begin(sentences2), std::end(sentences2));
	assert(success2);
	assert(p.match() == sentences2);

	// Verify positions have been updated for the second text
	assert(startpos[0].line == 25 && startpos[0].column == 14);
	assert(startpos[3].line == 22 && startpos[3].column == 38);
	assert(endpos[0].line == 25 && endpos[0].column == 24);
	assert(endpos[3].line == 22 && endpos[3].column == 43);

	// Verify maximum parsing position for second text
	assert(p.max_subject_index() == sentences2.size());
	assert(p.max_subject_position().line == 28 && p.max_subject_position().column == 1);
}

// Test environment class that counts the number of times the environment hooks are called
struct test_environment : public lug::environment
{
	int reset_count{0};
	int drain_count{0};
	int accept_started_count{0};
	int accept_ended_count{0};

	void on_reset() override { ++reset_count; }
	void on_drain() override { ++drain_count; }
	void on_accept_started() override { ++accept_started_count; }
	void on_accept_ended() override { ++accept_ended_count; }
};

void test_environment_callbacks()
{
	using namespace lug::language;

	// Create a simple grammar
	grammar G = start(+"[a-z]"_rx > eoi);

	// Create a test environment
	test_environment E;
	assert(E.reset_count == 0);
	assert(E.drain_count == 0);
	assert(E.accept_started_count == 0);
	assert(E.accept_ended_count == 0);

	// Parse a simple string
	bool const success = lug::parse("hello", G, E);
	assert(success);

	// Verify the environmenthooks were called the expected number of times
	assert(E.reset_count == 1);
	assert(E.drain_count == 0);
	assert(E.accept_started_count == 1);
	assert(E.accept_ended_count == 1);
}

int main()
{
	try {
		test_line_column_tracking();
		test_environment_callbacks();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

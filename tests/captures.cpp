// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <sstream>

#undef NDEBUG
#include <cassert>

static_assert(std::is_default_constructible_v<lug::syntax>);
static_assert(std::is_copy_constructible_v<lug::syntax>);
static_assert(std::is_move_constructible_v<lug::syntax>);
static_assert(std::is_copy_assignable_v<lug::syntax>);
static_assert(std::is_move_assignable_v<lug::syntax>);
static_assert(std::is_destructible_v<lug::syntax>);
static_assert(std::is_swappable_v<lug::syntax>);

static_assert(std::is_convertible_v<lug::syntax, std::string>);
static_assert(std::is_convertible_v<lug::syntax, std::string_view>);
static_assert(std::is_convertible_v<lug::syntax, lug::syntax_range>);
static_assert(std::is_constructible_v<std::string, lug::syntax const&>);
static_assert(std::is_constructible_v<std::string_view, lug::syntax const&>);
static_assert(std::is_constructible_v<lug::syntax_range, lug::syntax const&>);
static_assert(std::is_assignable_v<std::string, lug::syntax const&>);
static_assert(std::is_assignable_v<std::string_view, lug::syntax const&>);
static_assert(std::is_assignable_v<lug::syntax_range, lug::syntax const&>);

void test_capture_email_syntax()
{
	lug::syntax username;
	lug::syntax domain;
	lug::syntax tld;

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		return start(lexeme[capture(username)[+"[a-zA-Z0-9._%+-]"_rx] > '@' >
							capture(domain)[+"[a-zA-Z0-9-]"_rx] > '.' >
							capture(tld)["[a-zA-Z]"_rx > +"[a-zA-Z]"_rx]]);
	}();

	std::string_view const email = "user@example.com";
	assert(lug::parse(email, G));
	assert(username.str() == "user");
	assert(domain.str() == "example");
	assert(tld.str() == "com");
	assert(username.str().data() == email.data());
	assert(domain.str().data() == email.substr(5).data());
	assert(tld.str().data() == email.substr(13).data());

	std::string const email2 = "not.an@email";
	assert(!lug::parse(email2, G));
	// failure to parse the above should not change captures, as no semantic actions should be executed
	assert(username.str() == "user");
	assert(domain.str() == "example");
	assert(tld.str() == "com");
	assert(username.str().data() == email.data());
	assert(domain.str().data() == email.substr(5).data());
	assert(tld.str().data() == email.substr(13).data());
}

void test_capture_url_syntax()
{
	std::string_view protocol;
	std::string domain;
	lug::syntax path;

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		return start(lexeme[capture(protocol)["http"_sx > ~'s'_cx] > "://"_sx >
							capture(domain)[+"[a-zA-Z0-9.-]"_rx] >
							capture(path)['/' > *"[^?#]"_rx]]);
	}();

	std::string_view const url1 = "https://www.example.com/path/to/resource";
	assert(lug::parse(url1, G));
	assert(protocol == "https");
	assert(domain == "www.example.com");
	assert(path.str() == "/path/to/resource");
	assert(protocol.data() == url1.data());
	assert(domain.data() != url1.data()); // std::string makes a copy
	assert(path.str().data() == url1.substr(23).data());

	std::string const url2 = "http://api.example2.com/path/to/other/resource.html";
	assert(lug::parse(url2, G));
	assert(protocol == "http");
	assert(domain == "api.example2.com");
	assert(path.str() == "/path/to/other/resource.html");
	assert(protocol.data() == url2.c_str());
	assert(domain.data() != url2.data()); // std::string makes a copy
	assert(path.str().data() == &url2[23]);

	std::string const url3 = "https://www.example3.com$path/to/resource";
	assert(!lug::parse(url3, G));
	// failure to parse the above should not change captures, as no semantic actions should be executed
	assert(protocol == "http");
	assert(domain == "api.example2.com");
	assert(path.str() == "/path/to/other/resource.html");
	assert(protocol.data() == url2.c_str());
	assert(domain.data() != url2.data()); // std::string makes a copy
	assert(path.str().data() == &url2[23]);
}

void test_capture_comma_delimited_list()
{
	std::vector<std::string> items;
	lug::syntax item;

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		return start((capture(item)[lexeme[+"[a-zA-Z0-9_-]"_rx]] <[&]{items.emplace_back(item);}) >> ',');
	}();

	std::string const list1 = "apple, banana, cherry";
	assert(lug::parse(list1, G));

	assert(items.size() == 3);
	assert(items[0] == "apple");
	assert(items[1] == "banana");
	assert(items[2] == "cherry");
	assert(item.str() == "cherry"); // item should capture the last item parsed

	items.clear();
	std::string const list2 = "123 , 456 ,789,987";
	assert(lug::parse(list2, G));
	assert(items.size() == 4);
	assert(items[0] == "123");
	assert(items[1] == "456");
	assert(items[2] == "789");
	assert(items[3] == "987");
	assert(item.str() == "987"); // item should capture the last item parsed

	items.clear();
	std::string_view const list3 = "one_single-item";
	assert(lug::parse(list3, G));
	assert(items.size() == 1);
	assert(items[0] == "one_single-item");
	assert(item.str() == "one_single-item"); // item should capture the last item parsed

	// Test with an invalid list (no items)
	std::string const list4 = "";
	assert(!lug::parse(list4, G));
	// After failing to parse, items should remain unchanged from the last successful parse
	assert(items.size() == 1);
	assert(items[0] == "one_single-item");
	assert(item.str() == "one_single-item");
}

void test_capture_nested_calls()
{
	std::string name;
	std::string sequence;

	auto const add_to_sequence = [&]
	{
		if (!sequence.empty())
			sequence += ',';
		sequence += name;
	};

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		rule call;
		call = lexeme[capture(name)["[a-zA-Z]"_rx > *"[a-zA-Z0-9]"_rx]] > ~('(' > call > ')') < add_to_sequence;
		return start(call > eoi);
	}();

	std::string const text1 = "func";
	assert(lug::parse(text1, G));
	assert(sequence == "func");
	assert(name == "func"); // name is scoped to the recursive rule, so it should reflect the base case

	name.clear();
	sequence.clear();
	std::string const text2 = "func(nestedFunc)";
	assert(lug::parse(text2, G));
	assert(sequence == "nestedFunc,func");
	assert(name == "func"); // name is scoped to the recursive rule, so it should reflect the base case

	name.clear();
	sequence.clear();
	std::string const text3 = "func2(nestedFunc(deeperFunc))";
	assert(lug::parse(text3, G));
	assert(sequence == "deeperFunc,nestedFunc,func2");
	assert(name == "func2"); // name is scoped to the recursive rule, so it should reflect the base case

	std::string const text4 = "func(invalid";
	assert(!lug::parse(text4, G));
	// failure to parse the above should not change captures, as no semantic actions should be executed
	assert(sequence == "deeperFunc,nestedFunc,func2");
	assert(name == "func2"); // name is scoped to the recursive rule, so it should reflect the base case
}

void test_capture_arithmetic_expressions()
{
	std::vector<std::string> numbers;
	std::vector<char> operations;

	auto const add_number = [&](std::string n) { numbers.push_back(std::move(n)); };
	auto const add_operation = [&](std::string_view op) { operations.push_back(op[0]); };

	lug::grammar const G = [&]
	{
		using namespace lug::language;
		rule expression;
		rule number = capture(add_number)[lexeme[+"[0-9]"_rx]];
		rule operation = capture(add_operation)['+'_cx | '-' | '*' | '/'];
		expression = number >> operation;
		return start(expression > eoi);
	}();

	std::stringstream source{"3+5"};
	assert(lug::parse(source, G));
	assert(numbers.size() == 2);
	assert(numbers[0] == "3");
	assert(numbers[1] == "5");
	assert(operations.size() == 1);
	assert(operations[0] == '+');

	numbers.clear();
	operations.clear();
	std::string const source2 = "10-2+4";
	assert(lug::parse(source2, G));
	assert(numbers.size() == 3);
	assert(numbers[0] == "10");
	assert(numbers[1] == "2");
	assert(numbers[2] == "4");
	assert(operations.size() == 2);
	assert(operations[0] == '-');
	assert(operations[1] == '+');

	numbers.clear();
	operations.clear();
	std::string_view const source3 = "8*3/2";
	assert(lug::parse(source3, G));
	assert(numbers.size() == 3);
	assert(numbers[0] == "8");
	assert(numbers[1] == "3");
	assert(numbers[2] == "2");
	assert(operations.size() == 2);
	assert(operations[0] == '*');
	assert(operations[1] == '/');
}

int main()
{
	try {
		test_capture_email_syntax();
		test_capture_url_syntax();
		test_capture_comma_delimited_list();
		test_capture_nested_calls();
		test_capture_arithmetic_expressions();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

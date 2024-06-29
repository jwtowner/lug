// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

#undef NDEBUG
#include <cassert>

static_assert(std::is_default_constructible_v<lug::syntax>);
static_assert(std::is_copy_constructible_v<lug::syntax>);
static_assert(std::is_move_constructible_v<lug::syntax>);
static_assert(std::is_copy_assignable_v<lug::syntax>);
static_assert(std::is_move_assignable_v<lug::syntax>);
static_assert(std::is_destructible_v<lug::syntax>);
static_assert(std::is_swappable_v<lug::syntax>);

static_assert(!std::is_convertible_v<lug::syntax, std::string>);
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
		return start(lexeme[capture(username)[+"[a-zA-Z0-9._%+-]"_rx] > '@'_cx >
							capture(domain)[+"[a-zA-Z0-9-]"_rx] > '.'_cx >
							capture(tld)["[a-zA-Z]"_rx > +"[a-zA-Z]"_rx]]);
	}();

	std::string_view const email = "user@example.com";
	assert(lug::parse(email, G));

	assert(username.capture() == "user");
	assert(domain.capture() == "example");
	assert(tld.capture() == "com");

	assert(username.capture().data() == email.data());
	assert(domain.capture().data() == email.substr(5).data());
	assert(tld.capture().data() == email.substr(13).data());

	std::string const email2 = "not.an@email";
	assert(!lug::parse(email2, G));

	// failure to parse the above should not change captures, as no semantic actions should be executed
	assert(username.capture() == "user");
	assert(domain.capture() == "example");
	assert(tld.capture() == "com");

	assert(username.capture().data() == email.data());
	assert(domain.capture().data() == email.substr(5).data());
	assert(tld.capture().data() == email.substr(13).data());
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
							capture(path)['/'_cx > *"[^?#]"_rx]]);
	}();

	std::string_view const url1 = "https://www.example.com/path/to/resource";
	assert(lug::parse(url1, G));

	assert(protocol == "https");
	assert(domain == "www.example.com");
	assert(path.capture() == "/path/to/resource");

	assert(protocol.data() == url1.data());
	assert(domain.data() != url1.data()); // std::string makes a copy
	assert(path.capture().data() == url1.substr(23).data());

	std::string const url2 = "http://api.example2.com/path/to/other/resource.html";
	assert(lug::parse(url2, G));

	assert(protocol == "http");
	assert(domain == "api.example2.com");
	assert(path.capture() == "/path/to/other/resource.html");

	assert(protocol.data() == url2.c_str());
	assert(domain.data() != url2.data()); // std::string makes a copy
	assert(path.capture().data() == &url2[23]);

	std::string const url3 = "https://www.example3.com$path/to/resource";
	assert(!lug::parse(url3, G));

	// failure to parse the above should not change captures, as no semantic actions should be executed
	assert(protocol == "http");
	assert(domain == "api.example2.com");
	assert(path.capture() == "/path/to/other/resource.html");

	assert(protocol.data() == url2.c_str());
	assert(domain.data() != url2.data()); // std::string makes a copy
	assert(path.capture().data() == &url2[23]);
}

int main()
{
	try {
		test_capture_email_syntax();
		test_capture_url_syntax();
	} catch (std::exception const& e) {
		std::cerr << "Error: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "Unknown Error\n";
		return -1;
	}
	return 0;
}

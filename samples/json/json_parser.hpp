// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_SAMPLES_JSON_JSON_PARSER_HPP
#define LUG_SAMPLES_JSON_JSON_PARSER_HPP

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

#include <map>
#include <string>
#include <variant>
#include <vector>

// JSON node type
struct json_node;

// JSON node value types
using json_null = std::monostate;
using json_bool = bool;
using json_number = double;
using json_string = std::string;
using json_array = std::vector<json_node>;
using json_object = std::map<std::string, json_node>;

// JSON node
struct json_node
{
	std::variant<json_null, json_bool, json_number, json_string, lug::recursive_wrapper<json_array>, lug::recursive_wrapper<json_object>> value;

	json_node() = default;

	template <typename T, class = std::enable_if_t<!std::is_same_v<json_node, std::decay_t<T>>>>
	explicit json_node(T&& v)
		: value(std::forward<T>(v)) {}

	bool is_null() const noexcept { return std::holds_alternative<json_null>(value); }
	bool is_bool() const noexcept { return std::holds_alternative<json_bool>(value); }
	bool is_number() const noexcept { return std::holds_alternative<json_number>(value); }
	bool is_string() const noexcept { return std::holds_alternative<json_string>(value); }
	bool is_array() const noexcept { return std::holds_alternative<lug::recursive_wrapper<json_array>>(value); }
	bool is_object() const noexcept { return std::holds_alternative<lug::recursive_wrapper<json_object>>(value); }

	json_bool as_bool() const { return std::get<json_bool>(value); }
	json_number as_number() const { return std::get<json_number>(value); }
	json_string const& as_string() const { return std::get<json_string>(value); }
	json_array const& as_array() const { return std::get<lug::recursive_wrapper<json_array>>(value); }
	json_object const& as_object() const { return std::get<lug::recursive_wrapper<json_object>>(value); }
};

// Parser for JSON Data Interchange Standard (RFC7159)
class json_parser
{
public:
	json_parser()
	{
		using namespace lug::language;

		// JSON node value factory functions
		auto MakeNull = []{ return json_node{json_null{}}; };
		auto MakeTrue = []{ return json_node{true}; };
		auto MakeFalse = []{ return json_node{false}; };
		auto MakeNumber = [](std::string_view s) { return json_node{std::stod(std::string{s})}; };
		auto MakeKeyOrString = [](std::string_view s) { return std::string{s.substr(1, s.size() - 2)}; };

		// JSON grammar rules
		rule JSON;
		auto ExponentPart   = lexeme[ "Ee"_bx > ~"+-"_bx > +"0-9"_bx ];
		auto FractionalPart = lexeme[ '.'_cx > +"0-9"_bx ];
		auto IntegralPart   = lexeme[ '0'_cx | "1-9"_bx > *"0-9"_bx ];
		auto Number         = lexeme[ ~'-'_cx > IntegralPart > ~FractionalPart > ~ExponentPart ] < MakeNumber;
		auto True           = lexeme[ "true" ] < MakeTrue;
		auto False          = lexeme[ "false" ] < MakeFalse;
		auto Null           = lexeme[ "null" ] < MakeNull;
		auto UnicodeEscape  = lexeme[ 'u' > exactly<4>[ "0-9A-Fa-f"_bx ] ];
		auto Escape         = lexeme[ '\\' > ("/\"\\bfnrt"_bx | UnicodeEscape) ];
		rule KeyOrString    = lexeme[ '"' > *("^\"\\\u0000-\u001F"_bx | Escape) > '"' ] < MakeKeyOrString;
		auto String         = synthesize<json_node, std::string>[ KeyOrString ];
		auto Array          = '[' > synthesize_collect<json_node, json_array>[ JSON >> ',' ] > ']';
		auto Object         = '{' > synthesize_collect<json_node, json_object, std::string, json_node>[ (KeyOrString > ':' > JSON) >> ',' ] > '}';
		JSON                = Object | Array | String | Number | True | False | Null;
		grammar_            = start(JSON > eoi);
	}

	template <typename T>
	std::optional<json_node> parse(T&& t) const
	{
		lug::environment environment;
		if (lug::parse(std::forward<T>(t), grammar_, environment))
			return environment.pop_attribute<json_node>();
		return std::nullopt;
	}

	std::optional<json_node> parse_cin() const
	{
		lug::environment environment;
		if (lug::parse(grammar_, environment))
			return environment.pop_attribute<json_node>();
		return std::nullopt;
	}

private:
	lug::grammar grammar_;
};

#endif // LUG_SAMPLES_JSON_JSON_PARSER_HPP

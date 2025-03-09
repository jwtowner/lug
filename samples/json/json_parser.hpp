// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_SAMPLES_JSON_JSON_PARSER_HPP
#define LUG_SAMPLES_JSON_JSON_PARSER_HPP

#include <lug/lug.hpp>

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
	template <typename T> explicit json_node(T&& v) : value(std::forward<T>(v)) {}

	bool is_null() const { return std::holds_alternative<json_null>(value); }
	bool is_bool() const { return std::holds_alternative<json_bool>(value); }
	bool is_number() const { return std::holds_alternative<json_number>(value); }
	bool is_string() const { return std::holds_alternative<json_string>(value); }
	bool is_array() const { return std::holds_alternative<lug::recursive_wrapper<json_array>>(value); }
	bool is_object() const { return std::holds_alternative<lug::recursive_wrapper<json_object>>(value); }

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
		auto MakeBool = [](std::string_view s) { return json_node{s == "true"}; };
		auto MakeNumber = [](std::string_view s) { return json_node{std::stod(std::string{s})}; };
		auto MakeKeyOrString = [](std::string_view s) { return std::string{s.substr(1, s.size() - 2)}; };

		// JSON grammar rules
		rule JSON;
		rule ExponentPart   = lexeme[ "[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx ];
		rule FractionalPart = lexeme[ "."_sx > +"[0-9]"_rx ];
		rule IntegralPart   = lexeme[ "0"_sx | "[1-9]"_rx > *"[0-9]"_rx ];
		rule Number         = lexeme[ ~"-"_sx > IntegralPart > ~FractionalPart > ~ExponentPart ] < MakeNumber;
		rule Boolean        = lexeme[ "true"_sx | "false" ] < MakeBool;
		rule Null           = lexeme[ "null" ] < MakeNull;
		rule UnicodeEscape  = lexeme[ 'u' > "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]"_rx ];
		rule Escape         = lexeme[ '\\' > ("[/\"\\bfnrt]"_rx | UnicodeEscape) ];
		rule KeyOrString    = lexeme[ '"' > *("[^\"\\\u0000-\u001F]"_rx | Escape) > '"' ] < MakeKeyOrString;
		rule String         = synthesize<json_node, std::string>[ KeyOrString ];
		rule Array          = '[' > synthesize<json_node, json_array>[ collect<json_array>[ JSON >> ',' ] ] > ']';
		rule Object         = '{' > synthesize<json_node, json_object>[ collect<json_object, std::string, json_node>[ ( KeyOrString > ':' > JSON ) >> ',' ] ] > '}';
		JSON                = Null | Boolean | Number | String | Array | Object;
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

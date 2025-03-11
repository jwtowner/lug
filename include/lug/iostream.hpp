// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_IOSTREAM_HPP
#define LUG_INCLUDE_LUG_IOSTREAM_HPP

#include <lug/lug.hpp>
#include <iostream>

#ifndef LUG_NO_ISATTY
#ifdef _MSC_VER
#ifndef LUG_HAS_ISATTY_MSVC
#define LUG_HAS_ISATTY_MSVC
#endif
#else
#ifndef LUG_HAS_ISATTY_POSIX
#ifdef __has_include
#if __has_include(<unistd.h>)
#define LUG_HAS_ISATTY_POSIX
#endif
#endif
#endif
#endif
#endif // LUG_NO_ISATTY

#if defined LUG_HAS_ISATTY_MSVC
#include <io.h>
#elif defined LUG_HAS_ISATTY_POSIX
#include <unistd.h>
#endif

namespace lug {

[[nodiscard]] inline bool stdin_isatty() noexcept
{
#if defined LUG_HAS_ISATTY_MSVC
	return _isatty(_fileno(stdin)) != 0;
#elif defined LUG_HAS_ISATTY_POSIX
	return isatty(fileno(stdin)) != 0;
#else
	return false;
#endif
}

template <class CharT, class Traits, class OutputIt>
std::basic_istream<CharT, Traits>& readsource(std::basic_istream<CharT, Traits>& input, OutputIt output, CharT delim, source_options options = source_options::none)
{
	typename std::basic_istream<CharT, Traits>::sentry sentry{input, true};
	if (!sentry)
		return input;
	std::streamsize count = 0;
	for (typename std::basic_istream<CharT, Traits>::int_type ch = input.rdbuf()->sgetc(); ; ch = input.rdbuf()->snextc()) {
		if (Traits::eq_int_type(ch, Traits::eof())) {
			input.setstate(std::ios_base::eofbit);
			break;
		}
		*output = Traits::to_char_type(ch);
		++output;
		++count;
		if (Traits::eq_int_type(ch, delim) && ((options & source_options::interactive) != source_options::none)) {
			input.rdbuf()->sbumpc();
			break;
		}
	}
	if (count == 0)
		input.setstate(std::ios_base::failbit);
	return input;
}

template <class CharT, class Traits, class OutputIt>
inline std::basic_istream<CharT, Traits>& readsource(std::basic_istream<CharT, Traits>& input, OutputIt output, source_options options = source_options::none)
{
	return lug::readsource(input, output, input.widen('\n'), options);
}

inline bool parse(std::istream& input, grammar const& grmr, environment& envr, char delim, source_options options = source_options::none)
{
	return basic_parser<multi_input_source>{grmr, envr}.push_source([&input, delim](auto output, source_options opt) {
		return static_cast<bool>(lug::readsource(input, output, delim, opt));
	}, options).parse();
}

inline bool parse(std::istream& input, grammar const& grmr, environment& envr, source_options options = source_options::none)
{
	return parse(input, grmr, envr, input.widen('\n'), options);
}

inline bool parse(std::istream& input, grammar const& grmr, source_options opt = source_options::none)
{
	environment envr;
	return parse(input, grmr, envr, opt);
}

inline bool parse(grammar const& grmr, environment& envr, source_options opt)
{
	return parse(std::cin, grmr, envr, opt);
}

inline bool parse(grammar const& grmr, environment& envr)
{
	return parse(grmr, envr, stdin_isatty() ? source_options::interactive : source_options::none);
}

inline bool parse(grammar const& grmr, source_options opt)
{
	return parse(std::cin, grmr, opt);
}

inline bool parse(grammar const& grmr)
{
	return parse(grmr, stdin_isatty() ? source_options::interactive : source_options::none);
}

} // namespace lug

#endif

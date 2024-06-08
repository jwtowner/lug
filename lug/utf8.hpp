// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

// Based on the Flexible and Economical UTF-8 Decoder by Bjoern Hoehrmann
// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See LICENSE.md file or http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
// for more details.

#ifndef LUG_INCLUDE_LUG_UTF8_HPP
#define LUG_INCLUDE_LUG_UTF8_HPP

#include <lug/unicode.hpp>

namespace lug::utf8 {

template <class InputIt, class T = void>
using enable_if_char_input_iterator_t = std::enable_if_t<!std::is_integral_v<InputIt> &&
	std::is_base_of_v<std::input_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category> &&
	std::is_same_v<char, std::remove_cv_t<typename std::iterator_traits<InputIt>::value_type>>, T>;

inline constexpr unsigned int decode_accept = 0;
inline constexpr unsigned int decode_reject = 12;

constexpr bool is_lead(char octet) noexcept
{
	return (static_cast<unsigned char>(octet) & 0xc0) != 0x80;
}

inline unsigned int decode_rune_octet(char32_t& rune, char octet, unsigned int state)
{
	static constexpr std::array<unsigned char, 256> dfa_class_table
	{
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
		 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		 8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
		 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
		10, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3,
		11, 6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
	};

	static constexpr std::array<unsigned char, 108> dfa_transition_table
	{
		 0,12,24,36,60,96,84,12,12,12,48,72,12,12,12,12,
		12,12,12,12,12,12,12,12,12, 0,12,12,12,12,12, 0,
		12, 0,12,12,12,24,12,12,12,12,12,24,12,24,12,12,
		12,12,12,12,12,12,12,24,12,12,12,12,12,24,12,12,
		12,12,12,12,12,24,12,12,12,12,12,12,12,12,12,36,
		12,36,12,12,12,36,12,12,12,12,12,36,12,36,12,12,
		12,36,12,12,12,12,12,12,12,12,12,12
	};

	unsigned int const symbol = static_cast<unsigned char>(octet);
	unsigned int const dfa_class = dfa_class_table[symbol];
	rune = state == decode_accept ? symbol & (0xff >> dfa_class) : (symbol & 0x3f) | (rune << 6);
	return dfa_transition_table[state + dfa_class];
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline std::pair<InputIt, char32_t> decode_rune(InputIt first, InputIt last)
{
	char32_t rune = U'\0';
	unsigned int state = decode_accept;
	while (first != last && state != decode_reject)
		if (state = lug::utf8::decode_rune_octet(rune, *first++, state); state == decode_accept)
			return std::make_pair(first, rune);
	return std::make_pair(std::find_if(first, last, lug::utf8::is_lead), U'\U0000fffd');
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline InputIt next_rune(InputIt first, InputIt last)
{
	return lug::utf8::decode_rune(first, last).first;
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline std::size_t count_runes(InputIt first, InputIt last)
{
	std::size_t count = 0;
	for (; first != last; ++count)
		first = lug::utf8::next_rune(first, last);
	return count;
}

template <class OutputIt>
inline std::pair<OutputIt, bool> encode_rune(OutputIt dst, char32_t rune)
{
	if (rune < 0x80) {
		*dst++ = static_cast<char>(rune);
	} else {
		if (0x00110000U <= rune || (rune & 0xfffff800U) == 0x0000d800U)
			return {std::copy_n(u8"\U0000fffd", 3, dst), false};
		unsigned int const n = rune >= 0x00010000U ? 4 : rune >= 0x00000800U ? 3 : 2;
		for (unsigned int i = 0, c = (0xf0 << (4 - n)) & 0xf0; i < n; ++i, c = 0x80)
			*dst++ = static_cast<char>(((rune >> (6 * (n - i - 1))) & 0x3f) | c);
	}
	return {dst, true};
}

inline std::string encode_rune(char32_t rune)
{
	std::string result;
	encode_rune(std::back_inserter(result), rune);
	return result;
}

template <class InputIt, class OutputIt>
inline OutputIt tocasefold(InputIt first, InputIt last, OutputIt dst)
{
	while (first != last) {
		auto [next, rune] = lug::utf8::decode_rune(first, last);
		dst = lug::utf8::encode_rune(dst, unicode::tocasefold(rune)).first;
		first = next;
	}
	return dst;
}

inline std::string tocasefold(std::string_view src)
{
	std::string result;
	result.reserve(src.size());
	tocasefold(std::begin(src), std::end(src), std::back_inserter(result));
	return result;
}

template <class InputIt, class OutputIt>
inline OutputIt tolower(InputIt first, InputIt last, OutputIt dst)
{
	while (first != last) {
		auto [next, rune] = lug::utf8::decode_rune(first, last);
		dst = lug::utf8::encode_rune(dst, unicode::tolower(rune)).first;
		first = next;
	}
	return dst;
}

inline std::string tolower(std::string_view src)
{
	std::string result;
	result.reserve(src.size());
	tolower(std::begin(src), std::end(src), std::back_inserter(result));
	return result;
}

template <class InputIt, class OutputIt>
inline OutputIt toupper(InputIt first, InputIt last, OutputIt dst)
{
	while (first != last) {
		auto [next, rune] = lug::utf8::decode_rune(first, last);
		dst = lug::utf8::encode_rune(dst, unicode::toupper(rune)).first;
		first = next;
	}
	return dst;
}

inline std::string toupper(std::string_view src)
{
	std::string result;
	result.reserve(src.size());
	toupper(std::begin(src), std::end(src), std::back_inserter(result));
	return result;
}

} // namespace lug::utf8

#endif

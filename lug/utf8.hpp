// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details
//
// Based on the Flexible and Economical UTF-8 Decoder by Bjoern Hoehrmann
// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See LICENSE.md file or http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
// for more details.

#ifndef LUG_UTF8_HPP__
#define LUG_UTF8_HPP__

#include <array>
#include <algorithm>
#include <iterator>
#include <utility>
#include <type_traits>

namespace lug::utf8
{

template <class InputIt, class T = void>
using enable_if_char_input_iterator_t = std::enable_if_t<!std::is_integral_v<InputIt> &&
	std::is_base_of_v<std::input_iterator_tag, typename std::iterator_traits<InputIt>::iterator_category> &&
	std::is_same_v<char, std::remove_cv_t<typename std::iterator_traits<InputIt>::value_type>>, T>;

constexpr unsigned int decode_accept = 0;
constexpr unsigned int decode_reject = 12;

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
inline std::pair<char32_t, InputIt> decode_rune(InputIt first, InputIt last)
{
	char32_t rune = U'\0';
	unsigned int state = decode_accept;
	while (first != last && state != decode_reject)
		if (state = ::lug::utf8::decode_rune_octet(rune, *first++, state); state == decode_accept)
			return ::std::make_pair(rune, first);
	return ::std::make_pair(U'\U0000fffd', ::std::find_if(first, last, ::lug::utf8::is_lead));
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline InputIt next_rune(InputIt first, InputIt last)
{
	return ::lug::utf8::decode_rune(first, last).second;
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline std::size_t size_of_first_rune(InputIt first, InputIt last)
{
	return static_cast<std::size_t>(::std::distance(first, ::lug::utf8::next_rune(first, last)));
}

template <class InputIt, class = enable_if_char_input_iterator_t<InputIt>>
inline std::size_t count_runes(InputIt first, InputIt last)
{
	std::size_t count = 0;
	for ( ; first != last; ++count)
		first = ::lug::utf8::next_rune(first, last);
	return count;
}

} // namespace lug::utf8

#endif

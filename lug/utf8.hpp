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

#include <cstddef>
#include <iterator>
#include <utility>

namespace lug::utf8
{

constexpr bool is_lead(char octet) noexcept {
	return (static_cast<unsigned char>(octet) & 0xc0) != 0x80;
}

constexpr unsigned int decode_accept = 0;
constexpr unsigned int decode_reject = 12;

inline unsigned int decode_rune_octet(char32_t& rune, char octet, unsigned int state) {
	static constexpr unsigned char dfa_class[256] = {
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
		11, 6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 };

	static constexpr unsigned char dfa_transition[108] = {
		 0,12,24,36,60,96,84,12,12,12,48,72,12,12,12,12,
		12,12,12,12,12,12,12,12,12, 0,12,12,12,12,12, 0,
		12, 0,12,12,12,24,12,12,12,12,12,24,12,24,12,12,
		12,12,12,12,12,12,12,24,12,12,12,12,12,24,12,12,
		12,12,12,12,12,24,12,12,12,12,12,12,12,12,12,36,
		12,36,12,12,12,36,12,12,12,12,12,36,12,36,12,12,
		12,36,12,12,12,12,12,12,12,12,12,12 };

	const unsigned int uo = static_cast<unsigned char>(octet);
	const unsigned int cc = dfa_class[uo];
	rune = state == decode_accept ? uo & (0xff >> cc) : (uo & 0x3f) | (rune << 6);
	return dfa_transition[state + cc];
}

template <class InputIt>
inline std::pair<char32_t, InputIt> decode_rune(InputIt first, InputIt last) {
	char32_t rune = U'\0';
	unsigned int state = decode_accept;
	while (first != last && state != decode_reject) {
		state = ::lug::utf8::decode_rune_octet(rune, *first++, state);
		if (state == decode_accept)
			return ::std::make_pair(rune, first);
	}
	return ::std::make_pair(U'\Ufffd', ::std::find_if(first, last, ::lug::utf8::is_lead));
}

template <class InputIt>
inline InputIt next_rune(InputIt first, InputIt last) {
	return ::lug::utf8::decode_rune(first, last).second;
}

template <class InputIt>
inline std::size_t size_of_first_rune(InputIt first, InputIt last) {
	return static_cast<std::size_t>(::std::distance(first, ::lug::utf8::next_rune(first, last)));
}

template <class InputIt>
inline std::size_t count_runes(InputIt first, InputIt last) {
	std::size_t count = 0;
	for ( ; first != last; ++count)
		first = ::lug::utf8::next_rune(first, last);
	return count;
}

} // namespace lug::utf8

#endif

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_ASCII_HPP
#define LUG_INCLUDE_LUG_ASCII_HPP

#include <lug/detail.hpp>
#include <optional>

#undef isascii
#undef toascii
#undef tolower
#undef toupper

namespace lug::ascii {

struct ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE static constexpr auto inrange(int c, int cmin, int cmax) noexcept -> int
	{
		return ((static_cast<unsigned int>(c) - static_cast<unsigned int>(cmin)) <= static_cast<unsigned int>(cmax - cmin));
	}
};

struct ascii_isascii_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return static_cast<unsigned int>(c) <= 0x7fU;
	}
};

inline constexpr ascii_isascii_fn isascii{};

struct ascii_toascii_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> int
	{
		return static_cast<int>(static_cast<unsigned int>(c) & 0x7fU);
	}
};

inline constexpr ascii_toascii_fn toascii{};

struct ascii_islower_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, 'a', 'z');
	}
};

inline constexpr ascii_islower_fn islower{};

struct ascii_isupper_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, 'A', 'Z');
	}
};

inline constexpr ascii_isupper_fn isupper{};

struct ascii_tolower_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> int
	{
		return static_cast<int>(static_cast<unsigned int>(c) ^ (~(static_cast<unsigned int>(isupper(c)) - 1U) & 0x20U));
	}
};

inline constexpr ascii_tolower_fn tolower{};

struct ascii_toupper_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> int
	{
		return static_cast<int>(static_cast<unsigned int>(c) ^ (~(static_cast<unsigned int>(islower(c)) - 1U) & 0x20U));
	}
};

inline constexpr ascii_toupper_fn toupper{};

struct ascii_isalpha_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return islower(static_cast<int>(static_cast<unsigned int>(c) | 0x20U));
	}
};

inline constexpr ascii_isalpha_fn isalpha{};

struct ascii_isblank_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c == ' ') || (c == '\t');
	}
};

inline constexpr ascii_isblank_fn isblank{};

struct ascii_iscntrl_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c <= '\x1f') || (c == '\x7f');
	}
};

inline constexpr ascii_iscntrl_fn iscntrl{};

struct ascii_isdigit_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, '0', '9');
	}
};

inline constexpr ascii_isdigit_fn isdigit{};

struct ascii_isgraph_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, '!', '~');
	}
};

inline constexpr ascii_isgraph_fn isgraph{};

struct ascii_isprint_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, ' ', '~');
	}
};

inline constexpr ascii_isprint_fn isprint{};

struct ascii_isspace_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c == ' ') || inrange(c, '\t', '\r');
	}
};

inline constexpr ascii_isspace_fn isspace{};

struct ascii_isxdigit_fn : private ascii_inrange_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isdigit(c) || inrange(static_cast<int>(static_cast<unsigned int>(c) | 0x20U), 'a', 'f');
	}
};

inline constexpr ascii_isxdigit_fn isxdigit{};

struct ascii_isalnum_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isalpha(c) || isdigit(c);
	}
};

inline constexpr ascii_isalnum_fn isalnum{};

struct ascii_ispunct_fn
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isgraph(c) && !isalnum(c);
	}
};

inline constexpr ascii_ispunct_fn ispunct{};

struct ascii_match_space_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if ((c == ' ') || (('\t' <= c) && (c <= '\r'))) {
				++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_space_fn match_space{};

struct ascii_match_blank_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if ((c == ' ') || (c == '\t')) {
				++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_blank_fn match_blank{};

struct ascii_match_eol_fn
{
	template <class InputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputIt first, InputIt last) const -> std::optional<std::decay_t<InputIt>>
	{
		if LUG_LIKELY(first != last) {
			char const c = *first;
			if (('\n' <= c) && (c <= '\f')) {
				++first;
				return first;
			}
			if (c == '\r') {
				++first;
				if ((first != last) && (*first == '\n'))
					++first;
				return first;
			}
		}
		return std::nullopt;
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(InputRng&& rng) const -> std::optional<std::decay_t<decltype(std::begin(rng))>> // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng));
	}
};

inline constexpr ascii_match_eol_fn match_eol{};

} // namespace lug::ascii

#endif

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_ASCII_HPP
#define LUG_INCLUDE_LUG_ASCII_HPP

#include <lug/detail.hpp>

#include <array>
#include <bitset>
#include <optional>

#undef isascii
#undef toascii
#undef tolower
#undef toupper

namespace lug::ascii {

enum class ctype : std::uint_least16_t
{
	none     = 0,
	alpha    = 1U <<  0U,
	lower    = 1U <<  1U,
	upper    = 1U <<  2U,
	punct    = 1U <<  3U,
	digit    = 1U <<  4U,
	xdigit   = 1U <<  5U,
	alnum    = 1U <<  6U,
	space    = 1U <<  7U,
	blank    = 1U <<  8U,
	cntrl    = 1U <<  9U,
	graph    = 1U << 10U,
	print    = 1U << 11U,
	word     = 1U << 12U
};

} // namespace lug::ascii

template <> inline constexpr bool lug::is_flag_enum_v<lug::ascii::ctype> = true;

namespace lug::ascii {

inline constexpr std::array<ctype, 128> ascii_ctype_table
{
	ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},
	ctype{0x0200},ctype{0x0380},ctype{0x0280},ctype{0x0280},ctype{0x0280},ctype{0x0280},ctype{0x0200},ctype{0x0200},
	ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},
	ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},ctype{0x0200},
	ctype{0x0980},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},
	ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},
	ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},ctype{0x1c70},
	ctype{0x1c70},ctype{0x1c70},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},
	ctype{0x0c08},ctype{0x1c65},ctype{0x1c65},ctype{0x1c65},ctype{0x1c65},ctype{0x1c65},ctype{0x1c65},ctype{0x1c45},
	ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},
	ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},
	ctype{0x1c45},ctype{0x1c45},ctype{0x1c45},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x1c08},
	ctype{0x0c08},ctype{0x1c63},ctype{0x1c63},ctype{0x1c63},ctype{0x1c63},ctype{0x1c63},ctype{0x1c63},ctype{0x1c43},
	ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},
	ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},
	ctype{0x1c43},ctype{0x1c43},ctype{0x1c43},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0c08},ctype{0x0200}
};

struct ascii_niebloid_base
{
	template <class T> using char_type = std::conditional_t<std::is_same_v<std::decay_t<T>, char>, char, int>;
	
	static constexpr unsigned int alpha_mask = 0x20U;
	static constexpr unsigned int ascii_max = 0x7fU;

	[[nodiscard]] LUG_ALWAYS_INLINE static constexpr auto inrange(int c, int cmin, int cmax) noexcept -> bool
	{
		return ((static_cast<unsigned int>(c) - static_cast<unsigned int>(cmin)) <= static_cast<unsigned int>(cmax - cmin));
	}
};

struct ascii_isascii_fn : private ascii_niebloid_base
{
	template <class T, class = std::enable_if_t<std::is_integral_v<T>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(T c) const noexcept -> bool
	{
		if constexpr (std::is_signed_v<T>) {
			return static_cast<unsigned int>(static_cast<int>(c)) <= ascii_max;
		} else {
			return static_cast<unsigned int>(c) <= ascii_max;
		}
	}
};

inline constexpr ascii_isascii_fn isascii{};

struct ascii_toascii_fn : private ascii_niebloid_base
{
	template <class T, class = std::enable_if_t<std::is_signed_v<std::decay_t<T>>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(T c) const noexcept -> char_type<T>
	{
		return static_cast<char_type<T>>(static_cast<int>(static_cast<unsigned int>(static_cast<std::make_unsigned_t<T>>(c)) & ascii_max));
	}
};

inline constexpr ascii_toascii_fn toascii{};

struct ascii_islower_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, 'a', 'z');
	}
};

inline constexpr ascii_islower_fn islower{};

struct ascii_isupper_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, 'A', 'Z');
	}
};

inline constexpr ascii_isupper_fn isupper{};

struct ascii_tolower_fn : private ascii_niebloid_base
{
	template <class T, class = std::enable_if_t<std::is_signed_v<std::decay_t<T>>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(T c) const noexcept -> char_type<T>
	{
		return static_cast<char_type<T>>(static_cast<int>(
			static_cast<unsigned int>(static_cast<std::make_unsigned_t<T>>(c))
			^ (~(static_cast<unsigned int>(isupper(c)) - 1U) & alpha_mask)));
	}

	template <class InputIt, class OutputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	constexpr auto operator()(InputIt first, InputIt last, OutputIt dst) const -> OutputIt
	{
		for ( ; first != last; ++dst, ++first)
			*dst = static_cast<char>((*this)(*first));
		return dst;
	}

	template <class InputRng, class OutputIt, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	constexpr auto operator()(InputRng&& rng, OutputIt dst) const -> OutputIt // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng), dst);
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng> && !std::is_convertible_v<InputRng&&, std::string_view>>>
	[[nodiscard]] auto operator()(InputRng&& rng) const -> std::string // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		std::string result;
		result.reserve(rng.size());
		(*this)(std::begin(rng), std::end(rng), std::back_inserter(result));
		return result;
	}

	[[nodiscard]] auto operator()(std::string_view str) const -> std::string
	{
		std::string result;
		result.reserve(str.size());
		(*this)(std::begin(str), std::end(str), std::back_inserter(result));
		return result;
	}
};

inline constexpr ascii_tolower_fn tolower{};

struct ascii_toupper_fn : private ascii_niebloid_base
{
	template <class T, class = std::enable_if_t<std::is_signed_v<std::decay_t<T>>>>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(T c) const noexcept -> char_type<T>
	{
		return static_cast<char_type<T>>(static_cast<int>(
			static_cast<unsigned int>(static_cast<std::make_unsigned_t<T>>(c))
			^ (~(static_cast<unsigned int>(islower(c)) - 1U) & alpha_mask)));
	}

	template <class InputIt, class OutputIt, class = std::enable_if_t<lug::detail::is_char_input_iterator_v<InputIt>>>
	constexpr auto operator()(InputIt first, InputIt last, OutputIt dst) const -> OutputIt
	{
		for ( ; first != last; ++dst, ++first)
			*dst = static_cast<char>((*this)(*first));
		return dst;
	}

	template <class InputRng, class OutputIt, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng>>>
	constexpr auto operator()(InputRng&& rng, OutputIt dst) const -> OutputIt // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return (*this)(std::begin(rng), std::end(rng), dst);
	}

	template <class InputRng, class = std::enable_if_t<lug::detail::is_char_input_range_v<InputRng> && !std::is_convertible_v<InputRng&&, std::string_view>>>
	[[nodiscard]] auto operator()(InputRng&& rng) const -> std::string // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		std::string result;
		result.reserve(rng.size());
		(*this)(std::begin(rng), std::end(rng), std::back_inserter(result));
		return result;
	}

	[[nodiscard]] auto operator()(std::string_view str) const -> std::string
	{
		std::string result;
		result.reserve(str.size());
		(*this)(std::begin(str), std::end(str), std::back_inserter(result));
		return result;
	}
};

inline constexpr ascii_toupper_fn toupper{};

struct ascii_isalpha_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return islower(static_cast<int>(static_cast<unsigned int>(c) | alpha_mask));
	}
};

inline constexpr ascii_isalpha_fn isalpha{};

struct ascii_isblank_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c == ' ') || (c == '\t');
	}
};

inline constexpr ascii_isblank_fn isblank{};

struct ascii_iscntrl_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c <= '\x1f') || (c == '\x7f');
	}
};

inline constexpr ascii_iscntrl_fn iscntrl{};

struct ascii_isdigit_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, '0', '9');
	}
};

inline constexpr ascii_isdigit_fn isdigit{};

struct ascii_isgraph_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, '!', '~');
	}
};

inline constexpr ascii_isgraph_fn isgraph{};

struct ascii_isprint_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return inrange(c, ' ', '~');
	}
};

inline constexpr ascii_isprint_fn isprint{};

struct ascii_isspace_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c == ' ') || inrange(c, '\t', '\r');
	}
};

inline constexpr ascii_isspace_fn isspace{};

struct ascii_isxdigit_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isdigit(c) || inrange(static_cast<int>(static_cast<unsigned int>(c) | alpha_mask), 'a', 'f');
	}
};

inline constexpr ascii_isxdigit_fn isxdigit{};

struct ascii_isalnum_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isalpha(c) || isdigit(c);
	}
};

inline constexpr ascii_isalnum_fn isalnum{};

struct ascii_ispunct_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return isgraph(c) && !isalnum(c);
	}
};

inline constexpr ascii_ispunct_fn ispunct{};

struct ascii_isword_fn : private ascii_niebloid_base
{
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto operator()(int c) const noexcept -> bool
	{
		return (c == '_') || isalnum(c);
	}
};

inline constexpr ascii_isword_fn isword{};

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

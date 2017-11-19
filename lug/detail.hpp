// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_DETAIL_HPP__
#define LUG_DETAIL_HPP__

#include <functional>
#include <limits>
#include <string>
#include <string_view>
#include <type_traits>

namespace lug
{

inline namespace bitfield_ops
{

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator~(T x) noexcept
{
	return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator&(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) & static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator|(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) | static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T operator^(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) ^ static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator&=(T& x, T y) noexcept
{
	return (x = x & y);
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator|=(T& x, T y) noexcept
{
	return (x = x | y);
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
constexpr T& operator^=(T& x, T y) noexcept
{
	return (x = x ^ y);
}

} // namespace bitfield_ops

namespace detail
{

template <class T>
struct dynamic_cast_if_base_of
{
	std::remove_reference_t<T>& value;

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	operator U&() const volatile {
		return dynamic_cast<std::remove_reference_t<U>&>(value);
	}
};

template <class Error>
struct reentrancy_sentinel
{
	bool& value;

	reentrancy_sentinel(bool& x) : value{x} {
		if (value)
			throw Error{};
		value = true;
	}

	~reentrancy_sentinel() {
		value = false;
	}
};

template <class Error, class T, class U, class V>
inline void assure_in_range(T x, U minval, V maxval)
{
	if (!(minval <= x && x <= maxval))
		throw Error{};
}

template <class Error, class T, class U>
inline auto checked_add(T x, U y)
{
	if ((std::numeric_limits<decltype(x + y)>::max)() - x < y)
		throw Error{};
	return x + y;
}

template <std::size_t... Indices, class Tuple>
constexpr auto make_tuple_view(Tuple&& t) noexcept
{
	return ::std::forward_as_tuple(::std::get<Indices>(::std::forward<Tuple>(t))...);
}

template<class InputIt, class UnaryPredicate>
InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate p)
{
	for ( ; first != last; ++first) {
		if (int status = p(*first); status > 0)
			return first;
		else if (status < 0)
			break;
	}
	return last;
}

template <class Sequence>
inline auto pop_back(Sequence& s)
{
	typename Sequence::value_type result{::std::move(s.back())};
	s.pop_back();
	return result;
}

template <class Integral>
inline std::string string_pack(Integral n) noexcept
{
	return std::string{reinterpret_cast<char const*>(&n), sizeof(n)};
}

template <class Integral>
inline Integral string_unpack(std::string_view s) noexcept
{
	return *reinterpret_cast<Integral const*>(s.data());
}

} // namespace detail

} // namespace lug

#endif

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_DETAIL_HPP
#define LUG_INCLUDE_LUG_DETAIL_HPP

#include <algorithm>
#include <exception>
#include <functional>
#include <iterator>
#include <limits>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

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

#ifndef LUG_NO_RTTI
#if defined __GNUC__
#ifndef __GXX_RTTI
#define LUG_NO_RTTI
#endif
#elif defined _MSC_VER
#ifndef _CPPRTTI
#define LUG_NO_RTTI
#endif
#endif
#endif // LUG_NO_RTTI

#ifdef __GNUC__

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wlogical-not-parentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wuninitialized\"") \
_Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
_Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"")

#define LUG_DIAGNOSTIC_POP \
_Pragma("GCC diagnostic pop")

#else

#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE
#define LUG_DIAGNOSTIC_POP

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

inline namespace bitfield_ops {

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator~(T x) noexcept
{
	return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator&(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) & static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator|(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) | static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::void_t<decltype(T::is_bitfield_enum)>>
[[nodiscard]] constexpr T operator^(T x, T y) noexcept
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

namespace detail {

template <class T> inline constexpr bool always_false_v = false;

template <class V, class R, class Fn, class... Args> struct is_invocable_r_exact_impl : std::false_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact_impl<std::void_t<std::enable_if_t<std::is_same_v<R, std::invoke_result_t<Fn, Args...>>>>, R, Fn, Args...> : std::true_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact : is_invocable_r_exact_impl<void, R, Fn, Args...> {};
template <class R, class Fn, class... Args> inline constexpr bool is_invocable_r_exact_v = is_invocable_r_exact<R, Fn, Args...>::value;

template <class T> struct remove_cvref_from_tuple;
template <class T> struct remove_cvref_from_tuple<T const> : remove_cvref_from_tuple<T> {};
template <class T> struct remove_cvref_from_tuple<T volatile> : remove_cvref_from_tuple<T> {};
template <class T> struct remove_cvref_from_tuple<T const volatile> : remove_cvref_from_tuple<T> {};
template <class... Args> struct remove_cvref_from_tuple<std::tuple<Args...>> { using type = std::tuple<std::remove_cv_t<std::remove_reference_t<Args>>...>; };
template <class T> using remove_cvref_from_tuple_t = typename remove_cvref_from_tuple<T>::type;

template <class It>
inline constexpr bool is_char_input_iterator_v =
	!std::is_integral_v<It> &&
	std::is_base_of_v<std::input_iterator_tag, typename std::iterator_traits<It>::iterator_category> &&
	std::is_same_v<char, std::remove_cv_t<typename std::iterator_traits<It>::value_type>>;

template <class It>
inline constexpr bool is_char_contiguous_iterator_v =
	std::is_convertible_v<It, std::vector<char>::const_iterator> ||
	std::is_convertible_v<It, std::string::const_iterator> ||
	std::is_convertible_v<It, std::string_view::const_iterator> ||
	std::is_same_v<std::decay_t<It>, char*> ||
	std::is_same_v<std::decay_t<It>, const char*>;

template <class Rng> using range_begin_t = std::decay_t<decltype(std::declval<Rng>().begin())>;
template <class Rng> using range_end_t = std::decay_t<decltype(std::declval<Rng>().end())>;

template <class Rng, class = void> struct is_char_input_range_impl : std::false_type {};
template <class Rng> struct is_char_input_range_impl<Rng, std::void_t<decltype(std::declval<Rng>().begin()), decltype(std::declval<Rng>().end())>> : std::integral_constant<bool, is_char_input_iterator_v<range_begin_t<Rng>> && is_char_input_iterator_v<range_end_t<Rng>>> {};
template <class Rng> inline constexpr bool is_char_input_range_v = is_char_input_range_impl<Rng>::value;

template <class Rng, class = void> struct is_char_contiguous_range_impl : std::false_type {};
template <class Rng> struct is_char_contiguous_range_impl<Rng, std::void_t<decltype(std::declval<Rng>().begin()), decltype(std::declval<Rng>().end())>> : std::integral_constant<bool, is_char_contiguous_iterator_v<range_begin_t<Rng>> && is_char_contiguous_iterator_v<range_end_t<Rng>>> {};
template <class Rng> inline constexpr bool is_char_contiguous_range_v = is_char_contiguous_range_impl<Rng>::value;

template <class It, class T = void> using enable_if_char_input_iterator_t = std::enable_if_t<is_char_input_iterator_v<It>, T>;
template <class It, class T = void> using enable_if_char_contiguous_iterator_t = std::enable_if_t<is_char_contiguous_iterator_v<It>, T>;
template <class Rng, class T = void> using enable_if_char_input_range_t = std::enable_if_t<is_char_input_range_v<Rng>, T>;
template <class Rng, class T = void> using enable_if_char_contiguous_range_t = std::enable_if_t<is_char_contiguous_range_v<Rng>, T>;

struct identity
{
	template <class T>
	[[nodiscard]] constexpr T&& operator()( T&& t ) const noexcept
	{
		return std::forward<T>(t);
	}
};

template <class T> struct member_pointer_object {};
template <class T, class U> struct member_pointer_object<T U::*> { using type = U; };
template <class T> struct member_pointer_value {};
template <class T, class U> struct member_pointer_value<T U::*> { using type = T; };

template <class ObjectIterator, class MemberPtrType, MemberPtrType MemberPtr>
class member_access_iterator
{
public:
	using base_type = ObjectIterator;
	using object_type = typename member_pointer_object<MemberPtrType>::type;
	using value_type = typename member_pointer_value<MemberPtrType>::type;
	using reference = value_type&;
	using pointer = value_type*;
	using difference_type = typename std::iterator_traits<base_type>::difference_type;
	using iterator_category = typename std::iterator_traits<base_type>::iterator_category;
	constexpr member_access_iterator() noexcept : object_{} {}
	constexpr explicit member_access_iterator(ObjectIterator obj) : object_{obj} {}
	[[nodiscard]] constexpr base_type base() const { return object_; }
	[[nodiscard]] constexpr pointer operator->() const { return (*object_).*MemberPtr; }
	[[nodiscard]] constexpr reference operator*() const { return (*object_).*MemberPtr; }
	[[nodiscard]] constexpr reference operator[](difference_type n) const { return (object_[n]).*MemberPtr; }
	constexpr member_access_iterator& operator++() { ++object_; return *this; }
	constexpr member_access_iterator& operator--() { --object_; return *this; }
	constexpr member_access_iterator operator++(int) { member_access_iterator const i{object_}; ++object_; return i; }
	constexpr member_access_iterator operator--(int) { member_access_iterator const i{object_}; --object_; return i; }
	constexpr member_access_iterator& operator+=(difference_type n) { object_ += n; return *this; }
	constexpr member_access_iterator& operator-=(difference_type n) { object_ -= n; return *this; }
	[[nodiscard]] friend constexpr bool operator==(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ == y.object_; }
	[[nodiscard]] friend constexpr bool operator!=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ != y.object_; }
	[[nodiscard]] friend constexpr bool operator<(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ < y.object_; }
	[[nodiscard]] friend constexpr bool operator<=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ <= y.object_; }
	[[nodiscard]] friend constexpr bool operator>(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ > y.object_; }
	[[nodiscard]] friend constexpr bool operator>=(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ >= y.object_; }
	[[nodiscard]] friend constexpr member_access_iterator operator+(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ + n}; }
	[[nodiscard]] friend constexpr member_access_iterator operator+(difference_type n, member_access_iterator const& x) noexcept { return member_access_iterator{x.object_ + n}; }
	[[nodiscard]] friend constexpr member_access_iterator operator-(member_access_iterator const& x, difference_type n) noexcept { return member_access_iterator{x.object_ - n}; }
	[[nodiscard]] friend constexpr difference_type operator-(member_access_iterator const& x, member_access_iterator const& y) noexcept { return x.object_ - y.object_; }
private:
	ObjectIterator object_;
};

template <class MemberPtrType, MemberPtrType MemberPtr, class ObjectIterator>
[[nodiscard]] constexpr auto make_member_accessor(ObjectIterator b)
{
	return member_access_iterator<ObjectIterator, MemberPtrType, MemberPtr>{b};
}

template <class T>
class dynamic_cast_if_base_of
{
	std::reference_wrapper<std::remove_reference_t<T>> value_;

public:
	constexpr explicit dynamic_cast_if_base_of(std::remove_reference_t<T>& x) noexcept : value_{x} {}

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	[[nodiscard]] constexpr operator U&() const noexcept(std::is_same_v<std::decay_t<T>, std::decay_t<U>>) // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	{
#ifndef LUG_NO_RTTI
		if constexpr (std::is_same_v<std::decay_t<T>, std::decay_t<U>>)
#endif // LUG_NO_RTTI
			return static_cast<std::remove_reference_t<U>&>(value_.get());
#ifndef LUG_NO_RTTI
		else
			return dynamic_cast<std::remove_reference_t<U>&>(value_.get());
#endif // LUG_NO_RTTI
	}
};

template <class Error>
class reentrancy_sentinel
{
	std::reference_wrapper<bool> value_;

public:
	constexpr explicit reentrancy_sentinel(bool& x)
		: value_{x}
	{
		if (value_.get())
			throw Error();
		value_.get() = true;
	}

	~reentrancy_sentinel()
	{
		value_.get() = false;
	}

	reentrancy_sentinel(reentrancy_sentinel const&) = delete;
	reentrancy_sentinel(reentrancy_sentinel&&) = delete;
	reentrancy_sentinel& operator=(reentrancy_sentinel const&) = delete;
	reentrancy_sentinel& operator=(reentrancy_sentinel&&) = delete;
};

template <class EF>
class scope_exit
{
	static_assert(std::is_invocable_v<EF>);

	EF destructor_;
	bool released_{false};

public:
	template <class Fn, class = std::enable_if_t<std::is_constructible_v<EF, Fn&&>>>
	constexpr explicit scope_exit( Fn&& fn ) noexcept(std::is_nothrow_constructible_v<EF, Fn&&>)
		: destructor_{std::forward<Fn>(fn)}
	{}

	~scope_exit()
	{
		if (!released_)
			destructor_();
	}

	void release() noexcept
	{
		released_ = true;
	}

	scope_exit(scope_exit const&) = delete;
	scope_exit(scope_exit&&) = delete;
	scope_exit& operator=(scope_exit const&) = delete;
	scope_exit& operator=(scope_exit&&) = delete;
};

template <class Fn, class = std::enable_if_t<std::is_invocable_v<Fn>>>
scope_exit(Fn) -> scope_exit<std::decay_t<Fn>>;

template <class EF>
class scope_fail
{
	static_assert(std::is_invocable_v<EF>);

	EF destructor_;
	int uncaught_on_construction_;

public:
	template <class Fn, class = std::enable_if_t<std::is_constructible_v<EF, Fn&&>>>
	constexpr explicit scope_fail( Fn&& fn ) noexcept(std::is_nothrow_constructible_v<EF, Fn&&>)
		: destructor_{std::forward<Fn>(fn)}
		, uncaught_on_construction_(std::uncaught_exceptions())
	{}

	~scope_fail()
	{
		if (std::uncaught_exceptions() > uncaught_on_construction_)
			destructor_();
	}

	void release() noexcept
	{
		uncaught_on_construction_ = (std::numeric_limits<int>::max)();
	}

	scope_fail(scope_fail const&) = delete;
	scope_fail(scope_fail&&) = delete;
	scope_fail& operator=(scope_fail const&) = delete;
	scope_fail& operator=(scope_fail&&) = delete;
};

template <class Fn, class = std::enable_if_t<std::is_invocable_v<Fn>>>
scope_fail(Fn) -> scope_fail<std::decay_t<Fn>>;

template <class Error, class T, class U, class V, class = std::enable_if_t<std::is_integral_v<T> && std::is_integral_v<U> && std::is_integral_v<V>>>
constexpr void assure_in_range(T x, U minval, V maxval)
{
	if (!((minval <= x) && (x <= maxval)))
		throw Error();
}

template <class T, class Error, class S, class U, class V, class = std::enable_if_t<std::is_integral_v<T> && std::is_integral_v<S> && std::is_integral_v<U> && std::is_integral_v<V>>>
[[nodiscard]] constexpr T checked_cast(S x, U minval, V maxval)
{
	detail::assure_in_range<Error>(x, minval, maxval);
	return static_cast<T>(x);
}

template <class T, class Error, class S, class = std::enable_if_t<std::is_integral_v<T> && std::is_integral_v<S>>>
[[nodiscard]] constexpr T checked_cast(S x)
{
	return detail::checked_cast<T, Error>(x, (std::numeric_limits<std::decay_t<T>>::min)(), (std::numeric_limits<std::decay_t<T>>::max)());
}

template <class Error, class T, class U, class = std::enable_if_t<std::is_integral_v<T> && std::is_integral_v<U>>>
[[nodiscard]] constexpr auto checked_add(T x, U y)
{
	if (((std::numeric_limits<decltype(x + y)>::max)() - x) < y)
		throw Error();
	return x + y;
}

template<class InputIt, class UnaryPredicate>
[[nodiscard]] constexpr InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate pred)
{
	for ( ; first != last; ++first) {
		const int status = pred(*first);
		if (status > 0)
			return first;
		if (status < 0)
			break;
	}
	return last;
}

template <class Sequence>
[[nodiscard]] constexpr auto pop_back(Sequence& s) -> typename Sequence::value_type
{
	typename Sequence::value_type result{std::move(s.back())}; // NOLINT(misc-const-correctness)
	s.pop_back();
	return result;
}

template <class Integral>
[[nodiscard]] inline std::string string_pack(Integral n)
{
	return std::string{reinterpret_cast<char const*>(&n), sizeof(n)}; // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
}

template <class Integral>
[[nodiscard]] inline Integral string_unpack(std::string_view s)
{
	return *reinterpret_cast<Integral const*>(s.data()); // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
}

template <class T, class = std::enable_if_t<std::is_signed_v<T>>>
[[nodiscard]] constexpr T sar(T x, unsigned int n) noexcept
{
	if constexpr ((static_cast<T>(-1) >> 1U) == static_cast<T>(-1)) { // NOLINT(hicpp-signed-bitwise)
		return x >> n; // NOLINT(hicpp-signed-bitwise)
	} else {
		using U = std::make_unsigned_t<T>;
		auto const shifted = static_cast<U>(x) >> n;
		auto const sign_mask = static_cast<U>(-static_cast<T>(x < 0)) << (static_cast<unsigned int>(std::numeric_limits<U>::digits) - n);
		return static_cast<T>(shifted | sign_mask);
	}
}

} // namespace detail

} // namespace lug

#endif

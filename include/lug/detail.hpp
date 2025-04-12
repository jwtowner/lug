// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_DETAIL_HPP
#define LUG_INCLUDE_LUG_DETAIL_HPP

#include <lug/config.hpp>
#include <lug/error.hpp>

#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <functional>
#include <iterator>
#include <limits>
#include <new>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

namespace lug {

template <class T> inline constexpr bool is_flag_enum_v = false;

inline namespace flag_enum_ops {

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr T operator~(T x) noexcept
{
	return static_cast<T>(~static_cast<std::underlying_type_t<T>>(x));
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr T operator&(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) & static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr T operator|(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) | static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr T operator^(T x, T y) noexcept
{
	return static_cast<T>(static_cast<std::underlying_type_t<T>>(x) ^ static_cast<std::underlying_type_t<T>>(y));
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
LUG_ALWAYS_INLINE constexpr T& operator&=(T& x, T y) noexcept
{
	return (x = x & y);
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
LUG_ALWAYS_INLINE constexpr T& operator|=(T& x, T y) noexcept
{
	return (x = x | y);
}

template <class T, class = std::enable_if_t<is_flag_enum_v<T>>>
LUG_ALWAYS_INLINE constexpr T& operator^=(T& x, T y) noexcept
{
	return (x = x ^ y);
}

} // namespace flag_enum_ops

namespace detail {

template <class T> inline constexpr bool always_false_v = false;

template <class T, class U = T, class = void> struct is_equality_comparable : std::false_type {};
template <class T, class U> struct is_equality_comparable<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>> : std::true_type {};
template <class T, class U = T> inline constexpr bool is_equality_comparable_v = is_equality_comparable<T, U>::value;

template <class V, class R, class Fn, class... Args> struct is_invocable_r_exact_impl : std::false_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact_impl<std::void_t<std::enable_if_t<std::is_same_v<R, std::invoke_result_t<Fn, Args...>>>>, R, Fn, Args...> : std::true_type {};
template <class R, class Fn, class... Args> struct is_invocable_r_exact : is_invocable_r_exact_impl<void, R, Fn, Args...> {};
template <class R, class Fn, class... Args> inline constexpr bool is_invocable_r_exact_v = is_invocable_r_exact<R, Fn, Args...>::value;

template <class T, template <class...> class X> struct is_template_instantiation_of : std::false_type {};
template <template <class...> class X, class... Args> struct is_template_instantiation_of<X<Args...>, X> : std::true_type {};
template <class T, template <class...> class X> inline constexpr bool is_template_instantiation_of_v = is_template_instantiation_of<T, X>::value;

template <class T, template <auto...> class X> struct is_template_non_type_instantiation_of : std::false_type {};
template <template <auto...> class X, auto... Args> struct is_template_non_type_instantiation_of<X<Args...>, X> : std::true_type {};
template <class T, template <auto...> class X> inline constexpr bool is_template_non_type_instantiation_of_v = is_template_non_type_instantiation_of<T, X>::value;

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

template <class C, class... A> using container_emplace_after_t = decltype(std::declval<C&>().emplace_after(std::declval<typename C::const_iterator>(), std::declval<A>()...));
template <class C, class... A> using container_emplace_back_t = decltype(std::declval<C&>().emplace_back(std::declval<A>()...));
template <class C, class... A> using container_emplace_t = decltype(std::declval<C&>().emplace(std::declval<A>()...));
template <class C> using container_reserve_t = decltype(std::declval<C&>().reserve(std::declval<std::size_t>()));

template <class V, class C, class... A> struct container_has_emplace_after_impl : std::false_type {};
template <class C, class... A> struct container_has_emplace_after_impl<std::void_t<container_emplace_after_t<C, A...>>,  C, A...> : std::true_type {};
template <class C, class... A> inline constexpr bool container_has_emplace_after_v = container_has_emplace_after_impl<void, C, A...>::value;

template <class V, class C, class... A> struct container_has_emplace_back_impl : std::false_type {};
template <class C, class... A> struct container_has_emplace_back_impl<std::void_t<container_emplace_back_t<C, A...>>,  C, A...> : std::true_type {};
template <class C, class... A> inline constexpr bool container_has_emplace_back_v = container_has_emplace_back_impl<void, C, A...>::value;

template <class V, class C, class... A> struct container_has_emplace_impl : std::false_type {};
template <class C, class... A> struct container_has_emplace_impl<std::void_t<container_emplace_t<C, A...>>,  C, A...> : std::true_type {};
template <class C, class... A> inline constexpr bool container_has_emplace_v = container_has_emplace_impl<void, C, A...>::value;

template <class C, class = void> struct container_has_reserve_impl : std::false_type {};
template <class C> struct container_has_reserve_impl<C, std::void_t<container_reserve_t<C>>> : std::true_type {};
template <class C> inline constexpr bool container_has_reserve_v = container_has_reserve_impl<C>::value;

struct identity
{
	template <class T>
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr T&& operator()(T&& t) const noexcept
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
public:
	constexpr explicit dynamic_cast_if_base_of(std::remove_reference_t<T>& x) noexcept : value_{x} {}

	template <class U, class = std::enable_if_t<std::is_base_of_v<std::decay_t<T>, std::decay_t<U>>>>
	[[nodiscard]] constexpr operator U&() const noexcept(std::is_same_v<std::decay_t<T>, std::decay_t<U>>) // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	{
#ifndef LUG_NO_RTTI
		if constexpr (std::is_same_v<std::decay_t<T>, std::decay_t<U>>) {
#endif // LUG_NO_RTTI
			return static_cast<std::remove_reference_t<U>&>(value_.get());
#ifndef LUG_NO_RTTI
		} else {
			return dynamic_cast<std::remove_reference_t<U>&>(value_.get());
		}
#endif // LUG_NO_RTTI
	}

private:
	std::reference_wrapper<std::remove_reference_t<T>> value_;
};

template <class Error>
class reentrancy_sentinel
{
public:
	constexpr explicit reentrancy_sentinel(bool& x)
		: value_{x}
	{
		if LUG_UNLIKELY(value_.get()) {
			lug::throw_exception<Error>();
		}
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

private:
	std::reference_wrapper<bool> value_;
};

template <class EF>
class scope_exit
{
	static_assert(std::is_invocable_v<EF>);

public:
	template <class Fn, class = std::enable_if_t<std::is_constructible_v<EF, Fn&&>>>
	constexpr explicit scope_exit(Fn&& fn) noexcept(std::is_nothrow_constructible_v<EF, Fn&&>)
		: destructor_{std::forward<Fn>(fn)}
	{}

	~scope_exit()
	{
		if (!released_) {
			destructor_();
		}
	}

	void release() noexcept
	{
		released_ = true;
	}

	scope_exit(scope_exit const&) = delete;
	scope_exit(scope_exit&&) = delete;
	scope_exit& operator=(scope_exit const&) = delete;
	scope_exit& operator=(scope_exit&&) = delete;

private:
	EF destructor_;
	bool released_{false};
};

template <class Fn, class = std::enable_if_t<std::is_invocable_v<Fn>>>
scope_exit(Fn) -> scope_exit<std::decay_t<Fn>>;

template <class EF>
class scope_fail
{
	static_assert(std::is_invocable_v<EF>);

public:
	template <class Fn, class = std::enable_if_t<std::is_constructible_v<EF, Fn&&>>>
	constexpr explicit scope_fail(Fn&& fn) noexcept(std::is_nothrow_constructible_v<EF, Fn&&>)
		: destructor_{std::forward<Fn>(fn)}
#ifndef LUG_NO_EXCEPTIONS
		, uncaught_on_construction_{std::uncaught_exceptions()}
#else // LUG_NO_EXCEPTIONS
		, uncaught_on_construction_{(std::numeric_limits<int>::max)()}
#endif // LUG_NO_EXCEPTIONS
	{}

	~scope_fail()
	{
#ifndef LUG_NO_EXCEPTIONS
		if (std::uncaught_exceptions() > uncaught_on_construction_) {
			destructor_();
		}
#endif // LUG_NO_EXCEPTIONS
	}

	void release() noexcept
	{
#ifndef LUG_NO_EXCEPTIONS
		uncaught_on_construction_ = (std::numeric_limits<int>::max)();
#endif // LUG_NO_EXCEPTIONS
	}

	scope_fail(scope_fail const&) = delete;
	scope_fail(scope_fail&&) = delete;
	scope_fail& operator=(scope_fail const&) = delete;
	scope_fail& operator=(scope_fail&&) = delete;

private:
	EF destructor_;
	int uncaught_on_construction_;
};

template <class Fn, class = std::enable_if_t<std::is_invocable_v<Fn>>>
scope_fail(Fn) -> scope_fail<std::decay_t<Fn>>;

template <class Error, class T, class U, class V, class = std::enable_if_t<std::is_integral_v<T> && std::is_integral_v<U> && std::is_integral_v<V>>>
constexpr void assure_in_range(T x, U minval, V maxval)
{
	if LUG_UNLIKELY(!((minval <= x) && (x <= maxval))) {
		lug::throw_exception<Error>();
	}
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
	if LUG_UNLIKELY(((std::numeric_limits<decltype(x + y)>::max)() - x) < y) {
		lug::throw_exception<Error>();
	}
	return x + y;
}

template <class InputIt, class UnaryPredicate>
[[nodiscard]] constexpr InputIt escaping_find_if(InputIt first, InputIt last, UnaryPredicate pred)
{
	for ( ; first != last; ++first) {
		int const status = pred(*first);
		if (status > 0) {
			return first;
		}
		if (status < 0) {
			return last;
		}
	}
	return last;
}

template <class Sequence, class T, class Predicate = std::equal_to<typename Sequence::value_type>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr std::size_t push_back_unique(Sequence& s, T&& value, Predicate pred = Predicate{})
{
	if (auto const it = std::find_if(s.begin(), s.end(), [&pred, &value](const auto& elem) { return pred(elem, value); }); it != s.end()) {
		return static_cast<std::size_t>(std::distance(s.begin(), it));
	}
	s.push_back(std::forward<T>(value));
	return s.size() - 1;
}

template <class Sequence>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr auto pop_back(Sequence& s) -> typename Sequence::value_type
{
	typename Sequence::value_type result{std::move(s.back())}; // NOLINT(misc-const-correctness)
	s.pop_back();
	return result;
}

template <class Error, class Sequence>
[[nodiscard]] constexpr auto guarded_pop_back(Sequence& s) -> typename Sequence::value_type
{
	if LUG_UNLIKELY(s.empty()) {
		lug::throw_exception<Error>();
	}
	return detail::pop_back(s);
}

template <class T, class = std::enable_if_t<std::is_signed_v<T>>>
[[nodiscard]] LUG_ALWAYS_INLINE constexpr T sar(T x, unsigned int n) noexcept
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

template <class T> struct type_info_tag { constexpr type_info_tag() noexcept = default; };
template <class T> inline constexpr type_info_tag<T> type_info_tag_v{};

constexpr std::size_t move_only_any_buffer_align = alignof(std::max_align_t);
constexpr std::size_t move_only_any_buffer_size = 6 * sizeof(void*);

template <class T>
[[nodiscard]] constexpr bool is_move_only_any_small() noexcept
{
	return (sizeof(T) <= move_only_any_buffer_size) &&
		(alignof(T) <= move_only_any_buffer_align) &&
		std::is_nothrow_move_constructible_v<T>;
}

template <class T>
struct move_only_any_vtable_operations
{
	static constexpr void destroy(void* data) noexcept
	{
		if constexpr (detail::is_move_only_any_small<T>()) {
			std::destroy_at(static_cast<T*>(data));
		} else {
			delete static_cast<T*>(data); // NOLINT(cppcoreguidelines-owning-memory)
		}
	}

	static constexpr void* move(void* to, void* from) noexcept
	{
		if constexpr (detail::is_move_only_any_small<T>()) {
			T* src = static_cast<T*>(from);
			::new(to) T(std::move(*src));
			std::destroy_at(src);
			return to;
		} else {
			return from;
		}
	}
};

template <>
struct move_only_any_vtable_operations<void>
{
	static constexpr void destroy(void* /*data*/) noexcept {}
	static constexpr void* move(void* /*to*/, void* /*from*/) noexcept { return nullptr; }
};

struct move_only_any_vtable
{
	using destroy_fn = void (*)(void*) noexcept;
	using move_fn = void* (*)(void*, void*) noexcept;
	destroy_fn destroy;
	move_fn move;
	void const* type;
	constexpr move_only_any_vtable(destroy_fn d, move_fn m, void const* t) noexcept : destroy{d}, move{m}, type{t} {}
};

class move_only_any
{
	template <class T> friend T* move_only_any_cast(move_only_any* operand) noexcept;
	template <class T> friend T const* move_only_any_cast(move_only_any const* operand) noexcept;

public:
	constexpr move_only_any() noexcept = default;

	move_only_any(move_only_any&& other) noexcept
		: vtable_{std::exchange(other.vtable_, &vtable_instance<void>)}
		, data_{vtable_->move(&buffer_[0], std::exchange(other.data_, nullptr))}
	{}

	move_only_any& operator=(move_only_any&& other) noexcept
	{
		if (this != &other) {
			vtable_->destroy(data_);
			vtable_ = std::exchange(other.vtable_, &vtable_instance<void>);
			data_ = vtable_->move(&buffer_[0], std::exchange(other.data_, nullptr));
		}
		return *this;
	}

	~move_only_any()
	{
		vtable_->destroy(data_);
	}

	move_only_any(move_only_any const&) = delete;
	move_only_any& operator=(move_only_any const&) = delete;

	template <class ValueType, class... Args, class = std::enable_if_t<!std::is_same_v<std::decay_t<ValueType>, move_only_any>>>
	explicit move_only_any(std::in_place_type_t<ValueType>, Args&&... args) // NOLINT(hicpp-named-parameter,readability-named-parameter)
	{
		if constexpr (detail::is_move_only_any_small<std::decay_t<ValueType>>()) {
			data_ = ::new(&buffer_[0]) std::decay_t<ValueType>(std::forward<Args>(args)...); // NOLINT(cppcoreguidelines-owning-memory)
		} else {
			data_ = new std::decay_t<ValueType>(std::forward<Args>(args)...); // NOLINT(cppcoreguidelines-owning-memory)
		}
		vtable_ = &vtable_instance<std::decay_t<ValueType>>;
	}

	template <class ValueType, class = std::enable_if_t<!std::is_same_v<std::decay_t<ValueType>, move_only_any>>>
	move_only_any& operator=(ValueType&& value)
	{
		(void)emplace<ValueType>(std::forward<ValueType>(value));
		return *this;
	}

	template <class ValueType, class... Args>
	std::decay_t<ValueType>& emplace(Args&&... args)
	{
		reset();
		if constexpr (detail::is_move_only_any_small<std::decay_t<ValueType>>()) {
			data_ = ::new(&buffer_[0]) std::decay_t<ValueType>(std::forward<Args>(args)...); // NOLINT(cppcoreguidelines-owning-memory)
		} else {
			data_ = new std::decay_t<ValueType>(std::forward<Args>(args)...); // NOLINT(cppcoreguidelines-owning-memory)
		}
		vtable_ = &vtable_instance<std::decay_t<ValueType>>;
		return *static_cast<std::decay_t<ValueType>*>(data_);
	}

	void reset()
	{
		vtable_->destroy(data_);
		vtable_ = &vtable_instance<void>;
		data_ = nullptr;
	}

	void swap(move_only_any& other) noexcept
	{
		if (this != &other) {
			move_only_any temp{std::move(*this)};
			*this = std::move(other);
			other = std::move(temp);
		}
	}

	[[nodiscard]] bool has_value() const noexcept
	{
		return data_ != nullptr;
	}

	template <class T>
	[[nodiscard]] bool is_type() const noexcept
	{
		return vtable_->type == &type_info_tag_v<T>;
	}

private:
	template <class T>
	static constexpr move_only_any_vtable const vtable_instance
	{
		&move_only_any_vtable_operations<T>::destroy,
		&move_only_any_vtable_operations<T>::move,
		&type_info_tag_v<T>
	};

	// NOLINTNEXTLINE(cppcoreguidelines-avoid-c-arrays,hicpp-avoid-c-arrays,modernize-avoid-c-arrays)
	alignas(move_only_any_buffer_align) char buffer_[move_only_any_buffer_size]{};
	move_only_any_vtable const* vtable_{&vtable_instance<void>};
	void* data_{nullptr};
};

template <class T>
[[nodiscard]] inline T* move_only_any_cast(move_only_any* operand) noexcept
{
	if LUG_LIKELY((operand != nullptr) && operand->is_type<std::decay_t<T>>()) {
		return static_cast<T*>(operand->data_);
	}
	return nullptr;
}

template <class T>
[[nodiscard]] inline T const* move_only_any_cast(move_only_any const* operand) noexcept
{
	if LUG_LIKELY((operand != nullptr) && operand->is_type<std::decay_t<T>>()) {
		return static_cast<T const*>(operand->data_);
	}
	return nullptr;
}

template <class T>
[[nodiscard]] inline T* guarded_move_only_any_cast(move_only_any* operand)
{
	T* const p = detail::move_only_any_cast<T>(operand);
	if LUG_LIKELY(p != nullptr) {
		return p;
	}
	lug::throw_exception<bad_move_only_any_cast>();
}

template <class T>
[[nodiscard]] inline T const* guarded_move_only_any_cast(move_only_any const* operand)
{
	T const* const p = detail::move_only_any_cast<T>(operand);
	if LUG_LIKELY(p != nullptr) {
		return p;
	}
	lug::throw_exception<bad_move_only_any_cast>();
}

template <class T>
[[nodiscard]] inline std::decay_t<T> move_only_any_cast(move_only_any&& operand) // NOLINT(cppcoreguidelines-rvalue-reference-param-not-moved)
{
	std::decay_t<T> result(std::move(*detail::guarded_move_only_any_cast<std::decay_t<T>>(&operand)));
	operand.reset();
	return result;
}

class stack_allocator
{
public:
	static constexpr std::size_t default_page_size{16384};
	static constexpr std::size_t default_page_align{alignof(std::max_align_t)};
	static constexpr std::size_t default_large_object_threshold{4096};

	stack_allocator()
		: stack_allocator(default_page_size, default_page_align, default_large_object_threshold)
	{}

	stack_allocator(std::size_t psize, std::size_t palign, std::size_t lobj_thresh)
		: page_size_{psize}
		, page_align_{palign}
		, large_object_threshold_{lobj_thresh}
	{
		if LUG_UNLIKELY(psize <= sizeof(page)) {
			lug::throw_exception<lug::invalid_argument>("page size is too small");
		} else if LUG_UNLIKELY(palign < alignof(std::max_align_t)) {
			lug::throw_exception<lug::invalid_argument>("page align is too small");
		} else if LUG_UNLIKELY((palign & (palign - 1)) != 0) {
			lug::throw_exception<lug::invalid_argument>("page align is not a power of two");
		} else if LUG_UNLIKELY(lobj_thresh > (psize / 2)) {
			lug::throw_exception<lug::invalid_argument>("large object threshold must be no greater than half the page size");
		}
		auto const page_mem = static_cast<std::byte*>(::operator new[](page_size_, std::align_val_t{page_align_}));
		head_ = ::new(page_mem) page{nullptr, page_mem + sizeof(page), page_mem + sizeof(page), page_mem + page_size_}; // NOLINT(cppcoreguidelines-owning-memory)
	}

	stack_allocator(stack_allocator&& other) noexcept
		: page_size_{other.page_size_}
		, page_align_{other.page_align_}
		, large_object_threshold_{other.large_object_threshold_}
		, head_{std::exchange(other.head_, nullptr)}
	{}

	stack_allocator& operator=(stack_allocator&& other) noexcept
	{
		stack_allocator{std::move(other)}.swap(*this);
		return *this;
	}

	~stack_allocator()
	{
		auto current = std::exchange(head_, nullptr);
		while (current != nullptr) {
			auto const next{current->next};
			std::destroy_at(current);
			::operator delete[](static_cast<void*>(current), std::align_val_t{page_align_});
			current = next;
		}
	}

	stack_allocator(stack_allocator const&) = delete;
	stack_allocator& operator=(stack_allocator const&) = delete;

	void swap(stack_allocator& other) noexcept
	{
		std::swap(page_size_, other.page_size_);
		std::swap(page_align_, other.page_align_);
		std::swap(large_object_threshold_, other.large_object_threshold_);
		std::swap(head_, other.head_);
	}

	[[nodiscard]] LUG_RETURNS_NONNULL LUG_ALLOC LUG_ALLOC_SIZE(2) LUG_ALLOC_ALIGN(3)
	auto allocate(std::size_t size, std::size_t align) -> void*
	{
		if LUG_LIKELY(size < large_object_threshold_) {
			auto const align_mask{static_cast<std::uintptr_t>(align) - 1};
			auto const bptr{head_->current};
			auto const addr{reinterpret_cast<std::uintptr_t>(bptr)}; // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
			auto const aligned_addr{(addr + align_mask) & ~align_mask};
			auto const aligned_bptr{bptr + (aligned_addr - addr)};
			auto const next_bptr{aligned_bptr + size};
			if LUG_LIKELY(next_bptr <= head_->end) {
				head_->current = next_bptr;
				return aligned_bptr;
			}
			auto const new_page_mem = static_cast<std::byte*>(::operator new[](page_size_, std::align_val_t{page_align_}));
			auto const new_page_begin = new_page_mem + sizeof(page);
			auto const new_page_end = new_page_mem + page_size_;
			auto const new_addr{reinterpret_cast<std::uintptr_t>(new_page_begin)}; // NOLINT(cppcoreguidelines-pro-type-reinterpret-cast)
			auto const new_aligned_addr{(new_addr + align_mask) & ~align_mask};
			auto const new_aligned_bptr{new_page_begin + (new_aligned_addr - new_addr)};
			auto const new_page_current{new_aligned_bptr + size};
			head_ = ::new(new_page_mem) page{head_, new_page_current, new_page_begin, new_page_end}; // NOLINT(cppcoreguidelines-owning-memory)
			return new_aligned_bptr;
		}
		return ::operator new[](size, std::align_val_t{align});
	}

	LUG_NONNULL(2) void rewind(void* ptr, std::size_t size, std::size_t align) noexcept
	{
		if LUG_LIKELY(size < large_object_threshold_) {
			auto const bptr{static_cast<std::byte*>(ptr)};
			if LUG_LIKELY((head_->begin <= bptr) && (bptr <= head_->end)) {
				head_->current = bptr;
				return;
			}
			auto current_page{head_};
			while (current_page->next != nullptr) {
				auto const next_page{current_page->next};
				if ((next_page->begin <= bptr) && (bptr <= next_page->end)) {
					next_page->current = bptr;
					while (head_ != next_page) {
						auto const prev{head_};
						head_ = head_->next;
						std::destroy_at(prev);
						::operator delete[](static_cast<void*>(prev), std::align_val_t{page_align_});
					}
					break;
				}
				current_page = next_page;
			}
			return;
		}
		::operator delete[](ptr, std::align_val_t{align});
	}

	[[nodiscard]] auto page_size() const noexcept -> std::size_t { return page_size_; }
	[[nodiscard]] auto page_align() const noexcept -> std::size_t { return page_align_; }
	[[nodiscard]] auto large_object_threshold() const noexcept -> std::size_t { return large_object_threshold_; }

private:
	struct page
	{
		page* next;
		std::byte* current;
		std::byte* begin;
		std::byte* end;

		constexpr page(page* n, std::byte* c, std::byte* b, std::byte* e) noexcept
			: next{n}, current{c}, begin{b}, end{e}
		{}
	};

	std::size_t page_size_{0};
	std::size_t page_align_{0};
	std::size_t large_object_threshold_{0};
	page* head_{nullptr};
};

} // namespace detail

} // namespace lug

#endif

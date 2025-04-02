// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_CONFIG_HPP
#define LUG_INCLUDE_LUG_CONFIG_HPP

// NOLINTBEGIN(cppcoreguidelines-macro-to-enum,cppcoreguidelines-macro-usage,modernize-macro-to-enum)

#define LUG_VERSION 0x000600
#define LUG_VERSION_MAJOR 0
#define LUG_VERSION_MINOR 6
#define LUG_VERSION_PATCH 0
#define LUG_VERSION_STRING "0.6.0"

#ifndef LUG_NO_EXCEPTIONS
#if defined __GNUC__
#ifndef __EXCEPTIONS
#define LUG_NO_EXCEPTIONS
#endif
#elif defined _MSC_VER
#ifndef _CPPUNWIND
#define LUG_NO_EXCEPTIONS
#endif
#endif
#endif // LUG_NO_EXCEPTIONS

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

#ifndef LUG_ALWAYS_INLINE
#if !defined LUG_DEBUG && !defined _DEBUG
#if defined __GNUC__
#define LUG_ALWAYS_INLINE [[gnu::always_inline]]
#elif defined _MSC_VER
#define LUG_ALWAYS_INLINE __forceinline
#else
#define LUG_ALWAYS_INLINE
#endif
#else
#define LUG_ALWAYS_INLINE
#endif
#endif // LUG_ALWAYS_INLINE

#ifndef LUG_LIKELY
#if (__cplusplus >= 202002L) && __has_cpp_attribute(likely)
#define LUG_LIKELY(...) (__VA_ARGS__) [[likely]]
#else
#ifdef __GNUC__
#define LUG_LIKELY(...) (__builtin_expect((__VA_ARGS__), 1))
#else
#define LUG_LIKELY(...) (__VA_ARGS__)
#endif
#endif
#endif // LUG_LIKELY

#ifndef LUG_UNLIKELY
#if (__cplusplus >= 202002L) && __has_cpp_attribute(unlikely)
#define LUG_UNLIKELY(...) (__VA_ARGS__) [[unlikely]]
#else
#ifdef __GNUC__
#define LUG_UNLIKELY(...) (__builtin_expect((__VA_ARGS__), 0))
#else
#define LUG_UNLIKELY(...) (__VA_ARGS__)
#endif
#endif
#endif // LUG_UNLIKELY

#ifndef LUG_DIAGNOSTIC_PUSH_AND_IGNORE
#ifdef __GNUC__
#define LUG_DIAGNOSTIC_PUSH_AND_IGNORE \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wparentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wlogical-not-parentheses\"") \
_Pragma("GCC diagnostic ignored \"-Wuninitialized\"") \
_Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
_Pragma("GCC diagnostic ignored \"-Wunused-but-set-variable\"")
#else
#define LUG_DIAGNOSTIC_POP
#endif
#endif // LUG_DIAGNOSTIC_PUSH_AND_IGNORE

#ifndef LUG_DIAGNOSTIC_POP
#ifdef __GNUC__
#define LUG_DIAGNOSTIC_POP _Pragma("GCC diagnostic pop")
#else
#define LUG_DIAGNOSTIC_POP
#endif
#endif // LUG_DIAGNOSTIC_POP

// NOLINTEND(cppcoreguidelines-macro-to-enum,cppcoreguidelines-macro-usage,modernize-macro-to-enum)

#endif // LUG_INCLUDE_LUG_CONFIG_HPP

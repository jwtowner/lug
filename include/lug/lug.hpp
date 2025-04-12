// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_LUG_HPP
#define LUG_INCLUDE_LUG_LUG_HPP

#include <lug/ascii.hpp>
#include <lug/utf8.hpp>
#include <lug/rune.hpp>

#include <memory>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace lug {

class attribute_collection;
class encoder;
class environment;
class error_context;
class grammar;
class multi_input_source;
class parser_base;
class rule;
class string_input_source;
class string_view_input_source;
class syntax;
struct program;
struct syntax_position;
struct syntax_range;
template <class> class basic_parser;
template <class> class failure;
template <class> class recover_with;
template <class> class recursive_wrapper;
[[nodiscard]] grammar start(rule const& start_rule);
[[nodiscard]] grammar start(rule const& start_rule, rule const& skip_rule);

enum class error_response : std::uint_least8_t { halt, resume, accept, backtrack, rethrow };

struct registers
{
	static constexpr unsigned int flags_count = 2U;
	static constexpr unsigned int inhibited_shift = static_cast<unsigned int>(std::numeric_limits<std::size_t>::digits - 1);
	static constexpr unsigned int ignore_errors_shift = static_cast<unsigned int>(std::numeric_limits<std::size_t>::digits - 2);
	static constexpr std::size_t inhibited_flag = std::size_t{1} << inhibited_shift;
	static constexpr std::size_t ignore_errors_flag = std::size_t{1} << ignore_errors_shift;
	static constexpr std::size_t count_mask = ~(inhibited_flag | ignore_errors_flag);
	std::size_t sr{0}; // subject register
	std::size_t mr{0}; // match register
	std::size_t rc{0}; // response counter
	std::size_t cd{0}; // call depth counter
	std::size_t ci{0}; // accept/cut inhibited register
	std::size_t ri{0}; // raise inhibited register
	std::ptrdiff_t pc{0}; // program counter
	std::ptrdiff_t eh{-1}; // error handler register
	std::ptrdiff_t rh{-1}; // recovery handler register
	error_response rr{error_response::resume}; // recovery response latch register
};

enum class opcode : std::uint_least8_t
{
	jump,           choice,         commit,         commit_back,    commit_partial,
	rewind,         accept,         call,           ret,            fail,
	recover_push,   recover_pop,    recover_resp,   report_push,    report_pop,
	predicate,      action,         capture_start,  capture_end,    capture_full,
	attribute_push, attribute_pop,  condition_pop,  symbol_end,     symbol_pop,
	match_any,      match_blank,    match_space,    match_eol,      match_eoi,
	match_unit,     match_set,      match_all_of,   match_any_of,   match_none_of,
	repeat_any,     repeat_blank,   repeat_space,   skip_blank,     skip_space,
	repeat_unit,    repeat_set,     repeat_all_of,  repeat_any_of,  repeat_none_of,
	test_any,       test_blank,     test_space,     test_eol,       test_eoi,
	test_unit,      test_set,       test_all_of,    test_any_of,    test_none_of,
	match,          match_cf,       condition_test, condition_push,
	symbol_exists,  symbol_all,     symbol_all_cf,  symbol_any,
	symbol_any_cf,  symbol_head,    symbol_head_cf, symbol_tail,
	symbol_tail_cf, symbol_start,   symbol_push,    raise
};

struct alignas(std::uint_least64_t) instruction
{
	opcode op;
	std::uint_least8_t immediate8;
	std::uint_least16_t immediate16;
	std::int_least32_t offset32;

	static constexpr std::uint_least32_t limit_mask = 0x0000ffffU;
	static constexpr std::size_t max_limit_bias = 1;
	static constexpr unsigned int max_limit_shift = 16;

	[[nodiscard]] LUG_ALWAYS_INLINE static constexpr std::int_least32_t pack_min_max(std::size_t nmin, std::size_t nmax)
	{
		auto const nmin16 = static_cast<std::uint_least32_t>(nmin) & limit_mask;
		auto const nmax16 = static_cast<std::uint_least32_t>(nmax + max_limit_bias) & limit_mask;
		return static_cast<std::int_least32_t>(nmin16 | (nmax16 << max_limit_shift));
	}

	[[nodiscard]] LUG_ALWAYS_INLINE constexpr std::size_t unpack_min() const noexcept
	{
		return static_cast<std::size_t>(static_cast<std::uint_least32_t>(offset32) & limit_mask);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE constexpr std::size_t unpack_max() const noexcept
	{
		return static_cast<std::size_t>((static_cast<std::uint_least32_t>(offset32) >> max_limit_shift) & limit_mask) - max_limit_bias;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE friend constexpr bool operator==(instruction const& lhs, instruction const& rhs) noexcept
	{
		return (lhs.op == rhs.op) && (lhs.immediate8 == rhs.immediate8) && (lhs.immediate16 == rhs.immediate16) && (lhs.offset32 == rhs.offset32);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE friend constexpr bool operator!=(instruction const& lhs, instruction const& rhs) noexcept
	{
		return !(lhs == rhs);
	}
};

static_assert(sizeof(instruction) == sizeof(std::uint_least64_t), "expected instruction size to be same size as std::uint_least64_t");
static_assert(alignof(instruction) == alignof(std::uint_least64_t), "expected instruction alignment to be same size as std::uint_least64_t");

struct encoder_expression_trait_tag {};
template <class E, class = void> struct is_encoder_expression : std::false_type {};
template <class E> struct is_encoder_expression<E, std::enable_if_t<std::is_same_v<encoder_expression_trait_tag, typename std::decay_t<E>::expression_trait>>> : std::true_type {};
template <class E> inline constexpr bool is_encoder_expression_v = is_encoder_expression<E>::value;
template <class E> inline constexpr bool is_encoder_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> inline constexpr bool is_primitive_expression_v = std::is_same_v<std::decay_t<E>, char> || std::is_same_v<std::decay_t<E>, char32_t> || std::is_same_v<std::decay_t<E>, rune_set> || std::is_convertible_v<std::decay_t<E>, std::string_view>;
template <class E> inline constexpr bool is_recovery_expression_v = is_encoder_expression_v<E> || std::is_same_v<rule, std::decay_t<E>>;
template <class H> inline constexpr bool is_error_handler_v = std::is_invocable_v<std::decay_t<H>, error_context&>;
template <class P> inline constexpr bool is_predicate_v = std::is_invocable_r_v<bool, std::decay_t<P>, environment&>;
template <class A> inline constexpr bool is_capture_action_v = std::is_invocable_v<std::decay_t<A>, detail::dynamic_cast_if_base_of<environment&>, syntax const&> || std::is_invocable_v<std::decay_t<A>, syntax const&>;
template <class T> inline constexpr bool is_capture_target_v = std::is_same_v<std::decay_t<T>, syntax> || std::is_assignable_v<std::decay_t<T>, syntax const&>;
template <class E> inline constexpr bool is_expression_v = is_encoder_expression_v<E> || is_encoder_callable_v<E> || is_primitive_expression_v<E> || is_predicate_v<E>;

enum class directives : std::uint_least8_t { none = 0U, caseless = 1U, eps = 2U, lexeme = 4U, noskip = 8U, preskip = 16U, postskip = 32U };
template <> inline constexpr bool is_flag_enum_v<directives> = true;
enum class effect_traits : std::uint_least8_t { none = 0U, captures = 1U, cuts = 2U, raises = 4U, all = 7U };
template <> inline constexpr bool is_flag_enum_v<effect_traits> = true;
enum class match_traits : std::uint_least8_t { none = 0U, nullable = 1U, nofail = 2U, all = 3U };
template <> inline constexpr bool is_flag_enum_v<match_traits> = true;

using program_callees = std::vector<std::tuple<lug::rule const*, lug::program const*, std::ptrdiff_t, directives>>;
using error_handler = std::function<error_response(error_context&)>;
using semantic_action = std::function<void(environment&)>;
using semantic_capture_action = std::function<void(environment&, syntax const&)>;
using syntactic_predicate = std::function<bool(environment&)>;

template <class T>
inline constexpr bool is_attribute_frame_persistable_v =
	!std::is_const_v<T> &&
	std::is_object_v<T> &&
	std::is_copy_constructible_v<T> &&
	std::is_copy_assignable_v<T> &&
	std::is_nothrow_destructible_v<T>;

class attribute_frame_handle;

class attribute_frame_info : public std::enable_shared_from_this<attribute_frame_info>
{
	friend class attribute_frame_handle;

	struct descriptor;

	struct sentinel_operations
	{
		static void forward(std::byte* /*buffer*/, descriptor const* /*desc*/, descriptor const* /*last*/) {}
		static void reverse(std::byte* /*buffer*/, descriptor const* /*desc*/) {}
	};

	struct descriptor
	{
		using forward_operation_fn = void (*)(std::byte*, descriptor const*, descriptor const*);
		using reverse_operation_fn = void (*)(std::byte*, descriptor const*);
		std::size_t offset{0};
		void* target{nullptr};
		void const* type{&detail::type_info_tag_v<void>};
		forward_operation_fn persist{&sentinel_operations::forward};
		reverse_operation_fn restore{&sentinel_operations::reverse};
		reverse_operation_fn destroy{&sentinel_operations::reverse};
		descriptor() noexcept = default;
		descriptor(std::size_t off, void* tar, void const* typ, forward_operation_fn pfn, reverse_operation_fn rfn, reverse_operation_fn dfn) noexcept
			: offset{off}, target{tar}, type{typ}, persist{pfn}, restore{rfn}, destroy{dfn} {}
	};

	template <class T>
	struct operations
	{
		static void persist(std::byte* buffer, descriptor const* desc, descriptor const* last)
		{
			if constexpr (std::is_nothrow_copy_constructible_v<T>) {
				::new(buffer + desc->offset) T{*static_cast<T const*>(desc->target)};
			} else {
				detail::scope_exit guard{[buffer, desc]() noexcept { (*((desc - 1)->destroy))(buffer, desc - 1); }};
				::new(buffer + desc->offset) T{*static_cast<T const*>(desc->target)};
				guard.release();
			}
			if (desc == last)
				return;
			descriptor const* const next = desc + 1;
			LUG_MUSTTAIL return (*next->persist)(buffer, next, last); // NOLINT(readability-avoid-return-with-void-value)
		}

		static void restore(std::byte* buffer, descriptor const* desc)
		{
			auto* const from = static_cast<T*>(static_cast<void*>(buffer + desc->offset)); // NOLINT(bugprone-casting-through-void)
			if constexpr (std::is_nothrow_move_assignable_v<T>) {
				*static_cast<T*>(desc->target) = static_cast<T&&>(*from);
			} else if constexpr (std::is_nothrow_copy_assignable_v<T>) {
				*static_cast<T*>(desc->target) = static_cast<T const&>(*from);
			} else {
				detail::scope_exit guard{[buffer, desc]() noexcept { (*desc->destroy)(buffer, desc); }};
				*static_cast<T*>(desc->target) = static_cast<T const&>(*from);
				guard.release();
			}
			std::destroy_at(from);
			descriptor const* const prev = desc - 1;
			LUG_MUSTTAIL return (*prev->restore)(buffer, prev); // NOLINT(readability-avoid-return-with-void-value)
		}

		static void destroy(std::byte* buffer, descriptor const* desc)
		{
			std::destroy_at(static_cast<T*>(static_cast<void*>(buffer + desc->offset))); // NOLINT(bugprone-casting-through-void)
			descriptor const* const prev = desc - 1;
			LUG_MUSTTAIL return (*prev->destroy)(buffer, prev); // NOLINT(readability-avoid-return-with-void-value)
		}
	};

	std::vector<descriptor> descriptors_{2U, descriptor{}};
	std::size_t align_bytes_{1};
	std::size_t size_bytes_{0};

	[[nodiscard]] bool is_target_unique(void* target, void const* type) const
	{
		return std::all_of(descriptors_.begin(), descriptors_.end(), [target, type](descriptor const& desc) {
			if (desc.target == target) {
				if LUG_UNLIKELY(desc.type != type)
					lug::throw_exception<attribute_stack_error>();
				return false;
			}
			return true;
		});
	}

public:
	attribute_frame_info() = default;
	~attribute_frame_info() = default;
	attribute_frame_info(attribute_frame_info const&) = delete;
	attribute_frame_info(attribute_frame_info&&) = delete;
	attribute_frame_info& operator=(attribute_frame_info const&) = delete;
	attribute_frame_info& operator=(attribute_frame_info&&) = delete;
	[[nodiscard]] bool empty() const noexcept { return descriptors_.size() <= 2; }
	[[nodiscard]] std::size_t alignment() const noexcept { return align_bytes_; }
	[[nodiscard]] std::size_t size_bytes() const noexcept { return size_bytes_; }
	[[nodiscard]] attribute_frame_handle handle() const;

	template <class T, class = std::enable_if_t<is_attribute_frame_persistable_v<T>>>
	LUG_NONNULL(2) void add(T* target)
	{
		using U = std::remove_cv_t<T>;
		if (is_target_unique(target, &detail::type_info_tag_v<U>)) {
			std::size_t const offset{(size_bytes_ + (alignof(U) - 1)) & ~(alignof(U) - 1)};
			descriptors_.emplace(descriptors_.end() - 1, offset, target, &detail::type_info_tag_v<U>, &operations<U>::persist, &operations<U>::restore, &operations<U>::destroy);
			align_bytes_ = (std::max)(align_bytes_, alignof(U));
			size_bytes_ = offset + sizeof(U);
		}
	}
};

class attribute_frame_handle
{
	friend class attribute_frame_info;
	std::shared_ptr<attribute_frame_info const> info_;
	std::size_t head_{0};
	std::size_t tail_{0};
	attribute_frame_handle(std::shared_ptr<attribute_frame_info const> info, std::size_t first, std::size_t last) noexcept : info_{std::move(info)}, head_{first}, tail_{last} {}
	[[nodiscard]] LUG_ALWAYS_INLINE auto head() const noexcept { return info_->descriptors_.data() + head_; }
	[[nodiscard]] LUG_ALWAYS_INLINE auto tail() const noexcept { return info_->descriptors_.data() + tail_; }
public:
	constexpr attribute_frame_handle() noexcept = default;
	attribute_frame_handle(attribute_frame_handle const&) noexcept = default;
	attribute_frame_handle(attribute_frame_handle&&) noexcept = default;
	attribute_frame_handle& operator=(attribute_frame_handle const&) noexcept = default;
	attribute_frame_handle& operator=(attribute_frame_handle&&) noexcept = default;
	~attribute_frame_handle() noexcept = default;
	[[nodiscard]] LUG_ALWAYS_INLINE bool empty() const noexcept { return info_->empty(); }
	[[nodiscard]] LUG_ALWAYS_INLINE std::size_t alignment() const noexcept { return info_->alignment(); }
	[[nodiscard]] LUG_ALWAYS_INLINE std::size_t size_bytes() const noexcept { return info_->size_bytes(); }
	LUG_NONNULL(2) LUG_ALWAYS_INLINE void persist(std::byte* buffer) const { auto const h = head(); (*h->persist)(buffer, h, tail()); }
	LUG_NONNULL(2) LUG_ALWAYS_INLINE void restore(std::byte* buffer) const { auto const t = tail(); (*t->restore)(buffer, t); }
	LUG_NONNULL(2) LUG_ALWAYS_INLINE void destroy(std::byte* buffer) const noexcept { auto const t = tail(); (*t->destroy)(buffer, t); }
	[[nodiscard]] LUG_ALWAYS_INLINE bool operator==(attribute_frame_handle const& other) const noexcept { return (info_ == other.info_) && (head_ == other.head_) && (tail_ == other.tail_); }
	[[nodiscard]] LUG_ALWAYS_INLINE bool operator!=(attribute_frame_handle const& other) const noexcept { return !(*this == other); }
};

[[nodiscard]] inline attribute_frame_handle attribute_frame_info::handle() const
{
	return attribute_frame_handle{shared_from_this(), 1U, descriptors_.size() - 2U};
}

struct program
{
	std::vector<instruction> instructions;
	std::vector<char> data;
	std::vector<std::uint_least64_t> uniforms;
	std::vector<rune_set> runesets;
	std::vector<error_handler> handlers;
	std::vector<syntactic_predicate> predicates;
	std::vector<semantic_action> actions;
	std::vector<semantic_capture_action> captures;
	std::vector<attribute_frame_handle> frames;
	directives entry_mode{directives::eps};
	effect_traits first_etraits{effect_traits::none};
	effect_traits follow_etraits{effect_traits::none};
	match_traits first_mtraits{match_traits::nullable | match_traits::nofail};
	match_traits follow_mtraits{match_traits::nullable | match_traits::nofail};

	void concatenate(program const& src)
	{
		bool const was_empty = instructions.empty();
		std::size_t const data_offset = data.size();
		std::size_t const handlers_offset = handlers.size();
		std::size_t const predicates_offset = predicates.size();
		std::size_t const actions_offset = actions.size();
		std::size_t const captures_offset = captures.size();
		std::size_t const attribute_frames_offset = frames.size();
		instructions.reserve(detail::checked_add<program_limit_error>(instructions.size(), src.instructions.size()));
		for (auto const& instr : src.instructions) {
			instruction new_instr{instr};
			if (new_instr.op < opcode::match) {
				std::optional<std::size_t> object;
				switch (new_instr.op) {
					case opcode::match_any_of: case opcode::match_all_of: case opcode::match_none_of:
					case opcode::test_any_of: case opcode::test_all_of: case opcode::test_none_of:
					case opcode::repeat_any_of: case opcode::repeat_all_of: case opcode::repeat_none_of:
						object = detail::push_back_unique(uniforms, src.uniforms[instr.immediate16]);
						break;
					case opcode::match_set: case opcode::test_set: case opcode::repeat_set:
						object = detail::push_back_unique(runesets, src.runesets[instr.immediate16]);
						break;
					case opcode::report_push: object = instr.immediate16 + handlers_offset; break;
					case opcode::predicate: object = instr.immediate16 + predicates_offset; break;
					case opcode::action: object = instr.immediate16 + actions_offset; break;
					case opcode::capture_end: case opcode::capture_full: object = instr.immediate16 + captures_offset; break;
					case opcode::attribute_push: case opcode::attribute_pop: object = instr.immediate16 + attribute_frames_offset; break;
					default: break;
				}
				if (object.has_value())
					new_instr.immediate16 = detail::checked_cast<std::uint_least16_t, resource_limit_error>(*object, 0U, (std::numeric_limits<std::uint_least16_t>::max)());
			} else {
				std::size_t const offset32 = static_cast<std::uint_least32_t>(new_instr.offset32) + data_offset;
				new_instr.offset32 = detail::checked_cast<std::int_least32_t, resource_limit_error>(offset32, 0U, static_cast<std::size_t>((std::numeric_limits<std::int_least32_t>::max)()));
			}
			instructions.push_back(new_instr);
		}
		data.insert(data.end(), src.data.begin(), src.data.end());
		handlers.insert(handlers.end(), src.handlers.begin(), src.handlers.end());
		predicates.insert(predicates.end(), src.predicates.begin(), src.predicates.end());
		actions.insert(actions.end(), src.actions.begin(), src.actions.end());
		captures.insert(captures.end(), src.captures.begin(), src.captures.end());
		frames.insert(frames.end(), src.frames.begin(), src.frames.end());
		entry_mode = (entry_mode & ~directives::eps) | (entry_mode & src.entry_mode & directives::eps);
		if (was_empty) {
			first_etraits = src.first_etraits;
			follow_etraits = src.follow_etraits;
			first_mtraits = src.first_mtraits;
			follow_mtraits = src.follow_mtraits;
		} else {
			follow_etraits |= src.first_etraits | src.follow_etraits;
			follow_mtraits &= src.first_mtraits & src.follow_mtraits;
		}
	}

	void swap(program& p) noexcept
	{
		instructions.swap(p.instructions);
		data.swap(p.data);
		uniforms.swap(p.uniforms);
		runesets.swap(p.runesets);
		handlers.swap(p.handlers);
		predicates.swap(p.predicates);
		actions.swap(p.actions);
		captures.swap(p.captures);
		std::swap(entry_mode, p.entry_mode);
		std::swap(first_etraits, p.first_etraits);
		std::swap(follow_etraits, p.follow_etraits);
		std::swap(first_mtraits, p.first_mtraits);
		std::swap(follow_mtraits, p.follow_mtraits);
	}

	[[nodiscard]] effect_traits first_effects() const noexcept { return first_etraits; }
	[[nodiscard]] effect_traits follow_effects() const noexcept { return follow_etraits; }
	[[nodiscard]] match_traits first_matches() const noexcept { return first_mtraits; }
	[[nodiscard]] match_traits follow_matches() const noexcept { return follow_mtraits; }
};

class rule
{
	friend class encoder;
	friend grammar start(rule const& start_rule, rule const& skip_rule);
	program program_;
	program_callees callees_;
	effect_traits first_etraits_{effect_traits::captures | effect_traits::cuts};
	effect_traits follow_etraits_{effect_traits::captures | effect_traits::cuts};
	match_traits first_mtraits_{match_traits::none};
	match_traits follow_mtraits_{match_traits::none};
	bool currently_encoding_{false};
public:
	rule() noexcept = default;
	template <class E, class = std::enable_if_t<is_expression_v<E> && !std::is_same_v<E, rule>>> rule(E const& e); // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	rule(rule const& r);
	rule(rule&& r) noexcept = default;
	rule& operator=(rule const& r) { rule{r}.swap(*this); return *this; }
	rule& operator=(rule&& r) noexcept = default;
	~rule() = default;
	void swap(rule& r) noexcept { program_.swap(r.program_); callees_.swap(r.callees_); }
	[[nodiscard]] auto operator[](std::uint_least16_t prec) const noexcept;
	template <class Recovery> [[nodiscard]] auto operator[](failure<Recovery> const& reason) const;
	template <class Recovery> [[nodiscard]] auto operator[](recover_with<Recovery> const& rec) const;
	template <class Handler, class = std::enable_if_t<is_error_handler_v<Handler>>> [[nodiscard]] auto operator^=(Handler&& handler) const;
	[[nodiscard]] effect_traits first_effects() const noexcept { return first_etraits_; }
	[[nodiscard]] effect_traits follow_effects() const noexcept { return follow_etraits_; }
	[[nodiscard]] match_traits first_matches() const noexcept { return first_mtraits_; }
	[[nodiscard]] match_traits follow_matches() const noexcept { return follow_mtraits_; }
};

class grammar
{
	friend grammar start(rule const& start_rule, rule const& skip_rule);
	lug::program program_;
	explicit grammar(lug::program&& p) noexcept : program_{std::move(p)} {}
public:
	grammar() noexcept = default;
	void swap(grammar& g) noexcept { program_.swap(g.program_); }
	[[nodiscard]] lug::program const& program() const noexcept { return program_; }
	[[nodiscard]] effect_traits first_effects() const noexcept { return program_.first_effects(); }
	[[nodiscard]] effect_traits follow_effects() const noexcept { return program_.follow_effects(); }
	[[nodiscard]] match_traits first_matches() const noexcept { return program_.first_matches(); }
	[[nodiscard]] match_traits follow_matches() const noexcept { return program_.follow_matches(); }
};

struct syntax_position
{
	std::size_t line{0};
	std::size_t column{0};
	[[nodiscard]] constexpr bool operator==(syntax_position const& other) const noexcept { return line == other.line && column == other.column; }
	[[nodiscard]] constexpr bool operator!=(syntax_position const& other) const noexcept { return !(*this == other); }
	[[nodiscard]] constexpr bool operator<(syntax_position const& other) const noexcept { return line < other.line || (line == other.line && column < other.column); }
	[[nodiscard]] constexpr bool operator<=(syntax_position const& other) const noexcept { return !(other < *this); }
	[[nodiscard]] constexpr bool operator>(syntax_position const& other) const noexcept { return other < *this; }
	[[nodiscard]] constexpr bool operator>=(syntax_position const& other) const noexcept { return !(*this < other); }
};

struct syntax_range
{
	std::size_t index{0};
	std::size_t size{0};
	[[nodiscard]] constexpr bool operator==(syntax_range const& other) const noexcept { return index == other.index && size == other.size; }
	[[nodiscard]] constexpr bool operator!=(syntax_range const& other) const noexcept { return !(*this == other); }
	[[nodiscard]] constexpr bool operator<(syntax_range const& other) const noexcept { return index < other.index || (index == other.index && size < other.size); }
	[[nodiscard]] constexpr bool operator<=(syntax_range const& other) const noexcept { return !(other < *this); }
	[[nodiscard]] constexpr bool operator>(syntax_range const& other) const noexcept { return other < *this; }
	[[nodiscard]] constexpr bool operator>=(syntax_range const& other) const noexcept { return !(*this < other); }
};

class syntax
{
	std::string_view str_;
	std::size_t index_{0};
public:
	constexpr syntax() noexcept = default;
	constexpr syntax(std::string_view c, std::size_t i) noexcept : str_{c}, index_{i} {}
	[[nodiscard]] constexpr std::string_view str() const noexcept { return str_; }
	[[nodiscard]] constexpr std::size_t index() const noexcept { return index_; }
	[[nodiscard]] constexpr syntax_range range() const noexcept { return syntax_range{index_, str_.size()}; }
	[[nodiscard]] operator std::string() const { return std::string{str_}; } // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	[[nodiscard]] constexpr operator std::string_view() const noexcept { return str_; } // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	[[nodiscard]] constexpr operator syntax_range() const noexcept { return range(); } // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	[[nodiscard]] constexpr bool empty() const noexcept { return str_.empty(); }
	[[nodiscard]] constexpr std::size_t size() const noexcept { return str_.size(); }
	[[nodiscard]] constexpr bool operator==(syntax const& other) const noexcept { return str_ == other.str_ && index_ == other.index_; }
	[[nodiscard]] constexpr bool operator!=(syntax const& other) const noexcept { return str_ != other.str_ || index_ != other.index_; }
};

class environment
{
	friend class attribute_collection;
	friend class parser_base;
	template <class> friend class basic_parser;

	struct attribute_frame_instance
	{
		attribute_frame_instance* next;
		std::byte* buffer;
		attribute_frame_handle frame;
		attribute_frame_instance(attribute_frame_instance* np, std::byte* bp, attribute_frame_handle fh) noexcept : next{np}, buffer{bp}, frame{std::move(fh)} {}
	};

	static inline std::vector<std::string> const empty_symbols_{};
	detail::stack_allocator attribute_frame_allocator_;
	attribute_frame_instance* attribute_frame_stack_{nullptr};
	std::vector<detail::move_only_any> attribute_result_stack_;
	std::vector<std::size_t> attribute_collection_stack_;
	std::unordered_set<std::string_view> conditions_;
	std::unordered_map<std::string_view, std::vector<std::string>> symbols_;
	std::vector<std::pair<std::size_t, syntax_position>> positions_;
	std::string_view match_;
	std::string_view subject_;
	std::size_t call_depth_{0};
	std::size_t prune_depth_{(std::numeric_limits<std::size_t>::max)()};
	syntax_position origin_{1, 1};
	std::uint_least32_t tab_width_{default_tab_width};
	std::uint_least32_t tab_alignment_{default_tab_alignment};
	bool should_reset_on_parse_{true};
	bool needs_reset_{false};

	virtual void on_reset() {}
	virtual void on_drain() {}
	virtual void on_accept_started() {}
	virtual void on_accept_ended() {}

	void reset(std::string_view sub)
	{
		if (should_reset_on_parse_) {
			if (needs_reset_) {
				call_depth_ = 0;
				prune_depth_ = (std::numeric_limits<std::size_t>::max)();
				origin_ = position_at(match_.size());
				set_match_and_subject(sub.substr(0, 0), sub);
				clear_attribute_frame_stack();
				attribute_result_stack_.clear();
				attribute_collection_stack_.clear();
			}
			needs_reset_ = true;
			on_reset();
		}
	}

	void drain(std::string_view sub)
	{
		origin_ = position_at(match_.size());
		set_match_and_subject(sub.substr(0, 0), sub);
		on_drain();
	}

	[[nodiscard]] std::size_t start_accept()
	{
		on_accept_started();
		return call_depth_;
	}

	void end_accept(std::size_t prior_call_depth)
	{
		on_accept_ended();
		call_depth_ = prior_call_depth;
		prune_depth_ = (std::numeric_limits<std::size_t>::max)();
	}

	[[nodiscard]] bool accept_response(std::size_t response_call_depth) noexcept
	{
		if (prune_depth_ > response_call_depth) {
			call_depth_ = response_call_depth;
			prune_depth_ = (std::numeric_limits<std::size_t>::max)();
			return true;
		}
		return false;
	}

	void set_match_and_subject(std::string_view m, std::string_view s) noexcept
	{
		match_ = m;
		subject_ = s;
		positions_.clear();
	}

	void clear_attribute_frame_stack() noexcept
	{
		while (attribute_frame_stack_ != nullptr)
			pop_attribute_frame_instance([](auto const& f, auto* b) noexcept { f.destroy(b); });
	}

	template <class FrameOp, class = std::enable_if_t<std::is_invocable_v<FrameOp, attribute_frame_handle const&, std::byte*>>>
	void pop_attribute_frame_instance(FrameOp const& frame_op) noexcept(std::is_nothrow_invocable_v<FrameOp, attribute_frame_handle const&, std::byte*>)
	{
		attribute_frame_instance* const instance{attribute_frame_stack_};
		attribute_frame_instance* const next_instance{instance->next};
		std::byte* const buffer{instance->buffer};
		attribute_frame_handle const frame{std::move(instance->frame)};
		std::destroy_at(instance);
		detail::scope_exit const release_memory{[this, &frame, buffer, instance, next_instance]() noexcept {
			if (frame.size_bytes() >= attribute_frame_allocator_.large_object_threshold())
				attribute_frame_allocator_.rewind(buffer, frame.size_bytes(), frame.alignment());
			attribute_frame_allocator_.rewind(instance, sizeof(attribute_frame_instance), alignof(attribute_frame_instance));
			attribute_frame_stack_ = next_instance;
		}};
		frame_op(frame, buffer);
	}

public:
	static constexpr std::uint_least32_t default_tab_width{8};
	static constexpr std::uint_least32_t default_tab_alignment{8};
	environment() = default;
	environment(environment const&) = delete;
	environment(environment&&) noexcept = default;
	environment& operator=(environment const&) = delete;
	environment& operator=(environment&&) noexcept = default;
	virtual ~environment() { clear_attribute_frame_stack(); }
	[[nodiscard]] bool should_reset_on_parse() const noexcept { return should_reset_on_parse_; }
	void should_reset_on_parse(bool should_reset) noexcept { should_reset_on_parse_ = should_reset; }
	[[nodiscard]] std::uint_least32_t tab_width() const noexcept { return tab_width_; }
	void tab_width(std::uint_least32_t w) noexcept { tab_width_ = w; }
	[[nodiscard]] std::uint_least32_t tab_alignment() const noexcept { return tab_alignment_; }
	void tab_alignment(std::uint_least32_t a) noexcept { tab_alignment_ = a; }
	[[nodiscard]] bool has_attributes() const noexcept { return !attribute_result_stack_.empty(); }
	[[nodiscard]] std::size_t attributes_size() const noexcept { return attribute_result_stack_.size(); }
	void start_attribute_collection() { attribute_collection_stack_.push_back(attribute_result_stack_.size()); }
	[[nodiscard]] attribute_collection finish_attribute_collection(std::size_t element_multiple = 1);
	[[nodiscard]] attribute_collection tail_attribute_collection(std::size_t element_count = 1);
	[[nodiscard]] bool has_condition(std::string_view name) const noexcept { return (conditions_.count(name) > 0); }
	bool set_condition(std::string_view name, bool value) { return value ? (!conditions_.emplace(name).second) : (conditions_.erase(name) > 0); }
	void clear_conditions() noexcept { conditions_.clear(); }
	[[nodiscard]] bool has_symbol(std::string_view name) const noexcept { return (symbols_.count(name) > 0); }
	[[nodiscard]] std::vector<std::string> const& get_symbols(std::string_view name) const noexcept { auto it = symbols_.find(name); if (it == symbols_.end()) return empty_symbols_; return it->second; }
	void add_symbol(std::string_view name, std::string value) { symbols_[name].emplace_back(std::move(value)); }
	void clear_symbols(std::string_view name) noexcept { symbols_.erase(name); }
	[[nodiscard]] std::string_view match() const noexcept { return match_; }
	[[nodiscard]] std::string_view subject() const noexcept { return subject_; }
	[[nodiscard]] syntax_position position_begin(syntax const& stx) { return position_at(stx.index()); }
	[[nodiscard]] syntax_position position_end(syntax const& stx) { return position_at(stx.index() + stx.size()); }
	[[nodiscard]] syntax_position position_begin(syntax_range const& range) { return position_at(range.index); }
	[[nodiscard]] syntax_position position_end(syntax_range const& range) { return position_at(range.index + range.size); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax const& stx) { return {position_begin(stx), position_end(stx)}; }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax_range const& range) { return {position_begin(range), position_end(range)}; }
	[[nodiscard]] std::size_t call_depth() const noexcept { return call_depth_; }
	[[nodiscard]] std::size_t prune_depth() const noexcept { return prune_depth_; }
	void escape() noexcept { prune_depth_ = call_depth_; }

	[[nodiscard]] syntax_position position_at(std::size_t index)
	{
		auto const pos = std::lower_bound(std::begin(positions_), std::end(positions_), index, [](auto& x, auto& y) { return x.first < y; });
		if (pos != std::end(positions_) && index == pos->first)
			return pos->second;
		std::size_t startindex{0};
		syntax_position position{origin_};
		if (pos != std::begin(positions_)) {
			auto prevpos = std::prev(pos);
			startindex = prevpos->first;
			position = prevpos->second;
		}
		auto first = std::next(std::begin(match_), static_cast<std::ptrdiff_t>(startindex));
		auto const last = std::next(std::begin(match_), static_cast<std::ptrdiff_t>(index));
		char32_t rune{U'\0'};
		char32_t prevrune{U'\0'};
		for (auto curr = first, next = curr; curr < last; curr = next, prevrune = rune) {
			std::tie(next, rune) = utf8::decode_rune(curr, last);
			if (((unicode::query(rune).properties() & unicode::ptype::Line_Ending) != unicode::ptype::None) && (prevrune != U'\r' || rune != U'\n')) {
				++position.line;
				position.column = 1;
				first = next;
			}
		}
		for (auto curr = first, next = curr; curr < last; curr = next) {
			std::tie(next, rune) = utf8::decode_rune(curr, last);
			if (rune != U'\t') {
				position.column += unicode::ucwidth(rune);
			} else {
				auto const oldcolumn = position.column;
				auto const newcolumn = oldcolumn + tab_width_;
				auto const alignedcolumn = newcolumn - ((newcolumn - 1) % tab_alignment_);
				position.column = (std::max)((std::min)(newcolumn, alignedcolumn), oldcolumn);
			}
		}
		return positions_.insert(pos, std::make_pair(index, position))->second;
	}

	void push_attribute_frame(attribute_frame_handle const& frame)
	{
		void* const instance_storage{attribute_frame_allocator_.allocate(sizeof(attribute_frame_instance), alignof(attribute_frame_instance))};
		detail::scope_exit instance_cleanup{[this, instance_storage]() noexcept { attribute_frame_allocator_.rewind(instance_storage, sizeof(attribute_frame_instance), alignof(attribute_frame_instance)); }};
		std::byte* const buffer{static_cast<std::byte*>(attribute_frame_allocator_.allocate(frame.size_bytes(), frame.alignment()))};
		detail::scope_exit buffer_cleanup{[this, &frame, buffer]() noexcept { attribute_frame_allocator_.rewind(buffer, frame.size_bytes(), frame.alignment()); }};
		frame.persist(buffer);
		detail::scope_exit frame_cleanup{[&frame, buffer]() noexcept { frame.destroy(buffer); }};
		attribute_frame_stack_ = ::new(instance_storage) attribute_frame_instance{attribute_frame_stack_, buffer, frame}; // NOLINT(cppcoreguidelines-owning-memory)
		frame_cleanup.release();
		buffer_cleanup.release();
		instance_cleanup.release();
	}

	void pop_attribute_frame(attribute_frame_handle const& frame)
	{
		if LUG_UNLIKELY(!attribute_frame_stack_ || (attribute_frame_stack_->frame != frame))
			throw_exception<attribute_stack_error>();
		pop_attribute_frame_instance([](auto const& f, auto* b) { f.restore(b); });
	}

	template <class T>
	void push_attribute(T&& x)
	{
		attribute_result_stack_.emplace_back(std::in_place_type<T>, std::forward<T>(x));
	}

	template <class T>
	[[nodiscard]] T pop_attribute()
	{
		return detail::move_only_any_cast<T>(detail::guarded_pop_back<attribute_stack_error>(attribute_result_stack_));
	}

	template <class T>
	[[nodiscard]] T& top_attribute()
	{
		if LUG_UNLIKELY(attribute_result_stack_.empty())
			throw_exception<attribute_stack_error>();
		return *detail::guarded_move_only_any_cast<T>(&attribute_result_stack_.back());
	}

	template <class T>
	[[nodiscard]] T const& top_attribute() const
	{
		if LUG_UNLIKELY(attribute_result_stack_.empty())
			throw_exception<attribute_stack_error>();
		return *detail::guarded_move_only_any_cast<T>(&attribute_result_stack_.back());
	}
};

class attribute_collection
{
	friend class environment;
	environment* envr_;
	std::size_t first_initial_;
	std::size_t last_initial_;
	std::size_t first_;
	std::size_t last_;
	attribute_collection(environment* envr, std::size_t first) noexcept
		: envr_{envr}
		, first_initial_{first}, last_initial_{envr_->attribute_result_stack_.size()}
		, first_{first_initial_}, last_{last_initial_} {}
public:
	attribute_collection(attribute_collection&& other) noexcept
		: envr_{std::exchange(other.envr_, nullptr)}
		, first_initial_{std::exchange(other.first_initial_, 0)}, last_initial_{std::exchange(other.last_initial_, 0)}
		, first_{std::exchange(other.first_, 0)}, last_{std::exchange(other.last_, 0)} {}
	~attribute_collection() { if (first_initial_ < last_initial_) envr_->attribute_result_stack_.resize(first_initial_); }
	[[nodiscard]] bool empty() const noexcept { return first_ >= last_; }
	[[nodiscard]] std::size_t size() const noexcept { return (first_ < last_) ? (last_ - first_) : 0; }
	void consume_front(std::size_t n) noexcept { first_ += n; }
	void consume_back(std::size_t n) noexcept { last_ -= (std::min)(n, last_); }
	template <class T, std::size_t I> [[nodiscard]] T read_front() { return detail::move_only_any_cast<T>(std::move(envr_->attribute_result_stack_[first_ + I])); }
	template <class T, std::size_t I, std::size_t N> [[nodiscard]] T read_back() { return detail::move_only_any_cast<T>(std::move(envr_->attribute_result_stack_[last_ - N + I])); }
	attribute_collection(attribute_collection const&) = delete;
	attribute_collection& operator=(attribute_collection const&) = delete;
	attribute_collection& operator=(attribute_collection&&) = delete;
};

[[nodiscard]] inline attribute_collection environment::finish_attribute_collection(std::size_t element_multiple)
{
	std::size_t const index = detail::guarded_pop_back<attribute_stack_error>(attribute_collection_stack_);
	if LUG_UNLIKELY((index > attribute_result_stack_.size()) || (((attribute_result_stack_.size() - index) % element_multiple) != 0))
		throw_exception<attribute_stack_error>();
	return attribute_collection{this, index};
}

[[nodiscard]] inline attribute_collection environment::tail_attribute_collection(std::size_t element_count)
{
	if LUG_UNLIKELY(element_count > attribute_result_stack_.size())
		throw_exception<attribute_stack_error>();
	return attribute_collection{this, attribute_result_stack_.size() - element_count};
}

template <class T>
class recursive_wrapper // NOLINT(cppcoreguidelines-special-member-functions,hicpp-special-member-functions)
{
	static_assert(std::is_move_constructible_v<T>, "T must be move constructible");
	std::unique_ptr<T> ptr;
public:
	using type = T;
	template <class U = T, class = std::enable_if_t<std::is_default_constructible_v<U>>>
	recursive_wrapper() : ptr{std::make_unique<T>()} {}
	template <class U, class = std::enable_if_t<std::is_constructible_v<T, U&&> && !std::is_same_v<recursive_wrapper<T>, std::decay_t<U>>>>
	recursive_wrapper(U&& x) : ptr{std::make_unique<T>(std::forward<U>(x))} {} // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	template <class U = T, class = std::enable_if_t<!std::is_constructible_v<T, std::unique_ptr<U>&&>>>
	explicit recursive_wrapper(std::unique_ptr<U>&& p) noexcept : ptr{std::move(p)} {}
	template <class U = T, class = std::enable_if_t<std::is_copy_constructible_v<U>>>
	recursive_wrapper(recursive_wrapper const& other) : ptr{std::make_unique<T>(*other.ptr)} {} // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	recursive_wrapper(recursive_wrapper&&) noexcept = default;
	recursive_wrapper& operator=(recursive_wrapper&&) noexcept = default;
	~recursive_wrapper() = default;

	recursive_wrapper& operator=(recursive_wrapper const& other)
	{
		recursive_wrapper{other}.swap(*this);
		return *this;
	}

	template <class U, class = std::enable_if_t<std::is_constructible_v<T, U&&> && !std::is_same_v<recursive_wrapper<T>, std::decay_t<U>>>>
	recursive_wrapper& operator=(U&& x)
	{
		ptr = std::make_unique<T>(std::forward<U>(x));
		return *this;
	}

	template <class U = T, class = std::enable_if_t<!std::is_assignable_v<T, std::unique_ptr<U>&&>>>
	recursive_wrapper& operator=(std::unique_ptr<U>&& p)
	{
		ptr = std::move(p);
		return *this;
	}

	void swap(recursive_wrapper& other) noexcept { ptr.swap(other.ptr); }
	operator T&() noexcept { return *ptr; } // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	operator T const&() const noexcept { return *ptr; } // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
	T& get() noexcept { return *ptr; }
	T const & get() const noexcept { return *ptr; }
	T* get_pointer() noexcept { return ptr.get(); }
	T const* get_pointer() const noexcept { return ptr.get(); }
};

template <class T> recursive_wrapper(T&&) -> recursive_wrapper<std::decay_t<T>>;

class error_context
{
	std::reference_wrapper<lug::environment> envr_;
	lug::syntax syntax_;
	std::string_view label_;
	error_response recovery_response_;
public:
	error_context(lug::environment& envr, lug::syntax const& syn, std::string_view lab, error_response resp) : envr_{envr}, syntax_{syn}, label_{lab}, recovery_response_{resp} {}
	~error_context() = default;
	[[nodiscard]] lug::environment& environment() const noexcept { return envr_.get(); }
	[[nodiscard]] lug::syntax const& syntax() const noexcept { return syntax_; }
	[[nodiscard]] std::string_view label() const noexcept { return label_; }
	[[nodiscard]] error_response recovery_response() const noexcept { return recovery_response_; }
	[[nodiscard]] syntax_position position_begin() const { return environment().position_begin(syntax_); }
	[[nodiscard]] syntax_position position_end() const { return environment().position_end(syntax_); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range() const { return environment().position_range(syntax_); }
	error_context(error_context const&) = delete;
	error_context& operator=(error_context const&) = delete;
	error_context(error_context&&) = delete;
	error_context& operator=(error_context&&) = delete;
};

template <class Recovery>
class recover_with
{
	using storage_type = std::conditional_t<std::is_void_v<Recovery>, std::nullptr_t, std::conditional_t<is_encoder_expression_v<Recovery>, Recovery, std::reference_wrapper<rule const>>>;
	storage_type recovery_;
public:
	template <class R = Recovery, class = std::enable_if_t<std::is_void_v<R>>>
	constexpr recover_with() noexcept : recovery_{nullptr} {}
	template <class R, class = std::enable_if_t<std::is_constructible_v<storage_type, R&&>>>
	constexpr explicit recover_with(R&& r) noexcept(std::is_nothrow_constructible_v<storage_type, R&&>) : recovery_{std::forward<R>(r)} {}

	[[nodiscard]] constexpr auto const& recovery() const noexcept {
		if constexpr (is_encoder_expression_v<Recovery> || std::is_void_v<Recovery>)
			return recovery_;
		else
			return recovery_.get();
	}
};

template <class R, class = std::enable_if_t<is_recovery_expression_v<R>>> recover_with(R&&) -> recover_with<std::decay_t<R>>;
recover_with() -> recover_with<void>;

template <class Recovery = void>
class failure : public recover_with<Recovery>
{
	std::string_view label_;
public:
	template <class R = Recovery, class = std::enable_if_t<std::is_void_v<R>>>
	constexpr explicit failure(std::string_view lab) noexcept : label_{lab} {}
	template <class R, class = std::enable_if_t<std::is_constructible_v<recover_with<Recovery>, R&&>>>
	constexpr explicit failure(std::string_view lab, R&& rec) noexcept(std::is_nothrow_constructible_v<recover_with<Recovery>, R&&>) : recover_with<Recovery>{std::forward<R>(rec)}, label_{lab} {}
	[[nodiscard]] constexpr std::string_view label() const noexcept { return label_; }
};

template <class R, class = std::enable_if_t<is_recovery_expression_v<R>>> failure(std::string_view, R&&) -> failure<std::decay_t<R>>;
failure(std::string_view) -> failure<void>;

class encoder
{
	static constexpr std::size_t inline_max_instructions{8};
	static constexpr std::size_t inline_max_objects{4};

	rule* rule_{nullptr};
	program* program_{nullptr};
	program_callees* callees_{nullptr};
	std::shared_ptr<attribute_frame_info> attribute_frame_info_;
	std::vector<directives> mode_;
	directives entry_mode_{directives::none};

	template <class Item, class ItemValue, class = std::enable_if_t<std::is_constructible_v<Item, ItemValue&&>>>
	[[nodiscard]] std::uint_least16_t add_item(std::vector<Item>& items, ItemValue&& item)
	{
		if constexpr (std::is_same_v<std::decay_t<Item>, std::decay_t<ItemValue>> && detail::is_equality_comparable_v<std::decay_t<Item>>) {
			return detail::checked_cast<std::uint_least16_t, resource_limit_error>(detail::push_back_unique(items, std::forward<ItemValue>(item)));
		} else {
			items.push_back(std::forward<ItemValue>(item));
			return detail::checked_cast<std::uint_least16_t, resource_limit_error>(items.size() - 1);
		}
	}

	[[nodiscard]] std::pair<std::int_least32_t, std::uint_least16_t> add_string(std::string_view str)
	{
		std::size_t const index = program_->data.size();
		program_->data.insert(program_->data.end(), str.begin(), str.end());
		return {
			detail::checked_cast<std::int_least32_t, resource_limit_error>(index, 0U, static_cast<std::size_t>((std::numeric_limits<std::int_least32_t>::max)())),
			detail::checked_cast<std::uint_least16_t, resource_limit_error>(str.size(), 0U, (std::numeric_limits<std::uint_least16_t>::max)())
		};
	}

	[[nodiscard]] LUG_NONNULL(3) std::ptrdiff_t do_call(rule const* r, program const* p, std::ptrdiff_t off, std::uint_least16_t prec)
	{
		directives const callee_mode = mode_.back();
		skip(p->entry_mode ^ directives::eps, directives::noskip);
		callees_->emplace_back(r, p, here(), callee_mode);
		return encode(opcode::call, off, prec, 0);
	}

public:
	explicit encoder(program& p, program_callees& c, directives initial = directives::eps) : program_{&p}, callees_{&c}, attribute_frame_info_{std::make_shared<attribute_frame_info>()}, mode_{initial} {}
	explicit encoder(rule& r) : rule_{&r}, program_{&r.program_}, callees_{&r.callees_}, attribute_frame_info_{std::make_shared<attribute_frame_info>()}, mode_{directives::eps} { rule_->currently_encoding_ = true; }
	encoder(encoder const&) = delete;
	encoder(encoder&& e) noexcept : rule_{std::exchange(e.rule_, nullptr)}, program_{std::exchange(e.program_, nullptr)}, callees_{std::exchange(e.callees_, nullptr)}, attribute_frame_info_{std::exchange(e.attribute_frame_info_, nullptr)}, mode_{std::move(e.mode_)}, entry_mode_{e.entry_mode_} {}
	encoder& operator=(encoder const&) = delete;
	encoder& operator=(encoder&& e) noexcept { encoder{std::move(e)}.swap(*this); return *this; }
	~encoder() { if (program_ != nullptr) { program_->entry_mode = entry_mode(); } if (rule_ != nullptr) { rule_->currently_encoding_ = false; } }
	void swap(encoder& e) noexcept { std::swap(rule_, e.rule_); std::swap(program_, e.program_); std::swap(callees_, e.callees_); mode_.swap(e.mode_); std::swap(entry_mode_, e.entry_mode_); }
	[[nodiscard]] bool is_frame_empty() const noexcept { return attribute_frame_info_->empty(); }
	[[nodiscard]] attribute_frame_handle get_frame_handle() const noexcept { return attribute_frame_info_->handle(); }
	[[nodiscard]] std::uint_least16_t get_frame_handle_index() { return add_item(program_->frames, attribute_frame_info_->handle()); }
	template <class Target, class = std::enable_if_t<is_attribute_frame_persistable_v<Target>>> LUG_NONNULL(2) void add_to_frame(Target* target) { attribute_frame_info_->add(target); }
	[[nodiscard]] directives mode() const noexcept { return mode_.back(); }
	[[nodiscard]] directives entry_mode() const noexcept { return (entry_mode_ & ~directives::eps) | mode_.back(); }
	[[nodiscard]] std::ptrdiff_t here() const noexcept { return static_cast<std::ptrdiff_t>(program_->instructions.size()); }
	[[nodiscard]] instruction& instruction_at(std::ptrdiff_t addr) { return program_->instructions[static_cast<std::size_t>(addr)]; }
	void jump_to_target(std::ptrdiff_t addr, std::ptrdiff_t target) { instruction_at(addr).offset32 = detail::checked_cast<std::int_least32_t, program_limit_error>(target - addr - 1); }
	void jump_to_here(std::ptrdiff_t addr) { jump_to_target(addr, here()); }
	std::ptrdiff_t append(instruction instr) { std::ptrdiff_t const addr{here()}; program_->instructions.push_back(instr); return addr; }
	std::ptrdiff_t append(program const& p) { std::ptrdiff_t const addr{here()}; program_->concatenate(p); return addr; }
	std::ptrdiff_t encode(opcode op) { return append(instruction{op, 0, 0, 0}); }
	std::ptrdiff_t encode(opcode op, std::ptrdiff_t off, std::uint_least16_t imm16, std::uint_least8_t imm8) { return append(instruction{op, imm8, imm16, detail::checked_cast<std::int_least32_t, program_limit_error>(off)}); }
	std::ptrdiff_t encode(opcode op, std::uint_least16_t imm16, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, imm16, 0}); }
	std::ptrdiff_t encode(opcode op, std::string_view str, std::uint_least8_t imm8 = 0) { auto const rng = add_string(str); return append(instruction{op, imm8, rng.second, rng.first}); }
	std::ptrdiff_t encode(opcode op, error_handler&& h, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->handlers, std::move(h)), 0}); }
	std::ptrdiff_t encode(opcode op, semantic_action&& a, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->actions, std::move(a)), 0}); }
	std::ptrdiff_t encode(opcode op, semantic_capture_action&& a, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->captures, std::move(a)), 0}); }
	std::ptrdiff_t encode(opcode op, syntactic_predicate&& p, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->predicates, std::move(p)), 0}); }
	std::ptrdiff_t encode_min_max(opcode op, std::size_t nmin, std::size_t nmax, std::uint_least16_t imm16 = 0, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, imm16, instruction::pack_min_max(nmin, nmax)}); }
	template <typename RS, class = std::enable_if_t<std::is_constructible_v<rune_set, RS&&>>> [[nodiscard]] std::uint_least16_t add_rune_set(RS&& runes) { return add_item(program_->runesets, std::forward<RS>(runes)); }

	std::ptrdiff_t call(program const& p, std::uint_least16_t prec, [[maybe_unused]] bool allow_inlining = true)
	{
		return do_call(nullptr, &p, 0, prec);
	}

	std::ptrdiff_t call(rule const& r, std::uint_least16_t prec, bool allow_inlining = true)
	{
		if (auto const& p = r.program_; allow_inlining && (prec <= 0) && !r.currently_encoding_ && r.callees_.empty() &&
										(!p.instructions.empty() && (p.instructions.size() <= inline_max_instructions)) &&
										((p.uniforms.size() + p.runesets.size() + p.handlers.size() + p.actions.size() +
										  p.captures.size() + p.predicates.size()) <= inline_max_objects))
			return skip(p.entry_mode, directives::noskip).append(p);
		return do_call(&r, &r.program_, 0, prec);
	}

	template <class T, class... Args>
	std::ptrdiff_t call_with_frame(T&& target, std::uint_least16_t prec, Args&&... args)
	{
		std::uint_least16_t handle_index{0};
		if (!attribute_frame_info_->empty()) {
			handle_index = get_frame_handle_index();
			encode(opcode::attribute_push, handle_index);
		}
		std::ptrdiff_t const result{call(std::forward<T>(target), prec, std::forward<Args>(args)...)};
		if (!attribute_frame_info_->empty())
			encode(opcode::attribute_pop, handle_index);
		return result;
	}

	std::ptrdiff_t recover_push_call(rule const& r)
	{
		callees_->emplace_back(&r, &r.program_, here(), mode_.back());
		return encode(opcode::recover_push, 0, 0, 0);
	}

	template <class Recovery>
	[[nodiscard]] decltype(auto) raise_failure(failure<Recovery> const& reason)
	{
		if constexpr (is_encoder_expression_v<Recovery>) {
			auto const recovery_subroutine = encode(opcode::recover_push);
			encode(opcode::raise, reason.label(), 1);
			auto const finished = encode(opcode::jump);
			jump_to_here(recovery_subroutine);
			reason.recovery().evaluate(*this);
			encode(opcode::ret);
			jump_to_here(finished);
		} else if constexpr (std::is_same_v<Recovery, rule>) {
			recover_push_call(reason.recovery());
			encode(opcode::raise, reason.label(), 1);
		} else {
			encode(opcode::raise, reason.label(), 0);
		}
	}

	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	std::ptrdiff_t encode_class(opcode op, T properties)
	{
		return encode(op, add_item(program_->uniforms, static_cast<std::uint_least64_t>(properties)), static_cast<std::uint_least8_t>(unicode::to_property_enum_v<std::decay_t<T>>));
	}

	template <class T, class = std::enable_if_t<std::is_same_v<std::decay_t<T>, char> || std::is_same_v<std::decay_t<T>, char32_t>>>
	std::ptrdiff_t encode_unit_or_set(opcode unit_op, opcode set_op, T value, std::size_t nmin = 0, std::size_t nmax = (std::numeric_limits<std::size_t>::max)())
	{
		if ((mode() & directives::caseless) != directives::none) {
			auto const rune = static_cast<char32_t>(static_cast<std::make_unsigned_t<T>>(value));
			if (auto const properties = unicode::query(rune).properties(); ((properties & unicode::ptype::Cased) != unicode::ptype::None))
				return encode_min_max(set_op, nmin, nmax, add_rune_set(std::move(rune_set_builder{}.casefold().add_rune(rune)).build()));
		}
		if constexpr (std::is_same_v<std::decay_t<T>, char32_t>)
			if (!ascii::isascii(value))
				return encode_min_max(set_op, nmin, nmax, add_rune_set(std::move(rune_set_builder{}.add_rune(value)).build()));
		return encode_min_max(unit_op, nmin, nmax, std::uint_least16_t{0}, static_cast<std::uint_least8_t>(static_cast<std::make_unsigned_t<T>>(value)));
	}

	std::ptrdiff_t match(std::string_view pattern)
	{
		skip(!pattern.empty() ? directives::eps : directives::none);
		if (pattern.size() == 1)
			return encode_unit_or_set(opcode::match_unit, opcode::match_set, pattern.front());
		if (!pattern.empty() && ((mode() & directives::caseless) != directives::none))
			return encode(opcode::match_cf, utf8::tocasefold(pattern));
		return encode(opcode::match, pattern);
	}

	template <class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	std::ptrdiff_t match_class(opcode op, T properties)
	{
		return skip().encode_class(op, properties);
	}

	template <typename RS, class = std::enable_if_t<std::is_constructible_v<rune_set, RS&&>>>
	std::ptrdiff_t match_set(RS&& set)
	{
		return skip().encode(opcode::match_set, add_rune_set(std::forward<RS>(set)));
	}

	void dpsh(directives enable, directives disable)
	{
		mode_.push_back((mode_.back() & ~disable) | enable);
	}

	void dpop(directives relay)
	{
		directives const prev_mode = detail::pop_back(mode_);
		directives& curr_mode = mode_.back();
		curr_mode = (curr_mode & ~relay) | (prev_mode & relay);
		if (((curr_mode & directives::postskip) == directives::none) && ((prev_mode & (directives::lexeme | directives::noskip | directives::postskip)) == directives::postskip))
			encode(opcode::skip_space);
	}

	void ddrop()
	{
		mode_.pop_back();
	}

	[[nodiscard]] bool should_skip(directives callee_mode = directives::eps, directives inhibit_mask = directives::lexeme) const
	{
		return ((((mode_.back() | callee_mode)) & (inhibit_mask | directives::preskip)) == directives::preskip);
	}

	encoder& commit_eps(directives callee_mode = directives::eps)
	{
		directives& curr_mode = mode_.back();
		if (entry_mode_ == directives::none)
			entry_mode_ = (curr_mode & (directives::caseless | directives::lexeme | directives::noskip)) | directives::eps;
		curr_mode &= ~(callee_mode & directives::eps);
		return *this;
	}

	[[nodiscard]] bool prepare_skip(directives callee_mode = directives::eps, directives inhibit_mask = directives::lexeme)
	{
		bool const result = should_skip(callee_mode, inhibit_mask);
		commit_eps(callee_mode);
		return result;
	}

	encoder& skip(directives callee_mode = directives::eps, directives inhibit_mask = directives::lexeme)
	{
		if (prepare_skip(callee_mode, inhibit_mask))
			encode(opcode::skip_space);
		return *this;
	}
};

template <class Derived>
struct common_encoder_expression_interface
{
	using expression_trait = encoder_expression_trait_tag;
	[[nodiscard]] constexpr Derived& derived() noexcept { return static_cast<Derived&>(*this); }
	[[nodiscard]] constexpr const Derived& derived() const noexcept { return static_cast<const Derived&>(*this); }
	template <class Recovery> [[nodiscard]] constexpr auto operator[](failure<Recovery> const& reason) const;
	template <class Recovery> [[nodiscard]] constexpr auto operator[](recover_with<Recovery> const& rec) const;
	template <class Handler, class = std::enable_if_t<is_error_handler_v<Handler>>> [[nodiscard]] constexpr auto operator^=(Handler&& handler) const;
	[[nodiscard]] constexpr bool has_effects(effect_traits mask) const noexcept { return (this->derived().effects() & mask) == mask; }
};

template <class Derived, auto... Traits>
struct terminal_encoder_expression_interface : common_encoder_expression_interface<Derived>
{
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return flag_enum_fold_pack_or_v<effect_traits::none, Traits...>; }
	[[nodiscard]] constexpr match_traits matches() const noexcept { return flag_enum_fold_pack_or_v<match_traits::none, Traits...>; }
};

template <class Derived, class E1>
struct unary_encoder_expression_interface : common_encoder_expression_interface<Derived>
{
	E1 e1;
	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr explicit unary_encoder_expression_interface(X1&& x1) : e1(std::forward<X1>(x1)) {}
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects(); }
	[[nodiscard]] constexpr match_traits matches() const noexcept { return this->e1.matches(); }
};

template <class Derived, class E1, class E2>
struct binary_encoder_expression_interface : common_encoder_expression_interface<Derived>
{
	E1 e1;
	E2 e2;
	template <class X1, class X2, class = std::enable_if_t<std::is_constructible_v<E1, X1&&> && std::is_constructible_v<E2, X2&&>>>
	constexpr binary_encoder_expression_interface(X1&& x1, X2&& x2) : e1(std::forward<X1>(x1)), e2(std::forward<X2>(x2)) {}
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects() | this->e2.effects(); }
};

template <class Recovery>
struct raise_expression : terminal_encoder_expression_interface<raise_expression<Recovery>, effect_traits::raises>
{
	failure<Recovery> reason;
	constexpr explicit raise_expression(failure<Recovery> const& fail) noexcept : reason{fail} {}
	void evaluate(encoder& d) const { return d.raise_failure(reason); }
};

template <class E1, class Recovery>
struct expect_expression : unary_encoder_expression_interface<expect_expression<E1, Recovery>, E1>
{
	using base_type = unary_encoder_expression_interface<expect_expression<E1, Recovery>, E1>;
	failure<Recovery> reason;
	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr expect_expression(X1&& x1, failure<Recovery> const& fail) : base_type{std::forward<X1>(x1)}, reason{fail} {}
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects() | effect_traits::raises; }

	void evaluate(encoder& d) const
	{
		auto const choice = d.encode(opcode::choice);
		this->e1.evaluate(d);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		d.raise_failure(reason);
		d.jump_to_here(commit);
	}
};

template <class E1, class Recovery>
struct recover_with_expression : unary_encoder_expression_interface<recover_with_expression<E1, Recovery>, E1>
{
	using base_type = unary_encoder_expression_interface<recover_with_expression<E1, Recovery>, E1>;
	recover_with<Recovery> rec;
	template <class X1, class R, class = std::enable_if_t<std::is_constructible_v<E1, X1&&> && std::is_constructible_v<recover_with<Recovery>, R&&>>>
	constexpr recover_with_expression(X1&& x1, R&& r) : base_type{std::forward<X1>(x1)}, rec{std::forward<R>(r)} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_encoder_expression_v<Recovery>) {
			auto const recovery_subroutine = d.encode(opcode::recover_push);
			this->e1.evaluate(d);
			d.encode(opcode::recover_pop);
			auto const finished = d.encode(opcode::jump);
			d.jump_to_here(recovery_subroutine);
			rec.recovery().evaluate(d);
			d.encode(opcode::ret);
			d.jump_to_here(finished);
		} else if constexpr (std::is_same_v<Recovery, rule>) {
			d.recover_push_call(rec.recovery());
			this->e1.evaluate(d);
			d.encode(opcode::recover_pop);
		} else {
			this->e1.evaluate(d);
		}
	}
};

struct recover_response_expression : terminal_encoder_expression_interface<recover_response_expression, match_traits::nullable | match_traits::nofail>
{
	error_response response;
	constexpr explicit recover_response_expression(error_response r) noexcept : response{r} {}
	void evaluate(encoder& d) const { d.encode(opcode::recover_resp, 0, static_cast<std::uint_least8_t>(response)); }
};

template <class E1, class Handler>
struct report_expression : unary_encoder_expression_interface<report_expression<E1, Handler>, E1>
{
	using base_type = unary_encoder_expression_interface<report_expression<E1, Handler>, E1>;
	using base_type::base_type;
	Handler handler;

	template <class X1, class H, class = std::enable_if_t<std::is_constructible_v<Handler, H&&>>>
	constexpr report_expression(X1&& x1, H&& h) : base_type{std::forward<X1>(x1)}, handler{std::forward<H>(h)} {}

	void evaluate(encoder& d) const
	{
		if constexpr (std::is_invocable_r_v<error_response, Handler, error_context&>)
			d.encode(opcode::report_push, error_handler{handler});
		else
			d.encode(opcode::report_push, error_handler{[h = handler](error_context& e) -> error_response { (void)h(e); return e.recovery_response(); }});
		this->e1.evaluate(d);
		d.encode(opcode::report_pop);
	}
};

template <class X1, class H> report_expression(X1&&, H&&) -> report_expression<std::decay_t<X1>, std::decay_t<H>>;

template <class Derived> template <class Recovery>
[[nodiscard]] constexpr auto common_encoder_expression_interface<Derived>::operator[](failure<Recovery> const& reason) const
{
	return expect_expression<Derived, Recovery>{derived(), reason};
}

template <class Derived> template <class Recovery>
[[nodiscard]] constexpr auto common_encoder_expression_interface<Derived>::operator[](recover_with<Recovery> const& rec) const
{
	return recover_with_expression<Derived, Recovery>{derived(), rec};
}

template <class Derived> template <class Handler, class>
[[nodiscard]] constexpr auto common_encoder_expression_interface<Derived>::operator^=(Handler&& handler) const
{
	return report_expression<Derived, std::decay_t<Handler>>{derived(), std::forward<Handler>(handler)};
}

struct bracket_expression : terminal_encoder_expression_interface<bracket_expression>
{
	std::string_view pattern;
	constexpr explicit bracket_expression(std::string_view s) noexcept : pattern{s} {}
	void evaluate(encoder& d) const { d.match_set(make_rune_set(d.mode())); }

	[[nodiscard]] rune_set make_rune_set(directives mode = directives::none) const
	{
		rune_set_builder builder;
		builder.casefold((mode & directives::caseless) != directives::none);
		bool left_rune_present{false};
		char32_t left_rune{U'\0'};
		auto curr = pattern.begin();
		auto const last = pattern.end();
		if ((curr != last) && (*curr == '^')) {
			builder.negate();
			++curr;
		}
		if LUG_UNLIKELY(curr == last)
			throw_exception<bad_character_range>();
		while (curr != last) {
			auto const [next, next_rune] = utf8::decode_rune(curr, last);
			if ((next_rune == U'-') && (next != last)) {
				auto const [right, right_rune] = utf8::decode_rune(next, last);
				if LUG_UNLIKELY(!left_rune_present)
					throw_exception<bad_character_range>();
				builder.add_range(left_rune, right_rune);
				left_rune = U'\0';
				left_rune_present = false;
				curr = right;
			} else {
				if (left_rune_present)
					builder.add_rune(left_rune);
				left_rune = next_rune;
				left_rune_present = true;
				curr = next;
			}
		}
		if (left_rune_present)
			builder.add_rune(left_rune);
		return std::move(builder).build();
	}
};

struct string_expression : terminal_encoder_expression_interface<string_expression>
{
	std::string_view text;
	constexpr explicit string_expression(std::string_view t) noexcept : text{t} {}
	void evaluate(encoder& d) const { d.match(text); }
	[[nodiscard]] constexpr match_traits matches() const noexcept { return text.empty() ? (match_traits::nullable | match_traits::nofail) : match_traits::none; }
};

struct char_expression : terminal_encoder_expression_interface<char_expression>
{
	char c;
	constexpr explicit char_expression(char x) noexcept : c{x} {}
	void evaluate(encoder& d) const { d.match(std::string_view{&c, 1}); }
};

struct rune_expression : terminal_encoder_expression_interface<rune_expression>
{
	char32_t c;
	constexpr explicit rune_expression(char32_t x) noexcept : c{x} {}
	void evaluate(encoder& d) const { d.skip().encode_unit_or_set(opcode::match_unit, opcode::match_set, c); }
};

struct rune_range_expression : terminal_encoder_expression_interface<rune_range_expression>
{
	char32_t start;
	char32_t end;
	constexpr rune_range_expression(char32_t first, char32_t last) noexcept : start{first}, end{last} {}
	void evaluate(encoder& d) const { d.match_set(make_rune_set(d.mode())); }

	[[nodiscard]] rune_set make_rune_set(directives mode = directives::none) const
	{
		return std::move(rune_set_builder{}.casefold((mode & directives::caseless) != directives::none).add_range(start, end)).build();
	}
};

struct rune_set_expression : terminal_encoder_expression_interface<rune_set_expression>
{
	rune_set set;
	explicit rune_set_expression(rune_set const& rs) noexcept : set{rs} {}
	explicit rune_set_expression(rune_set&& rs) noexcept : set{std::move(rs)} {}
	void evaluate(encoder& d) const { d.match_set(make_rune_set(d.mode())); }

	[[nodiscard]] rune_set make_rune_set(directives mode = directives::none) const
	{
		if ((mode & directives::caseless) != directives::none)
			return std::move(rune_set_builder{}.casefold().add_rune_set(set)).build();
		return set;
	}
};

template <class Target>
struct callable_expression : terminal_encoder_expression_interface<callable_expression<Target>>
{
	std::reference_wrapper<Target> target;
	std::uint_least16_t prec{0};
	constexpr explicit callable_expression(Target& t) noexcept : target{t} {}
	constexpr explicit callable_expression(Target& t, std::uint_least16_t p) noexcept : target{t}, prec{p} {}
	void evaluate(encoder& d) const { d.call_with_frame(target.get(), prec); }
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return target.get().first_effects() | target.get().follow_effects(); }
	[[nodiscard]] constexpr match_traits matches() const noexcept { return target.get().first_matches() & target.get().follow_matches(); }
};

template <class Pred>
struct predicate_expression : terminal_encoder_expression_interface<predicate_expression<Pred>>
{
	Pred pred;
	template <class P, class = std::enable_if_t<std::is_constructible_v<Pred, P&&>>> constexpr explicit predicate_expression(P&& p) noexcept(std::is_nothrow_constructible_v<Pred, P&&>) : pred(std::forward<P>(p)) {}
	void evaluate(encoder& d) const { d.encode(opcode::predicate, syntactic_predicate{pred}); }
};

template <class P> predicate_expression(P&&) -> predicate_expression<std::decay_t<P>>;

template <class E, class = std::enable_if_t<is_encoder_expression_v<E>>>
[[nodiscard]] constexpr auto make_expression(E const& e) noexcept -> E const& { return e; }

template <class E, class = std::enable_if_t<!is_encoder_expression_v<E> && is_expression_v<E>>>
[[nodiscard]] constexpr auto make_expression(E&& e)
{
	if constexpr (is_encoder_callable_v<E>)
		return callable_expression{std::forward<E>(e)};
	else if constexpr (std::is_same_v<std::decay_t<E>, char>)
		return char_expression{std::forward<E>(e)};
	else if constexpr (std::is_same_v<std::decay_t<E>, char32_t>)
		return rune_expression{std::forward<E>(e)};
	else if constexpr (std::is_same_v<std::decay_t<E>, rune_set>)
		return rune_set_expression{std::forward<E>(e)};
	else if constexpr (std::is_convertible_v<std::decay_t<E>, std::string_view>)
		return string_expression{std::forward<E>(e)}; // NOLINT(cppcoreguidelines-pro-bounds-array-to-pointer-decay,hicpp-no-array-decay)
	else if constexpr (is_predicate_v<E>)
		return predicate_expression{std::forward<E>(e)};
	else
		static_assert(detail::always_false_v<E>, "invalid expression type");
}

template <class E, class>
inline rule::rule(E const& e)
{
	encoder rule_encoder{*this};
	(void)make_expression(e).evaluate(rule_encoder);
}

inline rule::rule(rule const& r)
{
	encoder rule_encoder{*this};
	rule_encoder.call(r, 1);
}

[[nodiscard]] inline auto rule::operator[](std::uint_least16_t prec) const noexcept
{
	return callable_expression<rule const>{*this, prec};
}

template <class Recovery>
[[nodiscard]] inline auto rule::operator[](failure<Recovery> const& reason) const
{
	return expect_expression<callable_expression<rule const>, Recovery>{callable_expression<rule const>{*this}, reason};
}

template <class Recovery>
[[nodiscard]] inline auto rule::operator[](recover_with<Recovery> const& rec) const
{
	return recover_with_expression<callable_expression<rule const>, Recovery>{callable_expression<rule const>{*this}, rec};
}

template <class Handler, class>
[[nodiscard]] inline auto rule::operator^=(Handler&& handler) const
{
	return report_expression<callable_expression<rule const>, std::decay_t<Handler>>{callable_expression<rule const>{*this}, std::forward<Handler>(handler)};
}

template <class E1>
struct directive_expression : unary_encoder_expression_interface<directive_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<directive_expression<E1>, E1>;
	directives enable_mask{directives::none};
	directives disable_mask{directives::none};
	directives relay_mask{directives::none};

	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr directive_expression(X1&& x1, directives enable, directives disable, directives relay)
		: base_type{std::forward<X1>(x1)}, enable_mask{enable}, disable_mask{disable}, relay_mask{relay} {}

	void evaluate(encoder& d) const
	{
		d.dpsh(enable_mask, disable_mask);
		this->e1.evaluate(d);
		d.dpop(relay_mask);
	}
};

template <class E> struct unwrap_directive_expression { using type = E; };
template <class E> struct unwrap_directive_expression<directive_expression<E>> { using type = typename unwrap_directive_expression<E>::type; };
template <class E> using unwrap_directive_expression_t = typename unwrap_directive_expression<std::decay_t<E>>::type;

template <directives EnableMask, directives DisableMask, directives RelayMask>
struct directive_modifier
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
	{
		return directive_expression<std::decay_t<decltype(make_expression(e))>>{make_expression(e), EnableMask, DisableMask, RelayMask};
	}

	template <class E>
	[[nodiscard]] constexpr auto operator[](directive_expression<E> const& e) const noexcept
	{
		return directive_expression<E>{e.e1, ((EnableMask & ~e.disable_mask) | e.enable_mask), (DisableMask | e.disable_mask), RelayMask};
	}
};

struct accept_cut_expression : terminal_encoder_expression_interface<accept_cut_expression, effect_traits::cuts>
{
	std::uint_least8_t imm8;
	constexpr explicit accept_cut_expression(std::size_t flags) noexcept : imm8{static_cast<std::uint_least8_t>(flags >> registers::ignore_errors_shift)} {}
	void evaluate(encoder& d) const { d.encode(opcode::accept, 0, imm8); }
};

struct eoi_expression : terminal_encoder_expression_interface<eoi_expression, match_traits::nullable>
{
	void evaluate(encoder& d) const { d.encode(opcode::match_eoi, 0, d.prepare_skip() ? 1 : 0); }
};

struct eol_expression : terminal_encoder_expression_interface<eol_expression>
{
	void evaluate(encoder& d) const { d.encode(opcode::match_eol, 0, d.prepare_skip() ? 1 : 0); }
};

struct eps_expression : terminal_encoder_expression_interface<eps_expression, match_traits::nullable | match_traits::nofail>
{
	void evaluate(encoder& /*d*/) const {}
};

template <class Property>
struct match_class_expression : terminal_encoder_expression_interface<match_class_expression<Property>>
{
	opcode mop;
	Property property;
	constexpr match_class_expression(opcode op, Property prop) noexcept : mop{op}, property{prop} {}
	void evaluate(encoder& d) const { d.match_class(mop, property); }
};

struct match_class_combinator
{
	opcode mop;
	constexpr explicit match_class_combinator(opcode op) noexcept : mop{op} {}
	template <class Property, class = std::enable_if_t<unicode::is_property_enum_v<Property>>>
	[[nodiscard]] constexpr match_class_expression<std::decay_t<Property>> operator()(Property prop) const { return match_class_expression<std::decay_t<Property>>{mop, prop}; }
};

struct match_any_expression : terminal_encoder_expression_interface<match_any_expression>, match_class_combinator
{
	constexpr match_any_expression() noexcept : match_class_combinator{opcode::match_any_of} {}
	void evaluate(encoder& d) const { d.skip().encode(opcode::match_any); }
};

template <ascii::ctype Property>
struct ascii_ctype_expression : terminal_encoder_expression_interface<ascii_ctype_expression<Property>>
{
	void evaluate(encoder& d) const
	{
		if constexpr (Property == ascii::ctype::blank)
			d.skip(directives::lexeme | directives::eps).encode(opcode::match_blank);
		else if constexpr (Property == ascii::ctype::space)
			d.skip(directives::lexeme | directives::eps).encode(opcode::match_space);
		else
			d.match_set(make_rune_set(d.mode()));
	}

	[[nodiscard]] rune_set make_rune_set([[maybe_unused]] directives mode = directives::none) const
	{
		return ascii::ctype_rune_set(Property);
	}
};

template <unicode::ctype Property>
struct unicode_ctype_expression : terminal_encoder_expression_interface<unicode_ctype_expression<Property>>
{
	void evaluate(encoder& d) const
	{
		if constexpr (Property == unicode::ctype::blank)
			d.skip(directives::lexeme | directives::eps).encode_class(opcode::match_any_of, Property);
		else if constexpr (Property == unicode::ctype::space)
			d.skip(directives::lexeme | directives::eps).encode_class(opcode::match_any_of, Property);
		else
			d.match_class(opcode::match_any_of, Property);
	}
};

struct condition_test_combinator
{
	std::uint_least8_t imm8;
	constexpr explicit condition_test_combinator(bool value) noexcept : imm8{static_cast<std::uint_least8_t>(value ? 1 : 0)} {}

	struct condition_test_expression : terminal_encoder_expression_interface<condition_test_expression, match_traits::nullable>
	{
		std::string_view name;
		std::uint_least8_t imm8;
		constexpr condition_test_expression(std::string_view n, std::uint_least8_t i) noexcept : name{n}, imm8{i} {}
		void evaluate(encoder& d) const { d.encode(opcode::condition_test, name, imm8); }
	};

	[[nodiscard]] constexpr condition_test_expression operator()(std::string_view name) const noexcept { return condition_test_expression{name, imm8}; }
};

struct condition_block_combinator
{
	std::uint_least8_t imm8;
	constexpr explicit condition_block_combinator(bool value) noexcept : imm8{static_cast<std::uint_least8_t>(value ? 1 : 0)} {}

	template <class E1>
	struct condition_block_expression : unary_encoder_expression_interface<condition_block_expression<E1>, E1>
	{
		using base_type = unary_encoder_expression_interface<condition_block_expression<E1>, E1>;
		using base_type::base_type;
		std::string_view name;
		std::uint_least8_t imm8;
		constexpr condition_block_expression(E1 const& x1, std::string_view n, std::uint_least8_t i) noexcept : base_type{x1}, name{n}, imm8{i} {}

		void evaluate(encoder& d) const
		{
			d.encode(opcode::condition_push, name, imm8);
			this->e1.evaluate(d);
			d.encode(opcode::condition_pop);
		}
	};

	struct condition_block_group
	{
		std::string_view name;
		std::uint_least8_t imm8;
		constexpr condition_block_group(std::string_view n, std::uint_least8_t i) noexcept : name{n}, imm8{i} {}

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
		{
			return condition_block_expression<std::decay_t<decltype(make_expression(e))>>{make_expression(e), name, imm8};
		}
	};

	[[nodiscard]] constexpr condition_block_group operator()(std::string_view name) const noexcept { return condition_block_group{name, imm8}; }
};

struct symbol_exists_combinator
{
	std::uint_least8_t imm8;
	constexpr explicit symbol_exists_combinator(bool value) noexcept : imm8{static_cast<std::uint_least8_t>(value ? 1 : 0)} {}

	struct symbol_exists_expression : terminal_encoder_expression_interface<symbol_exists_expression, match_traits::nullable>
	{
		std::string_view name;
		std::uint_least8_t imm8;
		constexpr symbol_exists_expression(std::string_view n, std::uint_least8_t i) noexcept : name{n}, imm8{i} {}
		void evaluate(encoder& d) const { d.encode(opcode::symbol_exists, name, imm8); }
	};

	[[nodiscard]] constexpr symbol_exists_expression operator()(std::string_view name) const noexcept { return symbol_exists_expression{name, imm8}; }
};

struct symbol_match_combinator
{
	opcode mop;
	opcode mopcf;
	constexpr symbol_match_combinator(opcode op, opcode opcf) noexcept : mop{op}, mopcf{opcf} {}

	struct symbol_match_expression : terminal_encoder_expression_interface<symbol_match_expression>
	{
		opcode mop;
		opcode mopcf;
		std::string_view name;
		constexpr symbol_match_expression(opcode op, opcode opcf, std::string_view n) noexcept : mop{op}, mopcf{opcf}, name{n} {}
		void evaluate(encoder& d) const { d.skip().encode(((d.mode() & directives::caseless) != directives::none) ? mopcf : mop, name);}
	};

	[[nodiscard]] constexpr symbol_match_expression operator()(std::string_view name) const noexcept { return symbol_match_expression{mop, mopcf, name}; }
};

struct symbol_match_offset_combinator
{
	opcode mop;
	opcode mopcf;
	constexpr symbol_match_offset_combinator(opcode op, opcode opcf) noexcept : mop{op}, mopcf{opcf} {}

	struct symbol_match_offset_expression : terminal_encoder_expression_interface<symbol_match_offset_expression>
	{
		opcode mop;
		opcode mopcf;
		std::string_view name;
		std::uint_least8_t offset;
		constexpr symbol_match_offset_expression(opcode op, opcode opcf, std::string_view n, std::uint_least8_t o) noexcept : mop{op}, mopcf{opcf}, name{n}, offset{o} {}
		void evaluate(encoder& d) const { d.skip().encode(((d.mode() & directives::caseless) != directives::none) ? mopcf : mop, name, offset); }
	};

	[[nodiscard]] constexpr symbol_match_offset_expression operator()(std::string_view name, std::size_t offset = 0) const
	{
		return symbol_match_offset_expression{mop, mopcf, name, detail::checked_cast<std::uint_least8_t, resource_limit_error>(offset)};
	}
};

template <class E1> struct negative_lookahead_expression;
template <class E1> struct positive_lookahead_expression;
template <class E1, std::size_t NMin, std::size_t NMax> struct repetition_expression;
template <class E1, class E2> struct choice_expression;
template <class E1, class E2> struct sequence_expression;

template <class E>
inline constexpr bool is_expression_always_head_optimizable_v =
	std::is_same_v<E, char_expression> ||
	std::is_same_v<E, rune_expression> ||
	std::is_same_v<E, rune_range_expression> ||
	std::is_same_v<E, rune_set_expression> ||
	std::is_same_v<E, bracket_expression> ||
	std::is_same_v<E, match_any_expression> ||
	detail::is_template_non_type_instantiation_of_v<E, ascii_ctype_expression>;

template <class E>
inline constexpr bool is_expression_maybe_head_optimizable_v =
	detail::is_template_instantiation_of_v<E, positive_lookahead_expression> ||
	detail::is_template_instantiation_of_v<E, choice_expression> ||
	detail::is_template_instantiation_of_v<E, sequence_expression>;
	// TODO: callable, capture/attribute bindings, condition and symbol blocks

template <class E1>
struct negative_lookahead_expression : unary_encoder_expression_interface<negative_lookahead_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<negative_lookahead_expression<E1>, E1>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		auto const choice = d.encode(opcode::choice, 0, 1);
		d.dpsh(directives::none, directives::none);
		this->e1.evaluate(d);
		d.dpop(directives::none);
		d.encode(opcode::fail, 0, 2);
		d.jump_to_here(choice);
	}

	[[nodiscard]] constexpr match_traits matches() const noexcept { return (this->e1.matches() & ~match_traits::nofail) | match_traits::nullable; }
};

template <class E1>
struct positive_lookahead_expression : unary_encoder_expression_interface<positive_lookahead_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<positive_lookahead_expression<E1>, E1>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		auto const choice = d.encode(opcode::choice, 0, 1);
		d.dpsh(directives::none, directives::none);
		this->e1.evaluate(d);
		d.dpop(directives::none);
		d.encode(opcode::commit_back, 1, 0, 0);
		d.jump_to_here(choice);
		d.encode(opcode::fail, 0, 1);
	}

	[[nodiscard]] constexpr match_traits matches() const noexcept { return this->e1.matches() | match_traits::nullable; }
};

template <class E>
inline constexpr bool is_expression_always_repeat_optimizable_v =
	std::is_same_v<E, char_expression> ||
	std::is_same_v<E, rune_expression> ||
	std::is_same_v<E, rune_range_expression> ||
	std::is_same_v<E, rune_set_expression> ||
	std::is_same_v<E, bracket_expression> ||
	std::is_same_v<E, match_any_expression> ||
	detail::is_template_non_type_instantiation_of_v<E, ascii_ctype_expression>;

template <class E>
inline constexpr bool is_expression_maybe_repeat_optimizable_v =
	std::is_same_v<E, string_expression>;

template <class E>
inline constexpr bool is_expression_repeat_optimizable_v =
	is_expression_always_repeat_optimizable_v<unwrap_directive_expression_t<E>> ||
	is_expression_maybe_repeat_optimizable_v<unwrap_directive_expression_t<E>>;

inline constexpr std::size_t forever = (std::numeric_limits<std::size_t>::max)();
inline constexpr std::size_t max_repetitions = (forever != 0xffff) ? 0xffff : 0xfffe;

template <class E>
[[nodiscard]] constexpr bool repetition_encode_optimized([[maybe_unused]] E const& e, encoder& d, std::size_t nmin, std::size_t nmax)
{
	if constexpr (is_expression_always_repeat_optimizable_v<std::decay_t<E>>) {
		if (d.should_skip(directives::preskip, directives::lexeme | directives::noskip | directives::postskip))
			return false;
		d.commit_eps();
		if constexpr (std::is_same_v<std::decay_t<E>, match_any_expression>)
			d.encode_min_max(opcode::repeat_any, nmin, nmax);
		else if constexpr (std::is_same_v<std::decay_t<E>, ascii_ctype_expression<ascii::ctype::blank>>)
			d.encode_min_max(opcode::repeat_blank, nmin, nmax);
		else if constexpr (std::is_same_v<std::decay_t<E>, ascii_ctype_expression<ascii::ctype::space>>)
			d.encode_min_max(opcode::repeat_space, nmin, nmax);
		else if constexpr (std::is_same_v<std::decay_t<E>, char_expression> || std::is_same_v<std::decay_t<E>, rune_expression>)
			d.encode_unit_or_set(opcode::repeat_unit, opcode::repeat_set, e.c, nmin, nmax);
		else if constexpr (std::is_same_v<std::decay_t<E>, rune_range_expression> ||
							std::is_same_v<std::decay_t<E>, rune_set_expression> ||
							std::is_same_v<std::decay_t<E>, bracket_expression> ||
							detail::is_template_non_type_instantiation_of_v<std::decay_t<E>, ascii_ctype_expression>)
			d.encode_min_max(opcode::repeat_set, nmin, nmax, d.add_rune_set(e.make_rune_set(d.mode())));
		return true;
	} else if constexpr (std::is_same_v<std::decay_t<E>, string_expression>) {
		if (d.should_skip(directives::preskip, directives::lexeme | directives::noskip | directives::postskip))
			return false;
		auto const [rest, rune] = utf8::decode_rune(e.text.begin(), e.text.end());
		if (rest != e.text.end())
			return false;
		d.commit_eps().encode_unit_or_set(opcode::repeat_unit, opcode::repeat_set, rune, nmin, nmax);
		return true;
	} else if constexpr (detail::is_template_instantiation_of_v<std::decay_t<E>, directive_expression>) {
		d.dpsh(e.enable_mask, e.disable_mask);
		bool const result = repetition_encode_optimized(e.e1, d, nmin, nmax);
		d.ddrop();
		return result;
	} else {
		static_assert(detail::always_false_v<E>, "unsupported repetition expression");
	}
}

template <class E1, std::size_t NMin, std::size_t NMax>
struct repetition_expression : unary_encoder_expression_interface<repetition_expression<E1, NMin, NMax>, E1>
{
	static_assert((NMin > 0) && (NMin < NMax) && (NMax <= max_repetitions));
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, NMin, NMax>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, NMin, NMax))
				return;
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const start = d.encode(opcode::jump);
		auto const loop_body = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(directives::eps);
		d.encode(opcode::ret);
		d.jump_to_here(start);
		for (std::size_t i = 0; i < NMin; ++i)
			d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
		std::ptrdiff_t const loop_end = (3 * static_cast<std::ptrdiff_t>(NMax - NMin)) + d.here();
		for (std::size_t i = NMin; i < NMax; ++i) {
			d.encode(opcode::choice, (loop_end - d.here() - 1), 0, 0);
			d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
			auto const commit = d.encode(opcode::commit);
			d.jump_to_here(commit);
		}
	}
};

template <class E1, std::size_t NCount>
struct repetition_expression<E1, NCount, NCount> : unary_encoder_expression_interface<repetition_expression<E1, NCount, NCount>, E1>
{
	static_assert((NCount > 1) && (NCount <= max_repetitions));
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, NCount, NCount>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, NCount, NCount))
				return;
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const start = d.encode(opcode::jump);
		auto const loop_body = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(directives::eps);
		d.encode(opcode::ret);
		d.jump_to_here(start);
		for (std::size_t i = 0; i < NCount; ++i)
			d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
	}
};

template <class E1, std::size_t NMin>
struct repetition_expression<E1, NMin, forever> : unary_encoder_expression_interface<repetition_expression<E1, NMin, forever>, E1>
{
	static_assert((NMin > 1) && (NMin <= max_repetitions));
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, NMin, forever>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, NMin, forever))
				return;
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const start = d.encode(opcode::jump);
		auto const loop_body = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(NMin > 0 ? directives::eps : directives::none);
		d.encode(opcode::ret);
		d.jump_to_here(start);
		for (std::size_t i = 0; i < NMin; ++i)
			d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
		auto const choice = d.encode(opcode::choice);
		auto const loop = d.here();
		d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
		auto const commit = d.encode(opcode::commit_partial);
		d.jump_to_here(choice);
		d.jump_to_target(commit, loop);
	}
};

template <class E1, std::size_t NMax>
struct repetition_expression<E1, 0, NMax> : unary_encoder_expression_interface<repetition_expression<E1, 0, NMax>, E1>
{
	static_assert((NMax > 0) && (NMax <= max_repetitions));
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 0, NMax>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}
	[[nodiscard]] constexpr match_traits matches() const noexcept { return match_traits::nullable | match_traits::nofail; }

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, 0, NMax))
				return;
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const start = d.encode(opcode::jump);
		auto const loop_body = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(directives::none);
		d.encode(opcode::ret);
		d.jump_to_here(start);
		std::ptrdiff_t const loop_end = (3 * static_cast<std::ptrdiff_t>(NMax)) + d.here();
		for (std::size_t i = 0; i < NMax; ++i) {
			d.encode(opcode::choice, (loop_end - d.here() - 1), 0, 0);
			d.encode(opcode::call, (loop_body - d.here() - 1), 0, 0);
			auto const commit = d.encode(opcode::commit);
			d.jump_to_here(commit);
		}
	}
};

template <class E1>
struct repetition_expression<E1, 0, 0> : unary_encoder_expression_interface<repetition_expression<E1, 0, 0>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 0, 0>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}
	void evaluate(encoder& /*d*/) const {}
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return effect_traits::none; }
	[[nodiscard]] constexpr match_traits matches() const noexcept { return match_traits::nullable | match_traits::nofail; }
};

template <class E1>
struct repetition_expression<E1, 0, 1> : unary_encoder_expression_interface<repetition_expression<E1, 0, 1>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 0, 1>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}
	[[nodiscard]] constexpr match_traits matches() const noexcept { return match_traits::nullable | match_traits::nofail; }

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, 0, 1))
				return;
		auto const choice = d.encode(opcode::choice);
		d.dpsh(directives::none, directives::none);
		this->e1.evaluate(d);
		d.dpop(directives::eps);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		d.jump_to_here(commit);
	}
};

template <class E1>
struct repetition_expression<E1, 0, forever> : unary_encoder_expression_interface<repetition_expression<E1, 0, forever>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 0, forever>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}
	[[nodiscard]] constexpr match_traits matches() const noexcept { return match_traits::nullable | match_traits::nofail; }

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, 0, forever))
				return;
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const choice = d.encode(opcode::choice);
		auto const expression = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(directives::none);
		auto const commit = d.encode(opcode::commit_partial);
		d.jump_to_here(choice);
		d.jump_to_target(commit, expression);
	}
};

template <class E1>
struct repetition_expression<E1, 1, 1> : unary_encoder_expression_interface<repetition_expression<E1, 1, 1>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 1, 1>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}
	void evaluate(encoder& d) const { this->e1.evaluate(d); }
};

template <class E1>
struct repetition_expression<E1, 1, 2> : unary_encoder_expression_interface<repetition_expression<E1, 1, 2>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 1, 2>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, 1, 2))
				return;
		this->e1.evaluate(d);
		auto const choice = d.encode(opcode::choice);
		d.dpsh(directives::preskip, directives::postskip);
		this->e1.evaluate(d);
		d.dpop(directives::eps);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		d.jump_to_here(commit);
	}
};

template <class E1>
struct repetition_expression<E1, 1, forever> : unary_encoder_expression_interface<repetition_expression<E1, 1, forever>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1, 1, forever>, E1>;
	constexpr explicit repetition_expression(E1 const& e) : base_type{e} {}

	void evaluate(encoder& d) const
	{
		if constexpr (is_expression_repeat_optimizable_v<E1>)
			if (repetition_encode_optimized(this->e1, d, 1, forever))
				return;
		this->e1.evaluate(d);
		d.skip(directives::none, directives::lexeme | directives::noskip);
		auto const choice = d.encode(opcode::choice);
		auto const expression = d.here();
		d.dpsh(directives::postskip, directives::preskip);
		this->e1.evaluate(d);
		d.dpop(directives::none);
		auto const commit = d.encode(opcode::commit_partial);
		d.jump_to_here(choice);
		d.jump_to_target(commit, expression);
	}
};

template <std::size_t NMin, std::size_t NMax>
struct repetition_combinator
{
	static_assert((NMin <= NMax), "min count must be less than or equal to max count");
	static_assert((NMin <= max_repetitions), "min count must be less than or equal to max repetitions");
	static_assert(((NMax <= max_repetitions) || (NMax == forever)), "max count must be less than or equal to max repetitions or forever");

	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
	{
		return repetition_expression<std::decay_t<decltype(make_expression(e))>, NMin, NMax>{make_expression(e)};
	}
};

template <class E1, class E2>
struct choice_expression : binary_encoder_expression_interface<choice_expression<E1, E2>, E1, E2>
{
	using base_type = binary_encoder_expression_interface<choice_expression<E1, E2>, E1, E2>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		auto const choice = d.encode(opcode::choice);
		d.dpsh(directives::none, directives::none);
		this->e1.evaluate(d);
		d.dpop(directives::eps);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		d.dpsh(directives::none, directives::none);
		this->e2.evaluate(d);
		d.dpop(directives::eps);
		d.jump_to_here(commit);
	}

	[[nodiscard]] constexpr match_traits matches() const noexcept
	{
		auto const matches1 = this->e1.matches();
		if (matches1 == match_traits::all)
			return match_traits::all;
		return matches1 | this->e2.matches();
	}
};

template <class E1, class E2>
struct sequence_expression : binary_encoder_expression_interface<sequence_expression<E1, E2>, E1, E2>
{
	using base_type = binary_encoder_expression_interface<sequence_expression<E1, E2>, E1, E2>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		this->e1.evaluate(d);
		d.dpsh(directives::preskip, directives::postskip);
		this->e2.evaluate(d);
		d.dpop(directives::eps);
	}

	[[nodiscard]] constexpr match_traits matches() const noexcept
	{
		auto const matches1 = this->e1.matches();
		if (matches1 == match_traits::none)
			return match_traits::none;
		return matches1 & this->e2.matches();
	}
};

template <class Derived, class E1, class Operand>
struct attribute_action_expression : unary_encoder_expression_interface<Derived, E1>
{
	using base_type = unary_encoder_expression_interface<Derived, E1>;
	using base_type::base_type;
	Operand operand;
	template <class X1, class O> constexpr attribute_action_expression(X1&& x1, O&& o) : base_type{std::forward<X1>(x1)}, operand(std::forward<O>(o)) {}

	void evaluate(encoder& d) const
	{
		if constexpr (detail::is_template_instantiation_of_v<std::decay_t<E1>, callable_expression>) {
			if (!d.is_frame_empty()) {
				static_cast<Derived const&>(*this).do_prologue_inlined(d);
				d.call(this->e1.target, this->e1.prec);
				static_cast<Derived const&>(*this).do_epilogue_inlined(d);
				return;
			}
		}
		static_cast<Derived const&>(*this).do_prologue(d);
		this->e1.evaluate(d);
		static_cast<Derived const&>(*this).do_epilogue(d);
	}
};

template <class Derived, class E1, class Target>
struct attribute_bind_to_expression : attribute_action_expression<Derived, E1, Target*>
{
	using base_type = attribute_action_expression<Derived, E1, Target*>;
	using base_type::base_type;
	void evaluate(encoder& d) const { attribute_action_expression<Derived, E1, Target*>::evaluate(d); d.add_to_frame(this->operand); }
};

template <class E1, class Action>
struct action_expression : attribute_action_expression<action_expression<E1, Action>, E1, Action>
{
	using base_type = attribute_action_expression<action_expression<E1, Action>, E1, Action>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[a = this->operand](environment& envr) { a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
	constexpr void do_prologue_inlined(encoder& d) const { d.encode(opcode::attribute_push, d.get_frame_handle_index()); }
	constexpr void do_epilogue_inlined(encoder& d) const { d.encode(opcode::action, semantic_action{[f = d.get_frame_handle(), a = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
};

template <class E1, class Action>
struct capture_expression : attribute_action_expression<capture_expression<E1, Action>, E1, Action>
{
	using base_type = attribute_action_expression<capture_expression<E1, Action>, E1, Action>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[a = this->operand](environment& envr, syntax const& sx) { a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
	constexpr void do_prologue_inlined(encoder& d) const { d.encode(opcode::attribute_push, d.get_frame_handle_index()); d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue_inlined(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[f = d.get_frame_handle(), a = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects() | effect_traits::captures; }
};

template <class E1, class Target>
struct assign_to_expression : attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>
{
	using base_type = attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[t = this->operand](environment& envr) { *t = envr.pop_attribute<Target>(); }}); }
	constexpr void do_prologue_inlined(encoder& d) const { d.encode(opcode::attribute_push, d.get_frame_handle_index()); }
	constexpr void do_epilogue_inlined(encoder& d) const { d.encode(opcode::action, semantic_action{[f = d.get_frame_handle(), t = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); *t = envr.pop_attribute<Target>(); }}); }
};

template <class E1, class Target>
struct capture_to_expression : attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>
{
	using base_type = attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[t = this->operand](environment&, syntax const& sx) { *t = sx; }}); }
	constexpr void do_prologue_inlined(encoder& d) const { d.encode(opcode::attribute_push, d.get_frame_handle_index()); d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue_inlined(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[f = d.get_frame_handle(), t = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); *t = sx; }}); }
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects() | effect_traits::captures; }
};

template <class E1>
struct symbol_assign_expression : unary_encoder_expression_interface<symbol_assign_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<symbol_assign_expression<E1>, E1>;
	std::string_view name;
	template <class X1> constexpr symbol_assign_expression(X1&& x1, std::string_view n) : base_type{std::forward<X1>(x1)}, name{n} {}
	void evaluate(encoder& d) const { d.skip().encode(opcode::symbol_start, name); this->e1.evaluate(d); d.encode(opcode::symbol_end); }
	[[nodiscard]] constexpr effect_traits effects() const noexcept { return this->e1.effects() | effect_traits::captures; }
};

template <class E1>
struct symbol_block_expression : unary_encoder_expression_interface<symbol_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<symbol_block_expression<E1>, E1>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		d.skip().encode(opcode::symbol_push);
		this->e1.evaluate(d);
		d.encode(opcode::symbol_pop);
	}
};

template <class E1>
struct local_block_expression : unary_encoder_expression_interface<local_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<local_block_expression<E1>, E1>;
	using base_type::base_type;

	void evaluate(encoder& d) const
	{
		d.skip().encode(opcode::symbol_push, 0, 2);
		this->e1.evaluate(d);
		d.encode(opcode::symbol_pop);
	}
};

template <class E1>
struct local_to_block_expression : unary_encoder_expression_interface<local_to_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<local_to_block_expression<E1>, E1>;
	using base_type::base_type;
	std::string_view name;
	template <class X1> constexpr local_to_block_expression(X1&& x1, std::string_view n) noexcept : base_type{std::forward<X1>(x1)}, name{n} {}

	void evaluate(encoder& d) const
	{
		d.skip().encode(opcode::symbol_push, name, 1);
		this->e1.evaluate(d);
		d.encode(opcode::symbol_pop);
	}
};

template <class X1> negative_lookahead_expression(X1&&) -> negative_lookahead_expression<std::decay_t<X1>>;
template <class X1> positive_lookahead_expression(X1&&) -> positive_lookahead_expression<std::decay_t<X1>>;
template <class X1, class X2> choice_expression(X1&&, X2&&) -> choice_expression<std::decay_t<X1>, std::decay_t<X2>>;
template <class X1, class X2> sequence_expression(X1&&, X2&&) -> sequence_expression<std::decay_t<X1>, std::decay_t<X2>>;
template <class X1, class Action> action_expression(X1&&, Action&&) -> action_expression<std::decay_t<X1>, std::decay_t<Action>>;
template <class X1, class Action> capture_expression(X1&&, Action&&) -> capture_expression<std::decay_t<X1>, std::decay_t<Action>>;
template <class X1, class Target> assign_to_expression(X1&&, Target*) -> assign_to_expression<std::decay_t<X1>, Target>;
template <class X1, class Target> capture_to_expression(X1&&, Target*) -> capture_to_expression<std::decay_t<X1>, Target>;
template <class X1> symbol_assign_expression(X1&&, std::string_view) -> symbol_assign_expression<std::decay_t<X1>>;
template <class X1> symbol_block_expression(X1&&) -> symbol_block_expression<std::decay_t<X1>>;
template <class X1> local_block_expression(X1&&) -> local_block_expression<std::decay_t<X1>>;
template <class X1> local_to_block_expression(X1&&, std::string_view) -> local_to_block_expression<std::decay_t<X1>>;

template <class Container, class... As, std::size_t... Is>
[[nodiscard]] Container build_container(environment& envr, std::index_sequence<Is...> const& seq)
{
	Container container;
	if (auto attributes = envr.finish_attribute_collection(seq.size()); !attributes.empty()) {
		if constexpr (detail::container_has_reserve_v<Container>)
			container.reserve(attributes.size() / seq.size());
		if constexpr (detail::container_has_emplace_back_v<Container, As...>) {
			for ( ; !attributes.empty(); attributes.consume_front(seq.size()))
				(void)container.emplace_back(attributes.template read_front<As, Is>()...);
		} else if constexpr (detail::container_has_emplace_after_v<Container, As...>) {
			for (auto last = container.cbegin(); !attributes.empty(); attributes.consume_front(seq.size()))
				last = container.emplace_after(last, attributes.template read_front<As, Is>()...);
		} else if constexpr (detail::container_has_emplace_v<Container, As...>) {
			for ( ; !attributes.empty(); attributes.consume_back(seq.size()))
				(void)container.emplace(attributes.template read_back<As, Is, sizeof...(Is)>()...);
		} else {
			static_assert(detail::always_false_v<Container>, "container type does not support attribute collection");
		}
	}
	return container;
}

template <class E1, class Container, class... ElementArgs>
struct collect_expression : unary_encoder_expression_interface<collect_expression<E1, Container, ElementArgs...>, E1>
{
	static_assert(sizeof...(ElementArgs) > 0, "no element types provided to collect expression" );
	static_assert(std::is_constructible_v<typename Container::value_type, std::decay_t<ElementArgs>...>, "synthesized element type does not support the provided constructor argument types" );
	using base_type = unary_encoder_expression_interface<collect_expression<E1, Container, ElementArgs...>, E1>;
	template <class X1, class C, class... As> constexpr collect_expression(X1&& x1, std::in_place_type_t<C> /*c*/, std::in_place_type_t<As>... /*a*/) noexcept : base_type{std::forward<X1>(x1)} {}

	void evaluate(encoder& d) const
	{
		d.encode(opcode::action, semantic_action{[](environment& envr) { envr.start_attribute_collection(); }});
		this->e1.evaluate(d);
		d.encode(opcode::action, semantic_action{[](environment& envr) { envr.push_attribute(lug::build_container<Container, ElementArgs...>(envr, std::index_sequence_for<ElementArgs...>{})); }});
	}
};

template <class X1, class C, class... As> collect_expression(X1&&, std::in_place_type_t<C>, std::in_place_type_t<As>...) -> collect_expression<std::decay_t<X1>, C, As...>;

template <class Container, class... ElementArgs>
struct collect_combinator
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
	{
		if constexpr (sizeof...(ElementArgs) == 0)
			return collect_expression{make_expression(e), std::in_place_type<Container>, std::in_place_type<typename Container::value_type>};
		else
			return collect_expression{make_expression(e), std::in_place_type<Container>, std::in_place_type<ElementArgs>...};
	}
};

template <class E1, class Factory, class T, class... Args>
struct synthesize_expression : unary_encoder_expression_interface<synthesize_expression<E1, Factory, T, Args...>, E1>
{
	static_assert(sizeof...(Args) > 0, "no arguments types provided to synthesize expression" );
	static_assert(std::is_constructible_v<T, std::decay_t<Args>...>, "synthesized type T does not support the provided constructor arguments" );
	using base_type = unary_encoder_expression_interface<synthesize_expression<E1, Factory, T, Args...>, E1>;
	template <class X1, class F, class U, class... As> constexpr synthesize_expression(X1&& x1, std::in_place_type_t<F> /*f*/, std::in_place_type_t<U> /*u*/, std::in_place_type_t<As>... /*a*/) noexcept : base_type{std::forward<X1>(x1)} {}

	void evaluate(encoder& d) const
	{
		this->e1.evaluate(d);
		d.encode(opcode::action, semantic_action{[](environment& envr) { synthesize_expression::build<Args...>(envr, std::index_sequence_for<Args...>{}); }});
	}

	template <class... As, std::size_t... Is>
	static void build(environment& envr, std::index_sequence<Is...> const& seq)
	{
		envr.push_attribute([&]{
			auto attributes = envr.tail_attribute_collection(seq.size());
			return Factory{}(std::in_place_type<T>, attributes.template read_front<As, Is>()...);
		}());
	}
};

template <class X1, class F, class T, class... As> synthesize_expression(X1&&, std::in_place_type_t<F>, std::in_place_type_t<T>, std::in_place_type_t<As>...) -> synthesize_expression<std::decay_t<X1>, F, T, As...>;

template <class Factory, class T, class... Args>
struct synthesize_combinator
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
	{
		if constexpr (sizeof...(Args) == 0)
			return synthesize_expression{make_expression(e), std::in_place_type<Factory>, std::in_place_type<T>, std::in_place_type<T>};
		else
			return synthesize_expression{make_expression(e), std::in_place_type<Factory>, std::in_place_type<T>, std::in_place_type<Args>...};
	}
};

template <class E1, class Factory, class T, class Container, class... ElementArgs>
struct synthesize_collect_expression : unary_encoder_expression_interface<synthesize_collect_expression<E1, Factory, T, Container, ElementArgs...>, E1>
{
	static_assert(sizeof...(ElementArgs) > 0, "no element types provided to collect expression");
	static_assert(std::is_constructible_v<typename Container::value_type, std::decay_t<ElementArgs>...>, "synthesized element type does not support the provided constructor argument types");
	static_assert(std::is_constructible_v<T, Container>, "synthesized type T not constructible from Container type argument");
	using base_type = unary_encoder_expression_interface<synthesize_collect_expression<E1, Factory, T, Container, ElementArgs...>, E1>;
	template <class X1, class F, class V, class C, class... As> constexpr synthesize_collect_expression(X1&& x1, std::in_place_type_t<F> /*f*/, std::in_place_type_t<V> /*v*/, std::in_place_type_t<C> /*c*/, std::in_place_type_t<As>... /*a*/) noexcept : base_type{std::forward<X1>(x1)} {}

	void evaluate(encoder& d) const
	{
		d.encode(opcode::action, semantic_action{[](environment& envr) { envr.start_attribute_collection(); }});
		this->e1.evaluate(d);
		d.encode(opcode::action, semantic_action{[](environment& envr) { envr.push_attribute(Factory{}(std::in_place_type<T>, lug::build_container<Container, ElementArgs...>(envr, std::index_sequence_for<ElementArgs...>{}))); }});
	}
};

template <class X1, class F, class V, class C, class... As> synthesize_collect_expression(X1&&, std::in_place_type_t<F>, std::in_place_type_t<V>, std::in_place_type_t<C>, std::in_place_type_t<As>...) -> synthesize_collect_expression<std::decay_t<X1>, F, V, C, As...>;

template <class Factory, class T, class Container, class... ElementArgs>
struct synthesize_collect_combinator
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
	{
		if constexpr (sizeof...(ElementArgs) == 0)
			return synthesize_collect_expression{make_expression(e), std::in_place_type<Factory>, std::in_place_type<T>, std::in_place_type<Container>, std::in_place_type<typename Container::value_type>};
		else
			return synthesize_collect_expression{make_expression(e), std::in_place_type<Factory>, std::in_place_type<T>, std::in_place_type<Container>, std::in_place_type<ElementArgs>...};
	}
};

struct synthesize_factory
{
	template <class T, class... Args>
	[[nodiscard]] constexpr T operator()(std::in_place_type_t<T> /*t*/, Args&&... args) const
	{
		return T(std::forward<Args>(args)...);
	}
};

struct synthesize_shared_factory
{
	template <class T, class... Args>
	[[nodiscard]] std::shared_ptr<T> operator()(std::in_place_type_t<T> /*t*/, Args&&... args) const
	{
		return std::shared_ptr<T>(std::forward<Args>(args)...);
	}
};

struct synthesize_unique_factory
{
	template <class T, class... Args>
	[[nodiscard]] std::unique_ptr<T> operator()(std::in_place_type_t<T> /*t*/, Args&&... args) const
	{
		return std::make_unique<T>(std::forward<Args>(args)...);
	}
};

namespace dsl {

using lug::environment; using lug::grammar; using lug::rule; using lug::start; using lug::forever; using lug::max_repetitions;
using lug::error_context; using lug::error_response; using lug::recover_with; using lug::failure;
using lug::syntax; using lug::syntax_position; using lug::syntax_range; using lug::rune_set; using lug::rune_set_builder;
inline constexpr directive_modifier<directives::none, directives::caseless, directives::eps> cased{};
inline constexpr directive_modifier<directives::caseless, directives::none, directives::eps> caseless{};
inline constexpr directive_modifier<directives::lexeme, directives::noskip, directives::eps> lexeme{};
inline constexpr directive_modifier<directives::lexeme | directives::noskip, directives::none, directives::eps> noskip{};
inline constexpr directive_modifier<directives::none, directives::lexeme | directives::noskip, directives::eps> skip{};
inline constexpr accept_cut_expression accept{lug::registers::ignore_errors_flag}; inline constexpr accept_cut_expression cut{lug::registers::inhibited_flag};
inline constexpr eoi_expression eoi{}; inline constexpr eol_expression eol{}; inline constexpr eps_expression eps{};
inline constexpr match_any_expression any{}; inline constexpr match_class_combinator all{opcode::match_all_of}; inline constexpr match_class_combinator none{opcode::match_none_of};
inline constexpr condition_test_combinator when{true}; inline constexpr condition_test_combinator unless{false};
inline constexpr condition_block_combinator on{true}; inline constexpr condition_block_combinator off{false};
inline constexpr symbol_exists_combinator exists{true}; inline constexpr symbol_exists_combinator missing{false};
inline constexpr symbol_match_offset_combinator match_front{opcode::symbol_head, opcode::symbol_head_cf};
inline constexpr symbol_match_offset_combinator match_back{opcode::symbol_tail, opcode::symbol_tail_cf};
inline constexpr symbol_match_combinator match_all{opcode::symbol_all, opcode::symbol_all_cf};
inline constexpr symbol_match_combinator match_any{opcode::symbol_any, opcode::symbol_any_cf};
inline constexpr symbol_match_combinator match{opcode::symbol_tail, opcode::symbol_tail_cf};
template <std::size_t NMin, std::size_t NMax> inline constexpr repetition_combinator<NMin, NMax> repeat{};
template <std::size_t NMin> inline constexpr repetition_combinator<NMin, forever> at_least{};
template <std::size_t NMax> inline constexpr repetition_combinator<0, NMax> at_most{};
template <std::size_t N> inline constexpr repetition_combinator<N, N> exactly{};
template <class Container, class... ElementArgs> inline constexpr collect_combinator<Container, ElementArgs...> collect{};
template <class T, class... Args> inline constexpr synthesize_combinator<synthesize_factory, T, Args...> synthesize{};
template <class T, class... Args> inline constexpr synthesize_combinator<synthesize_shared_factory, T, Args...> synthesize_shared{};
template <class T, class... Args> inline constexpr synthesize_combinator<synthesize_unique_factory, T, Args...> synthesize_unique{};
template <class T, class Container, class... ElementArgs> inline constexpr synthesize_collect_combinator<synthesize_factory, T, Container, ElementArgs...> synthesize_collect{};
template <class T, class Container, class... ElementArgs> inline constexpr synthesize_collect_combinator<synthesize_shared_factory, T, Container, ElementArgs...> synthesize_collect_shared{};
template <class T, class Container, class... ElementArgs> inline constexpr synthesize_collect_combinator<synthesize_unique_factory, T, Container, ElementArgs...> synthesize_collect_unique{};

inline namespace ascii {

using lug::ascii::ctype;
inline constexpr ascii_ctype_expression<ctype::alpha> alpha{};
inline constexpr ascii_ctype_expression<ctype::alnum> alnum{};
inline constexpr ascii_ctype_expression<ctype::lower> lower{};
inline constexpr ascii_ctype_expression<ctype::upper> upper{};
inline constexpr ascii_ctype_expression<ctype::digit> digit{};
inline constexpr ascii_ctype_expression<ctype::xdigit> xdigit{};
inline constexpr ascii_ctype_expression<ctype::space> space{};
inline constexpr ascii_ctype_expression<ctype::blank> blank{};
inline constexpr ascii_ctype_expression<ctype::punct> punct{};
inline constexpr ascii_ctype_expression<ctype::graph> graph{};
inline constexpr ascii_ctype_expression<ctype::print> print{};
inline constexpr ascii_ctype_expression<ctype::cntrl> cntrl{};
inline constexpr ascii_ctype_expression<ctype::word> word{};

} // namespace ascii

namespace unicode {

using lug::unicode::ctype;
using lug::unicode::ptype;
using lug::unicode::gctype;
using lug::unicode::sctype;
using lug::unicode::blktype;
using lug::unicode::agetype;
using lug::unicode::eawtype;
inline constexpr unicode_ctype_expression<ctype::alpha> alpha{};
inline constexpr unicode_ctype_expression<ctype::alnum> alnum{};
inline constexpr unicode_ctype_expression<ctype::lower> lower{};
inline constexpr unicode_ctype_expression<ctype::upper> upper{};
inline constexpr unicode_ctype_expression<ctype::digit> digit{};
inline constexpr unicode_ctype_expression<ctype::xdigit> xdigit{};
inline constexpr unicode_ctype_expression<ctype::space> space{};
inline constexpr unicode_ctype_expression<ctype::blank> blank{};
inline constexpr unicode_ctype_expression<ctype::punct> punct{};
inline constexpr unicode_ctype_expression<ctype::graph> graph{};
inline constexpr unicode_ctype_expression<ctype::print> print{};
inline constexpr unicode_ctype_expression<ctype::cntrl> cntrl{};
inline constexpr unicode_ctype_expression<ctype::word> word{};

} // namespace unicode

inline constexpr struct
{
	[[nodiscard]] bracket_expression operator()(std::string_view s) const { return bracket_expression{s}; }
	[[nodiscard]] bracket_expression operator()(char const* s, std::size_t n) const { return bracket_expression{std::string_view{s, n}}; }
}
bkt{};

inline constexpr struct
{
	[[nodiscard]] constexpr char_expression operator()(char c) const noexcept { return char_expression{c}; }
	[[nodiscard]] constexpr rune_expression operator()(char32_t c) const noexcept { return rune_expression{c}; }
	[[nodiscard]] constexpr rune_range_expression operator()(char32_t start, char32_t end) const noexcept { return rune_range_expression{start, end}; }
}
chr{};

inline constexpr struct
{
	[[nodiscard]] constexpr string_expression operator()(std::string_view s) const noexcept { return string_expression{s}; }
	[[nodiscard]] constexpr string_expression operator()(char const* s, std::size_t n) const noexcept { return string_expression{std::string_view{s, n}}; }
}
str{};

inline namespace operators {

[[nodiscard]] constexpr auto operator ""_cx(char c) { return chr(c); }
[[nodiscard]] constexpr auto operator ""_cx(char32_t c) { return chr(c); }
[[nodiscard]] constexpr auto operator ""_sx(char const* s, std::size_t n) { return string_expression{std::string_view{s, n}}; }
[[nodiscard]] constexpr auto operator ""_bx(char const* s, std::size_t n) { return bracket_expression{std::string_view{s, n}}; }
[[nodiscard]] constexpr auto operator ""_icx(char c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_icx(char32_t c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_isx(char const* s, std::size_t n) { return caseless[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_ibx(char const* s, std::size_t n) { return caseless[bracket_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_scx(char c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_scx(char32_t c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_ssx(char const* s, std::size_t n) { return cased[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_sbx(char const* s, std::size_t n) { return cased[bracket_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_fail(char const* s, std::size_t n) { return failure{std::string_view{s, n}}; }

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
[[nodiscard]] constexpr auto operator|(E1 const& e1, E2 const& e2)
{
	if constexpr (detail::is_template_instantiation_of_v<E1, choice_expression>)
		return choice_expression{e1.e1, e1.e2 | e2};
	else
		return choice_expression{make_expression(e1), make_expression(e2)};
}

template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>>
[[nodiscard]] constexpr auto operator>(E1 const& e1, E2 const& e2)
{
	if constexpr (detail::is_template_instantiation_of_v<E1, sequence_expression>)
		return sequence_expression{e1.e1, e1.e2 > e2};
	else
		return sequence_expression{make_expression(e1), make_expression(e2)};
}

template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator!(E const& e) { return negative_lookahead_expression{make_expression(e)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator&(E const& e) { return positive_lookahead_expression{make_expression(e)}; } // NOLINT(google-runtime-operator)
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator*(E const& e) { return repetition_expression<std::decay_t<decltype(make_expression(e))>, 0, forever>{make_expression(e)}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator>>(E1 const& e1, E2 const& e2) { return e1 > *(e2 > e1); }
template <class T, class E, class = std::enable_if_t<is_attribute_frame_persistable_v<T> && is_expression_v<E>>> [[nodiscard]] constexpr auto operator%(T& target, E const& e) { return assign_to_expression{make_expression(e), std::addressof(target)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator^(E const& e, error_response r) { return e > recover_response_expression{r}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator+(E const& e) { return repetition_expression<std::decay_t<decltype(make_expression(e))>, 1, forever>{make_expression(e)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator~(E const& e) { return repetition_expression<std::decay_t<decltype(make_expression(e))>, 0, 1>{make_expression(e)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator--(E const& e) { return cut > e; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator--(E const& e, int) { return e > cut; }

template <class E, class A, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto operator<(E const& e, A&& a)
{
	if constexpr (detail::is_invocable_r_exact_v<void, A, detail::dynamic_cast_if_base_of<environment&>, syntax>)
		return capture_expression{make_expression(e), std::forward<A>(a)};
	else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<environment&>, syntax>)
		return capture_expression{make_expression(e), [aa = std::forward<A>(a)](environment& envr, syntax const& sx) { envr.push_attribute(aa(detail::dynamic_cast_if_base_of<environment&>{envr}, sx)); }};
	else if constexpr (detail::is_invocable_r_exact_v<void, A, syntax>)
		return capture_expression{make_expression(e), [aa = std::forward<A>(a)](environment&, syntax const& sx) { aa(sx); }};
	else if constexpr (std::is_invocable_v<A, syntax>)
		return capture_expression{make_expression(e), [aa = std::forward<A>(a)](environment& envr, syntax const& sx) { envr.push_attribute(aa(sx)); }};
	else if constexpr (detail::is_invocable_r_exact_v<void, A, detail::dynamic_cast_if_base_of<environment&>>)
		return action_expression{make_expression(e), std::forward<A>(a)};
	else if constexpr (std::is_invocable_v<A, detail::dynamic_cast_if_base_of<environment&>>)
		return action_expression{make_expression(e), [aa = std::forward<A>(a)](environment& envr) { envr.push_attribute(aa(detail::dynamic_cast_if_base_of<environment&>{envr})); }};
	else if constexpr (detail::is_invocable_r_exact_v<void, A>)
		return action_expression{make_expression(e), [aa = std::forward<A>(a)](environment&) { aa(); }};
	else if constexpr (std::is_invocable_v<A>)
		return action_expression{make_expression(e), [aa = std::forward<A>(a)](environment& envr) { envr.push_attribute(aa()); }};
	else
		static_assert(detail::always_false_v<A>, "invalid action type");
}

} // namespace operators

inline constexpr struct
{
	template <class Target>
	struct capture_to
	{
		Target* target;
		template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const noexcept { return capture_to_expression{make_expression(e), target}; }
	};
	template <class Action>
	struct capture_with
	{
		Action action;
		template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const noexcept { return e < action; }
	};
	template <class Target, class = std::enable_if_t<is_capture_target_v<Target>>> [[nodiscard]] constexpr capture_to<Target> operator()(Target& t) const noexcept { return capture_to<Target>{std::addressof(t)}; }
	template <class Action, class = std::enable_if_t<is_capture_action_v<Action>>> [[nodiscard]] constexpr capture_with<std::decay_t<Action>> operator()(Action&& a) const noexcept { return capture_with<std::decay_t<Action>>{std::forward<Action>(a)}; }
}
capture{};

inline constexpr struct
{
	struct assign_to
	{
		std::string_view name;
		template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const noexcept { return symbol_assign_expression{make_expression(e), name}; }
	};
	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept { return assign_to{name}; }
}
symbol{};

inline constexpr struct
{
	template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const noexcept { return symbol_block_expression{make_expression(e)}; }
}
block{};

inline constexpr struct
{
	struct local_to
	{
		std::string_view name;
		template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const noexcept { return local_to_block_expression{make_expression(e), name}; }
	};
	[[nodiscard]] constexpr auto operator()(std::string_view name) const noexcept { return local_to{name}; }
	template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator[](E const& e) const { return local_block_expression{make_expression(e)}; }
}
local{};

inline constexpr struct
{
	[[nodiscard]] constexpr auto operator()(std::string_view label) const noexcept { return raise_expression{failure{label}}; }
	template <class Recovery> [[nodiscard]] constexpr auto operator()(failure<Recovery> const& reason) const noexcept { return raise_expression{reason}; }
	template <class Recovery, class = std::enable_if_t<is_recovery_expression_v<Recovery>>> [[nodiscard]] constexpr auto operator()(std::string_view label, Recovery&& recovery) const noexcept { return raise_expression{failure{label, std::forward<Recovery>(recovery)}}; }
}
raise{};

template <error_response Response = error_response::resume, class Pattern, class = std::enable_if_t<is_expression_v<Pattern>>>
[[nodiscard]] constexpr auto sync(Pattern const& pattern)
{
	return noskip[*(!pattern > any) ^ Response];
}

template <error_response Response = error_response::resume, class Pattern, class DefaultValue, class = std::enable_if_t<is_expression_v<Pattern>>>
[[nodiscard]] constexpr auto sync_with_value(Pattern const& pattern, DefaultValue&& default_value)
{
	if constexpr (std::is_invocable_v<std::add_const_t<std::decay_t<DefaultValue>>>)
		return noskip[*(!pattern > any) < std::forward<DefaultValue>(default_value) ^ Response];
	else
		return noskip[*(!pattern > any) < [value = std::forward<DefaultValue>(default_value)] { return value; } ^ Response];
}

template <error_response Response = error_response::resume, class DefaultValue>
[[nodiscard]] constexpr auto with_value(DefaultValue&& default_value)
{
	if constexpr (std::is_invocable_v<std::add_const_t<std::decay_t<DefaultValue>>>)
		return noskip[eps < std::forward<DefaultValue>(default_value) ^ Response];
	else
		return noskip[eps < [value = std::forward<DefaultValue>(default_value)] { return value; } ^ Response];
}

template <error_response Response>
[[nodiscard]] constexpr auto with_response()
{
	return noskip[eps ^ Response];
}

} // namespace dsl

[[nodiscard]] inline grammar start(rule const& start_rule, rule const& skip_rule)
{
	program grprogram;
	program_callees grcallees;
	encoder grencoder{grprogram, grcallees, directives::eps | directives::preskip};
	grencoder.skip(start_rule.program_.entry_mode, directives::noskip);
	std::vector<std::tuple<std::vector<std::pair<rule const*, bool>>, program const*, opcode>> unprocessed{
		{std::vector<std::pair<rule const*, bool>>{{&skip_rule, false}}, &skip_rule.program_, opcode::ret},
		{std::vector<std::pair<rule const*, bool>>{{&start_rule, false}}, &start_rule.program_, opcode::jump}};
	std::vector<std::pair<program const*, std::ptrdiff_t>> calls;
	std::unordered_map<program const*, std::ptrdiff_t> addresses;
	std::unordered_map<program const*, std::ptrdiff_t> epilogue_addresses;
	std::unordered_set<program const*> left_recursive;
	do {
		auto const&& [callstack, subprogram, epilogue_op] = detail::pop_back(unprocessed);
		if (auto const address = grencoder.here(); addresses.emplace(subprogram, address).second) {
			grencoder.append(*subprogram);
			epilogue_addresses.emplace(subprogram, grencoder.encode(epilogue_op));
			if (auto const top_rule = callstack.back().first; top_rule) {
				for (auto&& [callee_rule, callee_program, instr_offset, callee_mode] : top_rule->callees_) {
					calls.emplace_back(callee_program, address + instr_offset);
					if ((callee_rule != nullptr) && ((callee_mode & directives::eps) != directives::none) &&
							detail::escaping_find_if(callstack.crbegin(), callstack.crend(), [callee = callee_rule](auto const& caller) {
								if (caller.first == callee)
									return 1;
								return (caller.second ? 0 : -1);
							}) != callstack.crend()) {
						left_recursive.insert(callee_program);
					} else if (callee_rule != &skip_rule) {
						auto callee_callstack = callstack;
						callee_callstack.emplace_back(callee_rule, (callee_mode & directives::eps) != directives::none);
						unprocessed.emplace_back(std::move(callee_callstack), callee_program, opcode::ret);
					}
				}
			}
		}
	} while (!unprocessed.empty());
	std::ptrdiff_t const grammar_end_addr = grencoder.here();
	if (std::ptrdiff_t const start_epilogue_addr = epilogue_addresses[&start_rule.program_]; start_epilogue_addr >= 0)
		grencoder.jump_to_target(start_epilogue_addr, grammar_end_addr);
	std::ptrdiff_t const skip_prologue_addr = addresses[&skip_rule.program_];
	std::ptrdiff_t const skip_epilogue_addr = epilogue_addresses[&skip_rule.program_];
	if (std::ptrdiff_t const skip_subprogram_length = skip_epilogue_addr - skip_prologue_addr; skip_subprogram_length > 1) {
		for (std::ptrdiff_t instr_addr = 0; instr_addr < grammar_end_addr; ++instr_addr) {
			if (auto& instr = grencoder.instruction_at(instr_addr); instr.op == opcode::skip_space) {
				instr = instruction{opcode::call, 0, 0, 0};
				calls.emplace_back(&skip_rule.program_, instr_addr);
			}
		}
	} else {
		instruction const skip_instr = [&]{
			if (skip_subprogram_length == 1)
				return grencoder.instruction_at(skip_prologue_addr);
			return instruction{opcode::jump, 0, 0, 0};
		}();
		for (std::ptrdiff_t instr_addr = 0; instr_addr < grammar_end_addr; ++instr_addr)
			if (auto& instr = grencoder.instruction_at(instr_addr); instr.op == opcode::skip_space)
				instr = skip_instr;
	}
	for (auto [subprogram, instr_addr] : calls) {
		if (auto& instr = grencoder.instruction_at(instr_addr); (instr.op == opcode::call) || (instr.op == opcode::recover_push)) {
			instr.offset32 = detail::checked_cast<std::int_least32_t, program_limit_error>(instr.offset32 + addresses[subprogram] - (instr_addr + 1));
			if (instr.op == opcode::call) {
				instr.immediate16 = ((left_recursive.count(subprogram) != 0) ? (std::max)(instr.immediate16, std::uint_least16_t{1}) : std::uint_least16_t{0});
				if ((instr.immediate16 == 0) && (grencoder.instruction_at(instr_addr + 1).op == opcode::ret))
					instr.op = opcode::jump;
			}
		}
	}
	grprogram.data.emplace_back('\0');
	return grammar{std::move(grprogram)};
}

[[nodiscard]] inline grammar start(rule const& start_rule)
{
	return start(start_rule, rule{dsl::noskip[dsl::operator*(dsl::space)]});
}

enum class source_options : std::uint_least8_t { none = 0, interactive = 1 };
template <> inline constexpr bool is_flag_enum_v<source_options> = true;

namespace detail {

template <typename T, typename = void> struct input_source_enqueue_drains : std::false_type {};
template <typename T> struct input_source_enqueue_drains<T, std::enable_if_t<T::enqueue_drains::value>> : std::true_type {};
template <typename T, typename = void> struct input_source_has_options : std::false_type {};
template <typename T> struct input_source_has_options<T, std::enable_if_t<std::is_same_v<source_options, decltype(std::declval<T const&>().options())>>> : std::true_type {};
template <typename T, typename = void> struct input_source_has_fill_buffer : std::false_type {};
template <typename T> struct input_source_has_fill_buffer<T, std::enable_if_t<std::is_same_v<bool, decltype(std::declval<T&>().fill_buffer(std::declval<std::size_t>()))>>> : std::true_type {};
template <typename T, class It, typename = void> struct input_source_has_enqueue : std::false_type {};
template <typename T, class It> struct input_source_has_enqueue<T, It, std::void_t<decltype(std::declval<T>().enqueue(std::declval<It>(), std::declval<It>()))>> : std::true_type {};
template <typename T, class InputFunc, typename = void> struct input_source_has_push_source : std::false_type {};
template <typename T, class InputFunc> struct input_source_has_push_source<T, InputFunc, std::void_t<decltype(std::declval<T>().push_source(std::declval<InputFunc>(), std::declval<source_options>()))>> : std::true_type {};

} // namespace detail

class multi_input_source
{
	std::string buffer_;
	std::vector<std::pair<std::function<bool(std::back_insert_iterator<std::string>, source_options)>, source_options>> sources_;
	bool reading_{false};

public:
	[[nodiscard]] LUG_ALWAYS_INLINE std::string_view buffer() const noexcept { return buffer_; }
	[[nodiscard]] source_options options() const noexcept { return !sources_.empty() ? sources_.back().second : source_options::none; }
	void drain_buffer(std::size_t sr) { buffer_.erase(0, sr); }

	[[nodiscard]] bool fill_buffer(std::size_t fill_required)
	{
		if (sources_.empty())
			return false;
		detail::reentrancy_sentinel<reenterant_read_error> const guard{reading_};
		std::size_t const required_size = buffer_.size() + fill_required;
		while (!sources_.empty() && (buffer_.size() < required_size))
			if (auto const& [func, opt] = sources_.back(); !func(std::back_inserter(buffer_), opt))
				sources_.pop_back();
		return buffer_.size() >= required_size;
	}

	template <class InputIt, class = detail::enable_if_char_input_iterator_t<InputIt>>
	void enqueue(InputIt first, InputIt last)
	{
		buffer_.insert(buffer_.end(), first, last);
	}

	template <class InputFunc, class = std::enable_if_t<
			std::is_invocable_r_v<bool, InputFunc, std::back_insert_iterator<std::string>>
			|| std::is_invocable_r_v<bool, InputFunc, std::back_insert_iterator<std::string>, source_options>>>
	void push_source(InputFunc&& func, source_options opt = source_options::none)
	{
		if LUG_UNLIKELY(reading_)
			throw_exception<reenterant_read_error>();
		if constexpr (std::is_invocable_r_v<bool, InputFunc, std::back_insert_iterator<std::string>, source_options>)
			sources_.emplace_back(std::forward<InputFunc>(func), opt);
		else
			sources_.emplace_back([srcfn = std::forward<InputFunc>(func)](std::back_insert_iterator<std::string> out, source_options /*opt*/) -> bool { return srcfn(out); }, opt);
	}
};

class string_input_source
{
	std::string buffer_;
public:
	[[nodiscard]] LUG_ALWAYS_INLINE std::string_view buffer() const noexcept { return buffer_; }
	void drain_buffer(std::size_t sr) { buffer_.erase(0, sr); }
	template <class It, class = detail::enable_if_char_input_iterator_t<It>> void enqueue(It first, It last) { buffer_.insert(buffer_.end(), first, last); }
};

class string_view_input_source
{
	std::string_view buffer_;
public:
	using enqueue_drains = std::true_type;
	[[nodiscard]] LUG_ALWAYS_INLINE constexpr std::string_view buffer() const noexcept { return buffer_; }
	constexpr void drain_buffer(std::size_t sr) noexcept { buffer_.remove_prefix(sr); }
	template <class It, class = detail::enable_if_char_contiguous_iterator_t<It>> void enqueue(It first, It last) { buffer_ = (last > first) ? std::string_view{&(*first), static_cast<std::size_t>(last - first)} : std::string_view{}; }
};

class parser_base
{
protected:
	static constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();
	static constexpr std::size_t actioncode = (std::numeric_limits<std::size_t>::max)();
	static constexpr std::size_t attrpushcode = (std::numeric_limits<std::size_t>::max)() - 1;
	static constexpr std::size_t attrpopcode = (std::numeric_limits<std::size_t>::max)() - 2;

	struct action_response { std::size_t call_depth{0}; std::size_t action_index{0}; syntax_range range{0, 0}; constexpr action_response() noexcept = default; constexpr action_response(std::size_t c, std::size_t a, syntax_range const& r) noexcept : call_depth{c}, action_index{a}, range{r} {} };
	struct backtrack_frame { std::size_t sr; std::size_t rc; std::size_t ri; std::ptrdiff_t pc; constexpr backtrack_frame(std::size_t s, std::size_t r, std::size_t i, std::ptrdiff_t p) noexcept : sr{s}, rc{r}, ri{i}, pc{p} {} };
	struct call_frame { std::ptrdiff_t pc; constexpr explicit call_frame(std::ptrdiff_t p) noexcept : pc{p} {} };
	struct capture_frame { std::size_t sr; constexpr explicit capture_frame(std::size_t s) noexcept : sr{s} {} };
	struct condition_frame { std::string_view name; bool value; constexpr condition_frame(std::string_view n, bool v) noexcept : name{n}, value{v} {} };
	struct lrmemo_frame { std::size_t srr; std::size_t sra; std::size_t prec; std::ptrdiff_t pcr; std::ptrdiff_t pca; std::size_t rcr; std::vector<action_response> responses; lrmemo_frame(std::size_t sr, std::size_t sa, std::size_t p, std::ptrdiff_t pc, std::ptrdiff_t pa, std::size_t rc) noexcept : srr{sr}, sra{sa}, prec{p}, pcr{pc}, pca{pa}, rcr{rc} {} };
	struct raise_frame { std::string_view label; std::size_t sr; std::size_t rc; std::ptrdiff_t eh; std::ptrdiff_t pc; constexpr explicit raise_frame(std::string_view lab, std::size_t s, std::size_t r, std::ptrdiff_t e, std::ptrdiff_t p) noexcept : label{lab}, sr{s}, rc{r}, eh{e}, pc{p} {} };
	struct recover_frame { std::ptrdiff_t rh; constexpr explicit recover_frame(std::ptrdiff_t h) noexcept : rh{h} {} };
	struct report_frame { std::ptrdiff_t eh; constexpr explicit report_frame(std::ptrdiff_t h) noexcept : eh{h} {} };
	struct symbol_frame { std::string_view name; std::size_t sr; constexpr symbol_frame(std::string_view n, std::size_t s) noexcept : name{n}, sr{s} {} };
	using symbol_table_frame = std::unordered_map<std::string_view, std::vector<std::string>>;
	using stack_frame = std::variant<backtrack_frame, call_frame, capture_frame, condition_frame, lrmemo_frame, raise_frame, recover_frame, report_frame, symbol_frame, symbol_table_frame>;

	// NOLINTBEGIN(cppcoreguidelines-non-private-member-variables-in-classes,misc-non-private-member-variables-in-classes)
	lug::grammar const* grammar_;
	lug::program const* program_;
	lug::environment* environment_;
	std::vector<action_response> responses_;
	std::vector<stack_frame> stack_frames_;
	std::unordered_map<std::size_t, std::string> casefolded_subjects_;
	lug::registers registers_;
	bool needs_reset_{false};
	bool parsing_{false};
	bool success_{true};
	// NOLINTEND(cppcoreguidelines-non-private-member-variables-in-classes,misc-non-private-member-variables-in-classes)

	template <class Predicate>
	[[nodiscard]] LUG_ALWAYS_INLINE auto make_property_matcher(Predicate const& pred, instruction const& instr) const noexcept
	{
		return [&pred,
				prop = static_cast<unicode::property_enum>(instr.immediate8),
				mask = program_->uniforms[instr.immediate16]](unicode::record const& record) noexcept {
			return pred(record, prop, mask);
		};
	}

	template <class T>
	[[nodiscard]] LUG_ALWAYS_INLINE T& top_stack_frame()
	{
		if LUG_UNLIKELY(stack_frames_.empty())
			throw_exception<bad_stack>();
		return std::get<T>(stack_frames_.back());
	}

	LUG_ALWAYS_INLINE void pop_responses_after(std::size_t n)
	{
		if (n < responses_.size())
			responses_.resize(n);
	}

	[[nodiscard]] std::size_t restore_responses_after(std::size_t n, std::vector<action_response> const& restore)
	{
		pop_responses_after(n);
		responses_.insert(responses_.end(), restore.begin(), restore.end());
		return responses_.size();
	}

	[[nodiscard]] std::vector<action_response> drop_responses_after(std::size_t n)
	{
		std::vector<action_response> dropped;
		if (n < responses_.size()) {
			dropped.assign(responses_.begin() + static_cast<std::ptrdiff_t>(n), responses_.end());
			responses_.resize(n);
		}
		return dropped;
	}

	[[nodiscard]] std::ptrdiff_t call_into(std::size_t prec, std::ptrdiff_t off)
	{
		if (prec == 0) {
			stack_frames_.emplace_back(std::in_place_type<call_frame>, registers_.pc);
			++registers_.cd;
			registers_.pc += off;
			return 0;
		}
		auto const frame_it = detail::escaping_find_if(stack_frames_.crbegin(), stack_frames_.crend(), [srr = registers_.sr, pca = registers_.pc + off](auto const& frame) {
				if (auto const* const memo_ptr = std::get_if<lrmemo_frame>(&frame); memo_ptr != nullptr) {
					if ((memo_ptr->srr == srr) && (memo_ptr->pca == pca))
						return 1;
					if (memo_ptr->srr >= srr)
						return -1;
				}
				return 0;
			});
		if (frame_it != stack_frames_.crend()) {
			auto const& memo = std::get<lrmemo_frame>(*frame_it);
			if ((memo.sra == parser_base::lrfailcode) || (prec < memo.prec))
				return 1;
			registers_.sr = memo.sra;
			registers_.rc = restore_responses_after(registers_.rc, memo.responses);
			return 0;
		}
		stack_frames_.emplace_back(std::in_place_type<lrmemo_frame>, registers_.sr, parser_base::lrfailcode, prec, registers_.pc, registers_.pc + off, registers_.rc);
		++registers_.cd;
		++registers_.ci;
		registers_.pc += off;
		return 0;
	}

	[[nodiscard]] bool return_from_lrmemo_call(lrmemo_frame& memo)
	{
		if ((memo.sra == parser_base::lrfailcode) || (registers_.sr > memo.sra)) {
			memo.sra = registers_.sr;
			memo.responses = drop_responses_after(memo.rcr);
			registers_.sr = memo.srr;
			registers_.pc = memo.pca;
			registers_.rc = memo.rcr;
			return false;
		}
		--registers_.cd;
		--registers_.ci;
		registers_.sr = memo.sra;
		registers_.pc = memo.pcr;
		registers_.rc = restore_responses_after(memo.rcr, memo.responses);
		return true;
	}

	[[nodiscard]] error_response do_return_from_raise(raise_frame const& frame, syntax const& sx, error_response rec_res)
	{
		error_response err_res{rec_res};
		error_context err{*environment_, sx, frame.label, err_res};
		auto handler_index = static_cast<std::size_t>(frame.eh);
		auto const handler_count = program_->handlers.size();
		auto next_frame = stack_frames_.rbegin();
		auto const last_frame = stack_frames_.rend();
		while (handler_index < handler_count) {
			err_res = program_->handlers[handler_index](err);
			if (err_res != error_response::rethrow)
				break;
			err_res = error_response::halt;
			handler_index = (std::numeric_limits<std::size_t>::max)();
			for (++next_frame; next_frame != last_frame; ++next_frame) {
				if (auto const* const next_report_frame = std::get_if<report_frame>(&*next_frame); next_report_frame != nullptr) {
					handler_index = static_cast<std::size_t>(next_report_frame->eh);
					break;
				}
			}
		}
		--registers_.cd;
		--registers_.ci;
		if (err_res >= error_response::backtrack) {
			registers_.sr = frame.sr;
			registers_.rc = frame.rc;
			return err_res;
		}
		registers_.pc = frame.pc;
		return err_res;
	}

	void do_accept(std::string_view match)
	{
		detail::scope_exit const cleanup{[this, prior_call_depth = environment_->start_accept()]{
			environment_->end_accept(prior_call_depth);
			responses_.clear();
			registers_.rc = 0;
		}};
		for (auto& resp : responses_) {
			if (environment_->accept_response(resp.call_depth)) {
				switch (~resp.range.index) {
					case ~parser_base::attrpushcode: environment_->push_attribute_frame(program_->frames[resp.action_index]); break;
					case ~parser_base::attrpopcode: environment_->pop_attribute_frame(program_->frames[resp.action_index]); break;
					case ~parser_base::actioncode: program_->actions[resp.action_index](*environment_); break;
					default: program_->captures[resp.action_index](*environment_, syntax{match.substr(resp.range.index, resp.range.size), resp.range.index}); break;
				}
			}
		}
	}

	void do_drain(std::string_view sub)
	{
		for (auto& frame : stack_frames_) {
			if (auto* const backtrack = std::get_if<backtrack_frame>(&frame); backtrack) {
				if (backtrack->sr < registers_.sr)
					backtrack->sr = (std::numeric_limits<std::size_t>::max)();
			}
		}
		registers_.mr -= registers_.sr;
		registers_.sr = 0;
		registers_.rc = 0;
		registers_.ci &= lug::registers::count_mask;
		registers_.ri &= lug::registers::count_mask;
		casefolded_subjects_.clear();
		responses_.clear();
		environment_->drain(sub);
	}

	void do_reset(std::string_view sub)
	{
		if (needs_reset_) {
			needs_reset_ = false;
			success_ = true;
			registers_.sr = 0;
			registers_.mr = 0;
			registers_.rc = 0;
			registers_.cd = 0;
			registers_.ci = 0;
			registers_.ri = 0;
			registers_.pc = 0;
			registers_.eh = -1;
			registers_.rh = -1;
			registers_.rr = error_response::resume;
			casefolded_subjects_.clear();
			responses_.clear();
			stack_frames_.clear();
		}
		environment_->reset(sub);
	}

public:
	explicit parser_base(lug::grammar const& g, lug::environment& e) : grammar_{&g}, program_{&g.program()}, environment_{&e} {}
	[[nodiscard]] lug::grammar const& grammar() const noexcept { return *grammar_; }
	[[nodiscard]] lug::environment& environment() const noexcept { return *environment_; }
	[[nodiscard]] std::size_t subject_index() const noexcept { return registers_.sr; }
	[[nodiscard]] std::size_t max_subject_index() const noexcept { return registers_.mr; }
	[[nodiscard]] syntax_position subject_position() { return environment_->position_at(registers_.sr); }
	[[nodiscard]] syntax_position max_subject_position() { return environment_->position_at(registers_.mr); }
	[[nodiscard]] syntax_position position_at(std::size_t index) { return environment_->position_at(index); }
	[[nodiscard]] syntax_position position_begin(syntax_range const& range) { return environment_->position_at(range.index); }
	[[nodiscard]] syntax_position position_end(syntax_range const& range) { return environment_->position_at(range.index + range.size); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax_range const& range) { return {position_begin(range), position_end(range)}; }
	[[nodiscard]] lug::registers& registers() noexcept { return registers_; }
	[[nodiscard]] lug::registers const& registers() const noexcept { return registers_; }
};

template <class InputSource>
class basic_parser : public parser_base
{
	InputSource input_source_;

	[[nodiscard]] bool available(std::size_t position, std::size_t min_size = 1)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value) {
			for (;;) {
				std::size_t const buffer_size = input_source_.buffer().size();
				std::size_t const buffer_remaining = buffer_size - position;
				if LUG_LIKELY(position < buffer_size) {
					if LUG_LIKELY(min_size <= buffer_remaining)
						return true;
					if constexpr (detail::input_source_has_options<InputSource>::value)
						if ((input_source_.options() & source_options::interactive) != source_options::none)
							return false;
				}
				if LUG_UNLIKELY(!input_source_.fill_buffer(min_size - buffer_remaining))
					return false;
			}
		} else {
			std::size_t const buffer_size = input_source_.buffer().size();
			return (position < buffer_size) && (min_size <= (buffer_size - position));
		}
	}

	[[nodiscard]] LUG_ALWAYS_INLINE auto input_buffer(std::size_t position, [[maybe_unused]] std::size_t min_size = 1)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value) {
			bool const result = available(position, min_size);
			auto const buffer = input_source_.buffer();
			if LUG_LIKELY(result)
				return std::pair{std::next(buffer.cbegin(), static_cast<std::ptrdiff_t>(position)), buffer.cend()};
			return std::pair{buffer.cend(), buffer.cend()};
		} else {
			auto const buffer = input_source_.buffer();
			return std::pair{std::next(buffer.cbegin(), static_cast<std::ptrdiff_t>(position)), buffer.cend()};
		}
	}

	[[nodiscard]] LUG_ALWAYS_INLINE auto input_buffer_no_fill(std::size_t position) const noexcept
	{
		auto const buffer = input_source_.buffer();
		return std::pair{std::next(buffer.cbegin(), static_cast<std::ptrdiff_t>(position)), buffer.cend()};
	}

	[[nodiscard]] LUG_ALWAYS_INLINE bool compare(std::size_t sr, std::size_t sn, std::string_view str) const noexcept
	{
		return input_source_.buffer().compare(sr, sn, str) == 0;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE bool casefold_compare(std::size_t sr, std::size_t sn, std::string_view str) noexcept
	{
		std::string& subject = casefolded_subjects_[sr];
		if (subject.size() < sn)
			subject = utf8::tocasefold(input_source_.buffer().substr(sr, sn));
		return subject.compare(0, sn, str) == 0;
	}

	template <class MatchOneFn, class... ExtraArgs>
	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_match_incrementally(std::size_t& sr, std::size_t nmin, std::size_t nmax, MatchOneFn const& match_one, ExtraArgs const&... extra_args)
	{
		std::size_t const i = sr;
		std::size_t n = 0;
		for ( ; n <= nmax; ++n)
			if (match_one(*this, sr, extra_args...) != 0)
				break;
		if (n >= nmin)
			return 0;
		sr = i;
		return 1;
	}

	template <class MatchFn>
	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_match_buffered(std::size_t& sr, std::size_t nmin, std::size_t nmax, MatchFn const& match)
	{
		std::size_t const i = sr;
		std::size_t n = 0;
		auto const [first, last] = input_buffer_no_fill(i);
		auto curr = first;
		for ( ; n <= nmax; ++n) {
			auto const next = match(curr, last);
			if (!next)
				break;
			curr = *next;
		}
		if (n >= nmin) {
			sr = i + static_cast<std::size_t>(curr - first);
			return 0;
		}
		return 1;
	}

	template <class MatchFn>
	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_with(std::size_t& sr, MatchFn const& match)
	{
		std::size_t const i = sr;
		auto const [curr, last] = input_buffer(i);
		if (auto const next = match(curr, last); next) {
			sr = i + static_cast<std::size_t>(*next - curr);
			return 0;
		}
		return 1;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_any(std::size_t& sr)
	{
		std::size_t const i = sr;
		auto const [curr, last] = input_buffer(i);
		if LUG_LIKELY(curr != last) {
			auto const next = std::find_if(std::next(curr), last, utf8::is_lead_or_ascii);
			sr = i + static_cast<std::size_t>(next - curr);
			return 0;
		}
		return 1;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_any(std::size_t& sr, std::size_t nmin, std::size_t nmax)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value) {
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_any));
		} else {
			if ((nmin == 0) && (nmax == forever)) {
				sr = input_source_.buffer().size();
				return 0;
			}
			return repeat_match_buffered(sr, nmin, nmax, [](auto curr, auto last) -> std::optional<std::decay_t<decltype(curr)>> {
				if LUG_LIKELY(curr != last) {
					auto const next = std::find_if(curr + 1, last, utf8::is_lead_or_ascii);
					if (next != curr)
						return next;
				}
				return std::nullopt;
			});
		}
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_blank(std::size_t& sr)
	{
		return match_with(sr, ascii::match_blank);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_blank(std::size_t& sr, std::size_t nmin, std::size_t nmax)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value)
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_blank));
		else
			return repeat_match_buffered(sr, nmin, nmax, ascii::match_blank);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_space(std::size_t& sr)
	{
		return match_with(sr, ascii::match_space);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_space(std::size_t& sr, std::size_t nmin, std::size_t nmax)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value)
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_space));
		else
			return repeat_match_buffered(sr, nmin, nmax, ascii::match_space);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_eol(std::size_t& sr, std::uint_least8_t mode)
	{
		std::size_t i = sr;
		if (mode != 0)
			(void)repeat_blank(i, 0, forever);
		if (std::ptrdiff_t const eol_fail_count = match_with(i, ascii::match_eol); eol_fail_count != 0)
			return eol_fail_count;
		sr = i;
		return 0;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_eoi(std::size_t& sr, std::uint_least8_t mode)
	{
		std::size_t i = sr;
		if constexpr (detail::input_source_has_options<InputSource>::value) {
			if ((input_source_.options() & source_options::interactive) != source_options::none) {
				if (mode != 0)
					(void)repeat_match_buffered(i, 0, forever, ascii::match_space);
				if (i >= input_source_.buffer().size()) {
					sr = i;
					return 0;
				}
				return 1;
			}
		}
		if (mode != 0)
			(void)repeat_space(i, 0, forever);
		if (!available(i)) {
			sr = i;
			return 0;
		}
		return 1;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_unit(std::size_t& sr, std::uint_least8_t value)
	{
		std::size_t const i = sr;
		auto const [curr, last] = input_buffer(i);
		if LUG_LIKELY(curr != last) {
			if (static_cast<unsigned char>(*curr) == value) {
				sr = i + 1;
				return 0;
			}
		}
		return 1;
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_unit(std::size_t& sr, std::size_t nmin, std::size_t nmax, std::uint_least8_t unit)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value) {
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_unit), unit);
		} else {
			std::size_t const i = sr;
			auto const [first, last] = input_buffer_no_fill(i);
			auto const tail = (static_cast<std::size_t>(last - first) <= nmax) ? last : (first + static_cast<std::ptrdiff_t>(nmax));
			auto const next = std::find_if(first, tail, [unit](auto const c) { return static_cast<unsigned char>(c) != unit; });
			if (auto const count = static_cast<std::size_t>(next - first); count >= nmin) {
				sr = i + count;
				return 0;
			}
			return 1;
		}
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_set(std::size_t& sr, rune_set const& set)
	{
		return match_with(sr, set);
	}

	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t repeat_set(std::size_t& sr, std::size_t nmin, std::size_t nmax, rune_set const& set)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value)
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_set), set);
		else
			return repeat_match_buffered(sr, nmin, nmax, set);
	}

	template <class InputIt, class MatchFn>
	[[nodiscard]] static auto decode_and_match_rune(InputIt first, InputIt last, MatchFn const& match) -> std::optional<std::decay_t<InputIt>>
	{
		auto const [next, rune] = utf8::decode_rune(first, last);
		if LUG_LIKELY(next != first) {
			bool matched = false;
			if constexpr(std::is_invocable_v<MatchFn const&, unicode::record const&>) {
				matched = match(unicode::query(rune));
			} else if constexpr(std::is_invocable_v<MatchFn const&, char32_t>) {
				matched = match(rune);
			} else {
				static_assert(detail::always_false_v<MatchFn>, "unsupported match operation");
			}
			if (matched)
				return next;
		}
		return std::nullopt;
	}

	template <class MatchFn>
	[[nodiscard]] std::ptrdiff_t match_rune(std::size_t& sr, MatchFn const& match)
	{
		std::size_t const i = sr;
		auto const [curr, last] = input_buffer(i);
		if (auto const next = decode_and_match_rune(curr, last, match); next) {
			sr = i + static_cast<std::size_t>(*next - curr);
			return 0;
		}
		return 1;
	}

	template <class MatchFn>
	[[nodiscard]] std::ptrdiff_t repeat_rune(std::size_t& sr, std::size_t nmin, std::size_t nmax, MatchFn const& match)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value)
			return repeat_match_incrementally(sr, nmin, nmax, std::mem_fn(&basic_parser::match_rune<MatchFn>), match);
		else
			return repeat_match_buffered(sr, nmin, nmax, [&match](auto first, auto last) { return decode_and_match_rune(first, last, match); });
	}

	template <class Compare>
	[[nodiscard]] LUG_ALWAYS_INLINE std::ptrdiff_t match_sequence(std::size_t& sr, std::string_view str, Compare const& comp)
	{
		if (std::size_t const i = sr, n = str.size(); !n || (available(i, n) && comp(*this, i, n, str))) {
			sr = i + n;
			return 0;
		}
		return 1;
	}

	template <class Modify, class Compare>
	[[nodiscard]] std::ptrdiff_t match_symbol_all(std::size_t& sr, std::string_view symbol_name, Modify const& mod, Compare const& comp)
	{
		auto const& symbols = environment_->get_symbols(symbol_name);
		if (std::size_t tsr = sr; std::all_of(symbols.begin(), symbols.end(), [&tsr, &mod, &comp, this](auto const& symbol) { return (this->match_sequence(tsr, mod(symbol), comp) == 0); })) {
			sr = tsr;
			return 0;
		}
		return 1;
	}

	template <class Modify, class Compare>
	[[nodiscard]] std::ptrdiff_t match_symbol_any(std::size_t& sr, std::string_view symbol_name, Modify const& mod, Compare const& comp)
	{
		auto const& symbols = environment_->get_symbols(symbol_name);
		return std::any_of(symbols.begin(), symbols.end(), [&sr, &mod, &comp, this](auto const& symbol) { return (this->match_sequence(sr, mod(symbol), comp) == 0); }) ? 0 : 1;
	}

	template <class Modify, class Compare>
	[[nodiscard]] std::ptrdiff_t match_symbol_head(std::size_t& sr, std::string_view symbol_name, std::size_t symbol_index, Modify&& mod, Compare&& comp)
	{
		auto const& symbols = environment_->get_symbols(symbol_name);
		return (symbol_index < symbols.size()) ? match_sequence(sr, mod(symbols[symbol_index]), std::forward<Compare>(comp)) : 1;
	}

	template <class Modify, class Compare>
	[[nodiscard]] std::ptrdiff_t match_symbol_tail(std::size_t& sr, std::string_view symbol_name, std::size_t symbol_index, Modify&& mod, Compare&& comp)
	{
		auto const& symbols = environment_->get_symbols(symbol_name);
		return (symbol_index < symbols.size()) ? match_sequence(sr, mod(symbols[symbols.size() - symbol_index - 1]), std::forward<Compare>(comp)) : 1;
	}

	[[nodiscard]] std::ptrdiff_t match_default_recovery(std::size_t& sr)
	{
		if (std::size_t i = sr; available(i)) {
			do {
				auto const buffer = input_source_.buffer();
				if (auto const n = buffer.find_first_of(" \t\n\r\f\v", i); n != std::string::npos) {
					i = n;
					break;
				}
				i = buffer.size();
			} while (available(i));
			if (i > sr) {
				sr = i;
				return 0;
			}
		}
		return 1;
	}

	[[nodiscard]] error_response return_from_raise(raise_frame const& frame)
	{
		error_response rec_res{std::exchange(registers_.rr, error_response::resume)};
		if (registers_.pc == frame.pc) {
			registers_.sr = frame.sr;
			(void)match_default_recovery(registers_.sr);
			rec_res = error_response::halt;
		}
		registers_.mr = (std::max)(registers_.mr, registers_.sr);
		auto const sr0 = frame.sr;
		auto const sr1 = registers_.sr;
		auto const mat = match();
		auto const sub = subject();
		environment_->set_match_and_subject(mat, sub);
		return do_return_from_raise(frame, syntax{((sr0 < sr1) ? mat.substr(sr0, sr1 - sr0) : sub), sr0}, rec_res);
	}

	[[nodiscard]] std::pair<error_response, std::ptrdiff_t> return_from_call()
	{
		if LUG_UNLIKELY(stack_frames_.empty())
			throw_exception<bad_stack>();
		auto ret_result = std::visit([this](auto& frame) -> std::pair<error_response, std::ptrdiff_t> {
			using frame_type = std::decay_t<decltype(frame)>;
			if constexpr (std::is_same_v<frame_type, call_frame>) {
				--registers_.cd;
				registers_.pc = frame.pc;
				return std::pair{error_response::accept, std::ptrdiff_t{0}};
			} else if constexpr (std::is_same_v<frame_type, lrmemo_frame>) {
				if (!return_from_lrmemo_call(frame))
					return std::pair{error_response::rethrow, std::ptrdiff_t{0}};
				accept_or_drain_if_deferred();
				return std::pair{error_response::accept, std::ptrdiff_t{0}};
			} else if constexpr (std::is_same_v<frame_type, raise_frame>) {
				error_response const err_res = return_from_raise(frame);
				if (err_res >= error_response::backtrack)
					return std::pair{err_res, std::ptrdiff_t{1}};
				accept_or_drain_if_deferred();
				return std::pair{err_res, std::ptrdiff_t{0}};
			} else {
				throw_exception<bad_stack>();
			}
		}, stack_frames_.back());
		if (ret_result.first != error_response::rethrow)
			stack_frames_.pop_back();
		return ret_result;
	}

	[[nodiscard]] error_response fail_one()
	{
		if LUG_UNLIKELY(stack_frames_.empty())
			return error_response::halt;
		error_response const fail_result = std::visit([this](auto& frame) -> error_response {
			using frame_type = std::decay_t<decltype(frame)>;
			if constexpr (std::is_same_v<frame_type, backtrack_frame>) {
				if (frame.sr == (std::numeric_limits<std::size_t>::max)())
					return error_response::backtrack;
				registers_.sr = frame.sr;
				registers_.rc = frame.rc;
				registers_.ri = frame.ri;
				registers_.pc = frame.pc;
				return error_response::accept;
			} else if constexpr (std::is_same_v<frame_type, call_frame>) {
				--registers_.cd;
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, capture_frame>) {
				--registers_.ci;
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, condition_frame>) {
				environment_->set_condition(frame.name, frame.value);
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, lrmemo_frame>) {
				--registers_.cd;
				--registers_.ci;
				if (frame.sra == parser_base::lrfailcode)
					return error_response::backtrack;
				registers_.sr = frame.sra;
				registers_.rc = restore_responses_after(frame.rcr, frame.responses);
				registers_.pc = frame.pcr;
				return error_response::accept;
			} else if constexpr (std::is_same_v<frame_type, raise_frame>) {
				registers_.sr = frame.sr;
				registers_.rc = frame.rc;
				registers_.pc = frame.pc;
				return return_from_raise(frame);
			} else if constexpr (std::is_same_v<frame_type, recover_frame>) {
				registers_.rh = frame.rh;
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, report_frame>) {
				registers_.eh = frame.eh;
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, symbol_frame>) {
				return error_response::backtrack;
			} else if constexpr (std::is_same_v<frame_type, symbol_table_frame>) {
				environment_->symbols_.swap(frame);
				return error_response::backtrack;
			} else {
				static_assert(detail::always_false_v<frame_type>, "non-exhaustive visitor!");
			}
		}, stack_frames_.back());
		stack_frames_.pop_back();
		return fail_result;
	}

	[[nodiscard]] bool fail(std::ptrdiff_t fail_count)
	{
		registers_.mr = (std::max)(registers_.mr, registers_.sr);
		do {
			error_response const fail_result = fail_one();
			if LUG_UNLIKELY(fail_result == error_response::halt)
				return false;
			if (fail_result >= error_response::backtrack)
				continue;
			if (fail_result < error_response::accept)
				success_ = false;
			--fail_count;
		} while (fail_count > 0);
		pop_responses_after(registers_.rc);
		return true;
	}

	[[nodiscard]] bool unwind(std::size_t unwind_count)
	{
		registers_.mr = (std::max)(registers_.mr, registers_.sr);
		for (std::size_t i = 0; i < unwind_count; ++i) {
			error_response const fail_result = fail_one();
			if LUG_UNLIKELY(fail_result == error_response::halt)
				return false;
			if (fail_result < error_response::accept)
				success_ = false;
		}
		return true;
	}

	void accept()
	{
		registers_.mr = (std::max)(registers_.mr, registers_.sr);
		auto const mat = match();
		auto const sub = subject();
		environment_->set_match_and_subject(mat, sub);
		do_accept(mat);
	}

	void drain()
	{
		if (registers_.sr > 0) {
			input_source_.drain_buffer(registers_.sr);
			do_drain(input_source_.buffer());
		}
	}

	void accept_or_drain_if_deferred()
	{
		if ((registers_.ci & lug::registers::count_mask) == 0) {
			bool const should_cut = (registers_.ci & lug::registers::inhibited_flag) != 0;
			bool const should_accept = (registers_.ci & lug::registers::ignore_errors_flag) != 0;
			if (should_cut || should_accept) {
				registers_.ci = 0;
				registers_.mr = (std::max)(registers_.mr, registers_.sr);
				auto const mat = match();
				auto const sub = subject();
				environment_->set_match_and_subject(mat, sub);
				if ((should_cut && success_) || should_accept)
					do_accept(mat);
				if (should_cut)
					drain();
			}
		}
	}

	void reset()
	{
		if (registers_.sr > 0)
			input_source_.drain_buffer(registers_.sr);
		do_reset(input_source_.buffer());
	}

public:
	basic_parser(lug::grammar const& g, lug::environment& e) : parser_base{g, e} {}
	[[nodiscard]] std::string_view match() const noexcept { return input_source_.buffer().substr(0, registers_.sr); }
	[[nodiscard]] std::string_view subject() const noexcept { return input_source_.buffer().substr(registers_.sr, input_source_.buffer().size() - registers_.sr); }
	[[nodiscard]] std::string_view max_match() const noexcept { return input_source_.buffer().substr(0, registers_.mr); }
	[[nodiscard]] std::string_view max_subject() const noexcept { return input_source_.buffer().substr(registers_.mr, input_source_.buffer().size() - registers_.mr); }

	template <class InputIt, class = std::enable_if_t<detail::input_source_has_enqueue<InputSource, InputIt>::value>>
	basic_parser& enqueue(InputIt first, InputIt last)
	{
		if constexpr (detail::input_source_enqueue_drains<InputSource>::value)
			drain();
		input_source_.enqueue(std::move(first), std::move(last));
		return *this;
	}

	template <class InputRng, class = detail::enable_if_char_input_range_t<InputRng>>
	basic_parser& enqueue(InputRng&& rng) // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		return enqueue(rng.begin(), rng.end());
	}

	template <class InputFunc, class = std::enable_if_t<detail::input_source_has_push_source<InputSource, InputFunc&&>::value>>
	basic_parser& push_source(InputFunc&& func, source_options opt = source_options::none)
	{
		input_source_.push_source(std::forward<InputFunc>(func), opt);
		return *this;
	}

	template <class InputIt, class = std::enable_if_t<detail::input_source_has_enqueue<InputSource, InputIt>::value>>
	bool parse(InputIt first, InputIt last)
	{
		return enqueue(first, last).parse();
	}

	template <class InputRng, class = detail::enable_if_char_input_range_t<InputRng>>
	bool parse(InputRng&& rng)
	{
		return enqueue(std::forward<InputRng>(rng)).parse();
	}

	template <class InputFunc, class = std::enable_if_t<detail::input_source_has_push_source<InputSource, InputFunc&&>::value>>
	bool parse(InputFunc&& func, source_options opt = source_options::none)
	{
		return push_source(std::forward<InputFunc>(func), opt).parse();
	}

	bool parse()
	{
		detail::reentrancy_sentinel<reenterant_parse_error> const guard{parsing_};
		if LUG_UNLIKELY(program_->instructions.empty() || program_->data.empty())
			throw_exception<bad_grammar>();
		reset();
		needs_reset_ = true;
		detail::scope_fail const fixup_max_subject_position{[this]() noexcept { registers_.mr = (std::max)(registers_.mr, registers_.sr); }};
		std::ptrdiff_t fail_count{0};
		for (auto instr_index = static_cast<std::size_t>(registers_.pc++); instr_index < program_->instructions.size(); instr_index = static_cast<std::size_t>(registers_.pc++)) {
			instruction const instr{program_->instructions[instr_index]};
			std::string_view const str{program_->data.data() + instr.offset32, instr.immediate16};
			switch (instr.op) {
				case opcode::jump: {
					registers_.pc += instr.offset32;
				} break;
				case opcode::choice: {
					auto const predicate_inhibited_flag = static_cast<std::size_t>(instr.immediate8) << lug::registers::inhibited_shift;
					auto const predicate_frame_mask = static_cast<std::size_t>(detail::sar(static_cast<std::ptrdiff_t>(predicate_inhibited_flag), lug::registers::inhibited_shift)) >> lug::registers::flags_count;
					stack_frames_.emplace_back(std::in_place_type<backtrack_frame>, (registers_.sr - instr.immediate16), registers_.rc, registers_.ri, (registers_.pc + instr.offset32));
					registers_.ri = (stack_frames_.size() & predicate_frame_mask) | (registers_.ri & ~predicate_frame_mask) | predicate_inhibited_flag;
				} break;
				case opcode::commit: {
					if LUG_UNLIKELY(stack_frames_.empty())
						throw_exception<bad_stack>();
					stack_frames_.pop_back();
					registers_.pc += instr.offset32;
				} break;
				case opcode::commit_back: {
					auto const& backtrack = top_stack_frame<backtrack_frame>();
					registers_.sr = backtrack.sr;
					registers_.ri = backtrack.ri;
					stack_frames_.pop_back();
					registers_.pc += instr.offset32;
				} break;
				case opcode::commit_partial: {
					auto& backtrack = top_stack_frame<backtrack_frame>();
					backtrack.sr = registers_.sr;
					backtrack.rc = registers_.rc;
					registers_.pc += instr.offset32;
				} break;
				case opcode::accept: {
					registers_.ci |= static_cast<std::size_t>(instr.immediate8) << lug::registers::ignore_errors_shift;
					accept_or_drain_if_deferred();
				} break;
				case opcode::call: {
					fail_count = call_into(instr.immediate16, instr.offset32);
				} break;
				case opcode::ret: {
					auto const [ret_response, ret_fail_count] = return_from_call();
					if LUG_UNLIKELY(ret_response == error_response::halt)
						return false;
					if (ret_response < error_response::accept)
						success_ = false;
					fail_count = ret_fail_count;
				} break;
				case opcode::fail: {
					fail_count = static_cast<std::ptrdiff_t>(instr.immediate8);
				} break;
				case opcode::raise: {
					std::ptrdiff_t const recovery_handler{registers_.rh};
					if (instr.immediate8 != 0) {
						registers_.rh = top_stack_frame<recover_frame>().rh;
						stack_frames_.pop_back();
					}
					if ((registers_.ri & lug::registers::inhibited_flag) != 0) {
						if LUG_UNLIKELY(!unwind((stack_frames_.size() - (registers_.ri & lug::registers::count_mask))))
							return false;
						fail_count = 1;
						break;
					}
					stack_frames_.emplace_back(std::in_place_type<raise_frame>, str, registers_.sr, registers_.rc, registers_.eh, registers_.pc);
					++registers_.cd;
					++registers_.ci;
					if (recovery_handler < 0) {
						fail_count = 1;
						break;
					}
					registers_.rr = error_response::resume;
					registers_.pc = recovery_handler;
				} break;
				case opcode::recover_push: {
					stack_frames_.emplace_back(std::in_place_type<recover_frame>, registers_.rh);
					registers_.rh = registers_.pc + instr.offset32;
				} break;
				case opcode::recover_pop: {
					registers_.rh = top_stack_frame<recover_frame>().rh;
					stack_frames_.pop_back();
				} break;
				case opcode::report_push: {
					stack_frames_.emplace_back(std::in_place_type<report_frame>, registers_.eh);
					registers_.eh = static_cast<std::ptrdiff_t>(instr.immediate16);
				} break;
				case opcode::recover_resp: {
					registers_.rr = static_cast<error_response>(instr.immediate8);
				} break;
				case opcode::report_pop: {
					registers_.eh = top_stack_frame<report_frame>().eh;
					stack_frames_.pop_back();
				} break;
				case opcode::predicate: {
					registers_.mr = (std::max)(registers_.mr, registers_.sr);
					environment_->set_match_and_subject(match(), subject());
					bool const accepted = program_->predicates[instr.immediate16](*environment_);
					pop_responses_after(registers_.rc);
					fail_count = accepted ? 0 : 1;
				} break;
				case opcode::action: {
					responses_.emplace_back(registers_.cd, instr.immediate16, syntax_range{parser_base::actioncode, 0});
					registers_.rc = responses_.size();
				} break;
				case opcode::capture_start: {
					stack_frames_.emplace_back(std::in_place_type<capture_frame>, registers_.sr);
					++registers_.ci;
				} break;
				case opcode::capture_end: {
					auto const sr0 = top_stack_frame<capture_frame>().sr;
					auto const sr1 = registers_.sr;
					stack_frames_.pop_back();
					--registers_.ci;
					if LUG_UNLIKELY(sr0 > sr1) {
						fail_count = 1;
						break;
					}
					responses_.emplace_back(registers_.cd, instr.immediate16,  syntax_range{sr0, sr1 - sr0});
					registers_.rc = responses_.size();
					accept_or_drain_if_deferred();
				} break;
				case opcode::attribute_push: {
					responses_.emplace_back(registers_.cd, instr.immediate16, syntax_range{parser_base::attrpushcode, 0});
					registers_.rc = responses_.size();
				} break;
				case opcode::attribute_pop: {
					responses_.emplace_back(registers_.cd, instr.immediate16, syntax_range{parser_base::attrpopcode, 0});
					registers_.rc = responses_.size();
				} break;
				case opcode::match: {
					fail_count = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::match_cf: {
					fail_count = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::match_any: case opcode::test_any: {
					fail_count = match_any(registers_.sr);
				} break;
				case opcode::match_blank: case opcode::test_blank: {
					fail_count = match_blank(registers_.sr);
				} break;
				case opcode::match_space: case opcode::test_space: {
					fail_count = match_space(registers_.sr);
				} break;
				case opcode::match_eol: case opcode::test_eol: {
					fail_count = match_eol(registers_.sr, instr.immediate8);
				} break;
				case opcode::match_eoi: case opcode::test_eoi: {
					fail_count = match_eoi(registers_.sr, instr.immediate8);
				} break;
				case opcode::match_unit: case opcode::test_unit: {
					fail_count = match_unit(registers_.sr, instr.immediate8);
				} break;
				case opcode::match_set: case opcode::test_set: {
					fail_count = match_set(registers_.sr, program_->runesets[instr.immediate16]);
				} break;
				case opcode::match_all_of: case opcode::test_all_of: {
					fail_count = match_rune(registers_.sr, make_property_matcher(unicode::all_of, instr));
				} break;
				case opcode::match_any_of: case opcode::test_any_of: {
					fail_count = match_rune(registers_.sr, make_property_matcher(unicode::any_of, instr));
				} break;
				case opcode::match_none_of: case opcode::test_none_of: {
					fail_count = match_rune(registers_.sr, make_property_matcher(unicode::none_of, instr));
				} break;
				case opcode::repeat_any: {
					fail_count = repeat_any(registers_.sr, instr.unpack_min(), instr.unpack_max());
				} break;
				case opcode::repeat_blank: case opcode::skip_blank: {
					fail_count = repeat_blank(registers_.sr, instr.unpack_min(), instr.unpack_max());
				} break;
				case opcode::repeat_space: case opcode::skip_space: {
					fail_count = repeat_space(registers_.sr, instr.unpack_min(), instr.unpack_max());
				} break;
				case opcode::repeat_unit: {
					fail_count = repeat_unit(registers_.sr, instr.unpack_min(), instr.unpack_max(), instr.immediate8);
				} break;
				case opcode::repeat_set: {
					fail_count = repeat_set(registers_.sr, instr.unpack_min(), instr.unpack_max(), program_->runesets[instr.immediate16]);
				} break;
				case opcode::repeat_all_of: {
					fail_count = repeat_rune(registers_.sr, instr.unpack_min(), instr.unpack_max(), make_property_matcher(unicode::all_of, instr));
				} break;
				case opcode::repeat_any_of: {
					fail_count = repeat_rune(registers_.sr, instr.unpack_min(), instr.unpack_max(), make_property_matcher(unicode::any_of, instr));
				} break;
				case opcode::repeat_none_of: {
					fail_count = repeat_rune(registers_.sr, instr.unpack_min(), instr.unpack_max(), make_property_matcher(unicode::none_of, instr));
				} break;
				case opcode::condition_test: {
					fail_count = (environment_->has_condition(str) != (instr.immediate8 != 0)) ? 1 : 0;
				} break;
				case opcode::condition_push: {
					stack_frames_.emplace_back(std::in_place_type<condition_frame>, str, environment_->set_condition(str, instr.immediate8 != 0));
				} break;
				case opcode::condition_pop: {
					auto const& condition = top_stack_frame<condition_frame>();
					environment_->set_condition(condition.name, condition.value);
					stack_frames_.pop_back();
				} break;
				case opcode::symbol_exists: {
					fail_count = (environment_->has_symbol(str) != (instr.immediate8 != 0)) ? 1 : 0;
				} break;
				case opcode::symbol_all: {
					fail_count = match_symbol_all(registers_.sr, str, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_all_cf: {
					fail_count = match_symbol_all(registers_.sr, str, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_any: {
					fail_count = match_symbol_any(registers_.sr, str, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_any_cf: {
					fail_count = match_symbol_any(registers_.sr, str, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_head: {
					fail_count = match_symbol_head(registers_.sr, str, instr.immediate8, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_head_cf: {
					fail_count = match_symbol_head(registers_.sr, str, instr.immediate8, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_tail: {
					fail_count = match_symbol_tail(registers_.sr, str, instr.immediate8, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_tail_cf: {
					fail_count = match_symbol_tail(registers_.sr, str, instr.immediate8, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_start: {
					stack_frames_.emplace_back(std::in_place_type<symbol_frame>, str, registers_.sr);
				} break;
				case opcode::symbol_end: {
					auto const& symbol = top_stack_frame<symbol_frame>();
					auto const sr0 = static_cast<std::size_t>(symbol.sr);
					auto const sr1 = registers_.sr;
					auto const name = symbol.name;
					stack_frames_.pop_back();
					if LUG_UNLIKELY(sr0 > sr1) {
						fail_count = 1;
						break;
					}
					environment_->add_symbol(name, std::string{input_source_.buffer().substr(sr0, sr1 - sr0)});
				} break;
				case opcode::symbol_push: {
					stack_frames_.emplace_back(std::in_place_type<symbol_table_frame>, environment_->symbols_);
					if (instr.immediate8 == 1)
						environment_->symbols_.erase(str);
					else if (instr.immediate8 == 2)
						environment_->symbols_.clear();
				} break;
				case opcode::symbol_pop: {
					environment_->symbols_.swap(top_stack_frame<symbol_table_frame>());
					stack_frames_.pop_back();
				} break;
				default: throw_exception<bad_opcode>();
			}
			if (fail_count > 0) {
				if ((instr.op >= opcode::test_any) && (instr.op <= opcode::test_none_of)) {
					registers_.pc += instr.offset32;
				} else {
					if LUG_UNLIKELY(!fail(fail_count))
						return false;
					accept_or_drain_if_deferred();
				}
				fail_count = 0;
			}
		}
		if (!success_)
			return false;
		accept();
		return true;
	}
};

using parser = basic_parser<multi_input_source>;

template <class InputIt, class = detail::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, grammar const& grmr, environment& envr)
{
	if constexpr (detail::is_char_contiguous_iterator_v<InputIt>)
		return basic_parser<string_view_input_source>{grmr, envr}.enqueue(first, last).parse();
	else
		return basic_parser<string_input_source>{grmr, envr}.enqueue(first, last).parse();
}

template <class InputIt, class = detail::enable_if_char_input_iterator_t<InputIt>>
inline bool parse(InputIt first, InputIt last, grammar const& grmr)
{
	environment envr;
	return parse(first, last, grmr, envr);
}

template <class Rng, class = std::enable_if_t<
		detail::is_char_input_range_v<Rng>
		&& !std::is_same_v<std::decay_t<Rng>, std::string_view>
		&& !std::is_same_v<std::decay_t<Rng>, std::istream>>>
inline bool parse(Rng&& rng, grammar const& grmr, environment& envr) // NOLINT(cppcoreguidelines-missing-std-forward)
{
	return parse(rng.begin(), rng.end(), grmr, envr);
}

template <class Rng, class = std::enable_if_t<
		detail::is_char_input_range_v<Rng>
		&& !std::is_same_v<std::decay_t<Rng>, std::string_view>
		&& !std::is_same_v<std::decay_t<Rng>, std::istream>>>
inline bool parse(Rng&& rng, grammar const& grmr) // NOLINT(cppcoreguidelines-missing-std-forward)
{
	return parse(rng.begin(), rng.end(), grmr);
}

inline bool parse(std::string_view input, grammar const& grmr)
{
	return parse(input.cbegin(), input.cend(), grmr);
}

inline bool parse(std::string_view input, grammar const& grmr, environment& envr)
{
	return parse(input.cbegin(), input.cend(), grmr, envr);
}

} // namespace lug

#endif

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_INCLUDE_LUG_LUG_HPP
#define LUG_INCLUDE_LUG_LUG_HPP

#include <lug/error.hpp>
#include <lug/utf8.hpp>

#include <any>
#include <iostream>
#include <memory>
#include <numeric>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

namespace lug {

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
enum class directives : std::uint_least8_t { none = 0, caseless = 1, eps = 2, lexeme = 4, noskip = 8, preskip = 16, postskip = 32, is_bitfield_enum };
enum class error_response : std::uint_least8_t { halt, resume, accept, backtrack, rethrow };
using error_handler = std::function<error_response(error_context&)>;
using semantic_action = std::function<void(environment&)>;
using semantic_capture_action = std::function<void(environment&, syntax const&)>;
using syntactic_predicate = std::function<bool(environment&)>;
using parser = basic_parser<multi_input_source>;
using program_callees = std::vector<std::tuple<lug::rule const*, lug::program const*, std::ptrdiff_t, directives>>;

struct encoder_expression_trait_tag {};
template <class E, class = void> struct is_encoder_expression : std::false_type {};
template <class E> struct is_encoder_expression<E, std::enable_if_t<std::is_same_v<encoder_expression_trait_tag, typename std::decay_t<E>::expression_trait>>> : std::true_type {};
template <class E> inline constexpr bool is_encoder_expression_v = is_encoder_expression<E>::value;
template <class E> inline constexpr bool is_encoder_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> inline constexpr bool is_recovery_expression_v = is_encoder_expression_v<E> || std::is_same_v<rule, std::decay_t<E>>;
template <class H> inline constexpr bool is_error_handler_v = std::is_invocable_v<std::decay_t<H>, error_context&>;
template <class E> inline constexpr bool is_expression_v = is_encoder_expression_v<E> || is_encoder_callable_v<E> || std::is_same_v<std::decay_t<E>, char> || std::is_same_v<std::decay_t<E>, char32_t> || std::is_convertible_v<std::decay_t<E>, std::string_view> || std::is_invocable_r_v<bool, std::decay_t<E>, environment&>;
template <class A> inline constexpr bool is_capture_action_v = std::is_invocable_v<std::decay_t<A>, detail::dynamic_cast_if_base_of<environment&>, syntax const&> || std::is_invocable_v<std::decay_t<A>, syntax const&>;
template <class T> inline constexpr bool is_capture_target_v = std::is_same_v<std::decay_t<T>, syntax> || std::is_assignable_v<std::decay_t<T>, syntax const&>;

[[nodiscard]] grammar start(rule const& start_rule);

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
	std::ptrdiff_t eh{-1}; // error handler register
	std::ptrdiff_t rh{-1}; // recovery handler register
	error_response rr{error_response::resume}; // recovery response latch register
	std::ptrdiff_t pc{-1}; // program counter
};

enum class opcode : std::uint_least8_t
{
	jump,           choice,         commit,         commit_back,    commit_partial,
	accept,         call,           ret,            fail,           recover_push,
	recover_pop,    recover_resp,   report_push,    report_pop,     predicate,
	action,         capture_start,  capture_end,    condition_pop,  symbol_end,
	symbol_pop,     match_any,      match_set,      match_eol,
	match,          match_cf,       match_any_of,   match_all_of,   match_none_of,
	condition_test, condition_push, symbol_exists,  symbol_all,     symbol_all_cf,
	symbol_any,     symbol_any_cf,  symbol_head,    symbol_head_cf, symbol_tail,
	symbol_tail_cf, symbol_start,   symbol_push,    raise
};

namespace auxcode {

} // namespace auxcode

struct alignas(std::uint_least64_t) instruction
{
	opcode op;
	std::uint_least8_t immediate8;
	std::uint_least16_t immediate16;
	std::int_least32_t offset32;
};

static_assert(sizeof(instruction) == sizeof(std::uint_least64_t), "expected instruction size to be same size as std::uint_least64_t");
static_assert(alignof(instruction) == alignof(std::uint_least64_t), "expected instruction alignment to be same size as std::uint_least64_t");

struct program
{
	std::vector<instruction> instructions;
	std::vector<char> data;
	std::vector<unicode::rune_set> runesets;
	std::vector<error_handler> handlers;
	std::vector<syntactic_predicate> predicates;
	std::vector<semantic_action> actions;
	std::vector<semantic_capture_action> captures;
	directives entry_mode{directives::eps};

	void concatenate(program const& src)
	{
		std::size_t const data_offset = data.size();
		std::size_t const runesets_offset = runesets.size();
		std::size_t const handlers_offset = handlers.size();
		std::size_t const predicates_offset = predicates.size();
		std::size_t const actions_offset = actions.size();
		std::size_t const captures_offset = captures.size();
		instructions.reserve(detail::checked_add<program_limit_error>(instructions.size(), src.instructions.size()));
		for (auto const& instr : src.instructions) {
			instruction new_instr{instr};
			if (new_instr.op < opcode::match) {
				std::optional<std::size_t> object;
				switch (new_instr.op) {
					case opcode::match_set: object = instr.immediate16 + runesets_offset; break;
					case opcode::report_push: object = instr.immediate16 + handlers_offset; break;
					case opcode::predicate: object = instr.immediate16 + predicates_offset; break;
					case opcode::action: object = instr.immediate16 + actions_offset; break;
					case opcode::capture_end: object = instr.immediate16 + captures_offset; break;
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
		runesets.insert(runesets.end(), src.runesets.begin(), src.runesets.end());
		handlers.insert(handlers.end(), src.handlers.begin(), src.handlers.end());
		predicates.insert(predicates.end(), src.predicates.begin(), src.predicates.end());
		actions.insert(actions.end(), src.actions.begin(), src.actions.end());
		captures.insert(captures.end(), src.captures.begin(), src.captures.end());
		entry_mode = (entry_mode & ~directives::eps) | (entry_mode & src.entry_mode & directives::eps);
	}

	void swap(program& p) noexcept
	{
		instructions.swap(p.instructions);
		data.swap(p.data);
		runesets.swap(p.runesets);
		handlers.swap(p.handlers);
		predicates.swap(p.predicates);
		actions.swap(p.actions);
		captures.swap(p.captures);
		std::swap(entry_mode, p.entry_mode);
	}
};

class rule
{
	friend class encoder;
	friend grammar start(rule const& start_rule);
	program program_;
	program_callees callees_;
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
};

class grammar
{
	friend grammar start(rule const& start_rule);
	lug::program program_;
	explicit grammar(lug::program&& p) noexcept : program_{std::move(p)} {}
public:
	grammar() noexcept = default;
	void swap(grammar& g) noexcept { program_.swap(g.program_); }
	[[nodiscard]] lug::program const& program() const noexcept { return program_; }
	[[nodiscard]] static std::shared_ptr<std::function<void(encoder&)>> const& implicit_space();
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
	friend class parser_base;
	template <class> friend class basic_parser;

	static inline std::vector<std::string> const empty_symbols_{};
	std::vector<std::any> attribute_frame_stack_;
	std::vector<std::any> attribute_result_stack_;
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

	virtual void on_reset() {}
	virtual void on_drain() {}
	virtual void on_accept_started() {}
	virtual void on_accept_ended() {}

	void reset(std::string_view sub)
	{
		origin_ = position_at(match_.size());
		set_match_and_subject(sub.substr(0, 0), sub);
		on_reset();
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

public:
	static constexpr std::uint_least32_t default_tab_width{8};
	static constexpr std::uint_least32_t default_tab_alignment{8};
	environment() = default;
	environment(environment const&) = delete;
	environment(environment&&) noexcept = default;
	environment& operator=(environment const&) = delete;
	environment& operator=(environment&&) noexcept = default;
	virtual ~environment() = default;
	[[nodiscard]] std::uint_least32_t tab_width() const noexcept { return tab_width_; }
	void tab_width(std::uint_least32_t w) noexcept { tab_width_ = w; }
	[[nodiscard]] std::uint_least32_t tab_alignment() const noexcept { return tab_alignment_; }
	void tab_alignment(std::uint_least32_t a) noexcept { tab_alignment_ = a; }
	[[nodiscard]] bool has_condition(std::string_view name) const noexcept { return (conditions_.count(name) > 0); }
	bool set_condition(std::string_view name, bool value) { return value ? (!conditions_.emplace(name).second) : (conditions_.erase(name) > 0); }
	void clear_conditions() { conditions_.clear(); }
	[[nodiscard]] bool has_symbol(std::string_view name) const noexcept { return (symbols_.count(name) > 0); }
	[[nodiscard]] std::vector<std::string> const& get_symbols(std::string_view name) const noexcept { auto it = symbols_.find(name); if (it == symbols_.end()) return empty_symbols_; return it->second; }
	void add_symbol(std::string_view name, std::string value) { symbols_[name].emplace_back(std::move(value)); }
	void clear_symbols(std::string_view name) { symbols_.erase(name); }
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
	void escape() { prune_depth_ = call_depth_; }

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
			if ((unicode::query(rune).properties() & unicode::ptype::Line_Ending) != unicode::ptype::None && (prevrune != U'\r' || rune != U'\n')) {
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

	template <class Frame>
	void push_attribute_frame(Frame const& frame)
	{
		attribute_frame_stack_.emplace_back(std::in_place_type<detail::remove_cvref_from_tuple_t<Frame>>, frame);
	}

	template <class Frame>
	void pop_attribute_frame(Frame& frame)
	{
		if (attribute_frame_stack_.empty())
			throw attribute_stack_error{};
		frame = std::any_cast<detail::remove_cvref_from_tuple_t<Frame>>(detail::pop_back(attribute_frame_stack_));
	}

	template <class T>
	void push_attribute(T&& x)
	{
		attribute_result_stack_.emplace_back(std::in_place_type<T>, std::forward<T>(x));
	}

	template <class T>
	[[nodiscard]] T pop_attribute()
	{
		if (attribute_result_stack_.empty())
			throw attribute_stack_error{};
		return std::any_cast<T>(detail::pop_back(attribute_result_stack_));
	}
};

class error_context
{
	std::reference_wrapper<lug::environment> envr_;
	lug::syntax syntax_;
	std::string_view label_;
	error_response recovery_response_;
public:
	error_context(lug::environment& envr, lug::syntax const& syn, std::string_view lab, error_response resp) : envr_{envr}, syntax_{syn}, label_{lab}, recovery_response_{resp} {}
	error_context(error_context const&) = delete;
	error_context& operator=(error_context const&) = delete;
	error_context(error_context&&) = delete;
	error_context& operator=(error_context&&) = delete;
	~error_context() = default;
	[[nodiscard]] lug::environment& environment() const noexcept { return envr_.get(); }
	[[nodiscard]] lug::syntax const& syntax() const noexcept { return syntax_; }
	[[nodiscard]] std::string_view label() const noexcept { return label_; }
	[[nodiscard]] error_response recovery_response() const noexcept { return recovery_response_; }
	[[nodiscard]] syntax_position position_begin() const { return environment().position_begin(syntax_); }
	[[nodiscard]] syntax_position position_end() const { return environment().position_end(syntax_); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range() const { return environment().position_range(syntax_); }
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

template <class AttributeFrameType = std::tuple<>>
struct encoder_metadata
{
	using attribute_frame_type = AttributeFrameType;
	attribute_frame_type attribute_frame;
	template <class Frame = attribute_frame_type, class = std::enable_if_t<std::is_default_constructible_v<Frame>>>
	constexpr encoder_metadata() noexcept : attribute_frame{} {}
	template <class Frame, class = std::enable_if_t<std::is_constructible_v<attribute_frame_type, Frame&&>>>
	constexpr explicit encoder_metadata(Frame&& frame) noexcept : attribute_frame{std::forward<Frame>(frame)} {}
};

encoder_metadata() -> encoder_metadata<>;
template <class Frame> encoder_metadata(Frame&&) -> encoder_metadata<std::decay_t<Frame>>;

class encoder
{
	static constexpr std::size_t inline_max_instructions{8};
	static constexpr std::size_t inline_max_objects{4};

	rule* rule_{nullptr};
	program* program_{nullptr};
	program_callees* callees_{nullptr};
	std::vector<directives> mode_;
	directives entry_mode_{directives::none};

	template <class Item, class ItemValue, class = std::enable_if_t<std::is_constructible_v<Item, ItemValue&&>>>
	[[nodiscard]] std::uint_least16_t add_item(std::vector<Item>& items, ItemValue&& item)
	{
		items.push_back(std::forward<ItemValue>(item));
		return detail::checked_cast<std::uint_least16_t, resource_limit_error>(items.size() - 1);
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

	std::ptrdiff_t do_call(rule const* r, program const* p, std::ptrdiff_t off, std::uint_least16_t prec)
	{
		directives const callee_mode = mode_.back();
		skip(p->entry_mode ^ directives::eps, directives::noskip);
		callees_->emplace_back(r, p, here(), callee_mode);
		return encode(opcode::call, off, prec, 0);
	}

	void do_skip(directives& last_mode)
	{
		last_mode &= ~(directives::preskip | directives::postskip);
		last_mode |= (directives::lexeme | directives::noskip);
		(*grammar::implicit_space())(*this);
	}

public:
	explicit encoder(program& p, program_callees& c, directives initial) : program_{&p}, callees_{&c}, mode_{initial} {}
	explicit encoder(rule& r) : rule_{&r}, program_{&r.program_}, callees_{&r.callees_}, mode_{directives::eps} { rule_->currently_encoding_ = true; }
	encoder(encoder const&) = delete;
	encoder(encoder&& e) noexcept : rule_{std::exchange(e.rule_, nullptr)}, program_{std::exchange(e.program_, nullptr)}, callees_{std::exchange(e.callees_, nullptr)}, mode_{std::move(e.mode_)}, entry_mode_{e.entry_mode_} {}
	encoder& operator=(encoder const&) = delete;
	encoder& operator=(encoder&& e) noexcept { encoder{std::move(e)}.swap(*this); return *this; }
	~encoder() { if (program_ != nullptr) { program_->entry_mode = entry_mode(); } if (rule_ != nullptr) { rule_->currently_encoding_ = false; } }
	void swap(encoder& e) noexcept { std::swap(rule_, e.rule_); std::swap(program_, e.program_); std::swap(callees_, e.callees_); mode_.swap(e.mode_); std::swap(entry_mode_, e.entry_mode_); }
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
	std::ptrdiff_t match(unicode::rune_set&& runes) { return skip().encode(opcode::match_set, add_item(program_->runesets, std::move(runes)), 0); }
	std::ptrdiff_t match_any() { return skip().encode(opcode::match_any); }
	std::ptrdiff_t match_eps() { return skip(directives::lexeme).encode(opcode::match, std::string_view{}); }

	std::ptrdiff_t call(program const& p, std::uint_least16_t prec, [[maybe_unused]] bool allow_inlining = true)
	{
		return do_call(nullptr, &p, 0, prec);
	}

	std::ptrdiff_t call(rule const& r, std::uint_least16_t prec, bool allow_inlining = true)
	{
		if (auto const& p = r.program_; allow_inlining && (prec <= 0) && !r.currently_encoding_ && r.callees_.empty() &&
										(!p.instructions.empty() && (p.instructions.size() <= inline_max_instructions)) &&
										((p.runesets.size() + p.actions.size() + p.captures.size() + p.predicates.size()) <= inline_max_objects))
			return skip(p.entry_mode, directives::noskip).append(p);
		return do_call(&r, &r.program_, 0, prec);
	}

	template <class M, class T, class... Args>
	[[nodiscard]] auto call_with_frame(M const& m, T&& target, std::uint_least16_t prec, Args&&... args) -> M const&
	{
		if constexpr (std::tuple_size_v<typename M::attribute_frame_type> != 0)
			encode(opcode::action, semantic_action{[frame = m.attribute_frame](environment& envr) { envr.push_attribute_frame(frame); }});
		call(std::forward<T>(target), prec, std::forward<Args>(args)...);
		if constexpr (std::tuple_size_v<typename M::attribute_frame_type> != 0)
			encode(opcode::action, semantic_action{[frame = m.attribute_frame](environment& envr) mutable { envr.pop_attribute_frame(frame); }});
		return m;
	}

	std::ptrdiff_t recover_push_call(rule const& r)
	{
		callees_->emplace_back(&r, &r.program_, here(), mode_.back());
		return encode(opcode::recover_push, 0, 0, 0);
	}

	template <class M, class Recovery>
	[[nodiscard]] decltype(auto) raise_failure(M const& m, failure<Recovery> const& reason)
	{
		if constexpr (is_encoder_expression_v<Recovery>) {
			auto const recovery_subroutine = encode(opcode::recover_push);
			encode(opcode::raise, reason.label(), 1);
			auto const finished = encode(opcode::jump);
			jump_to_here(recovery_subroutine);
			auto m2 = reason.recovery().evaluate(*this, m);
			encode(opcode::ret);
			jump_to_here(finished);
			return m2;
		} else if constexpr (std::is_same_v<Recovery, rule>) {
			recover_push_call(reason.recovery());
			encode(opcode::raise, reason.label(), 1);
			return m;
		} else {
			encode(opcode::raise, reason.label(), 0);
			return m;
		}
	}

	std::ptrdiff_t match(std::string_view subject)
	{
		skip(!subject.empty() ? directives::eps : directives::none);
		if ((mode() & directives::caseless) != directives::none)
			return encode(opcode::match_cf, utf8::tocasefold(subject));
		return encode(opcode::match, subject);
	}

	template <opcode Op, class T, class = std::enable_if_t<unicode::is_property_enum_v<T>>>
	std::ptrdiff_t match_class(T properties)
	{
		return skip().encode(Op, detail::string_pack(properties), static_cast<std::uint_least8_t>(unicode::to_property_enum_v<std::decay_t<T>>));
	}

	void dpsh(directives enable, directives disable)
	{
		directives const prev = mode_.back();
		mode_.push_back((prev & ~disable) | enable);
	}

	void dpop(directives relay)
	{
		directives const prev = detail::pop_back(mode_);
		directives& last_mode = mode_.back();
		directives const next = (last_mode & ~relay) | (prev & relay);
		if (((next & directives::postskip) == directives::none) && ((prev & (directives::lexeme | directives::noskip | directives::postskip)) == directives::postskip))
			do_skip(last_mode);
		mode_.back() = next;
	}

	encoder& skip(directives callee_mode = directives::eps, directives callee_skip = directives::lexeme)
	{
		directives& last_mode = mode_.back();
		directives const prev = last_mode;
		directives const next = last_mode & ~(callee_mode & directives::eps);
		if (entry_mode_ == directives::none)
			entry_mode_ = (prev & (directives::caseless | directives::lexeme | directives::noskip)) | directives::eps;
		if ((((prev | callee_mode)) & (callee_skip | directives::preskip)) == directives::preskip)
			do_skip(last_mode);
		mode_.back() = next;
		return *this;
	}	
};

template <class RuneSet>
inline decltype(auto) add_rune_range(RuneSet&& runes, directives mode, char32_t first, char32_t last)
{
	if (first > last)
		throw bad_character_range{};
	if ((mode & directives::caseless) != directives::none)
		unicode::push_casefolded_range(runes, first, last);
	else
		unicode::push_range(runes, first, last);
	return std::forward<RuneSet>(runes);
}

template <class Derived>
struct common_encoder_expression_interface
{
	using expression_trait = encoder_expression_trait_tag;
	[[nodiscard]] constexpr Derived& derived() noexcept { return static_cast<Derived&>(*this); }
	[[nodiscard]] constexpr const Derived& derived() const noexcept { return static_cast<const Derived&>(*this); }
	template <class Recovery> [[nodiscard]] constexpr auto operator[](failure<Recovery> const& reason) const;
	template <class Recovery> [[nodiscard]] constexpr auto operator[](recover_with<Recovery> const& rec) const;
	template <class Handler, class = std::enable_if_t<is_error_handler_v<Handler>>> [[nodiscard]] constexpr auto operator^=(Handler&& handler) const;
};

template <class Derived>
struct terminal_encoder_expression_interface : common_encoder_expression_interface<Derived> {};

template <class Derived, class E1>
struct unary_encoder_expression_interface : common_encoder_expression_interface<Derived>
{
	E1 e1;
	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr explicit unary_encoder_expression_interface(X1&& x1) : e1(std::forward<X1>(x1)) {}
};

template <class Derived, class E1, class E2>
struct binary_encoder_expression_interface : common_encoder_expression_interface<Derived>
{
	E1 e1;
	E2 e2;
	template <class X1, class X2, class = std::enable_if_t<std::is_constructible_v<E1, X1&&> && std::is_constructible_v<E2, X2&&>>>
	constexpr binary_encoder_expression_interface(X1&& x1, X2&& x2) : e1(std::forward<X1>(x1)), e2(std::forward<X2>(x2)) {}
};

template <class Recovery>
struct raise_expression : terminal_encoder_expression_interface<raise_expression<Recovery>>
{
	failure<Recovery> reason;
	constexpr explicit raise_expression(failure<Recovery> const& fail) noexcept : reason{fail} {}

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		return d.raise_failure(m, reason);
	}
};

template <class E1, class Recovery>
struct expect_expression : unary_encoder_expression_interface<expect_expression<E1, Recovery>, E1>
{
	using base_type = unary_encoder_expression_interface<expect_expression<E1, Recovery>, E1>;
	failure<Recovery> reason;
	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr expect_expression(X1&& x1, failure<Recovery> const& fail) : base_type{std::forward<X1>(x1)}, reason{fail} {}

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice);
		auto m2 = this->e1.evaluate(d, m);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		auto m3 = d.raise_failure(m2, reason);
		d.jump_to_here(commit);
		return m3;
	}
};

template <class E1, class Recovery>
struct recover_with_expression : unary_encoder_expression_interface<recover_with_expression<E1, Recovery>, E1>
{
	using base_type = unary_encoder_expression_interface<recover_with_expression<E1, Recovery>, E1>;
	recover_with<Recovery> rec;
	template <class X1, class R, class = std::enable_if_t<std::is_constructible_v<E1, X1&&> && std::is_constructible_v<recover_with<Recovery>, R&&>>>
	constexpr recover_with_expression(X1&& x1, R&& r) : base_type{std::forward<X1>(x1)}, rec{std::forward<R>(r)} {}

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		if constexpr (is_encoder_expression_v<Recovery>) {
			auto const recovery_subroutine = d.encode(opcode::recover_push);
			auto m2 = this->e1.evaluate(d, m);
			d.encode(opcode::recover_pop);
			auto const finished = d.encode(opcode::jump);
			d.jump_to_here(recovery_subroutine);
			auto m3 = rec.recovery().evaluate(d, m2);
			d.encode(opcode::ret);
			d.jump_to_here(finished);
			return m3;
		} else if constexpr (std::is_same_v<Recovery, rule>) {
			d.recover_push_call(rec.recovery());
			auto m2 = this->e1.evaluate(d, m);
			d.encode(opcode::recover_pop);
			return m2;
		} else {
			return this->e1.evaluate(d, m);
		}
	}
};

struct recover_response_expression : terminal_encoder_expression_interface<recover_response_expression>
{
	error_response response;
	constexpr explicit recover_response_expression(error_response r) noexcept : response{r} {}

	template <class M>
	[[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const&
	{
		d.encode(opcode::recover_resp, 0, static_cast<std::uint_least8_t>(response));
		return m;
	}
};

template <class E1, class Handler>
struct report_expression : unary_encoder_expression_interface<report_expression<E1, Handler>, E1>
{
	using base_type = unary_encoder_expression_interface<report_expression<E1, Handler>, E1>;
	using base_type::base_type;
	Handler handler;

	template <class X1, class H, class = std::enable_if_t<std::is_constructible_v<Handler, H&&>>>
	constexpr report_expression(X1&& x1, H&& h) : base_type{std::forward<X1>(x1)}, handler{std::forward<H>(h)} {}

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		if constexpr (std::is_invocable_r_v<error_response, Handler, error_context&>)
			d.encode(opcode::report_push, error_handler{handler});
		else
			d.encode(opcode::report_push, error_handler{[h = handler](error_context& e) -> error_response { (void)h(e); return e.recovery_response(); }});
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::report_pop);
		return m2;
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

class basic_regular_expression : public terminal_encoder_expression_interface<basic_regular_expression>
{
	std::string expression_;
	std::shared_ptr<program> program_;

	[[nodiscard]] static grammar make_grammar();

	struct generator final : environment
	{
		lug::program_callees callees;
		lug::encoder encoder;
		unicode::rune_set runes;
		unicode::ctype classes{unicode::ctype::none};
		bool circumflex{false};

		generator(basic_regular_expression const& se, directives mode)
			: encoder{*se.program_, callees, mode | directives::eps | directives::lexeme} {}

		void bracket_class(std::string_view s)
		{
			if (auto c = unicode::stoctype(s); c.has_value())
				classes |= c.value();
			else
				throw bad_character_class{};
		}

		void bracket_range(std::string_view s)
		{
			bracket_range(s.substr(0, s.find('-')), s.substr(s.find('-') + 1));
		}

		void bracket_range(std::string_view a, std::string_view b)
		{
			add_rune_range(std::ref(runes), encoder.mode(),
				utf8::decode_rune(std::begin(a), std::end(a)).second,
				utf8::decode_rune(std::begin(b), std::end(b)).second);
		}

		void bracket_commit()
		{
			runes = unicode::sort_and_optimize(std::move(runes));
			if (!runes.empty() && classes == unicode::ctype::none) {
				if (circumflex)
					runes = unicode::negate(runes);
				encoder.match(std::move(runes));
			} else {
				if (circumflex)
					encoder.encode(opcode::choice, 2 + (!runes.empty() ? 1 : 0) + (classes != unicode::ctype::none ? 1 : 0), 0, 0);
				if (!runes.empty())
					encoder.match(std::move(runes));
				if (classes != unicode::ctype::none)
					encoder.match_class<opcode::match_any_of>(classes);
				if (circumflex) {
					encoder.encode(opcode::commit);
					encoder.encode(opcode::fail, 0, 1);
					encoder.match_any();
				}
			}
			runes.clear();
			classes = unicode::ctype::none;
			circumflex = false;
		}
	};

public:
	explicit basic_regular_expression(std::string_view e) : expression_{e}, program_{std::make_shared<program>()} {}
	template <class M> [[nodiscard]] auto evaluate(encoder& d, M const& m) const -> M const&;
};

struct string_expression : terminal_encoder_expression_interface<string_expression>
{
	std::string_view text;
	constexpr explicit string_expression(std::string_view t) noexcept : text{t} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(text); return m; }
};

struct char_expression : terminal_encoder_expression_interface<char_expression>
{
	char c;
	constexpr explicit char_expression(char x) noexcept : c{x} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(std::string_view{&c, 1}); return m; }
};

struct char32_expression : terminal_encoder_expression_interface<char32_expression>
{
	char32_t c;
	constexpr explicit char32_expression(char32_t x) noexcept : c{x} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(utf8::encode_rune(c)); return m; }
};

struct char32_range_expression : terminal_encoder_expression_interface<char32_range_expression>
{
	char32_t start;
	char32_t end;
	constexpr char32_range_expression(char32_t first, char32_t last) noexcept : start{first}, end{last} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(unicode::sort_and_optimize(add_rune_range(unicode::rune_set{}, d.mode(), start, end))); return m; }
};

template <class Target>
struct callable_expression : terminal_encoder_expression_interface<callable_expression<Target>>
{
	std::reference_wrapper<Target> target;
	constexpr explicit callable_expression(Target& t) noexcept : target{t} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { return d.call_with_frame(m, target.get(), 0); }
};

template <class T> struct is_callable_encoder_expression : std::false_type {};
template <class T> struct is_callable_encoder_expression<callable_expression<T>> : std::true_type {};
template <class T> inline constexpr bool is_callable_encoder_expression_v = is_callable_encoder_expression<T>::value;

template <class Pred>
struct predicate_expression : terminal_encoder_expression_interface<predicate_expression<Pred>>
{
	Pred pred;
	template <class P, class = std::enable_if_t<std::is_constructible_v<Pred, P&&>>> constexpr explicit predicate_expression(P&& p) noexcept(std::is_nothrow_constructible_v<Pred, P&&>) : pred(std::forward<P>(p)) {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::predicate, syntactic_predicate{pred}); return m; }
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
		return char32_expression{std::forward<E>(e)};
	else if constexpr (std::is_convertible_v<std::decay_t<E>, std::string_view>)
		return string_expression{std::forward<E>(e)}; // NOLINT(cppcoreguidelines-pro-bounds-array-to-pointer-decay,hicpp-no-array-decay)
	else if constexpr (std::is_invocable_r_v<bool, std::decay_t<E>, environment&>)
		return predicate_expression{std::forward<E>(e)};
	else
		static_assert(detail::always_false_v<E>, "invalid expression type");
}

template <class E, class = std::enable_if_t<is_expression_v<E>>>
[[nodiscard]] constexpr auto make_space_expression(E const& e)
{
	return [x = make_expression(e)](encoder& d) { (void)x.evaluate(d, encoder_metadata{}); };
}

template <class E, class>
inline rule::rule(E const& e)
{
	encoder rule_encoder{*this};
	(void)make_expression(e).evaluate(rule_encoder, encoder_metadata{});
}

inline rule::rule(rule const& r)
{
	encoder rule_encoder{*this};
	rule_encoder.call(r, 1);
}

struct rule_precedence_expression : terminal_encoder_expression_interface<rule_precedence_expression>
{
	std::reference_wrapper<rule const> target;
	std::uint_least16_t prec;
	rule_precedence_expression(rule const& t, std::uint_least16_t p) noexcept : target{t}, prec{p} {}
	template <class M> [[nodiscard]] auto evaluate(encoder& d, M const& m) const -> M const& { return d.call_with_frame(m, target.get(), prec); }
};

[[nodiscard]] inline auto rule::operator[](std::uint_least16_t prec) const noexcept
{
	return rule_precedence_expression{*this, prec};
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

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		d.dpsh(enable_mask, disable_mask);
		auto m2 = this->e1.evaluate(d, m);
		d.dpop(relay_mask);
		return m2;
	}
};

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

inline constexpr directive_modifier<directives::none, directives::none, directives::none> matches_eps{};
inline constexpr directive_modifier<directives::none, directives::none, directives::eps> relays_eps{};
inline constexpr directive_modifier<directives::postskip, directives::none, directives::eps> skip_after{};
inline constexpr directive_modifier<directives::preskip, directives::postskip, directives::eps> skip_before{};

struct accept_expression : terminal_encoder_expression_interface<accept_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::accept, 0, static_cast<std::uint_least8_t>(registers::ignore_errors_flag >> registers::ignore_errors_shift)); return m; } };
struct cut_expression : terminal_encoder_expression_interface<cut_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::accept, 0, static_cast<std::uint_least8_t>(registers::inhibited_flag >> registers::ignore_errors_shift)); return m; } };
struct nop_expression : terminal_encoder_expression_interface<nop_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& /*d*/, M const& m) const -> M const& { return m; } };
struct eps_expression : terminal_encoder_expression_interface<eps_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_eps(); return m; } };
struct eoi_expression : terminal_encoder_expression_interface<eoi_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::choice, 2, 0, 0); d.encode(opcode::match_any, 1, 0); d.encode(opcode::fail, 0, 2); return m; } };
struct eol_expression : terminal_encoder_expression_interface<eol_expression> { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::match_eol); return m; } };

template <opcode Op>
struct match_class_combinator
{
	template <class Property>
	struct match_class_expression : terminal_encoder_expression_interface<match_class_expression<Property>>
	{
		Property property;
		constexpr explicit match_class_expression(Property p) noexcept : property{p} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_class<Op>(property); return m; }
	};

	template <class Property, class = std::enable_if_t<unicode::is_property_enum_v<Property>>>
	[[nodiscard]] constexpr match_class_expression<std::decay_t<Property>> operator()(Property p) const { return match_class_expression<std::decay_t<Property>>{p}; }
};

struct match_any_expression : terminal_encoder_expression_interface<match_any_expression>, match_class_combinator<opcode::match_any_of>
{
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_any(); return m; }
};

template <unicode::ctype Property>
struct ctype_expression : terminal_encoder_expression_interface<ctype_expression<Property>>
{
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_class<opcode::match_any_of>(Property); return m; }
};

template <bool Value>
struct condition_test_combinator
{
	struct condition_test_expression : terminal_encoder_expression_interface<condition_test_expression>
	{
		std::string_view name;
		constexpr explicit condition_test_expression(std::string_view n) noexcept : name{n} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::condition_test, name, Value ? 1 : 0); return m; }
	};

	[[nodiscard]] constexpr condition_test_expression operator()(std::string_view name) const noexcept { return condition_test_expression{name}; }
};

template <bool Value>
struct condition_block_combinator
{
	template <class E1>
	struct condition_block_expression : unary_encoder_expression_interface<condition_block_expression<E1>, E1>
	{
		using base_type = unary_encoder_expression_interface<condition_block_expression<E1>, E1>;
		using base_type::base_type;
		std::string_view name;
		constexpr condition_block_expression(E1 const& x1, std::string_view n) noexcept : base_type{x1}, name{n} {}

		template <class M>
		[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
		{
			d.encode(opcode::condition_push, name, Value ? 1 : 0);
			auto m2 = this->e1.evaluate(d, m);
			d.encode(opcode::condition_pop);
			return m2;
		}
	};

	struct condition_block_group
	{
		std::string_view name;

		template <class E, class = std::enable_if_t<is_expression_v<E>>>
		[[nodiscard]] constexpr auto operator[](E const& e) const noexcept
		{
			return condition_block_expression<std::decay_t<decltype(make_expression(e))>>{make_expression(e), name};
		}
	};

	[[nodiscard]] constexpr condition_block_group operator()(std::string_view name) const noexcept { return condition_block_group{name}; }
};

template <bool Value>
struct symbol_exists_combinator
{
	struct symbol_exists_expression : terminal_encoder_expression_interface<symbol_exists_expression>
	{
		std::string_view name;
		constexpr explicit symbol_exists_expression(std::string_view n) noexcept : name{n} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::symbol_exists, name, Value ? 1 : 0); return m; }
	};

	[[nodiscard]] constexpr symbol_exists_expression operator()(std::string_view name) const noexcept { return symbol_exists_expression{name}; }
};

template <opcode Op, opcode OpCf>
struct symbol_match_combinator
{
	struct symbol_match_expression : terminal_encoder_expression_interface<symbol_match_expression>
	{
		std::string_view name;
		constexpr explicit symbol_match_expression(std::string_view n) noexcept : name{n} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.skip(directives::eps).encode(((d.mode() & directives::caseless) != directives::none) ? OpCf : Op, name); return m; }
	};

	[[nodiscard]] constexpr symbol_match_expression operator()(std::string_view name) const noexcept { return symbol_match_expression{name}; }
};

template <opcode Op, opcode OpCf>
struct symbol_match_offset_combinator
{
	struct symbol_match_offset_expression : terminal_encoder_expression_interface<symbol_match_offset_expression>
	{
		std::string_view name;
		std::uint_least8_t offset;
		constexpr symbol_match_offset_expression(std::string_view n, std::uint_least8_t o) noexcept : name{n}, offset{o} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.skip(directives::eps).encode(((d.mode() & directives::caseless) != directives::none) ? OpCf : Op, name, offset); return m; }
	};

	[[nodiscard]] constexpr symbol_match_offset_expression operator()(std::string_view name, std::size_t offset = 0) const
	{
		return symbol_match_offset_expression{name, detail::checked_cast<std::uint_least8_t, resource_limit_error>(offset)};
	}
};

template <class E1>
struct negative_lookahead_expression : unary_encoder_expression_interface<negative_lookahead_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<negative_lookahead_expression<E1>, E1>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice, 0, 1);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::fail, 0, 2);
		d.jump_to_here(choice);
		return m2;
	}
};

template <class E1>
struct positive_lookahead_expression : unary_encoder_expression_interface<positive_lookahead_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<positive_lookahead_expression<E1>, E1>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice, 0, 1);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::commit_back, 1, 0, 0);
		d.jump_to_here(choice);
		d.encode(opcode::fail, 0, 1);
		return m2;
	}
};

template <class E1>
struct repetition_expression : unary_encoder_expression_interface<repetition_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<repetition_expression<E1>, E1>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice);
		auto const expression = d.here();
		auto m2 = this->e1.evaluate(d, m);
		auto const commit = d.encode(opcode::commit_partial);
		d.jump_to_here(choice);
		d.jump_to_target(commit, expression);
		return m2;
	}
};

template <class E1, class E2>
struct choice_expression : binary_encoder_expression_interface<choice_expression<E1, E2>, E1, E2>
{
	using base_type = binary_encoder_expression_interface<choice_expression<E1, E2>, E1, E2>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice);
		auto m2 = this->e1.evaluate(d, m);
		auto const commit = d.encode(opcode::commit);
		d.jump_to_here(choice);
		auto m3 = this->e2.evaluate(d, m2);
		d.jump_to_here(commit);
		return m3;
	}
};

template <class E1, class E2>
struct sequence_expression : binary_encoder_expression_interface<sequence_expression<E1, E2>, E1, E2>
{
	using base_type = binary_encoder_expression_interface<sequence_expression<E1, E2>, E1, E2>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		return this->e2.evaluate(d, this->e1.evaluate(d, m));
	}
};

template <class Derived, class E1, class Operand>
struct attribute_action_expression : unary_encoder_expression_interface<Derived, E1>
{
	using base_type = unary_encoder_expression_interface<Derived, E1>;
	using base_type::base_type;
	Operand operand;
	template <class X1, class O> constexpr attribute_action_expression(X1&& x1, O&& o) : base_type{std::forward<X1>(x1)}, operand(std::forward<O>(o)) {}

	template <class M>
	[[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const
	{
		if constexpr (is_callable_encoder_expression_v<std::decay_t<E1>> && (std::tuple_size_v<typename M::attribute_frame_type> != 0)) {
			d.encode(opcode::action, semantic_action{[frame = m.attribute_frame](environment& envr) { envr.push_attribute_frame(frame); }});
			static_cast<Derived const&>(*this).do_prologue(d); d.call(this->e1.target, 0); static_cast<Derived const&>(*this).do_epilogue_inlined(d, m); return m;
		} else {
			static_cast<Derived const&>(*this).do_prologue(d); auto m2 = this->e1.evaluate(d, m); static_cast<Derived const&>(*this).do_epilogue(d); return m2;
		}
	}
};

template <class Derived, class E1, class Target>
struct attribute_bind_to_expression : attribute_action_expression<Derived, E1, Target*>
{
	using base_type = attribute_action_expression<Derived, E1, Target*>;
	using base_type::base_type;
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const { return encoder_metadata{std::tuple_cat((attribute_action_expression<Derived, E1, Target*>::evaluate(d, m)).attribute_frame, std::forward_as_tuple(*(this->operand)))}; }
};

template <class E1, class Action>
struct action_expression : attribute_action_expression<action_expression<E1, Action>, E1, Action>
{
	using base_type = attribute_action_expression<action_expression<E1, Action>, E1, Action>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[a = this->operand](environment& envr) { a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::action, semantic_action{[f = m.attribute_frame, a = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
};

template <class E1, class Action>
struct capture_expression : attribute_action_expression<capture_expression<E1, Action>, E1, Action>
{
	using base_type = attribute_action_expression<capture_expression<E1, Action>, E1, Action>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[a = this->operand](environment& envr, syntax const& sx) { a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::capture_end, semantic_capture_action{[f = m.attribute_frame, a = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
};

template <class E1, class Target>
struct assign_to_expression : attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>
{
	using base_type = attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[t = this->operand](environment& envr) { *t = envr.pop_attribute<Target>(); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::action, semantic_action{[f = m.attribute_frame, t = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); *t = envr.pop_attribute<Target>(); }}); }
};

template <class E1, class Target>
struct capture_to_expression : attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>
{
	using base_type = attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>;
	using base_type::base_type;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[t = this->operand](environment&, syntax const& sx) { *t = sx; }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::capture_end, semantic_capture_action{[f = m.attribute_frame, t = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); *t = sx; }}); }
};

template <class E1>
struct symbol_assign_expression : unary_encoder_expression_interface<symbol_assign_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<symbol_assign_expression<E1>, E1>;
	std::string_view name;
	template <class X1> constexpr symbol_assign_expression(X1&& x1, std::string_view n) : base_type{std::forward<X1>(x1)}, name{n} {}
	template <class M> [[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const { d.skip().encode(opcode::symbol_start, name); auto m2 = this->e1.evaluate(d, m); d.encode(opcode::symbol_end); return m2; }
};

template <class E1>
struct symbol_block_expression : unary_encoder_expression_interface<symbol_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<symbol_block_expression<E1>, E1>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		d.skip().encode(opcode::symbol_push);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::symbol_pop);
		return m2;
	}
};

template <class E1>
struct local_block_expression : unary_encoder_expression_interface<local_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<local_block_expression<E1>, E1>;
	using base_type::base_type;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		d.skip().encode(opcode::symbol_push, 0, 2);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::symbol_pop);
		return m2;
	}
};

template <class E1>
struct local_to_block_expression : unary_encoder_expression_interface<local_to_block_expression<E1>, E1>
{
	using base_type = unary_encoder_expression_interface<local_to_block_expression<E1>, E1>;
	using base_type::base_type;
	std::string_view name;
	constexpr local_to_block_expression(E1 const& x1, std::string_view n) noexcept : base_type{x1}, name{n} {}

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		d.skip().encode(opcode::symbol_push, name, 1);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::symbol_pop);
		return m2;
	}
};

template <class X1> negative_lookahead_expression(X1&&) -> negative_lookahead_expression<std::decay_t<X1>>;
template <class X1> positive_lookahead_expression(X1&&) -> positive_lookahead_expression<std::decay_t<X1>>;
template <class X1> repetition_expression(X1&&) -> repetition_expression<std::decay_t<X1>>;
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

namespace language {

using environment = lug::environment; using grammar = lug::grammar; using rule = lug::rule; using lug::start;
using error_context = lug::error_context; using error_response = lug::error_response; using lug::recover_with; using lug::failure;
using syntax = lug::syntax; using syntax_position = lug::syntax_position; using syntax_range = lug::syntax_range;
using unicode::ctype; using unicode::ptype; using unicode::gctype; using unicode::sctype; using unicode::blktype; using unicode::agetype; using unicode::eawtype;
inline constexpr directive_modifier<directives::none, directives::caseless, directives::eps> cased{};
inline constexpr directive_modifier<directives::caseless, directives::none, directives::eps> caseless{};
inline constexpr directive_modifier<directives::lexeme, directives::noskip, directives::eps> lexeme{};
inline constexpr directive_modifier<directives::lexeme | directives::noskip, directives::none, directives::eps> noskip{};
inline constexpr directive_modifier<directives::none, directives::lexeme | directives::noskip, directives::eps> skip{};
inline constexpr accept_expression accept{}; inline constexpr cut_expression cut{}; inline constexpr nop_expression nop{};
inline constexpr eps_expression eps{}; inline constexpr eoi_expression eoi{}; inline constexpr eol_expression eol{};
inline constexpr match_any_expression any{}; inline constexpr match_class_combinator<opcode::match_all_of> all{}; inline constexpr match_class_combinator<opcode::match_none_of> none{};
inline constexpr ctype_expression<ctype::alpha> alpha{}; inline constexpr ctype_expression<ctype::alnum> alnum{}; inline constexpr ctype_expression<ctype::lower> lower{};
inline constexpr ctype_expression<ctype::upper> upper{}; inline constexpr ctype_expression<ctype::digit> digit{}; inline constexpr ctype_expression<ctype::xdigit> xdigit{};
inline constexpr ctype_expression<ctype::space> space{}; inline constexpr ctype_expression<ctype::blank> blank{}; inline constexpr ctype_expression<ctype::punct> punct{};
inline constexpr ctype_expression<ctype::graph> graph{}; inline constexpr ctype_expression<ctype::print> print{}; inline constexpr ctype_expression<ctype::cntrl> cntrl{};
inline constexpr condition_test_combinator<true> when{}; inline constexpr condition_test_combinator<false> unless{};
inline constexpr condition_block_combinator<true> on{}; inline constexpr condition_block_combinator<false> off{};
inline constexpr symbol_exists_combinator<true> exists{}; inline constexpr symbol_exists_combinator<false> missing{};
inline constexpr symbol_match_offset_combinator<opcode::symbol_head, opcode::symbol_head_cf> match_front{};
inline constexpr symbol_match_offset_combinator<opcode::symbol_tail, opcode::symbol_tail_cf> match_back{};
inline constexpr symbol_match_combinator<opcode::symbol_all, opcode::symbol_all_cf> match_all{};
inline constexpr symbol_match_combinator<opcode::symbol_any, opcode::symbol_any_cf> match_any{};
inline constexpr symbol_match_combinator<opcode::symbol_tail, opcode::symbol_tail_cf> match{};

inline constexpr struct
{
	[[nodiscard]] basic_regular_expression operator()(std::string_view s) const { return basic_regular_expression{s}; }
	[[nodiscard]] basic_regular_expression operator()(char const* s, std::size_t n) const { return basic_regular_expression{std::string_view{s, n}}; }
}
bre{};

inline constexpr struct
{
	[[nodiscard]] constexpr char_expression operator()(char c) const noexcept { return char_expression{c}; }
	[[nodiscard]] constexpr char32_expression operator()(char32_t c) const noexcept { return char32_expression{c}; }
	[[nodiscard]] constexpr char32_range_expression operator()(char32_t start, char32_t end) const noexcept { return char32_range_expression{start, end}; }
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
[[nodiscard]] inline auto operator ""_rx(char const* s, std::size_t n) { return basic_regular_expression{std::string_view{s, n}}; }
[[nodiscard]] constexpr auto operator ""_icx(char c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_icx(char32_t c) { return caseless[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_isx(char const* s, std::size_t n) { return caseless[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] inline auto operator ""_irx(char const* s, std::size_t n) { return caseless[basic_regular_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_scx(char c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_scx(char32_t c) { return cased[chr(c)]; }
[[nodiscard]] constexpr auto operator ""_ssx(char const* s, std::size_t n) { return cased[string_expression{std::string_view{s, n}}]; }
[[nodiscard]] inline auto operator ""_srx(char const* s, std::size_t n) { return cased[basic_regular_expression{std::string_view{s, n}}]; }
[[nodiscard]] constexpr auto operator ""_fail(char const* s, std::size_t n) { return failure{std::string_view{s, n}}; }

template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator!(E const& e) { return negative_lookahead_expression{matches_eps[e]}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator&(E const& e) { return positive_lookahead_expression{matches_eps[e]}; } // NOLINT(google-runtime-operator)
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator*(E const& e) { return repetition_expression{matches_eps[skip_after[e]]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator|(E1 const& e1, E2 const& e2) { return choice_expression{relays_eps[e1], relays_eps[e2]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator>(E1 const& e1, E2 const& e2) { return sequence_expression{make_expression(e1), skip_before[e2]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator>>(E1 const& e1, E2 const& e2) { return e1 > *(e2 > e1); }
template <class T, class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator%(T& target, E const& e) { return assign_to_expression{make_expression(e), std::addressof(target)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator^(E const& e, error_response r) { return e > recover_response_expression{r}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator+(E const& e) { auto const& x = make_expression(e); return x > *x; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator~(E const& e) { return e | eps; }
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
auto sync(Pattern const& pattern)
{
	return noskip[*(!pattern > any) ^ Response];
}

template <error_response Response = error_response::resume, class Pattern, class DefaultValue, class = std::enable_if_t<is_expression_v<Pattern>>>
auto sync(Pattern const& pattern, DefaultValue&& default_value)
{
	auto default_value_action = [value = std::forward<DefaultValue>(default_value)] {
		if constexpr (std::is_invocable_v<std::add_const_t<std::decay_t<DefaultValue>>>)
			return value();
		else
			return value;
	};
	return noskip[*(!pattern > any) < std::move(default_value_action) ^ Response];
}

class implicit_space_rule
{
	std::function<void(encoder&)> prev_rule_;
	std::weak_ptr<std::function<void(encoder&)>> implicit_space_ref_;

public:
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	implicit_space_rule(E const& e) // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
		: prev_rule_{std::exchange(*grammar::implicit_space(), std::function<void(encoder&)>{make_space_expression(e)})}
		, implicit_space_ref_{grammar::implicit_space()} {}

	~implicit_space_rule()
	{
		if (auto const implicit_space_instance = implicit_space_ref_.lock(); implicit_space_instance)
			*implicit_space_instance = std::move(prev_rule_);
	}

	implicit_space_rule(implicit_space_rule const&) = delete;
	implicit_space_rule(implicit_space_rule&&) = delete;
	implicit_space_rule& operator=(implicit_space_rule const&) = delete;
	implicit_space_rule& operator=(implicit_space_rule&&) = delete;
};

} // namespace language

[[nodiscard]] inline std::shared_ptr<std::function<void(encoder&)>> const& grammar::implicit_space()
{
	static thread_local std::shared_ptr<std::function<void(encoder&)>> const instance{std::make_shared<std::function<void(encoder&)>>(make_space_expression(language::operator*(language::space)))};
	return instance;
}

[[nodiscard]] inline grammar start(rule const& start_rule)
{
	program grprogram;
	program_callees grcallees;
	encoder grencoder{grprogram, grcallees, directives::eps | directives::preskip};
	grencoder.skip(start_rule.program_.entry_mode, directives::noskip);
	std::vector<std::pair<std::vector<std::pair<rule const*, bool>>, program const*>> unprocessed{{std::vector<std::pair<rule const*, bool>>{{&start_rule, false}}, &start_rule.program_}};
	std::vector<std::pair<program const*, std::ptrdiff_t>> calls;
	std::unordered_map<program const*, std::ptrdiff_t> addresses;
	std::unordered_set<program const*> left_recursive;
	std::optional<std::ptrdiff_t> halt_address;
	do {
		auto const&& [callstack, subprogram] = detail::pop_back(unprocessed);
		if (auto const address = grencoder.here(); addresses.emplace(subprogram, address).second) {
			grencoder.append(*subprogram);
			if (!halt_address)
				halt_address = grencoder.encode(opcode::jump);
			else
				grencoder.encode(opcode::ret);
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
					} else {
						auto callee_callstack = callstack;
						callee_callstack.emplace_back(callee_rule, (callee_mode & directives::eps) != directives::none);
						unprocessed.emplace_back(std::move(callee_callstack), callee_program);
					}
				}
			}
		}
	} while (!unprocessed.empty());
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
	if (halt_address)
		grencoder.jump_to_here(*halt_address);
	grprogram.data.emplace_back('\0');
	return grammar{std::move(grprogram)};
}

enum class source_options : std::uint_least8_t { none = 0, interactive = 1, is_bitfield_enum };

namespace detail {

template <typename T, typename = void> struct input_source_enqueue_drains : std::false_type {};
template <typename T> struct input_source_enqueue_drains<T, std::enable_if_t<T::enqueue_drains::value>> : std::true_type {};
template <typename T, typename = void> struct input_source_has_options : std::false_type {};
template <typename T> struct input_source_has_options<T, std::enable_if_t<std::is_same_v<source_options, decltype(std::declval<T const&>().options())>>> : std::true_type {};
template <typename T, typename = void> struct input_source_has_fill_buffer : std::false_type {};
template <typename T> struct input_source_has_fill_buffer<T, std::enable_if_t<std::is_same_v<bool, decltype(std::declval<T>().fill_buffer())>>> : std::true_type {};
template <typename T, class It, typename = void> struct input_source_has_enqueue : std::false_type {};
template <typename T, class It> struct input_source_has_enqueue<T, It, std::void_t<decltype(std::declval<T>().enqueue(std::declval<It>(), std::declval<It>()))>> : std::true_type {};
template <typename T, class InputFunc, typename = void> struct input_source_has_push_source : std::false_type {};
template <typename T, class InputFunc> struct input_source_has_push_source<T, InputFunc, std::void_t<decltype(std::declval<T>().push_source(std::declval<InputFunc>(), std::declval<source_options>()))>> : std::true_type {};

} // namespace detail

class multi_input_source
{
	std::string buffer_;
	std::vector<std::pair<std::function<bool(std::string&)>, source_options>> sources_;
	bool reading_{false};

public:
	[[nodiscard]] source_options options() const noexcept { return !sources_.empty() ? sources_.back().second : source_options::none; }
	[[nodiscard]] std::string_view buffer() const noexcept { return buffer_; }
	void drain_buffer(std::size_t sr) { buffer_.erase(0, sr); }

	[[nodiscard]] bool fill_buffer()
	{
		if (sources_.empty())
			return false;
		std::string text;
		detail::reentrancy_sentinel<reenterant_read_error> const guard{reading_};
		while (!sources_.empty() && text.empty()) {
			text.clear();
			bool const more = sources_.back().first(text);
			buffer_.insert(buffer_.end(), text.begin(), text.end());
			if (!more)
				sources_.pop_back();
		}
		return !text.empty();
	}

	template <class InputIt, class = detail::enable_if_char_input_iterator_t<InputIt>>
	void enqueue(InputIt first, InputIt last)
	{
		buffer_.insert(buffer_.end(), first, last);
	}

	template <class InputRng, class = detail::enable_if_char_input_range_t<InputRng>>
	void enqueue(InputRng&& rng) // NOLINT(cppcoreguidelines-missing-std-forward)
	{
		enqueue(rng.begin(), rng.end());
	}

	template <class InputFunc, class = std::enable_if_t<std::is_invocable_r_v<bool, InputFunc, std::string&>>>
	void push_source(InputFunc&& func, source_options opt = source_options::none)
	{
		if (reading_)
			throw reenterant_read_error{};
		sources_.emplace_back(std::forward<InputFunc>(func), opt);
	}
};

class string_input_source
{
	std::string buffer_;
public:
	[[nodiscard]] std::string_view buffer() const noexcept { return buffer_; }
	void drain_buffer(std::size_t sr) { buffer_.erase(0, sr); }
	template <class It, class = detail::enable_if_char_input_iterator_t<It>> void enqueue(It first, It last) { buffer_.insert(buffer_.end(), first, last); }
	template <class Rng, class = detail::enable_if_char_input_range_t<Rng>> void enqueue(Rng&& rng) { enqueue(rng.begin(), rng.end()); } // NOLINT(cppcoreguidelines-missing-std-forward)
};

class string_view_input_source
{
	std::string_view buffer_;
public:
	using enqueue_drains = std::true_type;
	[[nodiscard]] constexpr std::string_view buffer() const noexcept { return buffer_; }
	constexpr void drain_buffer(std::size_t sr) noexcept { buffer_.remove_prefix(sr); }
	template <class It, class = detail::enable_if_char_contiguous_iterator_t<It>> void enqueue(It first, It last) { buffer_ = (last > first) ? std::string_view{&(*first), static_cast<std::size_t>(last - first)} : std::string_view{}; }
	template <class Rng, class = detail::enable_if_char_contiguous_range_t<Rng>> void enqueue(Rng&& rng) { enqueue(rng.begin(), rng.end()); } // NOLINT(cppcoreguidelines-missing-std-forward)
};

class parser_base
{
protected:
	static constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();
	static constexpr std::size_t max_size = (std::numeric_limits<std::size_t>::max)();

	struct action_response { std::size_t call_depth{0}; std::size_t action_index{0}; syntax_range range{0, 0}; constexpr action_response() noexcept = default; constexpr action_response(std::size_t c, std::size_t a, syntax_range const& r) noexcept : call_depth{c}, action_index{a}, range{r} {} };
	struct backtrack_frame { std::size_t sr; std::size_t rc; std::size_t ri; std::ptrdiff_t pc; constexpr backtrack_frame(std::size_t s, std::size_t r, std::size_t i, std::ptrdiff_t p) noexcept : sr{s}, rc{r}, ri{i}, pc{p} {} };
	struct call_frame { std::ptrdiff_t pc; constexpr explicit call_frame(std::ptrdiff_t p) noexcept : pc{p} {} };
	struct capture_frame { std::size_t sr; constexpr explicit capture_frame(std::size_t s) noexcept : sr{s} {} };
	struct condition_frame { std::string_view name; bool value; constexpr condition_frame(std::string_view n, bool v) noexcept : name{n}, value{v} {} };
	struct lrmemo_frame { std::size_t srr; std::size_t sra; std::size_t prec; std::ptrdiff_t pcr; std::ptrdiff_t pca; std::size_t rcr; std::vector<action_response> responses; lrmemo_frame(std::size_t sr, std::size_t sa, std::size_t p, std::ptrdiff_t pc, std::ptrdiff_t pa, std::size_t rc) noexcept : srr{sr}, sra{sa}, prec{p}, pcr{pc}, pca{pa}, rcr{rc} {} };
	struct raise_frame { std::string_view label;std::size_t sr; std::size_t rc; std::ptrdiff_t eh; std::ptrdiff_t pc; constexpr explicit raise_frame(std::string_view lab, std::size_t s, std::size_t r, std::ptrdiff_t e, std::ptrdiff_t p) noexcept : label{lab}, sr{s}, rc{r}, eh{e}, pc{p} {} };
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
	bool parsing_{false};
	bool success_{false};
	// NOLINTEND(cppcoreguidelines-non-private-member-variables-in-classes,misc-non-private-member-variables-in-classes)

	template <opcode Opcode>
	void commit(std::ptrdiff_t off)
	{
		if (stack_frames_.empty())
			throw bad_stack{};
		auto& backtrack = std::get<backtrack_frame>(stack_frames_.back());
		if constexpr (Opcode == opcode::commit_partial) {
			backtrack.sr = registers_.sr;
			backtrack.rc = registers_.rc;
		} else {
			if constexpr (Opcode == opcode::commit_back) {
				registers_.sr = backtrack.sr;
				registers_.ri = backtrack.ri;
			}
			stack_frames_.pop_back();
		}
		registers_.pc += off;
	}

	void pop_responses_after(std::size_t n)
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

	[[nodiscard]] std::size_t push_response(std::size_t call_depth, std::size_t action_index, syntax_range const& range = {parser_base::max_size, 0})
	{
		responses_.emplace_back(call_depth, action_index, range);
		return responses_.size();
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

	void do_accept(std::string_view match)
	{
		detail::scope_exit const cleanup{[this, prior_call_depth = environment_->start_accept()]{
			environment_->end_accept(prior_call_depth);
			responses_.clear();
			registers_.rc = 0;
		}};
		for (auto& resp : responses_) {
			if (environment_->accept_response(resp.call_depth)) {
				if (resp.range.index < parser_base::max_size)
					program_->captures[resp.action_index](*environment_, syntax{match.substr(resp.range.index, resp.range.size), resp.range.index});
				else
					program_->actions[resp.action_index](*environment_);
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
		success_ = true;
		registers_.sr = 0;
		registers_.mr = 0;
		registers_.rc = 0;
		registers_.cd = 0;
		registers_.ci = 0;
		registers_.ri = 0;
		registers_.eh = -1;
		registers_.rh = -1;
		registers_.rr = error_response::resume;
		registers_.pc = 0;
		casefolded_subjects_.clear();
		responses_.clear();
		stack_frames_.clear();
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

	[[nodiscard]] bool available(std::size_t sr, std::size_t sn)
	{
		if constexpr (detail::input_source_has_fill_buffer<InputSource>::value) {
			do {
				std::size_t const buffer_size = input_source_.buffer().size();
				if ((sr <= buffer_size) && (sn <= (buffer_size - sr)))
					return true;
				if (sr < buffer_size)
					return false;
			} while (input_source_.fill_buffer());
			return false;
		} else {
			std::size_t const buffer_size = input_source_.buffer().size();
			return (sr <= buffer_size) && (sn <= (buffer_size - sr));
		}
	}

	[[nodiscard]] bool compare(std::size_t sr, std::size_t sn, std::string_view str)
	{
		return input_source_.buffer().compare(sr, sn, str) == 0;
	}

	[[nodiscard]] bool casefold_compare(std::size_t sr, std::size_t sn, std::string_view str)
	{
		std::string& subject = casefolded_subjects_[sr];
		if (subject.size() < sn)
			subject = utf8::tocasefold(input_source_.buffer().substr(sr, sn));
		return subject.compare(0, sn, str) == 0;
	}

	template <class Compare>
	[[nodiscard]] std::ptrdiff_t match_sequence(std::size_t& sr, std::string_view str, Compare const& comp)
	{
		if (std::size_t const sn = str.size(); !sn || (available(sr, sn) && comp(*this, sr, sn, str))) {
			sr += sn;
			return 0;
		}
		return 1;
	}

	template <class Match>
	[[nodiscard]] std::ptrdiff_t match_single(std::size_t& sr, Match const& match)
	{
		if (!available(sr, 1))
			return 1;
		auto const buffer = input_source_.buffer();
		auto const curr = buffer.cbegin() + static_cast<std::ptrdiff_t>(sr);
		auto const last = buffer.cend();
		auto [next, rune] = utf8::decode_rune(curr, last);
		bool matched = false;
		if constexpr (std::is_invocable_v<Match, decltype(curr), decltype(last), decltype(next)&, char32_t>) {
			matched = match(curr, last, next, rune);
		} else if constexpr(std::is_invocable_v<Match, unicode::record const&>) {
			matched = match(unicode::query(rune));
		} else if constexpr(std::is_invocable_v<Match, char32_t>) {
			matched = match(rune);
		} else {
			matched = match();
			std::ignore = rune;
		}
		if (matched) {
			sr += static_cast<std::size_t>(std::distance(curr, next));
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
		if (!available(sr, 1))
			return 1;
		for (;;) {
			auto const buffer = input_source_.buffer();
			auto const next_space = buffer.find_first_of(" \t\n\r\f\v", sr);
			if (next_space != std::string::npos) {
				sr = next_space;
				break;
			}
			sr = buffer.size();
			if (!available(sr, 1))
				break;
		}
		return 0;
	}

	[[nodiscard]] error_response return_from_raise(raise_frame const& frame)
	{
		--registers_.cd;
		--registers_.ci;
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
		error_response err_res{rec_res};
		error_context err{*environment_, syntax{((sr0 < sr1) ? mat.substr(sr0, sr1 - sr0) : sub), sr0}, frame.label, err_res};
		auto handler_index = static_cast<std::size_t>(frame.eh);
		auto next_frame = stack_frames_.rbegin();
		auto const last_frame = stack_frames_.rend();
		while (handler_index < program_->handlers.size()) {
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
		if (err_res >= error_response::backtrack) {
			registers_.sr = frame.sr;
			registers_.rc = frame.rc;
			return err_res;
		}
		registers_.pc = frame.pc;
		return err_res;
	}

	[[nodiscard]] std::pair<error_response, std::ptrdiff_t> return_from_call()
	{
		if (stack_frames_.empty())
			throw bad_stack{};
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
				throw bad_stack{};
			}
		}, stack_frames_.back());
		if (ret_result.first != error_response::rethrow)
			stack_frames_.pop_back();
		return ret_result;
	}

	[[nodiscard]] error_response fail_one()
	{
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
			if (stack_frames_.empty())
				return false;
			error_response const fail_result = fail_one();
			if (fail_result >= error_response::backtrack)
				continue;
			if (fail_result == error_response::halt)
				return false;
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
			if (stack_frames_.empty())
				return false;
			error_response const fail_result = fail_one();
			if (fail_result == error_response::halt)
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
		input_source_.drain_buffer(registers_.sr);
		do_drain(input_source_.buffer());
	}

	void accept_or_drain_if_deferred()
	{
		if ((registers_.ci & lug::registers::count_mask) == 0)
		{
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
		input_source_.drain_buffer(registers_.sr);
		do_reset(input_source_.buffer());
	}

public:
	basic_parser(lug::grammar const& g, lug::environment& e) : parser_base{g, e} {}
	[[nodiscard]] std::string_view match() const noexcept { return input_source_.buffer().substr(0, registers_.sr); }
	[[nodiscard]] std::string_view subject() const noexcept { return input_source_.buffer().substr(registers_.sr, input_source_.buffer().size() - registers_.sr); }
	[[nodiscard]] std::string_view max_match() const noexcept { return input_source_.buffer().substr(0, registers_.mr); }
	[[nodiscard]] std::string_view max_subject() const noexcept { return input_source_.buffer().substr(registers_.mr, input_source_.buffer().size() - registers_.mr); }
	[[nodiscard]] bool available(std::size_t sn) { return available(registers_.sr, sn); }

	template <class InputIt, class = std::enable_if_t<detail::input_source_has_enqueue<InputSource, InputIt>::value>>
	basic_parser& enqueue(InputIt first, InputIt last)
	{
		if constexpr (detail::input_source_enqueue_drains<InputSource>::value)
			drain();
		input_source_.enqueue(std::move(first), std::move(last));
		return *this;
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

	template <class InputFunc, class = std::enable_if_t<detail::input_source_has_push_source<InputSource, InputFunc&&>::value>>
	bool parse(InputFunc&& func, source_options opt = source_options::none)
	{
		return push_source(std::forward<InputFunc>(func), opt).parse();
	}

	bool parse()
	{
		detail::reentrancy_sentinel<reenterant_parse_error> const guard{parsing_};
		if (program_->instructions.empty() || program_->data.empty())
			throw bad_grammar{};
		detail::scope_fail const fixup_max_subject_position{[this]() noexcept { registers_.mr = (std::max)(registers_.mr, registers_.sr); }};
		std::ptrdiff_t fail_count{0};
		reset();
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
					commit<opcode::commit>(instr.offset32);
				} break;
				case opcode::commit_back: {
					commit<opcode::commit_back>(instr.offset32);
				} break;
				case opcode::commit_partial: {
					commit<opcode::commit_partial>(instr.offset32);
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
					if (ret_response == error_response::halt)
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
						if (stack_frames_.empty())
							throw bad_stack{};
						registers_.rh = std::get<recover_frame>(stack_frames_.back()).rh;
						stack_frames_.pop_back();
					}
					if ((registers_.ri & lug::registers::inhibited_flag) != 0) {
						if (!unwind((stack_frames_.size() - (registers_.ri & lug::registers::count_mask))))
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
					if (stack_frames_.empty())
						throw bad_stack{};
					registers_.rh = std::get<recover_frame>(stack_frames_.back()).rh;
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
					if (stack_frames_.empty())
						throw bad_stack{};
					registers_.eh = std::get<report_frame>(stack_frames_.back()).eh;
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
					registers_.rc = push_response(registers_.cd, instr.immediate16);
				} break;
				case opcode::capture_start: {
					stack_frames_.emplace_back(std::in_place_type<capture_frame>, registers_.sr);
					++registers_.ci;
				} break;
				case opcode::capture_end: {
					if (stack_frames_.empty())
						throw bad_stack{};
					auto const sr0 = std::get<capture_frame>(stack_frames_.back()).sr;
					auto const sr1 = registers_.sr;
					stack_frames_.pop_back();
					--registers_.ci;
					if (sr0 > sr1) {
						fail_count = 1;
						break;
					}
					registers_.rc = push_response(registers_.cd, instr.immediate16, syntax_range{sr0, sr1 - sr0});
					accept_or_drain_if_deferred();
				} break;
				case opcode::match: {
					fail_count = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::match_cf: {
					fail_count = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::match_any: {
					if constexpr (detail::input_source_has_options<InputSource>::value) {
						if ((instr.immediate16 != 0) && ((input_source_.options() & source_options::interactive) != source_options::none)) {
							fail_count = 1;
							break;
						}
					}
					fail_count = match_single(registers_.sr, []{ return true; });
				} break;
				case opcode::match_any_of: {
					fail_count = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::any_of(r, pe, s); });
				} break;
				case opcode::match_all_of: {
					fail_count = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::all_of(r, pe, s); });
				} break;
				case opcode::match_none_of: {
					fail_count = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::none_of(r, pe, s); });
				} break;
				case opcode::match_set: {
					fail_count = match_single(registers_.sr, [&runes = program_->runesets[instr.immediate16]](char32_t rune) {
							auto const interval = std::lower_bound(runes.begin(), runes.end(), rune, [](auto& x, auto& y) { return x.second < y; });
							return (interval != runes.end()) && (interval->first <= rune) && (rune <= interval->second); });
				} break;
				case opcode::match_eol: {
					fail_count = match_single(registers_.sr, [](auto curr, auto last, auto& next, char32_t rune) {
							if ((curr == next) || ((unicode::query(rune).properties() & unicode::ptype::Line_Ending) == unicode::ptype::None))
								return false;
							if (U'\r' == rune)
								if (auto const [next2, rune2] = utf8::decode_rune(next, last); (next2 != next) && (rune2 == U'\n'))
									next = next2;
							return true; });
				} break;
				case opcode::condition_test: {
					fail_count = (environment_->has_condition(str) == (instr.immediate8 != 0)) ? 0 : 1;
				} break;
				case opcode::condition_push: {
					stack_frames_.emplace_back(std::in_place_type<condition_frame>, str, environment_->set_condition(str, instr.immediate8 != 0));
				} break;
				case opcode::condition_pop: {
					if (stack_frames_.empty())
						throw bad_stack{};
					auto const& condition = std::get<condition_frame>(stack_frames_.back());
					environment_->set_condition(condition.name, condition.value);
					stack_frames_.pop_back();
				} break;
				case opcode::symbol_exists: {
					fail_count = (environment_->has_symbol(str) == (instr.immediate8 != 0)) ? 0 : 1;
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
					if (stack_frames_.empty())
						throw bad_stack{};
					auto const symbol = std::get<symbol_frame>(stack_frames_.back());
					auto const sr0 = static_cast<std::size_t>(symbol.sr);
					auto const sr1 = registers_.sr;
					stack_frames_.pop_back();
					if (sr0 > sr1) {
						fail_count = 1;
						break;
					}
					environment_->add_symbol(symbol.name, std::string{input_source_.buffer().substr(sr0, sr1 - sr0)});
				} break;
				case opcode::symbol_push: {
					stack_frames_.emplace_back(std::in_place_type<symbol_table_frame>, environment_->symbols_);
					if (instr.immediate8 == 1)
						environment_->symbols_.erase(str);
					else if (instr.immediate8 == 2)
						environment_->symbols_.clear();
				} break;
				case opcode::symbol_pop: {
					if (stack_frames_.empty())
						throw bad_stack{};
					environment_->symbols_.swap(std::get<symbol_table_frame>(stack_frames_.back()));
					stack_frames_.pop_back();
				} break;
				default: throw bad_opcode{};
			}
			if (fail_count > 0) {
				if (!fail(fail_count))
					return false;
				accept_or_drain_if_deferred();
				fail_count = 0;
			}
		}
		if (!success_)
			return false;
		accept();
		return true;
	}
};

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

inline bool parse(std::istream& input, grammar const& grmr, environment& envr, source_options opt = source_options::none)
{
	return basic_parser<multi_input_source>{grmr, envr}.push_source([&input](std::string& line) {
		if (std::getline(input, line)) {
			line.push_back('\n');
			return true;
		}
		return false;
	}, opt).parse();
}

inline bool parse(std::istream& input, grammar const& grmr, source_options opt = source_options::none)
{
	environment envr;
	return parse(input, grmr, envr, opt);
}

inline bool parse(grammar const& grmr, environment& envr)
{
	return parse(std::cin, grmr, envr, stdin_isatty() ? source_options::interactive : source_options::none);
}

inline bool parse(grammar const& grmr)
{
	return parse(std::cin, grmr, stdin_isatty() ? source_options::interactive : source_options::none);
}

LUG_DIAGNOSTIC_PUSH_AND_IGNORE

[[nodiscard]] inline grammar basic_regular_expression::make_grammar()
{
	using namespace language;
	implicit_space_rule const default_space = nop;
	// NOLINTBEGIN(bugprone-chained-comparison)
	rule const Empty = eps                                    <[](generator& g) { g.encoder.match_eps(); };
	rule const Dot = chr('.')                                 <[](generator& g) { g.encoder.match_any(); };
	rule const Element = any > chr('-') > !chr(']') > any     <[](generator& g, syntax const& x) { g.bracket_range(x.str()); }
			   | str("[:") > +(!chr(':') > any) > str(":]")   <[](generator& g, syntax const& x) { g.bracket_class(x.str().substr(2, x.range().size - 4)); }
			   | any                                          <[](generator& g, syntax const& x) { g.bracket_range(x.str(), x.str()); };
	rule const Bracket = chr('[') > ~(chr('^')                <[](generator& g) { g.circumflex = true; })
			   > Element > *(!chr(']') > Element) > chr(']')  <[](generator& g) { g.bracket_commit(); };
	rule const Sequence = +(!(chr('.') | chr('[')) > any)     <[](generator& g, syntax const& x) { g.encoder.match(x.str()); };
	// NOLINTEND(bugprone-chained-comparison)
	return start((+(Dot | Bracket | Sequence) | Empty) > eoi);
}

LUG_DIAGNOSTIC_POP

template <class M>
[[nodiscard]] inline auto basic_regular_expression::evaluate(encoder& d, M const& m) const -> M const&
{
	if (program_->instructions.empty()) {
		static grammar const grmr = make_grammar();
		generator genr(*this, d.mode() & directives::caseless);
		if (!parse(expression_, grmr, genr))
			throw bad_string_expression{};
	}
	d.skip((program_->entry_mode & directives::eps) ^ directives::eps).append(*program_);
	return m;
}

} // namespace lug

#endif

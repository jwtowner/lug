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

class rule;
class grammar;
class encoder;
class syntax;
class environment;
class multi_input_source;
class string_input_source;
class string_view_input_source;
class parser_base;
template <class> class basic_parser;
using parser = basic_parser<multi_input_source>;
struct program;
struct syntax_position { std::size_t column; std::size_t line; };
struct syntax_range { std::size_t index; std::size_t size; };
using semantic_action = std::function<void(environment&)>;
using semantic_capture_action = std::function<void(environment&, syntax const&)>;
using syntactic_predicate = std::function<bool(environment&)>;

struct encoder_expression_trait_tag {};
template <class E, class = void> struct is_encoder_expression : std::false_type {};
template <class E> struct is_encoder_expression<E, std::enable_if_t<std::is_same_v<encoder_expression_trait_tag, typename std::decay_t<E>::expression_trait>>> : std::true_type {};
template <class E> inline constexpr bool is_encoder_expression_v = is_encoder_expression<E>::value;
template <class E> inline constexpr bool is_encoder_callable_v = std::is_same_v<grammar, std::decay_t<E>> || std::is_same_v<rule, std::decay_t<E>> || std::is_same_v<program, std::decay_t<E>>;
template <class E> inline constexpr bool is_expression_v = is_encoder_expression_v<E> || is_encoder_callable_v<E> || std::is_same_v<std::decay_t<E>, char> || std::is_same_v<std::decay_t<E>, char32_t> || std::is_convertible_v<std::decay_t<E>, std::string_view> || std::is_invocable_r_v<bool, std::decay_t<E>, environment&>;
template <class A> inline constexpr bool is_capture_action_v = std::is_invocable_v<std::decay_t<A>, detail::dynamic_cast_if_base_of<environment&>, syntax const&> || std::is_invocable_v<std::decay_t<A>, syntax const&>;
template <class T> inline constexpr bool is_capture_target_v = std::is_same_v<std::decay_t<T>, syntax> || std::is_assignable_v<std::decay_t<T>, syntax const&>;

[[nodiscard]] grammar start(rule const& start_rule);

struct registers
{
	static constexpr std::size_t cut_deferred_flag = std::size_t{1} << static_cast<unsigned int>(std::numeric_limits<std::size_t>::digits - 1);
	std::size_t sr; // subject register
	std::size_t mr; // match register
	std::size_t rc; // response counter
	std::size_t cd; // call depth
	std::size_t cf; // cut frame
	std::size_t ci; // cut inhibited
	std::ptrdiff_t pc; // program counter
};

enum class opcode : std::uint_least8_t
{
	choice,         commit,         commit_back,    commit_partial, jump,
	call,           ret,            fail,           raise,			cut,
	predicate,      action,         capture_start,  capture_end,    condition_pop,
	symbol_end,     symbol_pop,     match_any,      match_set,      match_eol,
	match,          match_cf,       match_any_of,   match_all_of,   match_none_of,
	condition_test, condition_push, symbol_exists,  symbol_all,     symbol_all_cf,
	symbol_any,     symbol_any_cf,  symbol_head,    symbol_head_cf, symbol_tail,
	symbol_tail_cf, symbol_start,   symbol_push
};

struct alignas(std::uint_least64_t) instruction
{
	opcode op;
	std::uint_least8_t immediate8;
	std::uint_least16_t immediate16;
	std::int_least32_t offset32;
};

static_assert(sizeof(instruction) == sizeof(std::uint_least64_t), "expected instruction size to be same size as std::uint_least64_t");
static_assert(alignof(instruction) == alignof(std::uint_least64_t), "expected instruction alignment to be same size as std::uint_least64_t");

enum class directives : std::uint_least8_t { none = 0, caseless = 1, eps = 2, lexeme = 4, noskip = 8, preskip = 16, postskip = 32, is_bitfield_enum };
using program_callees = std::vector<std::tuple<lug::rule const*, lug::program const*, std::ptrdiff_t, directives>>;

struct program
{
	std::vector<instruction> instructions;
	std::vector<char> data;
	std::vector<unicode::rune_set> runesets;
	std::vector<syntactic_predicate> predicates;
	std::vector<semantic_action> actions;
	std::vector<semantic_capture_action> captures;
	directives entry_mode{directives::eps};

	void concatenate(program const& src)
	{
		std::size_t const data_offset = data.size();
		std::size_t const runesets_offset = runesets.size();
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

class syntax
{
	std::string_view str_;
	std::size_t index_{0};
public:
	constexpr syntax() noexcept = default;
	constexpr syntax(std::string_view c, std::size_t i) noexcept : str_{c}, index_{i} {}
	[[nodiscard]] constexpr std::string_view str() const noexcept { return str_; }
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

	virtual void on_accept_started() {}
	virtual void on_accept_ended() {}

	std::size_t start_accept(std::string_view m, std::string_view s)
	{
		reset_match_and_subject(m, s);
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

	void reset_match_and_subject(std::string_view m, std::string_view s)
	{
		match_ = m;
		subject_ = s;
		positions_.clear();
	}

	void reset_origin()
	{
		origin_ = position_at(match_.size());
		reset_match_and_subject(std::string_view{}, std::string_view{});
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
	[[nodiscard]]std::uint_least32_t tab_width() const { return tab_width_; }
	void tab_width(std::uint_least32_t w) { tab_width_ = w; }
	[[nodiscard]] std::uint_least32_t tab_alignment() const { return tab_alignment_; }
	void tab_alignment(std::uint_least32_t a) { tab_alignment_ = a; }
	[[nodiscard]] bool has_condition(std::string_view name) const noexcept { return (conditions_.count(name) > 0); }
	bool set_condition(std::string_view name, bool value) { return value ? (!conditions_.emplace(name).second) : (conditions_.erase(name) > 0); }
	void clear_conditions() { conditions_.clear(); }
	[[nodiscard]] bool has_symbol(std::string_view name) const noexcept { return (symbols_.count(name) > 0); }
	[[nodiscard]] std::vector<std::string> const& get_symbols(std::string_view name) const { auto it = symbols_.find(name); if (it == symbols_.end()) return empty_symbols_; return it->second; }
	void add_symbol(std::string_view name, std::string value) { symbols_[name].emplace_back(std::move(value)); }
	void clear_symbols(std::string_view name) { symbols_.erase(name); }
	[[nodiscard]] std::string_view match() const noexcept { return match_; }
	[[nodiscard]] std::string_view subject() const noexcept { return subject_; }
	[[nodiscard]] syntax_position position_begin(syntax_range const& range) { return position_at(range.index); }
	[[nodiscard]] syntax_position position_end(syntax_range const& range) { return position_at(range.index + range.size); }
	[[nodiscard]] std::pair<syntax_position, syntax_position> position_range(syntax_range const& range) { return {position_begin(range), position_end(range)}; }
	[[nodiscard]] std::size_t call_depth() const noexcept { return call_depth_; }
	[[nodiscard]] std::size_t prune_depth() const noexcept { return prune_depth_; }
	void escape() { prune_depth_ = call_depth_; }

	[[nodiscard]] syntax_position position_at(std::size_t index)
	{
		auto const pos = std::lower_bound(std::begin(positions_), std::end(positions_), index, [](auto& x, auto& y) { return x.first < y; });
		if (pos != std::end(positions_) && index == pos->first)
			return pos->second;
		std::size_t startindex = 0;
		syntax_position position = origin_;
		if (pos != std::begin(positions_)) {
			auto prevpos = std::prev(pos);
			startindex = prevpos->first;
			position = prevpos->second;
		}
		auto first = std::next(std::begin(match_), static_cast<std::ptrdiff_t>(startindex));
		auto const last = std::next(std::begin(match_), static_cast<std::ptrdiff_t>(index));
		char32_t rune = U'\0';
		char32_t prevrune = U'\0';
		for (auto curr = first, next = curr; curr < last; curr = next, prevrune = rune) {
			std::tie(next, rune) = utf8::decode_rune(curr, last);
			if ((unicode::query(rune).properties() & unicode::ptype::Line_Ending) != unicode::ptype::None && (prevrune != U'\r' || rune != U'\n')) {
				position.column = 1;
				++position.line;
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

	template <class Item>
	[[nodiscard]] std::uint_least16_t add_item(std::vector<Item>& items, Item&& item)
	{
		items.push_back(std::forward<Item>(item));
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
		auto callee_mode = mode_.back();
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
	std::ptrdiff_t encode(opcode op, semantic_action&& a, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->actions, std::move(a)), 0}); }
	std::ptrdiff_t encode(opcode op, semantic_capture_action&& a, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->captures, std::move(a)), 0}); }
	std::ptrdiff_t encode(opcode op, syntactic_predicate&& p, std::uint_least8_t imm8 = 0) { return append(instruction{op, imm8, add_item(program_->predicates, std::move(p)), 0}); }
	std::ptrdiff_t call(program const& p, std::uint_least16_t prec) { return do_call(nullptr, &p, 0, prec); }
	std::ptrdiff_t match(unicode::rune_set&& runes) { return skip().encode(opcode::match_set, add_item(program_->runesets, std::move(runes)), 0); }
	std::ptrdiff_t match_any() { return skip().encode(opcode::match_any); }
	std::ptrdiff_t match_eps() { return skip(directives::lexeme).encode(opcode::match, std::string_view{}); }

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

struct terminal_encoder_expression_interface
{
	using expression_trait = encoder_expression_trait_tag;
};

template <class E1>
struct unary_encoder_expression_interface
{
	using expression_trait = encoder_expression_trait_tag; E1 e1;
	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr explicit unary_encoder_expression_interface(X1&& x1) : e1(std::forward<X1>(x1)) {}
};

template <class E1, class E2>
struct binary_encoder_expression_interface
{
	using expression_trait = encoder_expression_trait_tag; E1 e1; E2 e2;
	template <class X1, class X2, class = std::enable_if_t<std::is_constructible_v<E1, X1&&> && std::is_constructible_v<E2, X2&&>>>
	constexpr binary_encoder_expression_interface(X1&& x1, X2&& x2) : e1(std::forward<X1>(x1)), e2(std::forward<X2>(x2)) {}
};

class basic_regular_expression : public terminal_encoder_expression_interface
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
			: encoder{*se.program_, callees, mode | directives::eps | directives::lexeme}
		{}

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
			runes.clear(), classes = unicode::ctype::none, circumflex = false;
		}
	};

public:
	explicit basic_regular_expression(std::string_view e) : expression_{e}, program_{std::make_shared<program>()} {}
	template <class M> [[nodiscard]] auto evaluate(encoder& d, M const& m) const -> M const&;
};

struct string_expression : terminal_encoder_expression_interface
{
	std::string_view text;
	constexpr explicit string_expression(std::string_view t) noexcept : text{t} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(text); return m; }
};

struct char_expression : terminal_encoder_expression_interface
{
	char c;
	constexpr explicit char_expression(char x) noexcept : c{x} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(std::string_view{&c, 1}); return m; }
};

struct char32_expression : terminal_encoder_expression_interface
{
	char32_t c;
	constexpr explicit char32_expression(char32_t x) noexcept : c{x} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(utf8::encode_rune(c)); return m; }
};

struct char32_range_expression : terminal_encoder_expression_interface
{
	char32_t start;
	char32_t end;
	constexpr char32_range_expression(char32_t first, char32_t last) noexcept : start{first}, end{last} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match(unicode::sort_and_optimize(add_rune_range(unicode::rune_set{}, d.mode(), start, end))); return m; }
};

template <class Target>
struct callable_expression : terminal_encoder_expression_interface
{
	std::reference_wrapper<Target> target;
	constexpr explicit callable_expression(Target& t) noexcept : target{t} {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { return d.call_with_frame(m, target.get(), 0); }
};

template <class T> struct is_callable_encoder_expression : std::false_type {};
template <class Target> struct is_callable_encoder_expression<callable_expression<Target>> : std::true_type {};
template <class T> inline constexpr bool is_callable_encoder_expression_v = is_callable_encoder_expression<T>::value;

template <class Pred>
struct predicate_expression : terminal_encoder_expression_interface
{
	Pred pred;
	template <class P, class = std::enable_if_t<std::is_constructible_v<Pred, P&&>>> constexpr explicit predicate_expression(P&& p) noexcept(std::is_nothrow_constructible_v<Pred, P&&>) : pred(std::forward<P>(p)) {}
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::predicate, syntactic_predicate{pred}); return m; }
};

template <class X1> unary_encoder_expression_interface(X1&&) -> unary_encoder_expression_interface<std::decay_t<X1>>;
template <class X1, class X2> binary_encoder_expression_interface(X1&&, X2&&) -> binary_encoder_expression_interface<std::decay_t<X1>, std::decay_t<X2>>;
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

struct rule_precedence_expression : terminal_encoder_expression_interface
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

template <class E1>
struct directive_expression : unary_encoder_expression_interface<E1>
{
	directives enable_mask{directives::none};
	directives disable_mask{directives::none};
	directives relay_mask{directives::none};

	template <class X1, class = std::enable_if_t<std::is_constructible_v<E1, X1&&>>>
	constexpr directive_expression(X1&& x1, directives enable, directives disable, directives relay)
		: unary_encoder_expression_interface<E1>{std::forward<X1>(x1)}, enable_mask{enable}, disable_mask{disable}, relay_mask{relay} {}

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

struct nop_expression : terminal_encoder_expression_interface { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& /*d*/, M const& m) const -> M const& { return m; } };
struct eps_expression : terminal_encoder_expression_interface { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_eps(); return m; } };
struct eoi_expression : terminal_encoder_expression_interface { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::choice, 2, 0, 0); d.encode(opcode::match_any, 1, 0); d.encode(opcode::fail, 0, 2); return m; } };
struct eol_expression : terminal_encoder_expression_interface { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::match_eol); return m; } };
struct cut_expression : terminal_encoder_expression_interface { template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.encode(opcode::cut); return m; } };

template <opcode Op>
struct match_class_combinator
{
	template <class Property>
	struct match_class_expression : terminal_encoder_expression_interface
	{
		Property property;
		constexpr explicit match_class_expression(Property p) noexcept : property{p} {}
		template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_class<Op>(property); return m; }
	};

	template <class Property, class = std::enable_if_t<unicode::is_property_enum_v<Property>>>
	[[nodiscard]] constexpr match_class_expression<std::decay_t<Property>> operator()(Property p) const { return match_class_expression<std::decay_t<Property>>{p}; }
};

struct match_any_expression : terminal_encoder_expression_interface, match_class_combinator<opcode::match_any_of>
{
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_any(); return m; }
};

template <unicode::ctype Property>
struct ctype_expression : terminal_encoder_expression_interface
{
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const -> M const& { d.match_class<opcode::match_any_of>(Property); return m; }
};

template <bool Value>
struct condition_test_combinator
{
	struct condition_test_expression : terminal_encoder_expression_interface
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
	struct condition_block_expression : unary_encoder_expression_interface<E1>
	{
		std::string_view name;
		constexpr condition_block_expression(E1 const& x1, std::string_view n) noexcept : unary_encoder_expression_interface<E1>{x1}, name{n} {}

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
	struct symbol_exists_expression : terminal_encoder_expression_interface
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
	struct symbol_match_expression : terminal_encoder_expression_interface
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
	struct symbol_match_offset_expression : terminal_encoder_expression_interface
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
struct negative_lookahead_expression : unary_encoder_expression_interface<E1>
{
	using unary_encoder_expression_interface<E1>::unary_encoder_expression_interface;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::fail, 0, 2);
		d.jump_to_here(choice);
		return m2;
	}
};

template <class E1>
struct positive_lookahead_expression : unary_encoder_expression_interface<E1>
{
	using unary_encoder_expression_interface<E1>::unary_encoder_expression_interface;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		auto const choice = d.encode(opcode::choice);
		auto m2 = this->e1.evaluate(d, m);
		d.encode(opcode::commit_back, 1, 0, 0);
		d.jump_to_here(choice);
		d.encode(opcode::fail, 0, 1);
		return m2;
	}
};

template <class E1>
struct repetition_expression : unary_encoder_expression_interface<E1>
{
	using unary_encoder_expression_interface<E1>::unary_encoder_expression_interface;

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
struct choice_expression : binary_encoder_expression_interface<E1, E2>
{
	using binary_encoder_expression_interface<E1, E2>::binary_encoder_expression_interface;

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
struct sequence_expression : binary_encoder_expression_interface<E1, E2>
{
	using binary_encoder_expression_interface<E1, E2>::binary_encoder_expression_interface;

	template <class M>
	[[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const
	{
		return this->e2.evaluate(d, this->e1.evaluate(d, m));
	}
};

template <class Derived, class E1, class Operand>
struct attribute_action_expression : unary_encoder_expression_interface<E1>
{
	Operand operand;
	template <class X1, class O> constexpr attribute_action_expression(X1&& x1, O&& o) : unary_encoder_expression_interface<E1>{std::forward<X1>(x1)}, operand(std::forward<O>(o)) {}

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
	using attribute_action_expression<Derived, E1, Target*>::attribute_action_expression;
	template <class M> [[nodiscard]] constexpr auto evaluate(encoder& d, M const& m) const { return encoder_metadata{std::tuple_cat((attribute_action_expression<Derived, E1, Target*>::evaluate(d, m)).attribute_frame, std::forward_as_tuple(*(this->operand)))}; }
};

template <class E1, class Action>
struct action_expression : attribute_action_expression<action_expression<E1, Action>, E1, Action>
{
	using attribute_action_expression<action_expression<E1, Action>, E1, Action>::attribute_action_expression;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[a = this->operand](environment& envr) { a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::action, semantic_action{[f = m.attribute_frame, a = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}); }}); }
};

template <class E1, class Action>
struct capture_expression : attribute_action_expression<capture_expression<E1, Action>, E1, Action>
{
	using attribute_action_expression<capture_expression<E1, Action>, E1, Action>::attribute_action_expression;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[a = this->operand](environment& envr, syntax const& sx) { a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::capture_end, semantic_capture_action{[f = m.attribute_frame, a = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); a(detail::dynamic_cast_if_base_of<environment&>{envr}, sx); }}); }
};

template <class E1, class Target>
struct assign_to_expression : attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>
{
	using attribute_bind_to_expression<assign_to_expression<E1, Target>, E1, Target>::attribute_bind_to_expression;
	constexpr void do_prologue(encoder& /*d*/) const {}
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::action, semantic_action{[t = this->operand](environment& envr) { *t = envr.pop_attribute<Target>(); }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::action, semantic_action{[f = m.attribute_frame, t = this->operand](environment& envr) mutable { envr.pop_attribute_frame(f); *t = envr.pop_attribute<Target>(); }}); }
};

template <class E1, class Target>
struct capture_to_expression : attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>
{
	using attribute_bind_to_expression<capture_to_expression<E1, Target>, E1, Target>::attribute_bind_to_expression;
	constexpr void do_prologue(encoder& d) const { d.skip().encode(opcode::capture_start); }
	constexpr void do_epilogue(encoder& d) const { d.encode(opcode::capture_end, semantic_capture_action{[t = this->operand](environment&, syntax const& sx) { *t = sx; }}); }
	template <class M> constexpr void do_epilogue_inlined(encoder& d, M const& m) const { d.encode(opcode::capture_end, semantic_capture_action{[f = m.attribute_frame, t = this->operand](environment& envr, syntax const& sx) mutable { envr.pop_attribute_frame(f); *t = sx; }}); }
};

template <class E1>
struct symbol_assign_expression : unary_encoder_expression_interface<E1>
{
	std::string_view name;
	template <class X1> constexpr symbol_assign_expression(X1&& x1, std::string_view n) : unary_encoder_expression_interface<E1>{std::forward<X1>(x1)}, name{n} {}
	template <class M> [[nodiscard]] constexpr decltype(auto) evaluate(encoder& d, M const& m) const { d.skip().encode(opcode::symbol_start, name); auto m2 = this->e1.evaluate(d, m); d.encode(opcode::symbol_end); return m2; }
};

template <class E1>
struct symbol_block_expression : unary_encoder_expression_interface<E1>
{
	using unary_encoder_expression_interface<E1>::unary_encoder_expression_interface;

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
struct local_block_expression : unary_encoder_expression_interface<E1>
{
	using unary_encoder_expression_interface<E1>::unary_encoder_expression_interface;

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
struct local_to_block_expression : unary_encoder_expression_interface<E1>
{
	std::string_view name;
	constexpr local_to_block_expression(E1 const& x1, std::string_view n) noexcept : unary_encoder_expression_interface<E1>{x1}, name{n} {}

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

using lug::grammar; using environment = lug::environment; using lug::rule; using lug::start;
using syntax = lug::syntax; using syntax_position = lug::syntax_position; using syntax_range = lug::syntax_range;
using unicode::ctype; using unicode::ptype; using unicode::gctype; using unicode::sctype; using unicode::blktype; using unicode::agetype; using unicode::eawtype;
inline constexpr directive_modifier<directives::none, directives::caseless, directives::eps> cased{};
inline constexpr directive_modifier<directives::caseless, directives::none, directives::eps> caseless{};
inline constexpr directive_modifier<directives::lexeme, directives::noskip, directives::eps> lexeme{};
inline constexpr directive_modifier<directives::lexeme | directives::noskip, directives::none, directives::eps> noskip{};
inline constexpr directive_modifier<directives::none, directives::lexeme | directives::noskip, directives::eps> skip{};
inline constexpr nop_expression nop{}; inline constexpr eps_expression eps{}; inline constexpr eoi_expression eoi{}; inline constexpr eol_expression eol{}; inline constexpr cut_expression cut{};
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

template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator!(E const& e) { return negative_lookahead_expression{matches_eps[e]}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator&(E const& e) { return positive_lookahead_expression{matches_eps[e]}; } // NOLINT(google-runtime-operator)
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator*(E const& e) { return repetition_expression{matches_eps[skip_after[e]]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator|(E1 const& e1, E2 const& e2) { return choice_expression{relays_eps[e1], relays_eps[e2]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator>(E1 const& e1, E2 const& e2) { return sequence_expression{make_expression(e1), skip_before[e2]}; }
template <class E1, class E2, class = std::enable_if_t<is_expression_v<E1> && is_expression_v<E2>>> [[nodiscard]] constexpr auto operator>>(E1 const& e1, E2 const& e2) { return e1 > *(e2 > e1); }
template <class T, class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator%(T& target, E const& e) { return assign_to_expression{make_expression(e), std::addressof(target)}; }
template <class E, class = std::enable_if_t<is_expression_v<E>>> [[nodiscard]] constexpr auto operator+(E const& e) { auto x{make_expression(e)}; return x > *x; }
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

class implicit_space_rule
{
	std::function<void(encoder&)> prev_rule_;
	std::weak_ptr<std::function<void(encoder&)>> implicit_space_ref_;

public:
	template <class E, class = std::enable_if_t<is_expression_v<E>>>
	implicit_space_rule(E const& e) // NOLINT(google-explicit-constructor,hicpp-explicit-conversions)
		: prev_rule_{std::exchange(*grammar::implicit_space(), std::function<void(encoder&)>{make_space_expression(e)})}
		, implicit_space_ref_{grammar::implicit_space()}
	{}

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
				for (auto&& [callee_rule, callee_program, instr_offset, mode] : top_rule->callees_) {
					calls.emplace_back(callee_program, address + instr_offset);
					if ((callee_rule != nullptr) && ((mode & directives::eps) != directives::none) &&
							detail::escaping_find_if(callstack.crbegin(), callstack.crend(), [callee = callee_rule](auto const& caller) {
								if (caller.first == callee)
									return 1;
								return (caller.second ? 0 : -1);
							}) != callstack.crend()) {
						left_recursive.insert(callee_program);
					} else {
						auto callee_callstack = callstack;
						callee_callstack.emplace_back(callee_rule, (mode & directives::eps) != directives::none);
						unprocessed.emplace_back(std::move(callee_callstack), callee_program);
					}
				}
			}
		}
	} while (!unprocessed.empty());
	for (auto [subprogram, instr_addr] : calls) {
		if (auto& instr = grencoder.instruction_at(instr_addr); instr.op == opcode::call) {
			instr.immediate16 = ((left_recursive.count(subprogram) != 0) ? (std::max)(instr.immediate16, std::uint_least16_t{1}) : std::uint_least16_t{0});
			instr.offset32 = detail::checked_cast<std::int_least32_t, program_limit_error>(instr.offset32 + addresses[subprogram] - (instr_addr + 1));
			if ((instr.immediate16 == 0) && (grencoder.instruction_at(instr_addr + 1).op == opcode::ret))
				instr.op = opcode::jump;
		}
	}
	if (halt_address)
		grencoder.jump_to_here(*halt_address);
	grprogram.data.emplace_back('\0');
	return grammar{std::move(grprogram)};
}

enum class source_options : std::uint_least8_t { none = 0, interactive = 0x01, is_bitfield_enum };

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
};

class string_view_input_source
{
	std::string_view buffer_;
public:
	using enqueue_drains = std::true_type;
	[[nodiscard]] constexpr std::string_view buffer() const noexcept { return buffer_; }
	constexpr void drain_buffer(std::size_t sr) noexcept { buffer_.remove_prefix(sr); }
	template <class It, class = detail::enable_if_char_contiguous_iterator_t<It>> void enqueue(It first, It last) { buffer_ = (last > first) ? std::string_view{&(*first), static_cast<std::size_t>(last - first)} : std::string_view{}; }
};

class parser_base
{
protected:
	static constexpr std::size_t lrfailcode = (std::numeric_limits<std::size_t>::max)();
	static constexpr std::size_t max_size = (std::numeric_limits<std::size_t>::max)();

	struct response { std::size_t call_depth{0}; std::size_t action_index{0}; syntax_range range{0, 0}; constexpr response() noexcept = default; constexpr response(std::size_t c, std::size_t a, syntax_range const& r) noexcept : call_depth{c}, action_index{a}, range{r} {} };
	struct backtrack_frame { std::size_t sr; std::size_t rc; std::ptrdiff_t pc; constexpr backtrack_frame(std::size_t s, std::size_t r, std::ptrdiff_t p) noexcept : sr{s}, rc{r}, pc{p} {} };
	struct call_frame { std::ptrdiff_t pc; constexpr explicit call_frame(std::ptrdiff_t p) noexcept : pc{p} {} };
	struct capture_frame { std::size_t sr; constexpr explicit capture_frame(std::size_t s) noexcept : sr{s} {} };
	struct condition_frame { std::string_view name; bool value; constexpr condition_frame(std::string_view n, bool v) noexcept : name{n}, value{v} {} };
	struct lrmemo_frame { std::size_t srr; std::size_t sra; std::size_t prec; std::ptrdiff_t pcr; std::ptrdiff_t pca; std::size_t rcr; std::vector<response> responses; lrmemo_frame(std::size_t sr, std::size_t sa, std::size_t p, std::ptrdiff_t pc, std::ptrdiff_t pa, std::size_t rc) noexcept : srr{sr}, sra{sa}, prec{p}, pcr{pc}, pca{pa}, rcr{rc} {} };
	struct symbol_frame { std::string_view name; std::size_t sr; constexpr symbol_frame(std::string_view n, std::size_t s) noexcept : name{n}, sr{s} {} };
	using symbol_table_frame = std::unordered_map<std::string_view, std::vector<std::string>>;
	using stack_frame = std::variant<backtrack_frame, call_frame, capture_frame, condition_frame, lrmemo_frame, symbol_frame, symbol_table_frame>;

	// NOLINTBEGIN(cppcoreguidelines-non-private-member-variables-in-classes,misc-non-private-member-variables-in-classes)
	lug::grammar const* grammar_;
	lug::environment* environment_;
	std::vector<response> responses_;
	std::vector<stack_frame> stack_frames_;
	std::unordered_map<std::size_t, std::string> casefolded_subjects_;
	lug::registers registers_{0, 0, 0, 0, 0, 0, 0};
	bool parsing_{false};
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
			if constexpr (Opcode == opcode::commit_back)
				registers_.sr = backtrack.sr;
			stack_frames_.pop_back();
		}
		registers_.pc += off;
	}

	void pop_responses_after(std::size_t n)
	{
		if (n < responses_.size())
			responses_.resize(n);
	}

	[[nodiscard]] auto restore_responses_after(std::size_t n, std::vector<response> const& restore)
	{
		pop_responses_after(n);
		responses_.insert(responses_.end(), restore.begin(), restore.end());
		return responses_.size();
	}

	[[nodiscard]] auto drop_responses_after(std::size_t n)
	{
		std::vector<response> dropped;
		if (n < responses_.size()) {
			dropped.assign(responses_.begin() + static_cast<std::ptrdiff_t>(n), responses_.end());
			responses_.resize(n);
		}
		return dropped;
	}

	[[nodiscard]] auto push_response(std::size_t call_depth, std::size_t action_index, syntax_range const& range = {parser_base::max_size, 0})
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

	[[nodiscard]] bool return_from()
	{
		if (stack_frames_.empty())
			throw bad_stack{};
		auto& frame = stack_frames_.back();
		if (auto* const call = std::get_if<call_frame>(&frame); call != nullptr) {
			--registers_.cd;
			registers_.pc = call->pc;
			stack_frames_.pop_back();
			return false;
		}
		if (auto* const memo = std::get_if<lrmemo_frame>(&frame); memo != nullptr) {
			if ((memo->sra == parser_base::lrfailcode) || (registers_.sr > memo->sra)) {
				memo->sra = registers_.sr;
				memo->responses = drop_responses_after(memo->rcr);
				registers_.sr = memo->srr;
				registers_.pc = memo->pca;
				registers_.rc = memo->rcr;
				return false;
			}
			--registers_.cd;
			registers_.sr = memo->sra;
			registers_.pc = memo->pcr;
			registers_.rc = restore_responses_after(memo->rcr, memo->responses);
			stack_frames_.pop_back();
			return true;
		}
		throw bad_stack{};
	}

	[[nodiscard]] std::pair<std::ptrdiff_t, bool> fail_one()
	{
		auto const fail_result = std::visit([this](auto& frame) -> std::pair<std::ptrdiff_t, bool> {
			using frame_type = std::decay_t<decltype(frame)>;
			if constexpr (std::is_same_v<frame_type, backtrack_frame>) {
				registers_.sr = frame.sr;
				registers_.rc = frame.rc;
				registers_.pc = frame.pc;
				return {0, false};
			} else if constexpr (std::is_same_v<frame_type, call_frame>) {
				--registers_.cd;
				return {1, false};
			} else if constexpr (std::is_same_v<frame_type, capture_frame>) {
				return {1, true};
			} else if constexpr (std::is_same_v<frame_type, condition_frame>) {
				environment_->set_condition(frame.name, frame.value);
				return {1, false};
			} else if constexpr (std::is_same_v<frame_type, lrmemo_frame>) {
				--registers_.cd;
				if (frame.sra != parser_base::lrfailcode) {
					registers_.sr = frame.sra;
					registers_.rc = restore_responses_after(frame.rcr, frame.responses);
					registers_.pc = frame.pcr;
					return {0, true};
				}
				return {1, true};
			} else if constexpr (std::is_same_v<frame_type, symbol_frame>) {
				return {1, false};
			} else if constexpr (std::is_same_v<frame_type, symbol_table_frame>) {
				environment_->symbols_.swap(frame);
				return {1, false};
			} else {
				static_assert(detail::always_false_v<frame_type>, "non-exhaustive visitor!");
			}
		}, stack_frames_.back());
		stack_frames_.pop_back();
		return fail_result;
	}

	void do_accept(std::string_view match, std::string_view subject)
	{
		auto const prior_call_depth = environment_->start_accept(match, subject);
		detail::scope_exit const cleanup{[this, prior_call_depth]{ environment_->end_accept(prior_call_depth); }};
		auto const& actions = grammar_->program().actions;
		auto const& captures = grammar_->program().captures;
		for (auto& resp : responses_) {
			if (environment_->accept_response(resp.call_depth)) {
				if (resp.range.index < parser_base::max_size)
					captures[resp.action_index](*environment_, syntax{match.substr(resp.range.index, resp.range.size), resp.range.index});
				else
					actions[resp.action_index](*environment_);
			}
		}
	}

	void do_drain()
	{
		environment_->reset_origin();
		casefolded_subjects_.clear();
		responses_.clear();
		registers_.mr -= registers_.sr;
		registers_.sr = 0;
		registers_.rc = 0;
		registers_.cf = stack_frames_.size();
		registers_.ci &= ~lug::registers::cut_deferred_flag;
	}

public:
	explicit parser_base(lug::grammar const& g, lug::environment& e) : grammar_{&g}, environment_{&e} {}
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

	void accept()
	{
		registers_.mr = (std::max)(registers_.mr, registers_.sr);
		do_accept(match(), subject());
	}

	void accept_if_deferred()
	{
		--registers_.ci;
		if (registers_.ci == lug::registers::cut_deferred_flag) {
			registers_.ci &= ~lug::registers::cut_deferred_flag;
			accept();
		}
	}

	void drain()
	{
		input_source_.drain_buffer(registers_.sr);
		do_drain();
	}

public:
	basic_parser(lug::grammar const& g, lug::environment& e) : parser_base{g, e} {}
	[[nodiscard]] std::string_view match() const noexcept { return input_source_.buffer().substr(0, registers_.sr); }
	[[nodiscard]] std::string_view subject() const noexcept { return input_source_.buffer().substr(registers_.sr, input_source_.buffer().size() - registers_.sr); }
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
		program const& prog = grammar_->program();
		if (prog.instructions.empty())
			throw bad_grammar{};
		drain();
		registers_.pc = 0;
		std::ptrdiff_t fc = 0;
		bool result = true;
		for (auto instr_index = static_cast<std::size_t>(registers_.pc++); instr_index < prog.instructions.size(); instr_index = static_cast<std::size_t>(registers_.pc++)) {
			instruction const instr{prog.instructions[instr_index]};
			std::string_view const str{prog.data.data() + instr.offset32, instr.immediate16};
			switch (instr.op) {
				case opcode::choice: {
					stack_frames_.emplace_back(std::in_place_type<backtrack_frame>, registers_.sr - instr.immediate8, registers_.rc, registers_.pc + instr.offset32);
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
				case opcode::jump: {
					registers_.pc += instr.offset32;
				} break;
				case opcode::call: {
					fc = call_into(instr.immediate16, instr.offset32);
				} break;
				case opcode::ret: {
					if (return_from())
						accept_if_deferred();
				} break;
				case opcode::fail: {
					fc = static_cast<std::ptrdiff_t>(instr.immediate8);
				} break;
				case opcode::cut: {
					if (registers_.ci == 0) {
						accept();
						drain();
					} else {
						registers_.ci |= lug::registers::cut_deferred_flag;
					}
				} break;
				case opcode::predicate: {
					registers_.mr = (std::max)(registers_.mr, registers_.sr);
					environment_->reset_match_and_subject(match(), subject());
					bool const accepted = prog.predicates[instr.immediate16](*environment_);
					pop_responses_after(registers_.rc);
					fc = accepted ? 0 : 1;
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
					accept_if_deferred();
					if (sr0 > sr1) {
						fc = 1;
						break;
					}
					registers_.rc = push_response(registers_.cd, instr.immediate16, syntax_range{sr0, sr1 - sr0});
				} break;
				case opcode::match: {
					fc = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::match_cf: {
					fc = match_sequence(registers_.sr, str, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::match_any: {
					if constexpr (detail::input_source_has_options<InputSource>::value) {
						if ((instr.immediate16 != 0) && ((input_source_.options() & source_options::interactive) != source_options::none)) {
							fc = 1;
							break;
						}
					}
					fc = match_single(registers_.sr, []{ return true; });
				} break;
				case opcode::match_any_of: {
					fc = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::any_of(r, pe, s); });
				} break;
				case opcode::match_all_of: {
					fc = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::all_of(r, pe, s); });
				} break;
				case opcode::match_none_of: {
					fc = match_single(registers_.sr, [pe = static_cast<unicode::property_enum>(instr.immediate8), s = str](auto const& r) { return unicode::none_of(r, pe, s); });
				} break;
				case opcode::match_set: {
					fc = match_single(registers_.sr, [&runes = prog.runesets[instr.immediate16]](char32_t rune) {
							auto const interval = std::lower_bound(runes.begin(), runes.end(), rune, [](auto& x, auto& y) { return x.second < y; });
							return (interval != runes.end()) && (interval->first <= rune) && (rune <= interval->second); });
				} break;
				case opcode::match_eol: {
					fc = match_single(registers_.sr, [](auto curr, auto last, auto& next, char32_t rune) {
							if ((curr == next) || ((unicode::query(rune).properties() & unicode::ptype::Line_Ending) == unicode::ptype::None))
								return false;
							if (U'\r' == rune)
								if (auto const [next2, rune2] = utf8::decode_rune(next, last); (next2 != next) && (rune2 == U'\n'))
									next = next2;
							return true; });
				} break;
				case opcode::condition_test: {
					fc = (environment_->has_condition(str) == (instr.immediate8 != 0)) ? 0 : 1;
				} break;
				case opcode::condition_push: {
					stack_frames_.emplace_back(std::in_place_type<condition_frame>, str, environment_->set_condition(str, instr.immediate8 != 0));
				} break;
				case opcode::condition_pop: {
					if (stack_frames_.empty())
						throw bad_stack{};
					auto& condition = std::get<condition_frame>(stack_frames_.back());
					environment_->set_condition(condition.name, condition.value);
					stack_frames_.pop_back();
				} break;
				case opcode::symbol_exists: {
					fc = (environment_->has_symbol(str) == (instr.immediate8 != 0)) ? 0 : 1;
				} break;
				case opcode::symbol_all: {
					fc = match_symbol_all(registers_.sr, str, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_all_cf: {
					fc = match_symbol_all(registers_.sr, str, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_any: {
					fc = match_symbol_any(registers_.sr, str, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_any_cf: {
					fc = match_symbol_any(registers_.sr, str, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_head: {
					fc = match_symbol_head(registers_.sr, str, instr.immediate8, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_head_cf: {
					fc = match_symbol_head(registers_.sr, str, instr.immediate8, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
				} break;
				case opcode::symbol_tail: {
					fc = match_symbol_tail(registers_.sr, str, instr.immediate8, detail::identity{}, std::mem_fn(&basic_parser::compare));
				} break;
				case opcode::symbol_tail_cf: {
					fc = match_symbol_tail(registers_.sr, str, instr.immediate8, utf8::tocasefold, std::mem_fn(&basic_parser::casefold_compare));
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
						fc = 1;
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
				default: registers_.mr = (std::max)(registers_.mr, registers_.sr); throw bad_opcode{};
			}
			if (fc > 0) {
				registers_.mr = (std::max)(registers_.mr, registers_.sr);
				do {
					if (registers_.cf >= stack_frames_.size()) {
						registers_.pc = static_cast<std::ptrdiff_t>(prog.instructions.size());
						result = false;
						fc = 0;
						break;
					}
					auto const fail_result = fail_one();
					fc += fail_result.first;
					if (fail_result.second)
						accept_if_deferred();
					--fc;
				} while (fc > 0);
				pop_responses_after(registers_.rc);
			}
		}
		if (result) {
			accept();
			return true;
		}
		return false;
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

inline bool parse(std::string_view sv, grammar const& grmr, environment& envr)
{
	return parse(sv.cbegin(), sv.cend(), grmr, envr);
}

inline bool parse(std::string_view sv, grammar const& grmr)
{
	return parse(sv.cbegin(), sv.cend(), grmr);
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
inline auto basic_regular_expression::evaluate(encoder& d, M const& m) const -> M const&
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

// lug - Embedded DSL for PE grammar parsers
// Copyright (c) 2017 Jesse W. Towner

#ifndef LUG_HPP
#define LUG_HPP

#include <any>
#include <algorithm>
#include <functional>
#include <deque>
#include <iostream>
#include <memory>
#include <numeric>
#include <regex>
#include <stack>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lug
{

namespace utf8
{

constexpr bool is_lead(char c) noexcept
{
	return (static_cast<unsigned char>(c) & 0xC0) != 0x80;
}

template <class InputIt>
constexpr size_t length(InputIt first, InputIt last)
{
	return std::accumulate(first, last, size_t{0}, [&](size_t l, char c) { return l + is_lead(c) ? 1 : 0; });
}

template <class InputIt>
constexpr InputIt next(InputIt first, InputIt last)
{
	return first != last ? std::find_if(std::next(first), last, is_lead) : last;
}

} // namespace utf8

class lug_error : public std::runtime_error { using std::runtime_error::runtime_error; };
class grammar_error : public lug_error { using lug_error::lug_error; };
class parser_error : public lug_error { using lug_error::lug_error; };

namespace detail
{

template <class Error, class Condition>
inline void assert_throw(const Condition& cond, const char* msg)
{
	if (!cond)
		throw Error(msg);
}

template <class Error>
struct reentrancy_sentinel
{
	bool& value;
	explicit reentrancy_sentinel(bool& x, const char* msg) : value{x} { assert_throw<Error>(!value, msg); value = true; }
	reentrancy_sentinel(const reentrancy_sentinel&) = delete;
	reentrancy_sentinel& operator=(const reentrancy_sentinel&) = delete;
	~reentrancy_sentinel() { value = false; }
};

} // namespace detail

typedef std::deque<char> syntax_buffer;
typedef syntax_buffer::const_iterator syntax_iterator;

struct syntax_position
{
	size_t column;
	size_t line;
};

struct syntax_match
{
	syntax_iterator first;
	syntax_iterator last;
	syntax_position start;
	syntax_position end;
	std::string to_string() const { return std::string{first, last}; }
};

typedef std::stack<std::any> attribute_stack;
typedef std::function<void(const syntax_match&, attribute_stack&)> syntax_action;

class parser;
typedef std::function<void(parser&)> error_handler;
typedef std::function<bool(parser&)> parse_handler;

class grammar_rule : public std::enable_shared_from_this<grammar_rule>
{
public:
	grammar_rule(const grammar_rule&) = delete;
	grammar_rule& operator=(const grammar_rule&) = delete;
	virtual ~grammar_rule() {}
	virtual bool is_terminal() const { return false; }
	bool apply(parser& p);
protected:
	grammar_rule() = default;
	virtual bool eval(parser& p) = 0;
};

typedef std::unordered_set<std::shared_ptr<grammar_rule>> grammar_rule_set;

void visit_depth_first(grammar_rule* start, const std::function<void(grammar_rule&)>& visitor);

class grammar
{
public:
	grammar() : start_{nullptr} {}
	explicit grammar(std::shared_ptr<grammar_rule_set>& rs, grammar_rule* start) : rules_(std::move(rs)), start_{start} {}
	grammar_rule* start_rule() const { return start_; }
	void start_rule(grammar_rule* start) { start_ = start; }
	const std::shared_ptr<grammar_rule_set>& rule_set() const { return rules_; }
	void rule_set(std::shared_ptr<grammar_rule_set> rs) { rules_ = std::move(rs); }
	const lug::error_handler& error_handler() const { return error_handler_; }
	void error_handler(lug::error_handler handler) { error_handler_ = std::move(handler); }
	void visit_depth_first(const std::function<void(grammar_rule&)>& visitor) { return lug::visit_depth_first(start_, visitor); }
private:
	std::shared_ptr<grammar_rule_set> rules_;
	grammar_rule* start_;
	lug::error_handler error_handler_;
};

class tracer
{
public:
	virtual ~tracer() {}
	virtual void enter(const parser& p, const grammar_rule& r) = 0;
	virtual void leave(const parser& p, const grammar_rule& r, bool success) = 0;
};

struct parser_options
{
	lug::tracer* tracer = nullptr;
};

class parser
{
public:
	typedef std::tuple<syntax_iterator, syntax_iterator, syntax_position, size_t, size_t, std::pair<bool, bool>> state;

	parser(const grammar& g, const parser_options& o)
		: grammar_{g}, options_(o), parsing_{false}, reading_{false} { reset(); }

	template <class InputIt>
	parser& enqueue(InputIt first, InputIt last)
	{
		const auto readjust = should_readjust_iterators();
		buffer_.insert(buffer_.end(), first, last);
		readjust_iterators(readjust);
		return *this;
	}

	template <class InputFunc>
	parser& source(InputFunc&& func)
	{
		detail::assert_throw<parser_error>(!reading_, "new input source cannot be specified while reading from input sources");
		sources_.emplace_back(::std::forward<InputFunc>(func));
		return *this;
	}

	bool parse()
	{
		detail::reentrancy_sentinel<parser_error> guard{parsing_, "lug::parser::parse is non-reenterant"};
		bool result = true;
		reset();
		if (!grammar_.start_rule()->apply(*this)) {
			const auto& handler = grammar_.error_handler();
			if (handler)
				handler(*this);
			result = false;
		}
		if (result) {
			for (const auto& m : matches_)
				(m.first)(m.second, attributes_);
		}
		return result;
	}

	void advance(syntax_iterator next, size_t columns)
	{
		detail::assert_throw<parser_error>(begin_ <= next && next <= end_, "invalid iterator");
		begin_ = next;
		position_.column += columns;
		if (position_.line == max_position_.line && position_.column > max_position_.column) {
			max_column_ = begin_;
			max_position_.column = position_.column;
		}
	}

	bool available(size_t n)
	{
		do {
			if (n <= static_cast<size_t>(std::distance(begin_, end_)))
				return true;
			if (end_ < buffer_.end())
				return false;
		} while (read_more());
		return false;
	}

	void newline()
	{
		position_.column = 1;
		position_.line++;
		if (position_.line > max_position_.line) {
			max_line_ = begin_;
			max_position_ = position_;
		}
	}

	void push_match(state& s, const syntax_action& a)
	{
		matches_.push_back(std::make_pair(a, syntax_match{std::get<0>(s), begin_, std::get<2>(s), position_}));
	}

	void enter(const grammar_rule& r)
	{
		++rule_depth_;
		if (options_.tracer)
			options_.tracer->enter(*this, r);
	}

	void leave(const grammar_rule& r, bool success)
	{
		if (options_.tracer)
			options_.tracer->leave(*this, r, success);
		--rule_depth_;
	}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

	void restore(state& state)
	{
		std::pair<bool, bool> readjust;
		std::tie(begin_, end_, position_, rule_depth_, std::ignore, readjust) = state;
		matches_.resize(std::get<4>(state));
		readjust_iterators(readjust);
	}

	state save() const noexcept
	{
		return {begin_, end_, position_, rule_depth_, matches_.size(), should_readjust_iterators()};
	}

	bool empty() const noexcept { return begin_ >= end_; }
	bool global_empty() const noexcept { return buffer_.empty(); }
	syntax_iterator begin() const noexcept { return begin_; }
	syntax_iterator end() const noexcept { return end_; }
	syntax_iterator global_begin() const noexcept { return buffer_.begin(); }
	syntax_iterator global_end() const noexcept { return buffer_.end(); }
	syntax_iterator max_line() const noexcept { return max_line_; }
	syntax_iterator max_column() const noexcept { return max_column_; }
	const syntax_position& position() const noexcept { return position_; }
	const syntax_position& max_position() const noexcept { return max_position_; }
	size_t rule_depth() const noexcept { return rule_depth_; }

private:
	std::pair<bool, bool> should_readjust_iterators() const noexcept
	{
		return std::make_pair(buffer_.empty(), end_ == buffer_.end());
	}

	void readjust_iterators(const std::pair<bool, bool>& readjust) noexcept
	{
		if (readjust.first)
			begin_ = buffer_.begin();
		if (readjust.second)
			end_ = buffer_.end();
	}

	bool read_more()
	{
		detail::reentrancy_sentinel<parser_error> guard{reading_, "lug::parser::read_more is non-reenterant"};
		const auto readjust = should_readjust_iterators();
		std::string text;
		while (!sources_.empty() && text.empty()) {
			bool more = sources_.back()(text);
			buffer_.insert(buffer_.end(), text.begin(), text.end());
			if (!more)
				sources_.pop_back();
		}
		readjust_iterators(readjust);
		return !text.empty();
	}

	void reset()
	{
		begin_ = buffer_.begin();
		end_ = buffer_.end();
		max_line_ = begin_;
		max_column_ = begin_;
		position_ = {1, 1};
		max_position_ = {1, 1};
		rule_depth_ = 0;
		matches_.clear();
	}

private:
	const lug::grammar& grammar_;
	parser_options options_;
	syntax_iterator begin_;
	syntax_iterator end_;
	syntax_iterator max_line_;
	syntax_iterator max_column_;
	syntax_position position_;
	syntax_position max_position_;
	size_t rule_depth_;
	bool parsing_;
	bool reading_;
	syntax_buffer buffer_;
	std::vector<std::function<bool(std::string&)>> sources_;
	std::vector<std::pair<syntax_action, syntax_match>> matches_;
	attribute_stack attributes_;
};

template <class InputIt, class = typename std::enable_if<std::is_same<char, typename std::iterator_traits<InputIt>::value_type>::value>::type>
inline bool parse(InputIt first, InputIt last, const grammar& g, const parser_options& options = parser_options{})
{
	return parser{g, options}.enqueue(first, last).parse();
}

inline bool parse(const std::string& t, const grammar& g, const parser_options& options = parser_options{})
{
	return parse(t.cbegin(), t.cend(), g, options);
}

inline bool parse(std::istream& s, const grammar& g, const parser_options& options = parser_options{})
{
	return parser{g, options}.source([&s](std::string& t) {
		if (std::getline(s, t)) {
			t.push_back('\n');
			return true;
		}
		return false;
	}).parse();
}

inline bool parse(const grammar& g, const parser_options& options = parser_options{})
{
	return parse(std::cin, g, options);
}

inline bool grammar_rule::apply(parser& p)
{
	p.enter(*this);
	bool success = eval(p);
	p.leave(*this, success);
	return success;
}

class terminal_rule : public grammar_rule
{
public:
	bool is_terminal() const override { return true; }
};

class unary_rule : public grammar_rule
{
public:
	unary_rule() = default;
	explicit unary_rule(grammar_rule* r) : rule_{r} {}
	grammar_rule* rule() const { return rule_; }
	void rule(grammar_rule* r) { rule_ = r; }

protected:
	bool eval(parser& c) override { return rule_->apply(c); }

private:
	grammar_rule* rule_;
};

class multi_rule : public grammar_rule
{
public:
	multi_rule() = default;
	template <class... Args> explicit multi_rule(Args&&... args) : rules_{std::forward<Args>(args)...} {}
	void append_rule(grammar_rule* r) { rules_.push_back(r); }
	size_t rule_count() const { return rules_.size(); }
	grammar_rule* rule_at(size_t i) const { return rules_.at(i); }

private:
	std::vector<grammar_rule*> rules_;
};

class any_rule : public terminal_rule
{
public:
	static any_rule* shared() { static any_rule a; return &a; }

protected:
	bool eval(parser& p) override
	{
		if (!p.available(1))
			return false;
		p.advance(utf8::next(p.begin(), p.end()), 1);
		return true;
	}
};

class char_rule : public terminal_rule
{
public:
	static char_rule* empty() { static char_rule e; return &e; }

public:
	char_rule() : columns_{0} {}
	explicit char_rule(std::string s) : value_{std::move(s)}, columns_{utf8::length(value_.cbegin(), value_.cend())} {}
	const std::string& value() const { return value_; }

protected:
	bool eval(parser& p) override
	{
		if (value_.empty())
			return true;
		if (!p.available(value_.size()))
			return false;
		const auto result = std::mismatch(value_.cbegin(), value_.cend(), p.begin(), p.end());
		if (result.first != value_.cend())
			return false;
		p.advance(result.second, columns_);
		return true;
	}

private:
	std::string value_;
	size_t columns_;
};

class bracket_rule : public terminal_rule
{
public:
	explicit bracket_rule(std::string s)
		: regex_{"[" + s + "]", std::regex_constants::basic | std::regex_constants::optimize}
		, value_{std::move(s)} {}
	const std::regex& regex() const { return regex_; }
	const std::string& value() const { return value_; }

protected:
	bool eval(parser& p) override
	{
		if (!p.available(1))
			return false;
		const auto next = utf8::next(p.begin(), p.end());
		if (!std::regex_match(p.begin(), next, regex_))
			return false;
		p.advance(next, 1);
		return true;
	}

private:
	std::regex regex_;
	std::string value_;
};

class lambda_rule : public terminal_rule
{
public:
	explicit lambda_rule(parse_handler handler) : handler_{std::move(handler)} {}
	const parse_handler& handler() const { return handler_; }

protected:
	bool eval(parser& c) override
	{
		auto state = c.save();
		if (handler_ && handler_(c))
			return true;
		c.restore(state);
		return false;
	}

private:
	parse_handler handler_;
};

class action_rule : public unary_rule
{
public:
	explicit action_rule(grammar_rule* r, syntax_action action)
		: unary_rule{r}, action_{std::move(action)} {}

protected:
	bool eval(parser& p) override
	{
		auto state = p.save();
		if (rule()->apply(p)) {
			p.push_match(state, action_);
			return true;
		}
		p.restore(state);
		return false;
	}

private:
	syntax_action action_;
};

class not_rule : public unary_rule
{
public:
	using unary_rule::unary_rule;

protected:
	bool eval(parser& p) override
	{
		auto state = p.save();
		bool success = !rule()->apply(p);
		p.restore(state);
		return success;
	}
};

class zero_or_more_rule : public unary_rule
{
public:
	using unary_rule::unary_rule;

protected:
	bool eval(parser& p) override
	{
		for (;;) {
			auto state = p.save();
			if (!rule()->apply(p)) {
				p.restore(state);
				break;
			}
		}
		return true;
	}
};

class choice_rule : public multi_rule
{
public:
	using multi_rule::multi_rule;

protected:
	bool eval(parser& p) override
	{
		auto state = p.save();
		for (size_t i = 0, n = rule_count(); i < n; ++i) {
			if (rule_at(i)->apply(p))
				return true;
			p.restore(state);
		}
		return false;
	}
};

class sequence_rule : public multi_rule
{
public:
	using multi_rule::multi_rule;

protected:
	bool eval(parser& p) override
	{
		auto state = p.save();
		for (size_t i = 0, n = rule_count(); i < n; ++i) {
			if (!rule_at(i)->apply(p)) {
				p.restore(state);
				return false;
			}
		}
		return true;
	}
};

inline void visit_depth_first(grammar_rule* start, const std::function<void(grammar_rule&)>& visitor)
{
	std::vector<std::pair<multi_rule*, size_t>> multi_stack;
	std::unordered_set<grammar_rule*> visited;
	grammar_rule* r = start;

	while (r || !multi_stack.empty())
	{
		multi_rule* mr = nullptr;
		size_t i = 0;

		if (r && !visited.count(r)) {
			visited.insert(r);
			visitor(*r);
			unary_rule* ur = dynamic_cast<unary_rule*>(r);
			if (ur) {
				r = ur->rule();
				continue;
			}
			mr = dynamic_cast<multi_rule*>(r);
		}

		for (;;) {
			if (mr && i < mr->rule_count()) {
				multi_stack.emplace_back(mr, i + 1);
				r = mr->rule_at(i);
				break;
			}
			if (multi_stack.empty()) {
				r = nullptr;
				break;
			}
			mr = multi_stack.back().first;
			i = multi_stack.back().second;
			multi_stack.pop_back();
		}
	}
}

namespace language
{

using lug::grammar;
typedef const lug::syntax_match& match;
typedef lug::attribute_stack& attributes;

namespace detail
{

class environment
{
public:
	template <class T, class... Args>
	static auto make(Args&&... args) -> decltype(std::is_base_of<grammar_rule, T>::value, ::std::declval<T*>())
	{
		return static_cast<T*>(local().local_rules_.insert(::std::make_shared<T>(::std::forward<Args>(args)...)).first->get());
	}

	static grammar make_grammar(grammar_rule* start)
	{
		auto rules = std::make_shared<grammar_rule_set>();
		local().start_rules_.emplace_back(start, rules);
		return grammar{std::move(rules), start};
	}

	static void retain() { local().active_count_++; }
	static void release() { local().release_ownership(); }

private:
	static environment& local()
	{
		static thread_local environment e;
		return e;
	}

	void release_ownership()
	{
		if (--active_count_ <= 0) {
			for (const auto& s : start_rules_)
				visit_depth_first(s.first, [&](grammar_rule& r) { s.second->insert(r.shared_from_this()); });
			start_rules_.clear();
			start_rules_.shrink_to_fit();
			local_rules_.clear();
			active_count_ = 0;
		}
	}

	size_t active_count_ = 0;
	grammar_rule_set local_rules_;
	std::vector<std::pair<grammar_rule*, std::shared_ptr<grammar_rule_set>>> start_rules_;
};

class rule_activity_token
{
public:
	rule_activity_token() { environment::retain(); }
	rule_activity_token(rule_activity_token&) { environment::retain(); }
	rule_activity_token(rule_activity_token&&) { environment::retain(); }
	rule_activity_token& operator=(rule_activity_token&) = default;
	rule_activity_token& operator=(rule_activity_token&&) = default;
	//~rule_activity_token() { environment::release(); }
};

template <class E>
class expr : private rule_activity_token
{
public:
	template <class... Args> explicit expr(Args&&... args) : rp_{detail::environment::make<E>(std::forward<Args>(args)...)} {}
	operator grammar() const { return detail::environment::make_grammar(rp_); }
	E* rule() const noexcept { return rp_; }
	expr& append(grammar_rule* rp) { rp_->append_rule(rp); return *this; }
private:
	E* rp_;
};

struct any_rule_tag {};
struct empty_rule_tag {};

} // namespace detail

class rule : private detail::rule_activity_token
{
public:
	rule() : rp_{nullptr} {}
	rule(const rule& r) : rp_{r.get()} {}
	rule(parse_handler f) : rp_{detail::environment::make<lambda_rule>(std::move(f))} {}
	rule(detail::any_rule_tag) : rp_{any_rule::shared()} {}
	rule(detail::empty_rule_tag) : rp_{char_rule::empty()} {}
	explicit rule(grammar_rule* rp) : rp_{rp} {}
	template <class E> rule(detail::expr<E> e) : rp_{static_cast<grammar_rule*>(e.rule())} {}
	rule& operator=(const rule& r) { return set(r.get()); }
	operator grammar() const { return detail::environment::make_grammar(get()); }

	grammar_rule* get() const
	{
		if (!rp_)
			rp_ = detail::environment::make<unary_rule>();
		return rp_;
	}

	rule& set(grammar_rule* r)
	{
		unary_rule* urp = dynamic_cast<unary_rule*>(rp_);
		lug::detail::assert_throw<grammar_error>(urp || !rp_, "cannot re-assign bound rule");
		if (urp) {
			lug::detail::assert_throw<grammar_error>(!urp->rule(), "cannot re-assign bound and referenced rule");
			urp->rule(r);
		} else {
			rp_ = r;
		}
		return *this;
	}

private:
	mutable grammar_rule* rp_;
};

constexpr detail::any_rule_tag _any;
constexpr detail::empty_rule_tag _empty;

inline rule operator""_literal(const char* s, size_t n) { return rule{detail::environment::make<char_rule>(std::string(s, n))}; }
inline rule operator""_bracket(const char* s, size_t n) { return rule{detail::environment::make<bracket_rule>(std::string(s, n))}; }
inline rule operator*(rule r) { return rule{detail::environment::make<zero_or_more_rule>(r.get())}; }
inline rule operator+(rule r) { return rule{detail::environment::make<sequence_rule>(r.get(), detail::environment::make<zero_or_more_rule>(r.get()))}; }
inline rule operator~(rule r) { return rule{detail::environment::make<choice_rule>(r.get(), char_rule::empty())}; }
inline rule operator&(rule r) { return rule{detail::environment::make<not_rule>(detail::environment::make<not_rule>(r.get()))}; }
inline rule operator!(rule r) { return rule{detail::environment::make<not_rule>(r.get())}; }
inline rule operator<(rule r, syntax_action action) { return rule{detail::environment::make<action_rule>(r.get(), std::move(action))}; }
inline detail::expr<sequence_rule> operator>(rule r1, rule r2) { return detail::expr<sequence_rule>{r1.get(), r2.get()}; }
inline detail::expr<sequence_rule>& operator>(detail::expr<sequence_rule>& r1, rule r2) { return r1.append(r2.get()); }
inline detail::expr<choice_rule> operator|(rule r1, rule r2) { return detail::expr<choice_rule>{r1.get(), r2.get()}; }
inline detail::expr<choice_rule>& operator|(detail::expr<choice_rule>& r1, rule r2) { return r1.append(r2.get()); }

inline detail::expr<sequence_rule> operator>=(rule r1, rule r2)
{
	return r1 < [](match m, attributes s) { s.emplace(m.to_string()); } > r2;
}

template <class Action>
inline auto operator<=(rule r, Action a) -> decltype(a(::std::declval<std::string>()),
	typename std::enable_if<std::is_same<typename std::result_of<Action(std::string)>::type, void>::value, rule>::type())
{
	return r < [a](match m, attributes s) {
		std::string l{std::any_cast<std::string>(s.top())};
		s.pop();
		a(l);
	};
}

template <class Action>
inline auto operator<=(rule r, Action a) -> decltype(a(::std::declval<std::string>()),
	typename std::enable_if<!std::is_same<typename std::result_of<Action(std::string)>::type, void>::value, rule>::type())
{
	return r < [a](match m, attributes s) {
		std::string l{std::any_cast<std::string>(s.top())};
		s.pop();
		s.push(std::make_any<typename std::result_of<Action(std::string)>::type>(a(l)));
	};
}

template <class Attribute>
inline rule operator%(Attribute& l, rule r)
{
	return r < [&l](match, attributes s) {
		l = std::any_cast<Attribute>(s.top());
		s.pop();
	};
}

template <class Action>
inline auto operator<(rule r, Action a) -> decltype(a(),
	typename std::enable_if<std::is_same<typename std::result_of<Action()>::type, void>::value, rule>::type())
{
	return r < [a](match, attributes s) { a(); };
}

template <class Action>
inline auto operator<(rule r, Action a) -> decltype(a(),
	typename std::enable_if<!std::is_same<typename std::result_of<Action()>::type, void>::value, rule>::type())
{
	return r < [a](match, attributes s) { s.push(std::make_any<typename std::result_of<Action()>::type>(a())); };
}

} // namespace language

} // namespace lug

#endif // LUG_HPP

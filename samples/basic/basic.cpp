// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

// Derived from BASIC, Dartmouth College Computation Center, October 1st 1964
// http://www.bitsavers.org/pdf/dartmouth/BASIC_Oct64.pdf
// https://www.dartmouth.edu/basicfifty/commands.html

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

#include <cmath>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <list>
#include <map>
#include <random>
#include <unordered_map>
#include <vector>

#ifdef _MSC_VER
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#endif

class basic_interpreter
{
public:
	basic_interpreter()
	{
		using namespace lug::language;

		rule Expr;
		rule Stmnt;

		rule SP     = noskip[*"[ \t]"_rx];
		rule NL     = lexeme['\n'_cx | "\r\n"_sx | '\r'_cx];
		rule Delim  = lexeme[','_cx | ';'_cx];
		rule PrntDl = lexeme[','_cx | ';'_cx <[]{ std::cout << " "; }];
		rule LineNo = lexeme[capture(tok_)[+"[0-9]"_rx]]                 <[this]{ return std::stoi(std::string{tok_}); };
		rule Real   = lexeme[capture(tok_)[+"[0-9]"_rx > ~("."_sx > +"[0-9]"_rx)
		               > ~("[Ee]"_rx > ~"[+-]"_rx > +"[0-9]"_rx)]]       <[this]{ return std::stod(std::string{tok_}); };
		rule String = lexeme["\"" > capture(tok_)[*"[^\"]"_rx] > "\""]   <[this]{ return tok_.str(); };
		rule Var    = lexeme[capture(tok_)["[A-Za-z]"_rx > ~"[0-9]"_rx]] <[this]{ return lug::utf8::toupper(tok_); };
		rule Fn     = lexeme["FN"_isx > capture(tok_)["[A-Za-z]"_rx]]    <[this]{ return lug::utf8::toupper(tok_); };

		rule RelOp  = "="                             <[]() -> RelOpFn { return [](double x, double y) { return x == y; }; }
		            | ">="                            <[]() -> RelOpFn { return std::isgreaterequal; }
		            | ">"                             <[]() -> RelOpFn { return std::isgreater; }
		            | "<="                            <[]() -> RelOpFn { return std::islessequal; }
		            | "<>"                            <[]() -> RelOpFn { return [](double x, double y) { return x != y; }; }
		            | "<"                             <[]() -> RelOpFn { return std::isless; };

		rule Ref    = id_%Var > "(" > r1_%Expr > ","
		                            > r2_%Expr > ")"            <[this]{ return &at(tables_[id_], r1_, r2_); }
		            | id_%Var > "(" > r1_%Expr > ")"            <[this]{ return &at(lists_[id_], r1_); }
		            | id_%Var                                   <[this]{ return &vars_[id_]; };

		rule Value  = !"[A-Z][A-Z][A-Z]"_irx
		            > ( ref_%Ref                                <[this]{ return *ref_; }
		              | Real | "(" > Expr > ")" )
		            | fn_%Fn > "(" > r1_%Expr > ")"             <[this]{ return call(fn_, r1_); }
		            | "SIN"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::sin(r1_); }
		            | "COS"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::cos(r1_); }
		            | "TAN"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::tan(r1_); }
		            | "ATN"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::atan(r1_); }
		            | "EXP"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::exp(r1_); }
		            | "ABS"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::abs(r1_); }
		            | "LOG"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::log(r1_); }
		            | "SQR"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::sqrt(r1_); }
		            | "INT"_isx > "(" > r1_%Expr > ")"          <[this]{ return std::trunc(r1_); }
		            | "RND"_isx > ~ ( "(" > ~(r1_%Expr) > ")" ) <[this]{ return std::uniform_real_distribution{}(random_); };

		rule Factor = r1_%Value > ~("[↑^]"_rx > r2_%Value       <[this]{ r1_ = std::pow(r1_, r2_); }
		            )                                           <[this]{ return r1_; };

		rule Term   = r1_%Factor > *(
		                  "*"_sx > r2_%Factor                   <[this]{ r1_ *= r2_; }
		                | "/"_sx > r2_%Factor                   <[this]{ r1_ /= r2_; }
		            )                                           <[this]{ return r1_; };

		     Expr   = (  ~ "+"_sx > r1_%Term
		                 | "-"_sx > r1_%Term                    <[this]{ r1_ = -r1_; }
		            ) > *( "+"_sx > r2_%Term                    <[this]{ r1_ += r2_; }
		                 | "-"_sx > r2_%Term                    <[this]{ r1_ -= r2_; }
		            )                                           <[this]{ return r1_; };

		rule Rem    = "REM"_isx > *( !NL > any );

		rule FnEval = r1_%Expr > ~Rem                           <[this]{ fn_result_ = r1_; };

		rule DimEl  = id_%Var > "(" > r1_%Expr > ","
		                            > r2_%Expr > ")"            <[this]{ dim(tables_[id_], r1_, r2_); }
		            | id_%Var > "(" > r1_%Expr > ")"            <[this]{ dim(lists_[id_], r1_); };
		rule DataEl = r1_%Real                                  <[this]{ data(r1_); };
		rule ReadEl = ref_%Ref                                  <[this]{ read(*ref_); };
		rule InptEl = ref_%Ref                                  <[this]{ input(*ref_); };
		rule PrntEl = txt_%String                               <[this]{ std::cout << txt_; }
		            | r1_%Expr                                  <[this]{ std::cout << r1_; };

		     Stmnt  = "IF"_isx > r1_%Expr
		                > rop_%RelOp > r2_%Expr                 <[this]{ if (!(rop_)(r1_, r2_)) { environment_.escape(); } }
		                > "THEN"_isx > ( Stmnt
		                               | no_%LineNo             <[this]{ goto_line(no_); } )
		            | "FOR"_isx > id_%Var > "=" > r1_%Expr
		                > "TO"_isx > r2_%Expr
		                > ( "STEP"_isx > r3_%Expr
		                  | &NL < [this]{ r3_ = 1.0; } )        <[this]{ for_to_step(id_, r1_, r2_, r3_); }
		            | "NEXT"_isx > id_%Var                      <[this]{ next(id_); }
		            | "GOTO"_isx > no_%LineNo                   <[this]{ goto_line(no_); }
		            | "DEF"_isx > fn_%Fn
		                > "(" > id_%Var > ")"
		                > "=" > capture(tok_)[*(!NL > any)]     <[this]{ fn_param_body_[fn_] = { id_, std::string{tok_} }; }
		            | "LET"_isx > ref_%Ref > "=" > r1_%Expr     <[this]{ *ref_ = r1_; }
		            | "DIM"_isx > DimEl > *(Delim > DimEl)
		            | "RESTORE"_isx                             <[this]{ read_itr_ = data_.cbegin(); }
		            | "DATA"_isx > DataEl > *(Delim > DataEl)
		            | "READ"_isx > ReadEl > *(Delim > ReadEl)
		            | "INPUT"_isx > InptEl > *(Delim > InptEl)
		            | "PRINT"_isx > ~PrntEl > *(PrntDl > PrntEl)
		                          > (PrntDl | &NL               <[]    { std::cout << "\n"; })
		            | "GOSUB"_isx > no_%LineNo                  <[this]{ gosub(no_); }
		            | "RETURN"_isx                              <[this]{ retsub(); }
		            | "STOP"_isx                                <[this]{ haltline_ = line_; line_ = lines_.end(); }
		            | "END"_isx                                 <[this]{ line_ = lines_.end(); }
		            | ( "BYE"_isx | "GOODBYE"_isx
		              | "EXIT"_isx | "QUIT"_isx )               <[this]{ quit_ = true; };

		rule Cmnd   = "CLEAR"_isx                               <[this]{ clear(); }
		            | "CONT"_isx                                <[this]{ cont(); }
		            | "LIST"_isx                                <[this]{ list(std::cout); }
		            | "LOAD"_isx > txt_%String                  <[this]{ load(txt_); }
		            | "RUN"_isx                                 <[this]{ line_ = lines_.begin(); read_itr_ = data_.cbegin(); }
		            | "SAVE"_isx > txt_%String                  <[this]{ save(txt_); };

		rule Line   = Stmnt > ~Rem > NL
		            | Cmnd > ~Rem > NL
		            | no_%LineNo
		                > capture(tok_)[*(!NL > any) > NL]      <[this]{ update_line(no_, tok_); }
		            | Rem > NL
		            | NL
		            | ( *(!NL > any) > NL )                     <[this]{ print_error("ILLEGAL FORMULA"); };

		rule Init   = when("fnev") > FnEval
		            | unless("fnev") > Line;

		grammar_    = start(Init > eoi, SP);
	}

	void repl()
	{
		lug::parser parser{grammar_, environment_};
		parser.push_source([this](std::back_insert_iterator<std::string> out, lug::source_options opt) {
			if (quit_)
				return false;
			if (line_ != lines_.end()) {
				lastline_ = line_++;
				(void)std::copy(lastline_->second.begin(), lastline_->second.end(), out);
				return true;
			}
			if (tty_)
				std::cout << "> " << std::flush;
			return static_cast<bool>(lug::readsource(std::cin >> std::ws, out, opt));
		}, lug::source_options::interactive);
		std::cout.precision(10);
		while (parser.parse()) ;
	}

	void load(std::string_view name)
	{
		if (std::ifstream file{filename_with_ext(name), std::ifstream::in}; file) {
			while (!file.bad() && !file.eof()) {
				std::string line;
				if (std::getline(file >> std::ws, line)) {
					try {
						std::size_t pos = 0;
						int const lineno = std::stoi(line, &pos);
						while ((pos < line.size()) && std::isspace(line[pos]))
							++pos;
						update_line(lineno, line.substr(pos) + "\n");
					} catch (std::out_of_range const&) {
						print_error("ILLEGAL LINE NUMBER");
						return;
					} catch (...) {
						// ignore lines without a line number
					}
				} else {
					file.clear();
					file.ignore(std::numeric_limits<std::streamsize>::max(), file.widen('\n'));
				}
			}
		} else {
			print_error("FILE DOES NOT EXIST");
		}
	}

	void seed(int value)
	{
		random_.seed(static_cast<std::mt19937_64::result_type>(value));
	}

private:
	struct List { std::vector<double> values = std::vector<double>(11, 0.0); };
	struct Table { std::vector<double> values = std::vector<double>(121, 0.0); std::size_t width = 11, height = 11; };
	using RelOpFn = bool(*)(double, double);

	static std::string filename_with_ext(std::string_view name)
	{
		std::string filename{name};
		if (filename.size() < 4 || strncasecmp(filename.data() + filename.size() - 4, ".BAS", 4) != 0)
			filename.append(".BAS");
		return filename;
	}

	void print_error(std::string_view message)
	{
		std::cout << message << "\n";
		if (!fn_eval_) {
			if (lastline_ != lines_.end())
				std::cout << "LINE " << lastline_->first << ": " << lastline_->second << std::flush;
			line_ = lastline_ = lines_.end();
			stack_.clear();
			for_stack_.clear();
		}
	}

	void clear()
	{
		data_.clear();
		read_itr_ = data_.cbegin();
		vars_.clear();
		lists_.clear();
		tables_.clear();
		fn_param_body_.clear();
		lines_.clear();
		stack_.clear();
		for_stack_.clear();
	}

	void cont()
	{
		line_ = haltline_;
		haltline_ = lines_.end();
		if (line_ == haltline_)
			print_error("CANNOT CONTINUE");
	}

	void list(std::ostream& out)
	{
		for (auto const& [n, l] : lines_)
			out << n << "\t" << l;
		out << std::flush;
	}

	void save(std::string_view name)
	{
		std::ofstream file{filename_with_ext(name), std::ofstream::out};
		if (file)
			list(file);
		else
			print_error("UNABLE TO SAVE TO FILE");
	}

	void update_line(int n, std::string_view s)
	{
		haltline_ = lines_.end();
		if (s.empty() || s.front() < ' ')
			lines_.erase(n);
		else
			lines_[n] = s;
	}

	bool goto_line(int n)
	{
		if (lastline_ = line_, line_ = lines_.find(n); line_ == lines_.end()) {
			print_error("ILLEGAL LINE NUMBER");
			return false;
		}
		return true;
	}

	void gosub(int n)
	{
		lastline_ = line_;
		if (goto_line(n))
			stack_.push_back(lastline_);
	}

	void retsub()
	{
		if (!stack_.empty()) {
			line_ = stack_.back();
			stack_.pop_back();
		} else {
			print_error("ILLEGAL RETURN");
		}
	}

	void for_to_step(std::string const& id, double from, double to, double step)
	{
		if (lastline_ != lines_.end()) {
			double& v = vars_[id];
			v += step;
			if (for_stack_.empty() || (id != for_stack_.back().first)) {
				for_stack_.emplace_back(id, lastline_);
				v = from;
			}
			if (((step >= 0) && (v <= to)) || ((step < 0) && (v >= to)))
				return;
			for_stack_.pop_back();
			for (auto k = id.size(); line_ != lines_.end(); ++line_) {
				auto& t = line_->second;
				if (auto n = t.size(); n > 4 && !strncasecmp(t.data(), "NEXT", 4)) {
					if (auto i = t.find_first_not_of(" \t", 4); i != std::string::npos &&
								i + k <= n && !strncasecmp(t.data() + i, id.data(), k)) {
						lastline_ = line_++;
						return;
					}
				}
			}
		}
		print_error("FOR WITHOUT NEXT");
	}

	void next(std::string const& id)
	{
		if ((lastline_ != lines_.end()) && !for_stack_.empty() && (id == for_stack_.back().first)) {
			lastline_ = line_;
			line_ = for_stack_.back().second;
		} else {
			print_error("NOT MATCH WITH FOR");
		}
	}

	void data(double value)
	{
		bool const reset = data_.empty();
		data_.push_back(value);
		if (reset)
			read_itr_ = data_.cbegin();
	}

	void read(double& value)
	{
		if (read_itr_ != data_.cend())
			value = *(read_itr_++);
		else
			print_error("NO DATA");
	}

	void input(double& value)
	{
		std::cin >> value;
		if (std::cin.fail()) {
			std::cin.clear();
			std::cin.ignore(std::numeric_limits<std::streamsize>::max(), std::cin.widen('\n'));
			print_error("ILLEGAL INPUT");
		}
		if (!(tty_ || std::cin.eof()))
			std::cout << "\n";
	}

	double& at(List& lst, double i)
	{
		auto const index = static_cast<std::size_t>(i);
		if (index < lst.values.size())
			return lst.values[index];
		print_error("ARRAY INDEX OUT OF RANGE");
		return (invalid_value_ = std::numeric_limits<double>::quiet_NaN());
	}

	double& at(Table& tab, double i, double j)
	{
		auto const row = static_cast<std::size_t>(i);
		auto const col = static_cast<std::size_t>(j);
		if (row < tab.height || col < tab.width)
			return tab.values[tab.width * row + col];
		print_error("ARRAY INDEX OUT OF RANGE");
		return (invalid_value_ = std::numeric_limits<double>::quiet_NaN());
	}

	void dim(List& lst, double n)
	{
		if (n < 0.0) {
			print_error("ARRAY SIZE OUT OF RANGE");
			return;
		}
		lst.values = std::vector<double>(static_cast<std::size_t>(n) + 1, 0.0);
	}

	void dim(Table& tab, double m, double n)
	{
		if ((m < 0.0) || (n < 0.0)) {
			print_error("ARRAY SIZE OUT OF RANGE");
			return;
		}
		tab.width = static_cast<std::size_t>(m) + 1;
		tab.height = static_cast<std::size_t>(n) + 1;
		tab.values = std::vector<double>(tab.width * tab.height, 0.0);
	}

	double call(std::string const& name, double arg)
	{
		auto const fn = fn_param_body_.find(name);
		if (fn == fn_param_body_.end()) {
			print_error("UNDEFINED FUNCTION");
			return 0.0;
		}
		auto const& [param, body] = fn->second;
		if (param.empty() || body.empty()) {
			print_error("UNDEFINED FUNCTION");
			return 0.0;
		}
		environment_.should_reset_on_parse(false);
		bool const saved_fn_eval = environment_.set_condition("fnev", true);
		double& param_var = vars_[param];
		double const saved_var = param_var;
		param_var = arg;
		bool const success = lug::parse(body, grammar_, environment_);
		environment_.set_condition("fnev", saved_fn_eval);
		environment_.should_reset_on_parse(true);
		param_var = saved_var;
		if (!success) {
			print_error("EVALUATION ERROR");
			return 0.0;
		}
		return fn_result_;
	}

	lug::grammar grammar_;
	lug::environment environment_;
	std::string fn_;
	std::string id_;
	lug::syntax tok_;
	std::string_view txt_;
	double r1_{0.0};
	double r2_{0.0};
	double r3_{0.0};
	int no_{0};
	double* ref_{nullptr};
	RelOpFn rop_{nullptr};
	std::mt19937_64 random_{std::random_device{}()};
	std::list<double> data_;
	std::list<double>::const_iterator read_itr_{data_.cbegin()};
	std::unordered_map<std::string, double> vars_;
	std::unordered_map<std::string, List> lists_;
	std::unordered_map<std::string, Table> tables_;
	std::unordered_map<std::string, std::pair<std::string, std::string>> fn_param_body_;
	std::map<int, std::string> lines_;
	std::map<int, std::string>::iterator line_{lines_.end()}, lastline_{lines_.end()}, haltline_{lines_.end()};
	std::vector<std::map<int, std::string>::iterator> stack_;
	std::vector<std::pair<std::string, std::map<int, std::string>::iterator>> for_stack_;
	double invalid_value_{std::numeric_limits<double>::quiet_NaN()};
	double fn_result_{0.0};
	bool fn_eval_{false};
	bool quit_{false};
	bool tty_{lug::stdin_isatty()};
};

int main(int argc, char** argv)
try {
	basic_interpreter interpreter;
	for (int i = 1; i < argc; ++i) {
		if (std::string_view const arg{argv[i]}; arg == "-s" || arg == "--seed") {
			if ((i + 1) < argc) {
				try {
					interpreter.seed(std::stoi(argv[i + 1]));
					i++;
				} catch (std::exception const&) {
					throw std::runtime_error{"Invalid seed value for random number generator"};
				}
			} else {
				throw std::runtime_error{"No seed value provided for random number generator"};
			}
		} else if (arg != "-") {
			interpreter.load(argv[i]);
		}
	}
	interpreter.repl();
	return 0;
} catch (std::exception const& e) {
	std::cerr << "ERROR: " << e.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "UNKNOWN ERROR\n";
	return 1;
}

// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>
#include <cstdlib>
#include <map>

class basic_interpreter
{
public:
	basic_interpreter()
	{
		using namespace lug::language;

		rule Expr;

		rule _		= *"[ \t]"s;
		rule S		= +"[ \t]"s;
		rule CR		= "\n"s | "\r\n" | "\r";
		rule Ident	= id_<< +"[A-Z]" > _            <[this]{ return id_; };
		rule Number	= txt_<< (+"[0-9]"s) > _        <[this]{ return std::stoi(std::string{txt_}); };
		rule String	= "\"" > txt_<< *"[^\"]"s > "\"" > _ <[this]{ return txt_; };

		rule RelOp	= "=" > _                       <[]{ return [](int x, int y) { return x == y; }; }
					| ">=" > _                      <[]{ return [](int x, int y) { return x >= y; }; }
					| ">" > _                       <[]{ return [](int x, int y) { return x > y; }; }
					| "<=" > _                      <[]{ return [](int x, int y) { return x <= y; }; }
					| "<>" > _                      <[]{ return [](int x, int y) { return x != y; }; }
					| "<" > _                       <[]{ return [](int x, int y) { return x < y; }; };

		rule Factor	= id_%Ident                     <[this]{ return vars_[id_]; }
					| Number
					| "(" > _ > Expr > ")" > _;

		rule Term	= lnum_%Factor > *(
					      "*" > _ > rnum_%Factor    <[this]{ lnum_ *= rnum_; }
					    | "/" > _ > rnum_%Factor    <[this]{ lnum_ /= rnum_; }
					)                               <[this]{ return lnum_; };

		     Expr	= (  ~ "+"s > _ > lnum_%Term
					     | "-"  > _ > lnum_%Term    <[this]{ lnum_ = -lnum_; }
					) > *( "+"  > _ > rnum_%Term    <[this]{ lnum_ += rnum_; }
					     | "-"  > _ > rnum_%Term    <[this]{ lnum_ -= rnum_; }
					)                               <[this]{ return lnum_; };

		rule InpLst	= id_%Ident                     <[this]{ std::cin >> vars_[id_]; }
					> *( "," > _ > id_%Ident        <[this]{ std::cin >> vars_[id_]; } );

		rule PrtTok	= txt_%String                   <[this]{ std::cout << txt_; }
					| num_%Expr                     <[this]{ std::cout << num_; };

		rule PrtLst	= ~PrtTok > *("," > _ > PrtTok) <[this]{ std::cout << std::endl; };

		rule Stmnt	= "PRINT" > S > PrtLst
					| "IF" > S > Expr > RelOp > Expr > "THEN" > S > Stmnt
					| "GOTO" > S > Expr
					| "INPUT" > S > InpLst
					| "LET" > S > Ident > "=" > _ > Expr
					| "GOSUB" > S > Expr
					| "RETURN" > _
					| "CLEAR" > _
					| "LIST" > _
					| "RUN" > _
					| "END" > _
					| "STOP" > _                    <[this]{ std::exit(EXIT_SUCCESS); }
					| "REM" > _ > *(!CR>".");

		rule Line	= _ > Stmnt > CR
					| _ > num_%Number
					    > txt_<< (*(!CR>".")>CR)    <[this]{ update(num_, txt_); }
					| _ > CR
					| _ > txt_<< (*(!CR>".")>CR)    <[this]{ std::cerr << "syntax error" << std::endl; }
					| _ > !"."s                     <[this]{ std::exit(EXIT_SUCCESS); };

		grammar_ = start(Line);
	}

	int run()
	{
		while (lug::parse(std::cin, grammar_)) ;
		return -1;
	}

private:
	void update(int n, std::string_view s)
	{
		if (s.empty() || s.front() < ' ')
			lines_.erase(n);
		else
			lines_[n] = s;
	}

	lug::grammar grammar_;
	std::string id_;
	std::string_view txt_;
	int num_, lnum_, rnum_;
	std::unordered_map<std::string, int> vars_;
	std::map<int, std::string> lines_;
};

int main()
{
	try {
		basic_interpreter interp;
		return interp.run();
	}
	catch (std::exception& e) {
		std::cerr << "Error: " << e.what() << std::endl;
		return -1;
	}
}

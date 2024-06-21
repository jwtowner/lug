// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2024 Jesse W. Towner
// See LICENSE.md file for license details

#include <lug/lug.hpp>

// Matcher for Extensible Markup Language Standard 1.0
class xml_matcher
{
public:
	xml_matcher()
	{
		using namespace lug::language;

		implicit_space_rule SP = *"[ \t\r\n]"_rx;

		rule Text = noskip[+(!chr('<') > any)];
		rule Name = lexeme[bre("[A-Za-z_:]") > *bre("[A-Za-z0-9_:.-]")];
		rule Attribute = Name > chr('=') > ((chr('\"') > *("[^\"&]"_rx | "&quot;"_sx) > chr('\"')) | (chr('\'') > *("[^'&]"_rx | "&apos;"_sx) > chr('\'')));

		rule CData;
		rule CDataSec = str("<![CDATA[") > CData > str("]]>");
		CData = *(!str("]]>") > !str("<![CDATA[") > any) > ~(str("<![CDATA[") > CData > str("]]>") > CData);

		rule Xml;
		rule Content = Xml | CDataSec | Text;
		rule Comment = str("<!--") > *(!str("-->") > any) > str("-->");
		Xml = local[chr('<') > symbol("tag")[Name] > *Attribute > (str("/>") | (chr('>') > *(Content | Comment) > str("</") > match("tag") > chr('>')))];

		rule DTD = str("<!") > *bre("[^>]") > chr('>');
		rule SDDecl = str("standalone") > chr('=') > ("\"yes\""_sx | "'yes'"_sx | "\"no\""_sx | "'no'"_sx);
		rule EncodingDecl = str("encoding") > chr('=') > ("\"UTF-8\""_sx | "'UTF-8'"_sx);
		rule VersionInfo = str("version") > chr('=') > ("\"1.0\""_sx | "'1.0'"_sx);        
		rule Prolog = str("<?xml") > VersionInfo > ~EncodingDecl > ~SDDecl > str("?>");
		rule File = ~Prolog > *Comment > ~(DTD > *Comment) > Xml > *Comment;

		grammar_ = start(File);
	}

	bool parse(std::istream& input)
	{
		return lug::parse(input, grammar_);
	}

private:
	lug::grammar grammar_;
};

int main()
{
	try {
		xml_matcher matcher;
		if (!matcher.parse(std::cin)) {
			std::cout << "Invalid XML!\n";
			return -1;
		}
	} catch (std::exception& e) {
		std::cerr << "ERROR: " << e.what() << "\n";
		return -1;
	} catch (...) {
		std::cerr << "UNKNOWN ERROR\n";
		return -1;
	}
	return 0;
}

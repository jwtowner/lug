// lug - Embedded DSL for PE grammar parser combinators in C++
// Copyright (c) 2017-2025 Jesse W. Towner
// See LICENSE.md file for license details

#ifndef LUG_SAMPLES_XML_XML_MATCHER_HPP
#define LUG_SAMPLES_XML_XML_MATCHER_HPP

#include <lug/lug.hpp>
#include <lug/iostream.hpp>

// Matcher for Extensible Markup Language Standard 1.0
class xml_matcher
{
public:
	xml_matcher()
	{
		using namespace lug::language;
		namespace lang = lug::language;

		rule SP = noskip[*bkt(" \t\r\n")];
		rule Text = noskip[+(!chr('<') > any)];
		rule Name = lexeme[bkt("A-Za-z_:") > *bkt("A-Za-z0-9_:.-")];
		rule Attribute = Name > '=' > (('\"' > *(bkt("^\"&") | "&quot;"_sx) > '\"') | ('\'' > *(bkt("^'&") | "&apos;"_sx) > '\''));

		rule CData;
		rule CDataSec = "<![CDATA[" > CData > "]]>";
		CData = *(!str("]]>") > !str("<![CDATA[") > any) > ~("<![CDATA[" > CData > "]]>" > CData);

		rule Xml;
		rule Content = Xml | CDataSec | Text;
		rule Comment = "<!--" > *(!str("-->") > any) > "-->";
		Xml = local['<' > symbol("tag")[Name] > *Attribute > ("/>" | ('>' > *(Content | Comment) > "</"_sx > lang::match("tag") > '>'))];

		rule DTD = "<!" > *bkt("^>") > '>';
		rule SDDecl = str("standalone") > '=' > ("\"yes\""_sx | "'yes'"_sx | "\"no\""_sx | "'no'"_sx);
		rule EncodingDecl = str("encoding") > '=' > ("\"UTF-8\""_sx | "'UTF-8'"_sx);
		rule VersionInfo = str("version") > '=' > ("\"1.0\""_sx | "'1.0'"_sx);
		rule Prolog = "<?xml" > VersionInfo > ~EncodingDecl > ~SDDecl > "?>";

		rule File = ~Prolog > *Comment > ~(DTD > *Comment) > Xml > *Comment;
		grammar_ = start(File > eoi, SP);
	}

	template <typename T>
	bool match(T&& t) const
	{
		return lug::parse(std::forward<T>(t), grammar_);
	}

	bool match_cin() const
	{
		return lug::parse(grammar_);
	}

private:
	lug::grammar grammar_;
};

#endif // LUG_SAMPLES_XML_XML_MATCHER_HPP

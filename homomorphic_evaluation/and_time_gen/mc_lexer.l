%{
/* C++ string header, for string ops below */
#include <string>

/* Implementation of yyFlexScanner */ 
#include "mc_scanner.hpp"
#undef  YY_DECL
#define YY_DECL int MC::MC_Scanner::yylex( MC::MC_Parser::semantic_type * const lval, MC::MC_Parser::location_type *loc )

/* typedef to make the returns for the tokens shorter */
using token = MC::MC_Parser::token;

/* define yyterminate as this instead of NULL */
#define yyterminate() return( token::TK_EOF )

/* msvc2010 requires that we exclude this header file. */
#define YY_NO_UNISTD_H

/* update location on matching */
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

%}

%option debug
%option nodefault
%option yyclass="MC::MC_Scanner"
%option noyywrap
%option c++

%%

%{          /** Code executed at the beginning of yylex **/
	yylval = lval;
%}

[ \t\r\n]+
INORDER		{ return ( token::TK_INPUT_LIST ); }
OUTORDER	{ return ( token::TK_OUTPUT_LIST); }
\(			{ return ( token::TK_LPAREN); }
\)			{ return ( token::TK_RPAREN); }
[01]	{ 
			yylval->build( atoi(yytext) );
			return ( token::TK_CONST_BOOL );
			}
\+			{ return ( token::TK_OR ); }
\*			{ return ( token::TK_AND ); }
!			{ return ( token::TK_NOT ); }
=			{ return ( token::TK_EQUAL ); }
;			{ return ( token::TK_SEMICOLON ); }
[_a-zA-Z]([a-zA-Z\'0-9_])*	{ 
							yylval->build<std::string>( yytext );
							return ( token::TK_VAR );
							}
<<EOF>>		{ return ( token::TK_EOF ); }
.			{ return ( token::TK_EOF ); }
%%



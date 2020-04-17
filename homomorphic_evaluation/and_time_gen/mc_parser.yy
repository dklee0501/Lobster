%skeleton "lalr1.cc"
%require  "3.0"
%debug 
%defines 
%define api.value.type variant
%define parse.assert
%define api.namespace {MC}
%define parser_class_name {MC_Parser}

%code requires{

	#include "command.hpp"
   #include <tuple>

   namespace MC {
      class MC_Driver;
      class MC_Scanner;
   }

// The following definitions is missing when %locations isn't used
# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

}

%parse-param { MC_Scanner  &scanner  }
%parse-param { MC_Driver  &driver  }

%code{
   #include <iostream>
   #include <cstdlib>
   #include <fstream>
   
   /* include for all driver functions */
   #include "command.hpp"
   #include "mc_driver.hpp"

#undef yylex
#define yylex scanner.yylex
}

%define parse.error verbose

%token TK_EOF		0     "end of file"
%token TK_INPUT_LIST
%token TK_OUTPUT_LIST
%token TK_LPAREN
%token TK_RPAREN
%token TK_EQUAL
%token TK_SEMICOLON
%token<int> TK_CONST_BOOL
%token<std::string> TK_VAR
%token TK_OR
%token TK_AND
%token TK_NOT

%type<MC::Bexp*> bexp
%type<std::tuple<std::string, MC::Bexp*>> eqn

%locations

%%
main:
	TK_INPUT_LIST TK_EQUAL inputlist TK_SEMICOLON TK_OUTPUT_LIST TK_EQUAL outputlist TK_SEMICOLON eqnlist TK_EOF
	| TK_NOT TK_NOT eqn TK_EOF
;

inputlist
		: TK_VAR inputlist { driver.add_input( $1 ); }
		| TK_VAR { driver.add_input( $1 ); }
;

outputlist
		: TK_VAR outputlist { driver.add_output( $1 ); }
		| TK_VAR { driver.add_output( $1 ); }
;

eqnlist:
	   eqn eqnlist { driver.add_eqn( $1 ); }
	 | eqn { driver.add_eqn( $1 ); }
;

eqn:
   TK_VAR TK_EQUAL bexp TK_SEMICOLON { $$ = std::make_tuple($1, $3); }
;

bexp
	: TK_LPAREN TK_VAR TK_AND TK_NOT TK_VAR TK_RPAREN TK_OR TK_LPAREN TK_NOT TK_VAR TK_AND TK_VAR TK_RPAREN 
		{
		Bexp *b, *l, *r;
		b = new Bexp();
		l = new Bexp();
		r = new Bexp();
		b->head = Bexp::Head::XOR;

		l->head = Bexp::Head::VAR;
		r->head = Bexp::Head::VAR;
		l->var = $2;
		r->var = $5;

		b->left = l;
		b->right = r;

		$$ = b;
		}
	| TK_LPAREN TK_NOT TK_VAR TK_AND TK_VAR TK_RPAREN TK_OR TK_LPAREN TK_VAR TK_AND TK_NOT TK_VAR TK_RPAREN 
		{
		Bexp *b, *l, *r;
		b = new Bexp();
		l = new Bexp();
		r = new Bexp();
		b->head = Bexp::Head::XOR;

		l->head = Bexp::Head::VAR;
		r->head = Bexp::Head::VAR;
		l->var = $3;
		r->var = $5;

		b->left = l;
		b->right = r;

		$$ = b;
		}	
	| TK_LPAREN bexp TK_RPAREN 
		{
		$$ = $2;
		}
	| TK_VAR TK_AND TK_VAR 
		{
		Bexp *b = new Bexp();
		b->head = Bexp::Head::AND;
		
		Bexp *l = new Bexp();
		l->head = Bexp::Head::VAR;
		l->var = $1;
		Bexp *r = new Bexp();
		r->head = Bexp::Head::VAR;
		r->var = $3;
		

		b->left = l;
		b->right = r;
		
		$$ = b;
		}	
	| TK_VAR TK_OR TK_VAR 
		{
		Bexp *b = new Bexp();
		b->head = Bexp::Head::OR;
		
		Bexp *l = new Bexp();
		l->head = Bexp::Head::VAR;
		l->var = $1;
		Bexp *r = new Bexp();
		r->head = Bexp::Head::VAR;
		r->var = $3;
		

		b->left = l;
		b->right = r;
		
		$$ = b;
		}

	| TK_NOT TK_VAR 
		{
		Bexp *b = new Bexp();
		b->head = Bexp::Head::NOT;
		Bexp *r = new Bexp();
		r->head = Bexp::Head::VAR;
		r->var = $2;
		b->right = r;
		$$ = b;
		}
	| TK_CONST_BOOL 
		{ 
		Bexp *b = new Bexp();
		b->head = Bexp::Head::CONST;
		b->constant = $1;
		$$ = b;
		}
	| TK_VAR 
		{
		Bexp *b = new Bexp();
		b->head = Bexp::Head::VAR;
		b->var = $1;
		$$ = b;
		}
%%


void 
MC::MC_Parser::error( const location_type &l, const std::string &err_message )
{
   std::cerr << "Error: " << err_message << " at " << l << "\n";
}

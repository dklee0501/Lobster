%{
	open Circuit
	exception ParsingError
%}

%token EOF
%token TK_INPUT_LIST 
%token TK_OUTPUT_LIST
%token TK_LPAREN TK_RPAREN 
%token TK_EQUAL
%token TK_SEMICOLON

%token<bool> TK_CONST_BOOL
%token<string> TK_VAR
%token TK_OR 
%token TK_AND
%token TK_NOT


%left TK_OR
%left TK_AND
%right TK_NOT



%start main
%type <Circuit.circuit> main
%%

/*

type var = string
type bexp = CONST bool
	  | VAR var
          | AND (bexp * bexp)
          | XOR (bexp * bexp)
type eqn = var * bexp
type input = var
type output = var
type circuit = input list * output list * eqn list

*/

main: 
	  TK_INPUT_LIST TK_EQUAL varlist TK_SEMICOLON TK_OUTPUT_LIST TK_EQUAL varlist TK_SEMICOLON eqnlist EOF { ($3, $7, $9) }
;
varlist:
	  TK_VAR varlist { $1::$2 }
	| TK_VAR { $1::[] }
;
eqnlist:
	  eqn eqnlist { $1::$2 }
	| eqn { $1::[] }
;
eqn:
	  TK_VAR TK_EQUAL bexp TK_SEMICOLON { ($1, $3) }
;

bexp:
	 TK_LPAREN TK_VAR TK_AND TK_NOT TK_VAR TK_RPAREN TK_OR TK_LPAREN TK_NOT TK_VAR TK_AND TK_VAR TK_RPAREN { 
	  if( ($2 = $10) && ($5 = $12) ) then XOR(VAR $2, VAR $5) else let _ = print_string($2 ^ $5) in raise ParsingError 
	  }
	| TK_CONST_BOOL { CONST $1 }
	| TK_VAR { VAR $1 }
	| bexp TK_AND bexp { AND($1, $3) }
	| TK_NOT bexp {NOT $2}
;


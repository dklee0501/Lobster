%{
	open Circuit
        exception ParsingError

%}

%token EOF
%token TK_opt
%token TK_target
%token TK_input
%token TK_list
%token TK_colon
%token TK_old
%token TK_new
%token TK_bexp
%token TK_LP TK_RP
%token TK_BOOL

%token<bool> TK_BOOL_LITERAL
%token<string> TK_VAR
%token TK_AND
%token TK_OR
%token TK_XOR
%token TK_NOT


%left TK_AND
%left TK_OR
%left TK_XOR
%right TK_NOT

%start main
%type <(Circuit.bexp * Circuit.bexp) list> main
%%

main:
	caselist { $1 }
;
caselist: 
	case caselist { $1::$2 }
	| { [] }
;
case: 
	TK_opt TK_target TK_colon TK_VAR TK_LP TK_VAR TK_RP
        TK_input TK_list TK_colon inputlist 
        TK_old TK_bexp TK_colon bexp 
        TK_new TK_bexp TK_colon bexp { ($15, $19) }
;
bexp: 
	| TK_LP bexp TK_RP {$2}
	| bexp TK_AND bexp { AND($1, $3) }
	| bexp TK_OR bexp { OR($1, $3) }
	| bexp TK_XOR bexp { XOR($1, $3) }
	| TK_NOT bexp { NOT($2) }
	| TK_BOOL_LITERAL { CONST $1 }
	| TK_VAR {VAR $1}
;
inputlist:
        inputlist input {}
	| {  }
;
input:
	TK_VAR TK_LP TK_VAR TK_RP { }
;


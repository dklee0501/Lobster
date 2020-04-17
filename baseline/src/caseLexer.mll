{
	open CaseParser
	
}

	let id = ['_' 'a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_'])*
	let num = ('-')?['2'-'9']+
    let zero = ['0']
    let one = ['1']

  rule token = parse
      [' ' '\t' '\n' '\r'] { token lexbuf } 
		| num {token lexbuf}
		| "opt" { TK_opt }
        | "target" { TK_target }
        | "Input" { TK_input }
        | "list" { TK_list }
        | ":" { TK_colon }
        | "old" { TK_old }
        | "new" { TK_new }
        | "bexp" { TK_bexp }
	    | "(" { TK_LP }
	    | ")" { TK_RP }
        | "true" { TK_BOOL_LITERAL true }
        | one { TK_BOOL_LITERAL true }
        | "false" { TK_BOOL_LITERAL false }
        | zero { TK_BOOL_LITERAL false }
        | "and" { TK_AND }
		| "xor" { TK_XOR }
		| "or"  { TK_OR }
		| "not" { TK_NOT }  
		| id as lxm { TK_VAR lxm }
		| eof { EOF }

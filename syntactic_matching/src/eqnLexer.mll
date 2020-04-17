{
	open EqnParser
	
}

	let id = ['_' 'a'-'z' 'A'-'Z'](['a'-'z' 'A'-'Z' '\'' '0'-'9' '_' ':'])*
	let num0 = ['0'] 
	let num1 = ['1']
	
  rule token = parse
      [' ' '\t' '\n' '\r'] { token lexbuf } 
		| "INORDER" { TK_INPUT_LIST }
		| "OUTORDER" { TK_OUTPUT_LIST }
    		| "(" { TK_LPAREN }
    		| ")" { TK_RPAREN }
		| num0 { TK_CONST_BOOL false }
		| num1 { TK_CONST_BOOL true }
		(*| num as lxm { TK_CONST_BOOL (if(int_of_char(lxm) = 1) then true else false) }*)
		| "+" { TK_OR }
		| "*" { TK_AND }
		| "!" { TK_NOT }
		| "=" { TK_EQUAL } 
		| ";" { TK_SEMICOLON } 
    		| id as lxm { TK_VAR lxm }
    		| eof { EOF }

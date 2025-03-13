(* File lexer.mll *)
{
  open Parser
  open Lexing 

}


let int_reg_exp = ['0'-'9']+
let location = ['l'] ['0'-'9']*

	rule token = parse
	  | [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
	  | "skip" 		   { token lexbuf }
	  | '+'            { ADD }
	  | '-'			   { NEG }
	  | ">="		   { GEQ }
	  | '('            { LPAREN }
	  | ')'            { RPAREN }
	  | ';'	           { SEMICOLON }
	  | "if" 		   { IF }
	  | "then"		   { THEN }
	  | "else"		   { ELSE }
	  | "true"		   { BOOL true }
	  | "false"		   { BOOL false }
	  | "while"		   { WHILE }
	  | "do"           { DO }
	  | ":="		   { ASSIGN }
	  | '!'			   { DEREF }
	  | eof            { EOF }  
	  | int_reg_exp { INT (int_of_string (Lexing.lexeme lexbuf)) }
	  | location as l { LOC l }
	  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0)))
}


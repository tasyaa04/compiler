/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token <string> LOC
%token <bool> BOOL
%token ADD GEQ SEMICOLON
%token LPAREN RPAREN
%token IF THEN ELSE
%token WHILE DO
%token ASSIGN
%token DEREF
%token EOF
%left GEQ
%left ADD        
%nonassoc NEG        /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| BOOL 								 { Past.Boolean (get_loc(), $1) }
| LPAREN expr RPAREN                 { $2 }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr GEQ expr						 { Past.Op(get_loc(), $1, Past.GEQ, $3) }
| IF expr THEN expr ELSE expr 		 { Past.If(get_loc(), $2, $4, $6) }
| LOC ASSIGN expr 					 { Past.Assign(get_loc(), $1, $3)}
| DEREF LOC 						 { Past.Deref(get_loc(), $2)}
| expr SEMICOLON expr				 { Past.Seq(get_loc(), $1, $3)}
| WHILE expr DO expr				 { Past.While(get_loc(), $2, $4)}

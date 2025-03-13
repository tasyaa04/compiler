(* 
   The Parsed AST 
*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint 
   | TEbool 
   | TEunit 
   | TEref of TEint

type oper = ADD | GEQ

type unary_oper = NEG 

type expr = 
       | Integer of loc * int
       | Boolean of loc * int
       | UnaryOp of loc * unary_oper * expr
       | Op of loc * expr * oper * expr
       | If of loc * expr * expr * expr
       | Assign of loc * string * expr
       | Deref of loc * string
       | Seq of loc * expr * expr
       | While of loc * expr * expr

val loc_of_expr : expr -> loc 
val string_of_loc : loc -> string 





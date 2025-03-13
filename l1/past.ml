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

let  loc_of_expr = function 
    | Integer (loc, _)              -> loc 
    | Boolean (loc, _)              -> loc
    | UnaryOp(loc, _, _)            -> loc 
    | Op(loc, _, _, _)              -> loc 
	  | Seq(loc, _, _)                -> loc
    | If(loc, _, _, _)              -> loc
    | Assign(loc, _, _)             -> loc 
    | Deref(loc, _)                 -> loc 
    | While(loc, _, _)              -> loc 


let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))


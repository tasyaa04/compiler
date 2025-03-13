
type var = string 

type oper = ADD | GEQ

type unary_oper = NEG

type expr = 
       | Integer of int
       | Boolean of bool
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | Seq of expr * expr
       | If of expr * expr * expr
       | Assign of string * expr 
       | Deref of string
       | While of expr * expr

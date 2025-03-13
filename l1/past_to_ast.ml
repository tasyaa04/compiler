(*   translate_expr : Past.expr -> Ast.expr 
     
	 Lifted and amended from the original Slang interpreter

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.GEQ -> Ast.GEQ


let rec translate_expr = function 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Boolean(_, b)     -> Ast.Boolean b
    | Past.UnaryOp(_, op, e) -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.Seq(_, e1, e2)    -> Ast.Op(translate_expr e1, translate_expr e2)
    | Past.If(_, e, e1, e2)  -> Ast.If(translate_expr e, translate_expr e1, translate_expr e2)
    | Past.Assign(_, l, e)   -> Ast.Assign(l, translate_expr e)
    | Past.Deref(_, l)       -> Ast.Deref(l)
    | Past.While(_, e1, e2)  -> Ast.While(e1, e2)

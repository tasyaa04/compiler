type token =
  | INT of (
# 9 "parser.mly"
        int
# 6 "parser.mli"
)
  | LOC of (
# 10 "parser.mly"
        string
# 11 "parser.mli"
)
  | BOOL of (
# 11 "parser.mly"
        bool
# 16 "parser.mli"
)
  | ADD
  | GEQ
  | SEMICOLON
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | ASSIGN
  | DEREF
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr

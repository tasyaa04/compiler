# 2 "lexer.mll"
 
  open Parser
  open Lexing 


# 8 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\235\255\015\000\025\000\238\255\239\255\000\000\000\000\
    \000\000\000\000\001\000\003\000\001\000\248\255\249\255\250\255\
    \022\000\252\255\253\255\003\000\255\255\001\000\000\000\254\255\
    \251\255\247\255\001\000\012\000\004\000\246\255\019\000\244\255\
    \006\000\021\000\245\255\015\000\009\000\024\000\243\255\021\000\
    \019\000\027\000\242\255\241\255\240\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\019\000\018\000\255\255\255\255\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \020\000\255\255\255\255\020\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\000\000\255\255\255\255\000\000\
    \000\000\000\000\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\020\000\020\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \015\000\014\000\000\000\018\000\000\000\017\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\006\000\013\000\000\000\044\000\016\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\024\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\000\000\000\000\007\000\010\000\009\000\025\000\
    \039\000\012\000\022\000\027\000\002\000\032\000\021\000\043\000\
    \023\000\028\000\029\000\019\000\011\000\026\000\030\000\008\000\
    \031\000\033\000\034\000\036\000\037\000\038\000\040\000\041\000\
    \042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\000\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\006\000\000\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\016\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\009\000\255\255\255\255\000\000\000\000\000\000\012\000\
    \008\000\000\000\021\000\011\000\000\000\010\000\019\000\007\000\
    \022\000\027\000\028\000\000\000\000\000\011\000\026\000\000\000\
    \030\000\032\000\033\000\035\000\036\000\037\000\039\000\040\000\
    \041\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 13 "lexer.mll"
                         ( token lexbuf )
# 126 "lexer.ml"

  | 1 ->
# 14 "lexer.mll"
                 ( token lexbuf )
# 131 "lexer.ml"

  | 2 ->
# 15 "lexer.mll"
                    ( ADD )
# 136 "lexer.ml"

  | 3 ->
# 16 "lexer.mll"
              ( NEG )
# 141 "lexer.ml"

  | 4 ->
# 17 "lexer.mll"
              ( GEQ )
# 146 "lexer.ml"

  | 5 ->
# 18 "lexer.mll"
                    ( LPAREN )
# 151 "lexer.ml"

  | 6 ->
# 19 "lexer.mll"
                    ( RPAREN )
# 156 "lexer.ml"

  | 7 ->
# 20 "lexer.mll"
                    ( SEMICOLON )
# 161 "lexer.ml"

  | 8 ->
# 21 "lexer.mll"
               ( IF )
# 166 "lexer.ml"

  | 9 ->
# 22 "lexer.mll"
                ( THEN )
# 171 "lexer.ml"

  | 10 ->
# 23 "lexer.mll"
                ( ELSE )
# 176 "lexer.ml"

  | 11 ->
# 24 "lexer.mll"
                ( BOOL true )
# 181 "lexer.ml"

  | 12 ->
# 25 "lexer.mll"
                 ( BOOL false )
# 186 "lexer.ml"

  | 13 ->
# 26 "lexer.mll"
                 ( WHILE )
# 191 "lexer.ml"

  | 14 ->
# 27 "lexer.mll"
                    ( DO )
# 196 "lexer.ml"

  | 15 ->
# 28 "lexer.mll"
              ( ASSIGN )
# 201 "lexer.ml"

  | 16 ->
# 29 "lexer.mll"
              ( DEREF )
# 206 "lexer.ml"

  | 17 ->
# 30 "lexer.mll"
                    ( EOF )
# 211 "lexer.ml"

  | 18 ->
# 31 "lexer.mll"
                 ( INT (int_of_string (Lexing.lexeme lexbuf)) )
# 216 "lexer.ml"

  | 19 ->
let
# 32 "lexer.mll"
                 l
# 222 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 32 "lexer.mll"
                   ( LOC l )
# 226 "lexer.ml"

  | 20 ->
# 33 "lexer.mll"
       ( Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0)))
)
# 232 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;


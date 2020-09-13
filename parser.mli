
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TIMES
  | THEN
  | SMALLEQUAL
  | SMALL
  | SET
  | SEQ
  | RP
  | PRINT
  | PLUS
  | PASS
  | NEG
  | MINUS
  | LP
  | INT of (int)
  | IF
  | IDENT of (string)
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DO
  | DIV
  | COMP
  | BIGEQUAL
  | BIG

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

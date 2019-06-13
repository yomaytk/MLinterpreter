
(* The type of tokens. *)

type token = 
  | TYVAR of (string)
  | RPAREN
  | LPAREN
  | LIST
  | INT
  | EOF
  | BOOL
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (TySyntax.ty)

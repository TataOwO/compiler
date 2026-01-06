
(* The type of tokens. *)

type token = 
  | VOID
  | VERTICALBARVERTICALBAR
  | THIS
  | STATIC
  | STAR
  | SLASH
  | SEMICOLON
  | RIGHTPAR
  | RIGHTBRACKET
  | RIGHTBRACE
  | RETURN
  | PUBLIC
  | PLUS
  | PERCENT
  | NULL
  | NEW
  | MINUS
  | LEFTPAR
  | LEFTBRACKET
  | LEFTBRACE
  | INT
  | INSTANCEOF
  | IF
  | IDENT of (string)
  | FOR
  | EXTENDS
  | EQOP of (Ast.binop)
  | EQ
  | EOF
  | ELSE
  | DOT
  | CST of (Ast.constant)
  | COMP of (Ast.binop)
  | COMMA
  | CLASS
  | BOOLEAN
  | BANG
  | AMPERSANDAMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.pfile)

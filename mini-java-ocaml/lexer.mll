
{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

  let kwd_tbl = [
    "boolean", BOOLEAN;
    "class", CLASS;
    "else", ELSE;
    "extends", EXTENDS;
    "false", CST (Cbool false);
    "for", FOR;
    "if", IF;
    "instanceof", INSTANCEOF;
    "int", INT;
    "new", NEW;
    "null", NULL;
    "public", PUBLIC;
    "return", RETURN;
    "static", STATIC;
    "this", THIS;
    "true", CST (Cbool true);
    "void", VOID;
  ]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let string_buffer = Buffer.create 1024
}

let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = '0' | ['1'-'9'] digit*
let space = ' ' | '\t' | '\r'

rule next_token = parse
  | '\n'
      { newline lexbuf; next_token lexbuf }
  | space+
      { next_token lexbuf }
  | "//" [^'\n']* '\n'
      { newline lexbuf; next_token lexbuf }
  | "/*"
      { comment lexbuf; next_token lexbuf }
  | ident as id
      { id_or_kwd id }
  | ';'
      { SEMICOLON }
  | ','
      { COMMA }
  | '.'
      { DOT }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { STAR }
  | '/'
      { SLASH }
  | '%'
      { PERCENT }
  | "&&"
      { AMPERSANDAMPERSAND }
  | "||"
      { VERTICALBARVERTICALBAR }
  | "!"
      { BANG }
  | "="
      { EQ }
  | ">"
      { COMP Bgt }
  | ">="
      { COMP Bge }
  | "<"
      { COMP Blt }
  | "<="
      { COMP Ble }
  | "=="
      { EQOP Beq }
  | "!="
      { EQOP Bneq }
  | '('
      { LEFTPAR }
  | ')'
      { RIGHTPAR }
  | '{'
      { LEFTBRACE }
  | '}'
      { RIGHTBRACE }
  | '['
      { LEFTBRACKET }
  | ']'
      { RIGHTBRACKET }
  | integer as s
      { try CST (Cint (Int32.of_string s))
	with _ -> raise (Lexing_error "literal constant too large") }
  | '"'
      { CST (Cstring (string lexbuf)) }
  | eof
      { EOF }
  | _ as c
      { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n'; string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"'; string lexbuf }
  | "\\\\"
      { Buffer.add_char string_buffer '\\'; string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c; string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }

and comment = parse
  | "*/" { () }
  | "\n" { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexing_error "unterminated comment") }

{
}




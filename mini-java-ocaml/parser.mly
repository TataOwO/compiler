
%{
  open Ast

  let id_main =
    { id = "main"; loc = Lexing.dummy_pos, Lexing.dummy_pos }
%}

%token <Ast.constant> CST
%token <string> IDENT
%token EOF

%token NULL THIS
%token NEW
%token BOOLEAN
%token CLASS ELSE EXTENDS
%token FOR IF
%token INSTANCEOF INT
%token RETURN STATIC PUBLIC
%token VOID

%token LEFTPAR
%token RIGHTPAR LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET
%token SEMICOLON COMMA DOT

%token EQ
%token VERTICALBARVERTICALBAR
%token AMPERSANDAMPERSAND
%token <Ast.binop> EQOP
%token <Ast.binop> COMP
%token PLUS MINUS
%token STAR SLASH PERCENT
%token BANG

%nonassoc THEN
%nonassoc ELSE

%right EQ
%left VERTICALBARVERTICALBAR  /*r conditional OR ["||"] */
%left AMPERSANDAMPERSAND  /*r conditional AND ["&&"] */
%left EQOP                /*r ["=="] and ["!="] */
%left COMP INSTANCEOF  /*r ["<"], ["<="], [">"], [">="] and ["instanceof"] */
%left PLUS MINUS             /*r ["+"] and ["-"] */
%left STAR SLASH PERCENT     /*r ["*"], ["/"] and ["%"] */
%right UMINUS BANG CAST
           /*r unary ["-"], ["++"], ["--"], ["~"], ["!"] and cast */
%nonassoc par_expr
%left DOT

%start file

%type <Ast.pfile> file

%%

file:
| cl = list(class_def) EOF
    { match List.rev cl with
        | (id, None, [PDmethod (None, {id="main"}, [], s)]) :: _ as cl
          when id.id = "Main" -> cl
        | _ -> raise Parsing.Parse_error }
;

class_def:
| CLASS id = ident ext = option(extends) LEFTBRACE dl = list(decl) RIGHTBRACE
    { id, ext, dl }
;

extends:
| EXTENDS id = ident
    { id }
;

decl:
| PUBLIC STATIC VOID ident_main
  LEFTPAR ident_String ident LEFTBRACKET RIGHTBRACKET RIGHTPAR b = block
    { PDmethod(None, id_main, [], b) }
| ty = type_expr; id = ident SEMICOLON
    { PDattribute (ty, id) }
| id = ident; p = parameters; b = block
    { PDconstructor (id, p, b) }
| ty = return_type; id = ident; p = parameters; b = block
    { PDmethod (ty, id, p, b) }
;

%inline return_type:
| ty = type_expr { Some ty }
| VOID           { None    }
;

/* Types. [ident] not included in [base_type], to allow
   factorization in the rule for cast. */

base_type:
| BOOLEAN
    { PTboolean }
| INT
    { PTint }
;

type_expr:
| id = ident
    { PTident id }
| ty = base_type
    { ty }
;

parameters:
| LEFTPAR pl = separated_list(COMMA, parameter) RIGHTPAR
    { pl }
;

parameter:
| ty = type_expr; id = ident
    { ty, id }
;

block:
| LEFTBRACE l=list(stmt) RIGHTBRACE
    { { pstmt_desc = PSblock l; pstmt_loc = $startpos, $endpos } }
;

stmt:
| d = stmt_desc
    { { pstmt_desc = d; pstmt_loc = $startpos, $endpos } }
;

stmt_desc:
| SEMICOLON
    { PSblock [] }
| e = expr SEMICOLON
    { PSexpr e }
| ty = type_expr; id = ident; e = option(initialization) SEMICOLON;
    { PSvar (ty, id, e) }
| IF LEFTPAR e = expr RIGHTPAR s = stmt %prec THEN
    { PSif (e, s, { pstmt_desc = PSblock []; pstmt_loc = $startpos, $endpos }) }
| IF LEFTPAR e = expr RIGHTPAR s1 = stmt ELSE s2 = stmt
    { PSif (e, s1, s2) }
| RETURN e = option(expr) SEMICOLON
    { PSreturn e }
| b = block
    { b.pstmt_desc }
| FOR LEFTPAR
  e1 = option(expr) SEMICOLON e2 = option(expr) SEMICOLON e3 = option(expr)
  RIGHTPAR s = stmt
    { let loc = $startpos, $endpos in
      let stmt e =
        let d = match e with None -> PSblock [] | Some e -> PSexpr e in
        { pstmt_desc = d; pstmt_loc = loc }
      in
      let e2 = match e2 with
        | None -> { pexpr_desc = PEconstant (Cbool true); pexpr_loc = loc }
        | Some e -> e
      in
      PSfor (stmt e1, e2, stmt e3, s) }
;

initialization:
| EQ e = expr
    { e }
;

expr:
| d = expr_desc
    { { pexpr_desc = d; pexpr_loc = $startpos, $endpos } }
;

expr_desc:
| c = CST
    { PEconstant c }
| e1 = expr; op = binop; e2 = expr
    { PEbinop (op, e1, e2) }
| BANG; e1 = expr
    { PEunop (Unot, e1) }
| MINUS e1 = expr %prec UMINUS
    { PEunop (Uneg, e1) }
| THIS
    { PEthis }
| NULL
    { PEnull }
| NEW id = ident; al = arguments
    { PEnew (id, al) }
| id = ident; al = arguments
    { let this = { pexpr_desc = PEthis; pexpr_loc = $startpos, $endpos } in
      PEcall (this, id, al) }
| e = expr DOT id = ident; al = arguments
    { PEcall (e, id, al) }
| id = ident
    { PEident id }
| e = expr DOT id = ident
    { PEdot (e, id) }
| id = ident EQ e = expr
    { PEassign_ident (id, e) }
| e1 = expr DOT id = ident EQ e2 = expr
    { PEassign_dot (e1, id, e2) }
| LEFTPAR ty = base_type RIGHTPAR e = expr %prec CAST
    { PEcast (ty, e) }
| LEFTPAR ty = expr RIGHTPAR e = expr %prec CAST
    { match ty.pexpr_desc with
	| PEident c -> PEcast (PTident c, e)
	| _ -> raise Parsing.Parse_error }
| e = expr INSTANCEOF ty = type_expr
    { PEinstanceof (e, ty) }
| LEFTPAR e = expr RIGHTPAR  %prec par_expr
    { e.pexpr_desc }
;

arguments:
| LEFTPAR l = separated_list(COMMA, expr) RIGHTPAR
    { l }
;

%inline binop:
| PLUS     { Badd }
| MINUS    { Bsub }
| STAR     { Bmul }
| SLASH    { Bdiv }
| PERCENT  { Bmod }
| c = COMP { c }
| c = EQOP { c }
| AMPERSANDAMPERSAND     { Band }
| VERTICALBARVERTICALBAR { Bor  }
;

ident_main:
| id = ident { if id.id <> "main" then raise Parsing.Parse_error; id }
;

ident_String:
| id = ident { if id.id <> "String" then raise Parsing.Parse_error; id }
;

ident:
| id = IDENT { { loc = $startpos, $endpos; id = id } }
;



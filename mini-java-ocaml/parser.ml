
module MenhirBasics = struct
  
  exception Error = Parsing.Parse_error
  
  let _eRR =
    fun _s ->
      raise Error
  
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
    | IDENT of (
# 10 "parser.mly"
       (string)
# 38 "parser.ml"
  )
    | FOR
    | EXTENDS
    | EQOP of (
# 29 "parser.mly"
       (Ast.binop)
# 45 "parser.ml"
  )
    | EQ
    | EOF
    | ELSE
    | DOT
    | CST of (
# 9 "parser.mly"
       (Ast.constant)
# 54 "parser.ml"
  )
    | COMP of (
# 30 "parser.mly"
       (Ast.binop)
# 59 "parser.ml"
  )
    | COMMA
    | CLASS
    | BOOLEAN
    | BANG
    | AMPERSANDAMPERSAND
  
end

include MenhirBasics

# 2 "parser.mly"
  
  open Ast

  let id_main =
    { id = "main"; loc = Lexing.dummy_pos, Lexing.dummy_pos }

# 78 "parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_file) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: file. *)

  | MenhirState001 : (('s, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_state
    (** State 001.
        Stack shape : CLASS.
        Start symbol: file. *)

  | MenhirState004 : ((('s, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 004.
        Stack shape : CLASS ident.
        Start symbol: file. *)

  | MenhirState007 : ((('s, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_cell1_ident _menhir_cell0_option_extends_ _menhir_cell0_LEFTBRACE, _menhir_box_file) _menhir_state
    (** State 007.
        Stack shape : CLASS ident option(extends) LEFTBRACE.
        Start symbol: file. *)

  | MenhirState008 : (('s, _menhir_box_file) _menhir_cell1_VOID, _menhir_box_file) _menhir_state
    (** State 008.
        Stack shape : VOID.
        Start symbol: file. *)

  | MenhirState009 : ((('s, _menhir_box_file) _menhir_cell1_VOID, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 009.
        Stack shape : VOID ident.
        Start symbol: file. *)

  | MenhirState010 : ((('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 010.
        Stack shape : ident LEFTPAR.
        Start symbol: file. *)

  | MenhirState013 : (('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_state
    (** State 013.
        Stack shape : type_expr.
        Start symbol: file. *)

  | MenhirState017 : (('s, _menhir_box_file) _menhir_cell1_parameter, _menhir_box_file) _menhir_state
    (** State 017.
        Stack shape : parameter.
        Start symbol: file. *)

  | MenhirState023 : (((('s, _menhir_box_file) _menhir_cell1_VOID, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters, _menhir_box_file) _menhir_state
    (** State 023.
        Stack shape : VOID ident parameters.
        Start symbol: file. *)

  | MenhirState024 : (('s, _menhir_box_file) _menhir_cell1_LEFTBRACE, _menhir_box_file) _menhir_state
    (** State 024.
        Stack shape : LEFTBRACE.
        Start symbol: file. *)

  | MenhirState027 : (('s, _menhir_box_file) _menhir_cell1_RETURN, _menhir_box_file) _menhir_state
    (** State 027.
        Stack shape : RETURN.
        Start symbol: file. *)

  | MenhirState029 : (('s, _menhir_box_file) _menhir_cell1_NEW, _menhir_box_file) _menhir_state
    (** State 029.
        Stack shape : NEW.
        Start symbol: file. *)

  | MenhirState030 : ((('s, _menhir_box_file) _menhir_cell1_NEW, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 030.
        Stack shape : NEW ident.
        Start symbol: file. *)

  | MenhirState031 : ((('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 031.
        Stack shape : ident LEFTPAR.
        Start symbol: file. *)

  | MenhirState032 : (('s, _menhir_box_file) _menhir_cell1_MINUS, _menhir_box_file) _menhir_state
    (** State 032.
        Stack shape : MINUS.
        Start symbol: file. *)

  | MenhirState033 : (('s, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 033.
        Stack shape : LEFTPAR.
        Start symbol: file. *)

  | MenhirState035 : (('s, _menhir_box_file) _menhir_cell1_BANG, _menhir_box_file) _menhir_state
    (** State 035.
        Stack shape : BANG.
        Start symbol: file. *)

  | MenhirState036 : (('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 036.
        Stack shape : ident.
        Start symbol: file. *)

  | MenhirState037 : ((('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_EQ, _menhir_box_file) _menhir_state
    (** State 037.
        Stack shape : ident EQ.
        Start symbol: file. *)

  | MenhirState040 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 040.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState042 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 042.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState044 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 044.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState045 : ((('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 045.
        Stack shape : expr ident.
        Start symbol: file. *)

  | MenhirState046 : (((('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_EQ, _menhir_box_file) _menhir_state
    (** State 046.
        Stack shape : expr ident EQ.
        Start symbol: file. *)

  | MenhirState048 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 048.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState050 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 050.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState052 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 052.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState054 : (('s, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_MINUS, _menhir_box_file) _menhir_state
    (** State 054.
        Stack shape : expr MINUS.
        Start symbol: file. *)

  | MenhirState056 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 056.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState058 : (('s, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_EQOP, _menhir_box_file) _menhir_state
    (** State 058.
        Stack shape : expr EQOP.
        Start symbol: file. *)

  | MenhirState060 : (('s, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_COMP, _menhir_box_file) _menhir_state
    (** State 060.
        Stack shape : expr COMP.
        Start symbol: file. *)

  | MenhirState062 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 062.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState068 : ((('s, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_state
    (** State 068.
        Stack shape : LEFTPAR expr RIGHTPAR.
        Start symbol: file. *)

  | MenhirState071 : ((('s, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_cell1_base_type _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_state
    (** State 071.
        Stack shape : LEFTPAR base_type RIGHTPAR.
        Start symbol: file. *)

  | MenhirState078 : (('s, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_state
    (** State 078.
        Stack shape : expr.
        Start symbol: file. *)

  | MenhirState085 : (('s, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 085.
        Stack shape : IF LEFTPAR.
        Start symbol: file. *)

  | MenhirState087 : ((('s, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_state
    (** State 087.
        Stack shape : IF LEFTPAR expr RIGHTPAR.
        Start symbol: file. *)

  | MenhirState089 : (('s, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 089.
        Stack shape : FOR LEFTPAR.
        Start symbol: file. *)

  | MenhirState091 : ((('s, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_state
    (** State 091.
        Stack shape : FOR LEFTPAR option(expr) SEMICOLON.
        Start symbol: file. *)

  | MenhirState093 : (((('s, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_state
    (** State 093.
        Stack shape : FOR LEFTPAR option(expr) SEMICOLON option(expr) SEMICOLON.
        Start symbol: file. *)

  | MenhirState095 : ((((('s, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_state
    (** State 095.
        Stack shape : FOR LEFTPAR option(expr) SEMICOLON option(expr) SEMICOLON option(expr) RIGHTPAR.
        Start symbol: file. *)

  | MenhirState096 : (('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_state
    (** State 096.
        Stack shape : type_expr.
        Start symbol: file. *)

  | MenhirState098 : ((('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 098.
        Stack shape : type_expr ident.
        Start symbol: file. *)

  | MenhirState105 : (('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 105.
        Stack shape : ident.
        Start symbol: file. *)

  | MenhirState110 : (((('s, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_cell1_stmt, _menhir_box_file) _menhir_state
    (** State 110.
        Stack shape : IF LEFTPAR expr RIGHTPAR stmt.
        Start symbol: file. *)

  | MenhirState112 : (('s, _menhir_box_file) _menhir_cell1_stmt, _menhir_box_file) _menhir_state
    (** State 112.
        Stack shape : stmt.
        Start symbol: file. *)

  | MenhirState119 : (('s, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_state
    (** State 119.
        Stack shape : PUBLIC.
        Start symbol: file. *)

  | MenhirState121 : ((('s, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_state
    (** State 121.
        Stack shape : PUBLIC ident_main LEFTPAR.
        Start symbol: file. *)

  | MenhirState122 : (((('s, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_ident_String, _menhir_box_file) _menhir_state
    (** State 122.
        Stack shape : PUBLIC ident_main LEFTPAR ident_String.
        Start symbol: file. *)

  | MenhirState126 : ((((('s, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_ident_String, _menhir_box_file) _menhir_cell1_ident _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_state
    (** State 126.
        Stack shape : PUBLIC ident_main LEFTPAR ident_String ident RIGHTPAR.
        Start symbol: file. *)

  | MenhirState130 : (('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_state
    (** State 130.
        Stack shape : type_expr.
        Start symbol: file. *)

  | MenhirState131 : ((('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 131.
        Stack shape : type_expr ident.
        Start symbol: file. *)

  | MenhirState133 : (((('s, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters, _menhir_box_file) _menhir_state
    (** State 133.
        Stack shape : type_expr ident parameters.
        Start symbol: file. *)

  | MenhirState137 : (('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_state
    (** State 137.
        Stack shape : ident.
        Start symbol: file. *)

  | MenhirState138 : ((('s, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters, _menhir_box_file) _menhir_state
    (** State 138.
        Stack shape : ident parameters.
        Start symbol: file. *)

  | MenhirState140 : (('s, _menhir_box_file) _menhir_cell1_decl, _menhir_box_file) _menhir_state
    (** State 140.
        Stack shape : decl.
        Start symbol: file. *)

  | MenhirState146 : (('s, _menhir_box_file) _menhir_cell1_class_def, _menhir_box_file) _menhir_state
    (** State 146.
        Stack shape : class_def.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_base_type = 
  | MenhirCell1_base_type of 's * ('s, 'r) _menhir_state * (Ast.pexpr_typ) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_class_def = 
  | MenhirCell1_class_def of 's * ('s, 'r) _menhir_state * (Ast.pclass)

and ('s, 'r) _menhir_cell1_decl = 
  | MenhirCell1_decl of 's * ('s, 'r) _menhir_state * (Ast.pdecl)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.pexpr) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_ident = 
  | MenhirCell1_ident of 's * ('s, 'r) _menhir_state * (Ast.ident) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_ident_String = 
  | MenhirCell1_ident_String of 's * ('s, 'r) _menhir_state * (Ast.ident)

and ('s, 'r) _menhir_cell1_ident_main = 
  | MenhirCell1_ident_main of 's * ('s, 'r) _menhir_state * (Ast.ident)

and ('s, 'r) _menhir_cell1_option_expr_ = 
  | MenhirCell1_option_expr_ of 's * ('s, 'r) _menhir_state * (Ast.pexpr option)

and 's _menhir_cell0_option_extends_ = 
  | MenhirCell0_option_extends_ of 's * (Ast.ident option)

and ('s, 'r) _menhir_cell1_parameter = 
  | MenhirCell1_parameter of 's * ('s, 'r) _menhir_state * (Ast.pparam)

and ('s, 'r) _menhir_cell1_parameters = 
  | MenhirCell1_parameters of 's * ('s, 'r) _menhir_state * (Ast.pparam list)

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Ast.pstmt) * Lexing.position

and ('s, 'r) _menhir_cell1_type_expr = 
  | MenhirCell1_type_expr of 's * ('s, 'r) _menhir_state * (Ast.pexpr_typ) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_BANG = 
  | MenhirCell1_BANG of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_CLASS = 
  | MenhirCell1_CLASS of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_COMP = 
  | MenhirCell0_COMP of 's * (
# 30 "parser.mly"
       (Ast.binop)
# 421 "parser.ml"
)

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_EQOP = 
  | MenhirCell0_EQOP of 's * (
# 29 "parser.mly"
       (Ast.binop)
# 431 "parser.ml"
)

and ('s, 'r) _menhir_cell1_FOR = 
  | MenhirCell1_FOR of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LEFTBRACE = 
  | MenhirCell1_LEFTBRACE of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_LEFTBRACE = 
  | MenhirCell0_LEFTBRACE of 's * Lexing.position

and ('s, 'r) _menhir_cell1_LEFTPAR = 
  | MenhirCell1_LEFTPAR of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_LEFTPAR = 
  | MenhirCell0_LEFTPAR of 's * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_MINUS = 
  | MenhirCell0_MINUS of 's * Lexing.position

and ('s, 'r) _menhir_cell1_NEW = 
  | MenhirCell1_NEW of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_PUBLIC = 
  | MenhirCell1_PUBLIC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_RIGHTPAR = 
  | MenhirCell0_RIGHTPAR of 's * Lexing.position

and 's _menhir_cell0_SEMICOLON = 
  | MenhirCell0_SEMICOLON of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_VOID = 
  | MenhirCell1_VOID of 's * ('s, 'r) _menhir_state

and _menhir_box_file = 
  | MenhirBox_file of (Ast.pfile) [@@unboxed]

let _menhir_action_01 =
  fun xs ->
    let l = 
# 241 "<standard.mly>"
    ( xs )
# 484 "parser.ml"
     in
    (
# 210 "parser.mly"
    ( l )
# 489 "parser.ml"
     : (Ast.pexpr list))

let _menhir_action_02 =
  fun () ->
    (
# 96 "parser.mly"
    ( PTboolean )
# 497 "parser.ml"
     : (Ast.pexpr_typ))

let _menhir_action_03 =
  fun () ->
    (
# 98 "parser.mly"
    ( PTint )
# 505 "parser.ml"
     : (Ast.pexpr_typ))

let _menhir_action_04 =
  fun _endpos__3_ _startpos__1_ l ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 120 "parser.mly"
    ( { pstmt_desc = PSblock l; pstmt_loc = _startpos, _endpos } )
# 515 "parser.ml"
     : (Ast.pstmt))

let _menhir_action_05 =
  fun dl ext id ->
    (
# 66 "parser.mly"
    ( id, ext, dl )
# 523 "parser.ml"
     : (Ast.pclass))

let _menhir_action_06 =
  fun b ->
    (
# 77 "parser.mly"
    ( PDmethod(None, id_main, [], b) )
# 531 "parser.ml"
     : (Ast.pdecl))

let _menhir_action_07 =
  fun id ty ->
    (
# 79 "parser.mly"
    ( PDattribute (ty, id) )
# 539 "parser.ml"
     : (Ast.pdecl))

let _menhir_action_08 =
  fun b id p ->
    (
# 81 "parser.mly"
    ( PDconstructor (id, p, b) )
# 547 "parser.ml"
     : (Ast.pdecl))

let _menhir_action_09 =
  fun b id p ty ->
    let ty = 
# 87 "parser.mly"
                 ( Some ty )
# 555 "parser.ml"
     in
    (
# 83 "parser.mly"
    ( PDmethod (ty, id, p, b) )
# 560 "parser.ml"
     : (Ast.pdecl))

let _menhir_action_10 =
  fun b id p ->
    let ty = 
# 88 "parser.mly"
                 ( None    )
# 568 "parser.ml"
     in
    (
# 83 "parser.mly"
    ( PDmethod (ty, id, p, b) )
# 573 "parser.ml"
     : (Ast.pdecl))

let _menhir_action_11 =
  fun _endpos_d_ _startpos_d_ d ->
    let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    (
# 165 "parser.mly"
    ( { pexpr_desc = d; pexpr_loc = _startpos, _endpos } )
# 583 "parser.ml"
     : (Ast.pexpr))

let _menhir_action_12 =
  fun c ->
    (
# 170 "parser.mly"
    ( PEconstant c )
# 591 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_13 =
  fun e1 e2 ->
    let op = 
# 214 "parser.mly"
           ( Badd )
# 599 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 604 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_14 =
  fun e1 e2 ->
    let op = 
# 215 "parser.mly"
           ( Bsub )
# 612 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 617 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_15 =
  fun e1 e2 ->
    let op = 
# 216 "parser.mly"
           ( Bmul )
# 625 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 630 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_16 =
  fun e1 e2 ->
    let op = 
# 217 "parser.mly"
           ( Bdiv )
# 638 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 643 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_17 =
  fun e1 e2 ->
    let op = 
# 218 "parser.mly"
           ( Bmod )
# 651 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 656 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_18 =
  fun c e1 e2 ->
    let op = 
# 219 "parser.mly"
           ( c )
# 664 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 669 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_19 =
  fun c e1 e2 ->
    let op = 
# 220 "parser.mly"
           ( c )
# 677 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 682 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_20 =
  fun e1 e2 ->
    let op = 
# 221 "parser.mly"
                         ( Band )
# 690 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 695 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_21 =
  fun e1 e2 ->
    let op = 
# 222 "parser.mly"
                         ( Bor  )
# 703 "parser.ml"
     in
    (
# 172 "parser.mly"
    ( PEbinop (op, e1, e2) )
# 708 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_22 =
  fun e1 ->
    (
# 174 "parser.mly"
    ( PEunop (Unot, e1) )
# 716 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_23 =
  fun e1 ->
    (
# 176 "parser.mly"
    ( PEunop (Uneg, e1) )
# 724 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_24 =
  fun () ->
    (
# 178 "parser.mly"
    ( PEthis )
# 732 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_25 =
  fun () ->
    (
# 180 "parser.mly"
    ( PEnull )
# 740 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_26 =
  fun al id ->
    (
# 182 "parser.mly"
    ( PEnew (id, al) )
# 748 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_27 =
  fun _endpos_al_ _startpos_id_ al id ->
    let _endpos = _endpos_al_ in
    let _startpos = _startpos_id_ in
    (
# 184 "parser.mly"
    ( let this = { pexpr_desc = PEthis; pexpr_loc = _startpos, _endpos } in
      PEcall (this, id, al) )
# 759 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_28 =
  fun al e id ->
    (
# 187 "parser.mly"
    ( PEcall (e, id, al) )
# 767 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_29 =
  fun id ->
    (
# 189 "parser.mly"
    ( PEident id )
# 775 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_30 =
  fun e id ->
    (
# 191 "parser.mly"
    ( PEdot (e, id) )
# 783 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_31 =
  fun e id ->
    (
# 193 "parser.mly"
    ( PEassign_ident (id, e) )
# 791 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_32 =
  fun e1 e2 id ->
    (
# 195 "parser.mly"
    ( PEassign_dot (e1, id, e2) )
# 799 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_33 =
  fun e ty ->
    (
# 197 "parser.mly"
    ( PEcast (ty, e) )
# 807 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_34 =
  fun e ty ->
    (
# 199 "parser.mly"
    ( match ty.pexpr_desc with
	| PEident c -> PEcast (PTident c, e)
	| _ -> raise Parsing.Parse_error )
# 817 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_35 =
  fun e ty ->
    (
# 203 "parser.mly"
    ( PEinstanceof (e, ty) )
# 825 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_36 =
  fun e ->
    (
# 205 "parser.mly"
    ( e.pexpr_desc )
# 833 "parser.ml"
     : (Ast.pexpr_desc))

let _menhir_action_37 =
  fun id ->
    (
# 71 "parser.mly"
    ( id )
# 841 "parser.ml"
     : (Ast.ident))

let _menhir_action_38 =
  fun cl ->
    (
# 58 "parser.mly"
    ( match List.rev cl with
        | (id, None, [PDmethod (None, {id="main"}, [], s)]) :: _ as cl
          when id.id = "Main" -> cl
        | _ -> raise Parsing.Parse_error )
# 852 "parser.ml"
     : (Ast.pfile))

let _menhir_action_39 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 234 "parser.mly"
             ( { loc = _startpos, _endpos; id = id } )
# 862 "parser.ml"
     : (Ast.ident))

let _menhir_action_40 =
  fun id ->
    (
# 230 "parser.mly"
             ( if id.id <> "String" then raise Parsing.Parse_error; id )
# 870 "parser.ml"
     : (Ast.ident))

let _menhir_action_41 =
  fun id ->
    (
# 226 "parser.mly"
             ( if id.id <> "main" then raise Parsing.Parse_error; id )
# 878 "parser.ml"
     : (Ast.ident))

let _menhir_action_42 =
  fun e ->
    (
# 160 "parser.mly"
    ( e )
# 886 "parser.ml"
     : (Ast.pexpr))

let _menhir_action_43 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 894 "parser.ml"
     : (Ast.pclass list))

let _menhir_action_44 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 902 "parser.ml"
     : (Ast.pclass list))

let _menhir_action_45 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 910 "parser.ml"
     : (Ast.pdecl list))

let _menhir_action_46 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 918 "parser.ml"
     : (Ast.pdecl list))

let _menhir_action_47 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 926 "parser.ml"
     : (Ast.pstmt list))

let _menhir_action_48 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 934 "parser.ml"
     : (Ast.pstmt list))

let _menhir_action_49 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 942 "parser.ml"
     : (Ast.pexpr list))

let _menhir_action_50 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 950 "parser.ml"
     : (Ast.pexpr list))

let _menhir_action_51 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 958 "parser.ml"
     : (Ast.pparam list))

let _menhir_action_52 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 966 "parser.ml"
     : (Ast.pparam list))

let _menhir_action_53 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 974 "parser.ml"
     : (Ast.pexpr option))

let _menhir_action_54 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 982 "parser.ml"
     : (Ast.pexpr option))

let _menhir_action_55 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 990 "parser.ml"
     : (Ast.ident option))

let _menhir_action_56 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 998 "parser.ml"
     : (Ast.ident option))

let _menhir_action_57 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 1006 "parser.ml"
     : (Ast.pexpr option))

let _menhir_action_58 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 1014 "parser.ml"
     : (Ast.pexpr option))

let _menhir_action_59 =
  fun id ty ->
    (
# 115 "parser.mly"
    ( ty, id )
# 1022 "parser.ml"
     : (Ast.pparam))

let _menhir_action_60 =
  fun xs ->
    let pl = 
# 241 "<standard.mly>"
    ( xs )
# 1030 "parser.ml"
     in
    (
# 110 "parser.mly"
    ( pl )
# 1035 "parser.ml"
     : (Ast.pparam list))

let _menhir_action_61 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 1043 "parser.ml"
     : (Ast.pexpr list))

let _menhir_action_62 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 1051 "parser.ml"
     : (Ast.pexpr list))

let _menhir_action_63 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 1059 "parser.ml"
     : (Ast.pparam list))

let _menhir_action_64 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 1067 "parser.ml"
     : (Ast.pparam list))

let _menhir_action_65 =
  fun _endpos_d_ _startpos_d_ d ->
    let _endpos = _endpos_d_ in
    let _startpos = _startpos_d_ in
    (
# 125 "parser.mly"
    ( { pstmt_desc = d; pstmt_loc = _startpos, _endpos } )
# 1077 "parser.ml"
     : (Ast.pstmt))

let _menhir_action_66 =
  fun () ->
    (
# 130 "parser.mly"
    ( PSblock [] )
# 1085 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_67 =
  fun e ->
    (
# 132 "parser.mly"
    ( PSexpr e )
# 1093 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_68 =
  fun e id ty ->
    (
# 134 "parser.mly"
    ( PSvar (ty, id, e) )
# 1101 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_69 =
  fun _endpos_s_ _startpos__1_ e s ->
    let _endpos = _endpos_s_ in
    let _startpos = _startpos__1_ in
    (
# 136 "parser.mly"
    ( PSif (e, s, { pstmt_desc = PSblock []; pstmt_loc = _startpos, _endpos }) )
# 1111 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_70 =
  fun e s1 s2 ->
    (
# 138 "parser.mly"
    ( PSif (e, s1, s2) )
# 1119 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_71 =
  fun e ->
    (
# 140 "parser.mly"
    ( PSreturn e )
# 1127 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_72 =
  fun b ->
    (
# 142 "parser.mly"
    ( b.pstmt_desc )
# 1135 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_73 =
  fun _endpos_s_ _startpos__1_ e1 e2 e3 s ->
    let _endpos = _endpos_s_ in
    let _startpos = _startpos__1_ in
    (
# 146 "parser.mly"
    ( let loc = _startpos, _endpos in
      let stmt e =
        let d = match e with None -> PSblock [] | Some e -> PSexpr e in
        { pstmt_desc = d; pstmt_loc = loc }
      in
      let e2 = match e2 with
        | None -> { pexpr_desc = PEconstant (Cbool true); pexpr_loc = loc }
        | Some e -> e
      in
      PSfor (stmt e1, e2, stmt e3, s) )
# 1154 "parser.ml"
     : (Ast.pstmt_desc))

let _menhir_action_74 =
  fun id ->
    (
# 103 "parser.mly"
    ( PTident id )
# 1162 "parser.ml"
     : (Ast.pexpr_typ))

let _menhir_action_75 =
  fun ty ->
    (
# 105 "parser.mly"
    ( ty )
# 1170 "parser.ml"
     : (Ast.pexpr_typ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AMPERSANDAMPERSAND ->
        "AMPERSANDAMPERSAND"
    | BANG ->
        "BANG"
    | BOOLEAN ->
        "BOOLEAN"
    | CLASS ->
        "CLASS"
    | COMMA ->
        "COMMA"
    | COMP _ ->
        "COMP"
    | CST _ ->
        "CST"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQOP _ ->
        "EQOP"
    | EXTENDS ->
        "EXTENDS"
    | FOR ->
        "FOR"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | INSTANCEOF ->
        "INSTANCEOF"
    | INT ->
        "INT"
    | LEFTBRACE ->
        "LEFTBRACE"
    | LEFTBRACKET ->
        "LEFTBRACKET"
    | LEFTPAR ->
        "LEFTPAR"
    | MINUS ->
        "MINUS"
    | NEW ->
        "NEW"
    | NULL ->
        "NULL"
    | PERCENT ->
        "PERCENT"
    | PLUS ->
        "PLUS"
    | PUBLIC ->
        "PUBLIC"
    | RETURN ->
        "RETURN"
    | RIGHTBRACE ->
        "RIGHTBRACE"
    | RIGHTBRACKET ->
        "RIGHTBRACKET"
    | RIGHTPAR ->
        "RIGHTPAR"
    | SEMICOLON ->
        "SEMICOLON"
    | SLASH ->
        "SLASH"
    | STAR ->
        "STAR"
    | STATIC ->
        "STATIC"
    | THIS ->
        "THIS"
    | VERTICALBARVERTICALBAR ->
        "VERTICALBARVERTICALBAR"
    | VOID ->
        "VOID"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_143 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let cl = _v in
      let _v = _menhir_action_38 cl in
      MenhirBox_file _v
  
  let rec _menhir_run_147 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_class_def -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let MenhirCell1_class_def (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_44 x xs in
      _menhir_goto_list_class_def_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_class_def_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState146 ->
          _menhir_run_147 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_143 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CLASS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState001 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_39 _endpos_id_ _startpos_id_ id in
      _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_goto_ident : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState140 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState008 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState001 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_137 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState137
      | IDENT _ ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_74 id in
          _menhir_goto_type_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState010 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHTPAR ->
          let _v = _menhir_action_51 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_03 () in
      _menhir_goto_base_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_base_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState033 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_070 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_LEFTPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_base_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | RIGHTPAR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos) in
          let _menhir_s = MenhirState071 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_025 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_24 () in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_expr_desc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_d_, _startpos_d_, d) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_11 _endpos_d_ _startpos_d_ d in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_d_ _startpos_d_ _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState024 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_106 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _endpos_1 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos_e_, e, _endpos__2_) = (_startpos, _v, _endpos_1) in
          let _v = _menhir_action_67 e in
          _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos_e_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_2 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState040 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_25 () in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_NEW (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState032 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState033 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_12 c in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_02 () in
      _menhir_goto_base_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState035 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState042 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState048 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_stmt_desc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_d_, _startpos_d_, d) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_65 _endpos_d_ _startpos_d_ d in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_d_ _v _menhir_s _tok
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState112 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState087 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_112 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | SEMICOLON ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | RETURN ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LEFTBRACE ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | IF ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState112
      | FOR ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | CST _v_1 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState112
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | RIGHTBRACE ->
          let _v_2 = _menhir_action_47 () in
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_66 () in
      _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState027
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState027
      | SEMICOLON ->
          let _v = _menhir_action_53 () in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (e, _endpos__3_) = (_v, _endpos) in
          let _v = _menhir_action_71 e in
          _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFTBRACE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | SEMICOLON ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | RETURN ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LEFTBRACE ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | IF ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState024
      | FOR ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState024
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | RIGHTBRACE ->
          let _v = _menhir_action_47 () in
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LEFTPAR (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState085 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FOR (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LEFTPAR (_menhir_stack, _startpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState089
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState089
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
          | SEMICOLON ->
              let _v = _menhir_action_53 () in
              _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState089 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_SEMICOLON (_menhir_stack, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState091
          | CST _v_1 ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState091
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
          | SEMICOLON ->
              let _v_2 = _menhir_action_53 () in
              _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState091 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_SEMICOLON (_menhir_stack, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState093
          | CST _v_1 ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState093
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
          | RIGHTPAR ->
              let _v_2 = _menhir_action_53 () in
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState093 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RIGHTPAR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos) in
          let _menhir_s = MenhirState095 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMICOLON ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTBRACE ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOLEAN ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_LEFTBRACE -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LEFTBRACE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos__3_, l) = (_endpos, _v) in
      let _v = _menhir_action_04 _endpos__3_ _startpos__1_ l in
      _menhir_goto_block _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_block : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState138 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState133 ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState126 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState023 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_139 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameters (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, id, _, _) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_08 b id p in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | PUBLIC ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState140
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | RIGHTBRACE ->
          let _v_1 = _menhir_action_45 () in
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_VOID (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState008 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PUBLIC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOID ->
              let _menhir_s = MenhirState119 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | IDENT _v ->
                  _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_decl -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_46 x xs in
      _menhir_goto_list_decl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_decl_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState140 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState007 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_135 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_cell1_ident _menhir_cell0_option_extends_ _menhir_cell0_LEFTBRACE -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_LEFTBRACE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell0_option_extends_ (_menhir_stack, ext) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_CLASS (_menhir_stack, _menhir_s) = _menhir_stack in
      let dl = _v in
      let _v = _menhir_action_05 dl ext id in
      let _menhir_stack = MenhirCell1_class_def (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | CLASS ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | EOF ->
          let _v_0 = _menhir_action_43 () in
          _menhir_run_147 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameters (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_type_expr (_menhir_stack, _menhir_s, ty, _, _) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_09 b id p ty in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_127 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_ident_String, _menhir_box_file) _menhir_cell1_ident _menhir_cell0_RIGHTPAR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _, _, _, _) = _menhir_stack in
      let MenhirCell1_ident_String (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell0_LEFTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident_main (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_PUBLIC (_menhir_stack, _menhir_s) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_06 b in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_116 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_VOID, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_parameters -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameters (_menhir_stack, _, p) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_VOID (_menhir_stack, _menhir_s) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_10 b id p in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_108 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_b_, _startpos_b_, b) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_72 b in
      _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_b_ _v _menhir_s _tok
  
  and _menhir_run_113 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_stmt -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_stmt (_menhir_stack, _menhir_s, x, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_48 x xs in
      _menhir_goto_list_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_stmt_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState024 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState112 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_111 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR, _menhir_box_file) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, s1, _) = _menhir_stack in
      let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell0_LEFTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_s2_, s2) = (_endpos, _v) in
      let _v = _menhir_action_70 e s1 s2 in
      _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s2_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_109 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState110 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMICOLON ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTBRACE ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOLEAN ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | BANG | BOOLEAN | CST _ | FOR | IDENT _ | IF | INT | LEFTBRACE | LEFTPAR | MINUS | NEW | NULL | RETURN | RIGHTBRACE | SEMICOLON | THIS ->
          let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
          let MenhirCell0_LEFTPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_s_, s) = (_endpos, _v) in
          let _v = _menhir_action_69 _endpos_s_ _startpos__1_ e s in
          _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_FOR _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_SEMICOLON, _menhir_box_file) _menhir_cell1_option_expr_ _menhir_cell0_RIGHTPAR -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_option_expr_ (_menhir_stack, _, e3) = _menhir_stack in
      let MenhirCell0_SEMICOLON (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_option_expr_ (_menhir_stack, _, e2) = _menhir_stack in
      let MenhirCell0_SEMICOLON (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_option_expr_ (_menhir_stack, _, e1) = _menhir_stack in
      let MenhirCell0_LEFTPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_s_, s) = (_endpos, _v) in
      let _v = _menhir_action_73 _endpos_s_ _startpos__1_ e1 e2 e3 s in
      _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState050 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState052 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell0_MINUS (_menhir_stack, _startpos) in
      let _menhir_s = MenhirState054 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState056 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOLEAN ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_EQOP (_menhir_stack, _v) in
      let _menhir_s = MenhirState058 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState044 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _menhir_stack = MenhirCell0_COMP (_menhir_stack, _v) in
      let _menhir_s = MenhirState060 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState062 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let e = _v in
          let _v = _menhir_action_42 e in
          let x = _v in
          let _v = _menhir_action_58 x in
          _menhir_goto_option_initialization_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_initialization_ : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_type_expr (_menhir_stack, _menhir_s, ty, _startpos_ty_, _) = _menhir_stack in
      let (e, _endpos__4_) = (_v, _endpos) in
      let _v = _menhir_action_68 e id ty in
      _menhir_goto_stmt_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_run_086 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_IF _menhir_cell0_LEFTPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHTPAR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos) in
          let _menhir_s = MenhirState087 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMICOLON ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTBRACE ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FOR ->
              _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOLEAN ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_3 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DOT ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_4 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | AMPERSANDAMPERSAND ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHTPAR | SEMICOLON ->
          let x = _v in
          let _v = _menhir_action_54 x in
          _menhir_goto_option_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState093 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState078 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHTPAR ->
          let x = _v in
          let _v = _menhir_action_61 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState031 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, x, _, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_62 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_074 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_50 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let (xs, _endpos__3_) = (_v, _endpos) in
      let _v = _menhir_action_01 xs in
      _menhir_goto_arguments _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
  
  and _menhir_goto_arguments : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState030 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState105 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState036 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState045 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_080 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_NEW, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_NEW (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_al_, al) = (_endpos, _v) in
      let _v = _menhir_action_26 al id in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_al_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _menhir_s, id, _startpos_id_, _) = _menhir_stack in
      let (_endpos_al_, al) = (_endpos, _v) in
      let _v = _menhir_action_27 _endpos_al_ _startpos_id_ al id in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_al_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_al_, al) = (_endpos, _v) in
      let _v = _menhir_action_28 al e id in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_al_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e1_, e1) = (_endpos, _v) in
          let _v = _menhir_action_23 e1 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_cell1_base_type _menhir_cell0_RIGHTPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_base_type (_menhir_stack, _, ty, _, _) = _menhir_stack in
          let MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_33 e ty in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_LEFTPAR, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_RIGHTPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell0_RIGHTPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, ty, _, _) = _menhir_stack in
          let MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_34 e ty in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_LEFTPAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RIGHTPAR ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
          | NULL ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
          | NEW ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
          | LEFTPAR ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
          | IDENT _v_1 ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState068
          | CST _v_2 ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState068
          | BANG ->
              let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
              let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos_0) in
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState068
          | AMPERSANDAMPERSAND | COMMA | COMP _ | DOT | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
              let MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (e, _endpos__3_) = (_v, _endpos_0) in
              let _v = _menhir_action_36 e in
              _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
          | _ ->
              _eRR ())
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_3 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_4 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_BANG as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_BANG (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e1_, e1) = (_endpos, _v) in
          let _v = _menhir_action_22 e1 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e1_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND | COMMA | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_20 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_COMP as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell0_COMP (_menhir_stack, c) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_18 c e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_EQOP as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | AMPERSANDAMPERSAND | COMMA | EQOP _ | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell0_EQOP (_menhir_stack, c) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_19 c e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr _menhir_cell0_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PLUS | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell0_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_14 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_17 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PLUS | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_13 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_16 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_expr, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RIGHTPAR | SEMICOLON ->
          let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _, id, _, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_32 e1 e2 id in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AMPERSANDAMPERSAND | COMMA | COMP _ | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_15 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RIGHTPAR | SEMICOLON | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_21 e1 e2 in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VERTICALBARVERTICALBAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PERCENT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INSTANCEOF ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQOP _v_0 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | DOT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMP _v_1 ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | AMPERSANDAMPERSAND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | RIGHTPAR | SEMICOLON ->
          let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _menhir_s, id, _startpos_id_, _) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_31 e id in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_ty_, _startpos_ty_, ty) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_75 ty in
      _menhir_goto_type_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_ty_ _startpos_ty_ _v _menhir_s _tok
  
  and _menhir_goto_type_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState140 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState017 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_130 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState130
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState096
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
      let (_endpos_ty_, ty) = (_endpos, _v) in
      let _v = _menhir_action_35 e ty in
      _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_ty_ _startpos_e_ _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState013
      | _ ->
          _eRR ()
  
  and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_60 xs in
      _menhir_goto_parameters _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameters : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState137 ->
          _menhir_run_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_138 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_parameters (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFTBRACE ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_parameters (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFTBRACE ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState133
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_VOID, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_parameters (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFTBRACE ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | _ ->
          _eRR ()
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_expr (_menhir_stack, _menhir_s, ty, _, _) = _menhir_stack in
          let id = _v in
          let _v = _menhir_action_07 id ty in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | LEFTPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_PUBLIC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let id = _v in
      let _v = _menhir_action_41 id in
      let _menhir_stack = MenhirCell1_ident_main (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LEFTPAR (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState121 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let id = _v in
      let _v = _menhir_action_40 id in
      let _menhir_stack = MenhirCell1_ident_String (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState122
      | _ ->
          _eRR ()
  
  and _menhir_run_123 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_PUBLIC, _menhir_box_file) _menhir_cell1_ident_main _menhir_cell0_LEFTPAR, _menhir_box_file) _menhir_cell1_ident_String as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | LEFTBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | RIGHTBRACKET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RIGHTPAR ->
                  let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _menhir_stack = MenhirCell0_RIGHTPAR (_menhir_stack, _endpos) in
                  let _menhir_s = MenhirState126 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | LEFTBRACE ->
                      _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | EQ ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | AMPERSANDAMPERSAND | COMP _ | DOT | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_29 id in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | IDENT _ ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_74 id in
          _menhir_goto_type_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFTPAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState031 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHTPAR ->
          let _v = _menhir_action_49 () in
          _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | THIS ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NULL ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NEW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFTPAR ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CST _v ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_s = MenhirState098 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SEMICOLON ->
          let _v = _menhir_action_57 () in
          _menhir_goto_option_initialization_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | EQ ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_EQ (_menhir_stack, MenhirState045) in
          let _menhir_s = MenhirState046 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | THIS ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NULL ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NEW ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFTPAR ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CST _v ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AMPERSANDAMPERSAND | COMMA | COMP _ | DOT | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let MenhirCell1_expr (_menhir_stack, _menhir_s, e, _startpos_e_, _) = _menhir_stack in
          let (_endpos_id_, id) = (_endpos, _v) in
          let _v = _menhir_action_30 e id in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | EQ ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | AMPERSANDAMPERSAND | COMMA | COMP _ | DOT | EQOP _ | INSTANCEOF | MINUS | PERCENT | PLUS | RIGHTPAR | SEMICOLON | SLASH | STAR | VERTICALBARVERTICALBAR ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_29 id in
          _menhir_goto_expr_desc _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_NEW as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_74 id in
      _menhir_goto_type_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_run_014 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_expr -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_expr (_menhir_stack, _menhir_s, ty, _, _) = _menhir_stack in
      let id = _v in
      let _v = _menhir_action_59 id ty in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_parameter (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState017 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOLEAN ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RIGHTPAR ->
          let x = _v in
          let _v = _menhir_action_63 x in
          _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState010 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_018 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_parameter -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_parameter (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_64 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_015 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ident, _menhir_box_file) _menhir_cell1_LEFTPAR -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_52 x in
      _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_009 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_VOID as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | LEFTPAR ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState009
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let id = _v in
      let _v = _menhir_action_37 id in
      let x = _v in
      let _v = _menhir_action_56 x in
      _menhir_goto_option_extends_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_option_extends_ : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_CLASS, _menhir_box_file) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_option_extends_ (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | LEFTBRACE ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LEFTBRACE (_menhir_stack, _startpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState007
          | PUBLIC ->
              _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState007
          | INT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState007
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState007
          | BOOLEAN ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState007
          | RIGHTBRACE ->
              let _v_1 = _menhir_action_45 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_CLASS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | EXTENDS ->
          let _menhir_s = MenhirState004 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | LEFTBRACE ->
          let _v = _menhir_action_55 () in
          _menhir_goto_option_extends_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | CLASS ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_43 () in
          _menhir_run_143 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

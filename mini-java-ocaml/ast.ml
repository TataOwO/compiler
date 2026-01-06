
(** {2 Abstract Syntax of Mini Java} *)

(** {3 Parsed trees}

   This is the output of the parser and the input of the type checker. *)

type location = Lexing.position * Lexing.position

type ident = { loc : location; id : string }

type unop =
  | Uneg (** -e *) | Unot (** !e *)
  | Ustring_of_int (** integer to string conversion,
                       introduced during type checking *)

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod   (** + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge (** == != < <= > >= *)
  | Band | Bor                         (** && || *)
  | Badd_s (** string concatenation, introduced during type checking *)

type constant =
  | Cbool of bool
  | Cint of int32
  | Cstring of string

type pexpr_typ =
  | PTboolean
  | PTint
  | PTident of ident

type pexpr =
  { pexpr_desc : pexpr_desc;
    pexpr_loc  : location; }

and pexpr_desc =
  | PEconstant of constant
  | PEbinop of binop * pexpr * pexpr
  | PEunop of unop * pexpr
  | PEthis
  | PEnull
  | PEident of ident
  | PEdot of pexpr * ident
  | PEassign_ident of ident * pexpr
  | PEassign_dot of pexpr * ident * pexpr
  | PEnew of ident * pexpr list
  | PEcall of pexpr * ident * pexpr list
  | PEcast of pexpr_typ * pexpr
  | PEinstanceof of pexpr * pexpr_typ

type pstmt =
  { pstmt_desc : pstmt_desc;
    pstmt_loc  : location; }

and pstmt_desc =
  | PSexpr of pexpr
  | PSvar of pexpr_typ * ident * pexpr option
  | PSif of pexpr * pstmt * pstmt
  | PSreturn of pexpr option
  | PSblock of pstmt list
  | PSfor of pstmt * pexpr * pstmt * pstmt

type pparam = pexpr_typ * ident

type pdecl =
  | PDattribute of pexpr_typ * ident
  | PDconstructor of ident * pparam list * pstmt
  | PDmethod of pexpr_typ option * ident * pparam list * pstmt

type pclass = ident * ident option * pdecl list

type pfile = pclass list


(** {3 Typed trees}

   This is the output of the type checker and the input of the code
   generation. *)

type typ =
  | Tvoid
  | Tnull
  | Tboolean
  | Tint
  | Tclass of class_

and class_ = {
  class_name      : string;
  mutable class_extends   : class_;
  class_methods   : (string, method_  ) Hashtbl.t;
  class_attributes: (string, attribute) Hashtbl.t;
}

and attribute = {
  attr_name : string;
  attr_type : typ;
  mutable attr_ofs : int; (** position within the object *)
}
  (** All the occurrences of the same attribute
      point to a single record of the following type. *)

and method_ = {
  meth_name  : string; (** unique name *)
  meth_type  : typ;
  meth_params: var list;
  mutable meth_ofs : int; (** position within the method table *)
}

and var = {
  var_name : string;
  var_type : typ;
  mutable var_ofs : int;  (** position wrt %rbp *)
}
  (** All the occurrences of the same variable
      point to a single record of the following type. *)

type expr =
  { expr_desc : expr_desc;
    expr_type : typ; }

and expr_desc =
  | Econstant of constant
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ethis
  | Enull
  | Evar of var
  | Eassign_var of var * expr
  | Eattr of expr * attribute
  | Eassign_attr of expr * attribute * expr
  | Enew of class_ * expr list
  | Ecall of expr * method_ * expr list
  | Ecast of class_ * expr
  | Einstanceof of expr * string
  | Eprint of expr

type stmt =
  | Sexpr of expr
  | Svar of var * expr
  | Sif of expr * stmt * stmt
  | Sreturn of expr option
  | Sblock of stmt list
  | Sfor of stmt * expr * stmt * stmt

type decl =
  | Dconstructor of var list * stmt
  | Dmethod of method_ * stmt

type tclass = class_ * decl list

type tfile = tclass list

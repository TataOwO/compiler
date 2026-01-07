
open Ast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(* ========================================================================= *)
(* 1.1 Data Structures Setup                                                 *)
(* ========================================================================= *)

(* Global class table: name -> class_ *)
let class_table : (string, class_) Hashtbl.t = Hashtbl.create 17

(* Predefined classes - created lazily *)
let class_Object : class_ = {
  class_name = "Object";
  class_extends = Obj.magic 0;  (* will point to itself *)
  class_methods = Hashtbl.create 3;
  class_attributes = Hashtbl.create 3;
}

let class_String : class_ = {
  class_name = "String";
  class_extends = class_Object;
  class_methods = Hashtbl.create 3;
  class_attributes = Hashtbl.create 3;
}

(* Initialize Object's parent to itself and add predefined classes *)
let () =
  class_Object.class_extends <- class_Object;
  (* Add equals method to String: boolean equals(String) *)
  let equals_method = {
    meth_name = "equals";
    meth_type = Tboolean;
    meth_params = [{ var_name = "s"; var_type = Tclass class_String; var_ofs = 0 }];
    meth_ofs = 0;
  } in
  Hashtbl.add class_String.class_methods "equals" equals_method;
  (* Add predefined classes to the table *)
  Hashtbl.add class_table "Object" class_Object;
  Hashtbl.add class_table "String" class_String

(* Helper: find a class by name *)
let find_class ~loc name =
  try Hashtbl.find class_table name
  with Not_found -> error ~loc "unknown class %s" name

(* Helper: check if c1 is a subclass of c2 (c1 -> c2) *)
let rec is_subclass c1 c2 =
  c1 == c2 ||
  (c1 != class_Object && is_subclass c1.class_extends c2)

(* Helper: check if type t1 is a subtype of t2 (t1 ⊑ t2) *)
let is_subtype t1 t2 =
  match t1, t2 with
  | Tvoid, Tvoid -> true
  | Tboolean, Tboolean -> true
  | Tint, Tint -> true
  | Tnull, Tnull -> true  (* null is subtype of itself for compatibility *)
  | Tclass c1, Tclass c2 -> is_subclass c1 c2
  | Tnull, Tclass _ -> true
  | _, _ -> false

(* Helper: check if types are compatible (t1 ≡ t2) *)
let are_compatible t1 t2 =
  is_subtype t1 t2 || is_subtype t2 t1

(* Helper: convert parsed type to typed type *)
let typ_of_pexpr_typ ~loc = function
  | PTboolean -> Tboolean
  | PTint -> Tint
  | PTident { id; loc } -> Tclass (find_class ~loc id)

(* Helper: check that a type is well-formed *)
let check_type_wf ~loc typ =
  match typ with
  | Tboolean | Tint | Tvoid | Tnull -> ()
  | Tclass c ->
    if not (Hashtbl.mem class_table c.class_name) then
      error ~loc "unknown type %s" c.class_name

(* Helper: string representation of type *)
let rec string_of_typ = function
  | Tvoid -> "void"
  | Tnull -> "null"
  | Tboolean -> "boolean"
  | Tint -> "int"
  | Tclass c -> c.class_name

(* Helper: type equality - uses physical equality for classes to avoid cycles *)
let typ_equal t1 t2 =
  match t1, t2 with
  | Tvoid, Tvoid -> true
  | Tnull, Tnull -> true
  | Tboolean, Tboolean -> true
  | Tint, Tint -> true
  | Tclass c1, Tclass c2 -> c1 == c2
  | _, _ -> false

(* ========================================================================= *)
(* 1.2 First Pass: Collect Class Names                                       *)
(* ========================================================================= *)

let collect_classes (pfile: pfile) : unit =
  (* Check that Main class is present - parser already checks this,
     but Main is first in the list (parser reverses order) *)
  (match pfile with
   | (id, _, _) :: _ when id.id = "Main" -> ()
   | _ -> error "missing Main class");

  List.iter (fun (id, _, _) ->
    let name = id.id in
    let loc = id.loc in
    (* Check for reserved names *)
    if name = "Object" || name = "String" then
      error ~loc "cannot redefine class %s" name;
    (* Check for duplicate definition *)
    if Hashtbl.mem class_table name then
      error ~loc "class %s is already defined" name;
    (* Create new class record *)
    let c = {
      class_name = name;
      class_extends = class_Object;  (* default, will be set in pass 2 *)
      class_methods = Hashtbl.create 7;
      class_attributes = Hashtbl.create 7;
    } in
    Hashtbl.add class_table name c
  ) pfile

(* ========================================================================= *)
(* 1.3 Second Pass: Build Inheritance Hierarchy                              *)
(* ========================================================================= *)

let resolve_inheritance (pfile: pfile) : unit =
  (* First, set extends for each class *)
  List.iter (fun (id, extends_opt, _) ->
    let c = Hashtbl.find class_table id.id in
    match extends_opt with
    | None ->
      c.class_extends <- class_Object
    | Some parent_id ->
      let parent_name = parent_id.id in
      let loc = parent_id.loc in
      (* Cannot extend String *)
      if parent_name = "String" then
        error ~loc "cannot extend String";
      (* Find parent class *)
      let parent = find_class ~loc parent_name in
      c.class_extends <- parent
  ) pfile;

  (* Detect cycles in inheritance using DFS *)
  let visited = Hashtbl.create 17 in
  let rec check_cycle c path =
    if List.memq c path then
      error "cyclic inheritance involving class %s" c.class_name;
    if not (Hashtbl.mem visited c.class_name) then begin
      Hashtbl.add visited c.class_name true;
      if c != class_Object then
        check_cycle c.class_extends (c :: path)
    end
  in
  Hashtbl.iter (fun _ c ->
    if c != class_Object && c != class_String then
      check_cycle c []
  ) class_table

(* ========================================================================= *)
(* 1.4 Third Pass: Collect Members (Attributes, Constructors, Methods)       *)
(* ========================================================================= *)

(* Store constructor info per class *)
let class_constructors : (string, var list) Hashtbl.t = Hashtbl.create 17

(* Helper: check parameter names are distinct *)
let check_params_distinct ~loc params =
  let seen = Hashtbl.create 7 in
  List.iter (fun (_, id) ->
    if Hashtbl.mem seen id.id then
      error ~loc "duplicate parameter %s" id.id;
    Hashtbl.add seen id.id true
  ) params

(* Helper: convert parsed params to typed vars *)
let params_to_vars ~loc params =
  List.map (fun (pty, id) ->
    let typ = typ_of_pexpr_typ ~loc:id.loc pty in
    { var_name = id.id; var_type = typ; var_ofs = 0 }
  ) params

(* Helper: find attribute in class or parents *)
let rec find_attribute c name =
  try Some (Hashtbl.find c.class_attributes name)
  with Not_found ->
    if c == class_Object then None
    else find_attribute c.class_extends name

(* Helper: find method in class or parents *)
let rec find_method c name =
  try Some (Hashtbl.find c.class_methods name)
  with Not_found ->
    if c == class_Object then None
    else find_method c.class_extends name

let collect_members (pfile: pfile) : unit =
  (* Process classes in topological order (parents before children) *)
  (* First, compute inheritance depth for each class *)
  let depth = Hashtbl.create 17 in
  let rec get_depth c =
    try Hashtbl.find depth c.class_name
    with Not_found ->
      let d = if c == class_Object then 0 else 1 + get_depth c.class_extends in
      Hashtbl.add depth c.class_name d;
      d
  in
  Hashtbl.iter (fun _ c -> ignore (get_depth c)) class_table;

  (* Sort classes by depth *)
  let sorted = List.sort (fun (id1, _, _) (id2, _, _) ->
    let c1 = Hashtbl.find class_table id1.id in
    let c2 = Hashtbl.find class_table id2.id in
    compare (get_depth c1) (get_depth c2)
  ) pfile in

  (* Process each class *)
  List.iter (fun (class_id, _, decls) ->
    let c = Hashtbl.find class_table class_id.id in
    let loc = class_id.loc in

    (* Inherit attributes from parent *)
    if c != class_Object && c != class_String then begin
      Hashtbl.iter (fun name attr ->
        Hashtbl.add c.class_attributes name attr
      ) c.class_extends.class_attributes;

      (* Inherit methods from parent *)
      Hashtbl.iter (fun name meth ->
        Hashtbl.add c.class_methods name meth
      ) c.class_extends.class_methods
    end;

    (* Track if we've seen a constructor *)
    let has_constructor = ref false in
    (* Track attributes declared in THIS class (for duplicate detection) *)
    let declared_attrs = Hashtbl.create 7 in

    (* Process declarations *)
    List.iter (fun decl ->
      match decl with
      | PDattribute (pty, attr_id) ->
        let attr_name = attr_id.id in
        let attr_loc = attr_id.loc in
        (* In Java, subclass attributes can shadow parent attributes.
           We need to check that this attribute isn't declared twice in THIS class.
           Since we process attributes in order and inherited ones are added before,
           we use Hashtbl.replace to allow shadowing. But we track declared_attrs
           to detect duplicates within this class. *)
        if Hashtbl.mem declared_attrs attr_name then
          error ~loc:attr_loc "duplicate attribute %s" attr_name;
        Hashtbl.add declared_attrs attr_name true;
        let typ = typ_of_pexpr_typ ~loc:attr_loc pty in
        check_type_wf ~loc:attr_loc typ;
        let attr = {
          attr_name;
          attr_type = typ;
          attr_ofs = 0;  (* will be set during code gen *)
        } in
        (* Use replace to allow shadowing inherited attributes *)
        Hashtbl.replace c.class_attributes attr_name attr

      | PDconstructor (constr_id, params, _body) ->
        let constr_loc = constr_id.loc in
        (* Check constructor name matches class name *)
        if constr_id.id <> c.class_name then
          error ~loc:constr_loc "constructor name %s does not match class name %s"
            constr_id.id c.class_name;
        (* Check only one constructor *)
        if !has_constructor then
          error ~loc:constr_loc "duplicate constructor for class %s" c.class_name;
        has_constructor := true;
        (* Check params are distinct *)
        check_params_distinct ~loc:constr_loc params;
        (* Convert params *)
        let vars = params_to_vars ~loc:constr_loc params in
        Hashtbl.replace class_constructors c.class_name vars

      | PDmethod (ret_opt, meth_id, params, _body) ->
        let meth_name = meth_id.id in
        let meth_loc = meth_id.loc in

        (* Special case: Main.main *)
        if c.class_name = "Main" && meth_name = "main" then begin
          (* Main.main has no return type and no params in the typed AST *)
          ()
        end else begin
          (* Check params are distinct *)
          check_params_distinct ~loc:meth_loc params;

          (* Convert return type *)
          let ret_type = match ret_opt with
            | None -> Tvoid
            | Some pty -> typ_of_pexpr_typ ~loc:meth_loc pty
          in
          check_type_wf ~loc:meth_loc ret_type;

          (* Convert params *)
          let vars = params_to_vars ~loc:meth_loc params in

          (* Check for duplicate method in THIS class *)
          (* Note: inherited methods are OK to override *)
          let parent_method = find_method c.class_extends meth_name in
          let this_method =
            try Some (Hashtbl.find c.class_methods meth_name)
            with Not_found -> None
          in

          (match this_method, parent_method with
           | Some m, Some pm when m == pm ->
             (* We have inherited method, ok to override *)
             (* Check override has same signature *)
             let param_types = List.map (fun v -> v.var_type) vars in
             let parent_param_types = List.map (fun v -> v.var_type) pm.meth_params in
             if not (typ_equal ret_type pm.meth_type) then
               error ~loc:meth_loc "method %s: return type differs from parent" meth_name;
             if List.length param_types <> List.length parent_param_types then
               error ~loc:meth_loc "method %s: parameter count differs from parent" meth_name;
             List.iter2 (fun t1 t2 ->
               if not (typ_equal t1 t2) then
                 error ~loc:meth_loc "method %s: parameter type differs from parent" meth_name
             ) param_types parent_param_types
           | Some _, None ->
             (* Method already declared in this class *)
             error ~loc:meth_loc "duplicate method %s" meth_name
           | Some m, Some pm when m != pm ->
             (* Method declared in this class shadows inherited one - error *)
             error ~loc:meth_loc "duplicate method %s" meth_name
           | None, Some pm ->
             (* Overriding parent method - check signature *)
             let param_types = List.map (fun v -> v.var_type) vars in
             let parent_param_types = List.map (fun v -> v.var_type) pm.meth_params in
             if not (typ_equal ret_type pm.meth_type) then
               error ~loc:meth_loc "method %s: return type differs from parent" meth_name;
             if List.length param_types <> List.length parent_param_types then
               error ~loc:meth_loc "method %s: parameter count differs from parent" meth_name;
             List.iter2 (fun t1 t2 ->
               if not (typ_equal t1 t2) then
                 error ~loc:meth_loc "method %s: parameter type differs from parent" meth_name
             ) param_types parent_param_types
           | None, None ->
             (* New method *)
             ()
          );

          (* Create method record *)
          let meth = {
            meth_name;
            meth_type = ret_type;
            meth_params = vars;
            meth_ofs = 0;  (* will be set during code gen *)
          } in
          Hashtbl.replace c.class_methods meth_name meth
        end
    ) decls;

    (* If no constructor, add default (no params) *)
    if not !has_constructor then
      Hashtbl.add class_constructors c.class_name []

  ) sorted

(* ========================================================================= *)
(* 1.5-1.8 Type Checking (Expressions, Statements, Methods)                  *)
(* ========================================================================= *)

(* Typing environment *)
type env = {
  vars: (string, var) Hashtbl.t;
  current_class: class_;
  return_type: typ;  (* expected return type for current method *)
}

let create_env c ret_type =
  let vars = Hashtbl.create 17 in
  (* Add 'this' to environment *)
  let this_var = { var_name = "this"; var_type = Tclass c; var_ofs = 0 } in
  Hashtbl.add vars "this" this_var;
  { vars; current_class = c; return_type = ret_type }

(* Helper: look up variable in environment *)
let find_var env name =
  try Some (Hashtbl.find env.vars name)
  with Not_found -> None

(* Type check expression: returns typed expression *)
let rec typecheck_expr env (pe: pexpr) : expr =
  let loc = pe.pexpr_loc in
  let mk desc typ = { expr_desc = desc; expr_type = typ } in

  match pe.pexpr_desc with
  | PEconstant c ->
    let typ = match c with
      | Cbool _ -> Tboolean
      | Cint _ -> Tint
      | Cstring _ -> Tclass class_String
    in
    mk (Econstant c) typ

  | PEthis ->
    mk Ethis (Tclass env.current_class)

  | PEnull ->
    mk Enull Tnull

  | PEident id ->
    let name = id.id in
    (match find_var env name with
     | Some v -> mk (Evar v) v.var_type
     | None ->
       (* Look in class attributes *)
       match find_attribute env.current_class name with
       | Some attr ->
         let this_expr = mk Ethis (Tclass env.current_class) in
         mk (Eattr (this_expr, attr)) attr.attr_type
       | None ->
         error ~loc:id.loc "unbound variable %s" name)

  | PEdot (pe1, id) ->
    let e1 = typecheck_expr env pe1 in
    (match e1.expr_type with
     | Tclass c ->
       (match find_attribute c id.id with
        | Some attr -> mk (Eattr (e1, attr)) attr.attr_type
        | None -> error ~loc:id.loc "class %s has no attribute %s" c.class_name id.id)
     | _ -> error ~loc "attribute access on non-object type")

  | PEassign_ident (id, pe2) ->
    let e2 = typecheck_expr env pe2 in
    let name = id.id in
    (match find_var env name with
     | Some v ->
       if not (is_subtype e2.expr_type v.var_type) then
         error ~loc "type mismatch in assignment: expected %s, got %s"
           (string_of_typ v.var_type) (string_of_typ e2.expr_type);
       mk (Eassign_var (v, e2)) v.var_type
     | None ->
       (* Look in class attributes *)
       match find_attribute env.current_class name with
       | Some attr ->
         if not (is_subtype e2.expr_type attr.attr_type) then
           error ~loc "type mismatch in assignment";
         let this_expr = mk Ethis (Tclass env.current_class) in
         mk (Eassign_attr (this_expr, attr, e2)) attr.attr_type
       | None ->
         error ~loc:id.loc "unbound variable %s" name)

  | PEassign_dot (pe1, id, pe2) ->
    let e1 = typecheck_expr env pe1 in
    let e2 = typecheck_expr env pe2 in
    (match e1.expr_type with
     | Tclass c ->
       (match find_attribute c id.id with
        | Some attr ->
          if not (is_subtype e2.expr_type attr.attr_type) then
            error ~loc "type mismatch in assignment";
          mk (Eassign_attr (e1, attr, e2)) attr.attr_type
        | None -> error ~loc:id.loc "class %s has no attribute %s" c.class_name id.id)
     | _ -> error ~loc "attribute access on non-object type")

  | PEunop (Uneg, pe1) ->
    let e1 = typecheck_expr env pe1 in
    if e1.expr_type <> Tint then
      error ~loc "unary minus requires int, got %s" (string_of_typ e1.expr_type);
    mk (Eunop (Uneg, e1)) Tint

  | PEunop (Unot, pe1) ->
    let e1 = typecheck_expr env pe1 in
    if e1.expr_type <> Tboolean then
      error ~loc "logical not requires boolean, got %s" (string_of_typ e1.expr_type);
    mk (Eunop (Unot, e1)) Tboolean

  | PEunop (Ustring_of_int, _) ->
    (* This is only introduced during type checking, not in parsed AST *)
    assert false

  | PEbinop (op, pe1, pe2) ->
    let e1 = typecheck_expr env pe1 in
    let e2 = typecheck_expr env pe2 in
    typecheck_binop env loc op e1 e2

  | PEnew (class_id, args) ->
    let c = find_class ~loc:class_id.loc class_id.id in
    let typed_args = List.map (typecheck_expr env) args in
    (* Find constructor *)
    let constr_params =
      try Hashtbl.find class_constructors c.class_name
      with Not_found -> []  (* default constructor *)
    in
    (* Check argument count *)
    if List.length typed_args <> List.length constr_params then
      error ~loc "constructor %s expects %d arguments, got %d"
        c.class_name (List.length constr_params) (List.length typed_args);
    (* Check argument types *)
    List.iter2 (fun arg param ->
      if not (is_subtype arg.expr_type param.var_type) then
        error ~loc "constructor argument type mismatch"
    ) typed_args constr_params;
    mk (Enew (c, typed_args)) (Tclass c)

  | PEcall (pe_recv, meth_id, args) ->
    (* Check for System.out.print special case *)
    (match pe_recv.pexpr_desc, meth_id.id with
     | PEdot ({ pexpr_desc = PEident { id = "System"; _ }; _ },
              { id = "out"; _ }), "print" ->
       (* System.out.print(string) *)
       if List.length args <> 1 then
         error ~loc "System.out.print expects 1 argument";
       let arg = typecheck_expr env (List.hd args) in
       (match arg.expr_type with
        | Tclass c when c == class_String -> ()
        | _ -> error ~loc "System.out.print expects String argument");
       mk (Eprint arg) Tvoid
     | _ ->
       let e_recv = typecheck_expr env pe_recv in
       let typed_args = List.map (typecheck_expr env) args in
       (match e_recv.expr_type with
        | Tclass c ->
          (match find_method c meth_id.id with
           | Some meth ->
             (* Check argument count *)
             if List.length typed_args <> List.length meth.meth_params then
               error ~loc "method %s expects %d arguments, got %d"
                 meth_id.id (List.length meth.meth_params) (List.length typed_args);
             (* Check argument types *)
             List.iter2 (fun arg param ->
               if not (is_subtype arg.expr_type param.var_type) then
                 error ~loc "argument type mismatch for method %s" meth_id.id
             ) typed_args meth.meth_params;
             mk (Ecall (e_recv, meth, typed_args)) meth.meth_type
           | None ->
             (* Check for String.equals *)
             if c == class_String && meth_id.id = "equals" then begin
               if List.length typed_args <> 1 then
                 error ~loc "equals expects 1 argument";
               let arg = List.hd typed_args in
               (match arg.expr_type with
                | Tclass c when c == class_String -> ()
                | _ -> error ~loc "equals expects String argument");
               let meth = Hashtbl.find class_String.class_methods "equals" in
               mk (Ecall (e_recv, meth, typed_args)) Tboolean
             end else
               error ~loc:meth_id.loc "class %s has no method %s" c.class_name meth_id.id)
        | Tnull ->
          error ~loc "cannot call method on null"
        | _ ->
          error ~loc "method call on non-object type"))

  | PEcast (pty, pe1) ->
    let e1 = typecheck_expr env pe1 in
    let target_type = typ_of_pexpr_typ ~loc pty in
    (* Check types are compatible *)
    if not (are_compatible e1.expr_type target_type) then
      error ~loc "invalid cast from %s to %s"
        (string_of_typ e1.expr_type) (string_of_typ target_type);
    (match target_type with
     | Tclass c -> mk (Ecast (c, e1)) target_type
     | _ ->
       (* Cast to primitive type - just return with new type *)
       { e1 with expr_type = target_type })

  | PEinstanceof (pe1, pty) ->
    let e1 = typecheck_expr env pe1 in
    let target_type = typ_of_pexpr_typ ~loc pty in
    (* Check e1 is object or null *)
    (match e1.expr_type with
     | Tclass _ | Tnull -> ()
     | _ -> error ~loc "instanceof requires object type on left");
    (* Check target is class type *)
    (match target_type with
     | Tclass c ->
       if not (are_compatible e1.expr_type target_type) then
         error ~loc "instanceof with incompatible types";
       mk (Einstanceof (e1, c.class_name)) Tboolean
     | _ -> error ~loc "instanceof requires class type on right")

and typecheck_binop env loc op e1 e2 =
  let mk desc typ = { expr_desc = desc; expr_type = typ } in
  match op with
  | Badd ->
    (* Special case: string concatenation *)
    (match e1.expr_type, e2.expr_type with
     | Tclass c, _ when c == class_String ->
       let e2' = convert_to_string env e2 in
       mk (Ebinop (Badd_s, e1, e2')) (Tclass class_String)
     | _, Tclass c when c == class_String ->
       let e1' = convert_to_string env e1 in
       mk (Ebinop (Badd_s, e1', e2)) (Tclass class_String)
     | Tint, Tint ->
       mk (Ebinop (Badd, e1, e2)) Tint
     | _ ->
       error ~loc "invalid operand types for +")

  | Bsub | Bmul | Bdiv | Bmod ->
    if e1.expr_type <> Tint || e2.expr_type <> Tint then
      error ~loc "arithmetic operation requires int operands";
    mk (Ebinop (op, e1, e2)) Tint

  | Blt | Ble | Bgt | Bge ->
    if e1.expr_type <> Tint || e2.expr_type <> Tint then
      error ~loc "comparison requires int operands";
    mk (Ebinop (op, e1, e2)) Tboolean

  | Beq | Bneq ->
    if not (are_compatible e1.expr_type e2.expr_type) then
      error ~loc "equality comparison requires compatible types";
    mk (Ebinop (op, e1, e2)) Tboolean

  | Band | Bor ->
    if e1.expr_type <> Tboolean || e2.expr_type <> Tboolean then
      error ~loc "logical operation requires boolean operands";
    mk (Ebinop (op, e1, e2)) Tboolean

  | Badd_s ->
    (* Only introduced during type checking *)
    assert false

and convert_to_string env e =
  match e.expr_type with
  | Tclass c when c == class_String -> e
  | Tint -> { expr_desc = Eunop (Ustring_of_int, e); expr_type = Tclass class_String }
  | _ ->
    (* For other types in string concat, we'd need toString - just error for now *)
    raise (Error (dummy_loc, "cannot convert to string"))

(* Type check statement: returns typed statement and possibly updated env *)
let rec typecheck_stmt env (ps: pstmt) : stmt =
  let loc = ps.pstmt_loc in
  match ps.pstmt_desc with
  | PSexpr pe ->
    let e = typecheck_expr env pe in
    Sexpr e

  | PSvar (pty, id, init_opt) ->
    let name = id.id in
    (* Check not already declared in this scope *)
    if Hashtbl.mem env.vars name then
      error ~loc:id.loc "variable %s is already declared" name;
    let typ = typ_of_pexpr_typ ~loc:id.loc pty in
    check_type_wf ~loc:id.loc typ;
    let v = { var_name = name; var_type = typ; var_ofs = 0 } in
    Hashtbl.add env.vars name v;
    (match init_opt with
     | None ->
       (* Uninitialized variable - create default value *)
       let default_expr = match typ with
         | Tint -> { expr_desc = Econstant (Cint 0l); expr_type = Tint }
         | Tboolean -> { expr_desc = Econstant (Cbool false); expr_type = Tboolean }
         | Tclass _ -> { expr_desc = Enull; expr_type = Tnull }
         | _ -> error ~loc "cannot have variable of type %s" (string_of_typ typ)
       in
       Svar (v, default_expr)
     | Some pe ->
       let e = typecheck_expr env pe in
       if not (is_subtype e.expr_type typ) then
         error ~loc "type mismatch in variable initialization";
       Svar (v, e))

  | PSif (pe, ps1, ps2) ->
    let e = typecheck_expr env pe in
    if e.expr_type <> Tboolean then
      error ~loc "if condition must be boolean";
    (* Create new scopes for branches *)
    let env1 = { env with vars = Hashtbl.copy env.vars } in
    let env2 = { env with vars = Hashtbl.copy env.vars } in
    let s1 = typecheck_stmt env1 ps1 in
    let s2 = typecheck_stmt env2 ps2 in
    Sif (e, s1, s2)

  | PSfor (ps_init, pe_cond, ps_update, ps_body) ->
    (* Create new scope for loop *)
    let loop_env = { env with vars = Hashtbl.copy env.vars } in
    let s_init = typecheck_stmt loop_env ps_init in
    let e_cond = typecheck_expr loop_env pe_cond in
    if e_cond.expr_type <> Tboolean then
      error ~loc "for condition must be boolean";
    let s_update = typecheck_stmt loop_env ps_update in
    let s_body = typecheck_stmt loop_env ps_body in
    Sfor (s_init, e_cond, s_update, s_body)

  | PSblock stmts ->
    (* Create new scope for block *)
    let block_env = { env with vars = Hashtbl.copy env.vars } in
    let typed_stmts = List.map (typecheck_stmt block_env) stmts in
    Sblock typed_stmts

  | PSreturn None ->
    if env.return_type <> Tvoid then
      error ~loc "return without value in non-void method";
    Sreturn None

  | PSreturn (Some pe) ->
    if env.return_type = Tvoid then
      error ~loc "return with value in void method";
    let e = typecheck_expr env pe in
    if not (is_subtype e.expr_type env.return_type) then
      error ~loc "return type mismatch: expected %s, got %s"
        (string_of_typ env.return_type) (string_of_typ e.expr_type);
    Sreturn (Some e)

(* Check that all execution paths return for non-void methods *)
let rec always_returns (s: stmt) : bool =
  match s with
  | Sreturn _ -> true
  | Sif (_, s1, s2) -> always_returns s1 && always_returns s2
  | Sblock stmts -> List.exists always_returns stmts
  | Sfor _ -> false  (* loop might not execute *)
  | Sexpr _ -> false
  | Svar _ -> false

(* ========================================================================= *)
(* Main Entry Point                                                          *)
(* ========================================================================= *)

let file ?debug:(b=false) (p: Ast.pfile) : Ast.tfile =
  debug := b;

  (* Reset state *)
  Hashtbl.clear class_table;
  Hashtbl.clear class_constructors;
  (* Also clear predefined classes' methods/attributes to avoid accumulation *)
  Hashtbl.clear class_Object.class_methods;
  Hashtbl.clear class_Object.class_attributes;
  Hashtbl.clear class_String.class_methods;
  Hashtbl.clear class_String.class_attributes;
  (* Re-add String.equals *)
  let equals_method = {
    meth_name = "equals";
    meth_type = Tboolean;
    meth_params = [{ var_name = "s"; var_type = Tclass class_String; var_ofs = 0 }];
    meth_ofs = 0;
  } in
  Hashtbl.add class_String.class_methods "equals" equals_method;

  (* Re-add predefined classes *)
  Hashtbl.add class_table "Object" class_Object;
  Hashtbl.add class_table "String" class_String;

  (* Phase 1.2: Collect class names *)
  collect_classes p;

  (* Phase 1.3: Build inheritance hierarchy *)
  resolve_inheritance p;

  (* Phase 1.4: Collect members *)
  collect_members p;

  (* Phase 1.5-1.8: Type check bodies *)
  let result = List.map (fun (class_id, _, decls) ->
    let c = Hashtbl.find class_table class_id.id in

    let typed_decls = List.filter_map (fun decl ->
      match decl with
      | PDattribute _ -> None  (* Attributes don't produce typed decls *)
      | PDconstructor (_, params, body) ->
        let env = create_env c Tvoid in
        (* Add params to environment *)
        List.iter (fun (pty, id) ->
          let typ = typ_of_pexpr_typ ~loc:id.loc pty in
          let v = { var_name = id.id; var_type = typ; var_ofs = 0 } in
          Hashtbl.add env.vars id.id v
        ) params;
        let typed_body = typecheck_stmt env body in
        let vars = params_to_vars ~loc:class_id.loc params in
        Some (Dconstructor (vars, typed_body))
      | PDmethod (ret_opt, meth_id, params, body) ->
        let ret_type = match ret_opt with
          | None -> Tvoid
          | Some pty -> typ_of_pexpr_typ ~loc:meth_id.loc pty
        in
        let env = create_env c ret_type in
        (* Add params to environment *)
        List.iter (fun (pty, id) ->
          let typ = typ_of_pexpr_typ ~loc:id.loc pty in
          let v = { var_name = id.id; var_type = typ; var_ofs = 0 } in
          Hashtbl.add env.vars id.id v
        ) params;
        let typed_body = typecheck_stmt env body in
        (* Check return paths for non-void methods *)
        if ret_type <> Tvoid && not (always_returns typed_body) then
          error ~loc:meth_id.loc "method %s may not return a value" meth_id.id;
        (* Get or create method record *)
        let meth =
          if c.class_name = "Main" && meth_id.id = "main" then
            { meth_name = "main"; meth_type = Tvoid; meth_params = []; meth_ofs = 0 }
          else
            try Hashtbl.find c.class_methods meth_id.id
            with Not_found ->
              error ~loc:meth_id.loc "internal error: method %s not found" meth_id.id
        in
        Some (Dmethod (meth, typed_body))
    ) decls in

    (c, typed_decls)
  ) p in

  result


open Format
open X86_64
open Ast

let debug = ref false

(* ========================================================================= *)
(* Infrastructure                                                            *)
(* ========================================================================= *)

(* Label generation *)
let label_counter = ref 0
let fresh_label prefix =
  incr label_counter;
  Printf.sprintf "%s_%d" prefix !label_counter

(* String literals table: string content -> label *)
let string_literals : (string, string) Hashtbl.t = Hashtbl.create 17

(* Register a string literal and return its label *)
let register_string s =
  try Hashtbl.find string_literals s
  with Not_found ->
    let lbl = fresh_label "S" in
    Hashtbl.add string_literals s lbl;
    lbl

(* Class descriptor label *)
let descriptor_label c = "D_" ^ c.class_name

(* Method label *)
let method_label c m = c.class_name ^ "_" ^ m.meth_name

(* Constructor label *)
let constructor_label c = c.class_name ^ "_constructor"

(* Get class_Object and class_String from typing module *)
let class_Object = Typing.class_Object
let class_String = Typing.class_String

(* ========================================================================= *)
(* Offset Computation                                                        *)
(* ========================================================================= *)

(* Object size table: class -> size in bytes *)
let object_sizes : (string, int) Hashtbl.t = Hashtbl.create 17

(* Get object size for a class *)
let get_object_size c =
  try Hashtbl.find object_sizes c.class_name
  with Not_found -> 8  (* just descriptor pointer *)

(* Compute attribute offsets for all classes *)
let compute_attr_offsets (tfile : tfile) =
  (* Object has no attributes, size = 8 (just descriptor) *)
  Hashtbl.add object_sizes "Object" 8;
  (* String has no user attributes, size = 8 (descriptor, string data handled separately) *)
  Hashtbl.add object_sizes "String" 8;

  (* Process user classes in topological order (parents before children) *)
  (* tfile is already in order from type checking *)
  List.iter (fun (c, _decls) ->
    if c != class_Object && c != class_String then begin
      (* Start with parent's size *)
      let parent_size = get_object_size c.class_extends in
      let next_ofs = ref parent_size in

      (* Assign offsets to new attributes (not inherited) *)
      Hashtbl.iter (fun _name attr ->
        (* Check if this is a new attribute (not inherited) *)
        let inherited =
          c.class_extends != class_Object &&
          Hashtbl.mem c.class_extends.class_attributes attr.attr_name &&
          Hashtbl.find c.class_extends.class_attributes attr.attr_name == attr
        in
        if not inherited then begin
          attr.attr_ofs <- !next_ofs;
          next_ofs := !next_ofs + 8
        end
      ) c.class_attributes;

      Hashtbl.add object_sizes c.class_name !next_ofs
    end
  ) tfile

(* Compute method offsets for all classes *)
let compute_method_offsets (tfile : tfile) =
  (* Object has no methods, next offset = 8 *)
  (* String has equals method at offset 8 *)
  let equals_meth = Hashtbl.find class_String.class_methods "equals" in
  equals_meth.meth_ofs <- 8;

  (* Process user classes *)
  List.iter (fun (c, _decls) ->
    if c != class_Object && c != class_String then begin
      (* Find max offset from parent *)
      let max_parent_ofs = ref 0 in
      if c.class_extends != class_Object then begin
        Hashtbl.iter (fun _name meth ->
          if meth.meth_ofs > !max_parent_ofs then
            max_parent_ofs := meth.meth_ofs
        ) c.class_extends.class_methods
      end;
      let next_ofs = ref (!max_parent_ofs + 8) in

      (* Assign offsets to methods *)
      Hashtbl.iter (fun name meth ->
        (* Check if overriding parent method *)
        let parent_meth_opt =
          if c.class_extends == class_Object then None
          else try Some (Hashtbl.find c.class_extends.class_methods name)
          with Not_found -> None
        in
        match parent_meth_opt with
        | Some pm when pm != meth ->
          (* Overriding - use same offset as parent *)
          meth.meth_ofs <- pm.meth_ofs
        | Some _ ->
          (* Inherited method, offset already set *)
          ()
        | None ->
          (* New method *)
          meth.meth_ofs <- !next_ofs;
          next_ofs := !next_ofs + 8
      ) c.class_methods
    end
  ) tfile

(* ========================================================================= *)
(* Runtime Support                                                           *)
(* ========================================================================= *)

let generate_runtime () =
  (* Wrappers for libc functions with stack alignment *)
  aligned_call_wrapper ~f:"malloc" ~newf:"my_malloc" ++
  aligned_call_wrapper ~f:"printf" ~newf:"my_printf" ++
  aligned_call_wrapper ~f:"sprintf" ~newf:"my_sprintf" ++
  aligned_call_wrapper ~f:"strlen" ~newf:"my_strlen" ++
  aligned_call_wrapper ~f:"strcmp" ~newf:"my_strcmp" ++
  aligned_call_wrapper ~f:"strcpy" ~newf:"my_strcpy" ++
  aligned_call_wrapper ~f:"strcat" ~newf:"my_strcat" ++
  aligned_call_wrapper ~f:"exit" ~newf:"my_exit" ++

  (* Null check routine *)
  label "_check_null" ++
  testq !%rdi !%rdi ++
  jz "_null_error" ++
  ret ++

  (* Null error handler *)
  label "_null_error" ++
  leaq (lab "error_msg") rdi ++
  xorq !%rax !%rax ++
  call "my_printf" ++
  movq (imm 1) !%rdi ++
  call "my_exit" ++

  (* Division by zero check: %rcx is divisor *)
  label "_check_div_zero" ++
  testq !%rcx !%rcx ++
  jz "_div_zero_error" ++
  ret ++

  label "_div_zero_error" ++
  leaq (lab "error_msg") rdi ++
  xorq !%rax !%rax ++
  call "my_printf" ++
  movq (imm 1) !%rdi ++
  call "my_exit" ++

  (* Cast check: %rdi = object, %rsi = target descriptor *)
  label "_check_cast" ++
  testq !%rdi !%rdi ++
  jz "_cast_null_ok" ++
  movq (ind rdi) !%rax ++  (* object's descriptor *)
  label "_cast_loop" ++
  cmpq !%rax !%rsi ++
  je "_cast_ok" ++
  movq (ind rax) !%rax ++  (* parent descriptor *)
  testq !%rax !%rax ++
  jnz "_cast_loop" ++
  (* cast failed *)
  leaq (lab "error_msg") rdi ++
  xorq !%rax !%rax ++
  call "my_printf" ++
  movq (imm 1) !%rdi ++
  call "my_exit" ++
  label "_cast_null_ok" ++
  label "_cast_ok" ++
  movq !%rdi !%rax ++
  ret ++

  (* Instanceof: %rdi = object, %rsi = target descriptor *)
  (* Returns 0 or 1 in %rax *)
  label "_instanceof" ++
  testq !%rdi !%rdi ++
  jz "_instanceof_false" ++
  movq (ind rdi) !%rax ++  (* object's descriptor *)
  label "_instanceof_loop" ++
  cmpq !%rax !%rsi ++
  je "_instanceof_true" ++
  movq (ind rax) !%rax ++  (* parent descriptor *)
  testq !%rax !%rax ++
  jnz "_instanceof_loop" ++
  label "_instanceof_false" ++
  xorq !%rax !%rax ++
  ret ++
  label "_instanceof_true" ++
  movq (imm 1) !%rax ++
  ret ++

  (* String concatenation: %rdi = str1, %rsi = str2 *)
  (* Returns new string in %rax *)
  label "_concat_strings" ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  pushq !%r12 ++
  pushq !%r13 ++
  pushq !%r14 ++
  movq !%rdi !%r12 ++  (* save str1 *)
  movq !%rsi !%r13 ++  (* save str2 *)
  (* Get length of str1 (skip descriptor) *)
  leaq (ind ~ofs:8 r12) rdi ++
  call "my_strlen" ++
  movq !%rax !%r14 ++  (* len1 *)
  (* Get length of str2 *)
  leaq (ind ~ofs:8 r13) rdi ++
  call "my_strlen" ++
  (* Allocate: 8 (descriptor) + len1 + len2 + 1 (null terminator) *)
  addq !%r14 !%rax ++
  addq (imm 9) !%rax ++
  movq !%rax !%rdi ++
  call "my_malloc" ++
  (* Set descriptor *)
  leaq (lab "D_String") rdi ++
  movq !%rdi (ind rax) ++
  (* Copy str1 *)
  pushq !%rax ++
  leaq (ind ~ofs:8 rax) rdi ++
  leaq (ind ~ofs:8 r12) rsi ++
  call "my_strcpy" ++
  popq rax ++
  (* Concatenate str2 *)
  pushq !%rax ++
  leaq (ind ~ofs:8 rax) rdi ++
  leaq (ind ~ofs:8 r13) rsi ++
  call "my_strcat" ++
  popq rax ++
  (* Return new string *)
  popq r14 ++
  popq r13 ++
  popq r12 ++
  popq rbp ++
  ret ++

  (* Integer to string: %rdi = integer *)
  (* Returns new string in %rax *)
  label "_int_to_string" ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  pushq !%r12 ++
  movq !%rdi !%r12 ++  (* save integer *)
  (* Allocate buffer: 8 (descriptor) + 21 (max int64 digits + sign + null) *)
  movq (imm 32) !%rdi ++
  call "my_malloc" ++
  (* Set descriptor *)
  pushq !%rax ++
  leaq (lab "D_String") rdi ++
  movq !%rdi (ind rax) ++
  (* Format integer *)
  popq rax ++
  pushq !%rax ++
  leaq (ind ~ofs:8 rax) rdi ++
  leaq (lab "int_format") rsi ++
  movq !%r12 !%rdx ++
  xorq !%rax !%rax ++
  call "my_sprintf" ++
  popq rax ++
  popq r12 ++
  popq rbp ++
  ret ++

  (* String.equals: this at 16(%rbp), arg at 24(%rbp) *)
  label "String_equals" ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ind ~ofs:16 rbp) !%rdi ++
  movq (ind ~ofs:24 rbp) !%rsi ++
  (* Handle null cases *)
  testq !%rdi !%rdi ++
  jz "_string_equals_false" ++
  testq !%rsi !%rsi ++
  jz "_string_equals_false" ++
  (* Compare string contents *)
  leaq (ind ~ofs:8 rdi) rdi ++
  leaq (ind ~ofs:8 rsi) rsi ++
  call "my_strcmp" ++
  testq !%rax !%rax ++
  jnz "_string_equals_false" ++
  movq (imm 1) !%rax ++
  popq rbp ++
  ret ++
  label "_string_equals_false" ++
  xorq !%rax !%rax ++
  popq rbp ++
  ret

let generate_runtime_data () =
  label "error_msg" ++ string "error\n" ++
  label "int_format" ++ string "%ld" ++
  label "print_format" ++ string "%s"

(* ========================================================================= *)
(* Class Descriptors                                                         *)
(* ========================================================================= *)

(* Get the method implementation for a class (own or inherited) *)
let rec get_method_impl c meth_name =
  try
    let m = Hashtbl.find c.class_methods meth_name in
    (* Check if it's our own method or inherited *)
    if c == class_Object then
      None
    else if c == class_String then
      Some (c, m)
    else begin
      (* Check if parent has this method *)
      match get_method_impl c.class_extends meth_name with
      | None -> Some (c, m)
      | Some (pc, pm) ->
        if pm == m then Some (pc, pm)  (* inherited *)
        else Some (c, m)  (* overridden *)
    end
  with Not_found -> None

(* Generate class descriptor for a class *)
let generate_descriptor (c : class_) =
  if c == class_Object then
    label "D_Object" ++
    address ["0"]  (* no parent *)
  else if c == class_String then
    label "D_String" ++
    address ["D_Object"] ++
    address ["String_equals"]
  else begin
    (* Collect methods with their offsets *)
    let methods = Hashtbl.fold (fun name meth acc ->
      (meth.meth_ofs, name, meth) :: acc
    ) c.class_methods [] in
    (* Sort by offset *)
    let methods = List.sort (fun (o1, _, _) (o2, _, _) -> compare o1 o2) methods in

    (* Generate descriptor *)
    let parent_label = descriptor_label c.class_extends in
    label (descriptor_label c) ++
    address [parent_label] ++
    List.fold_left (fun code (_ofs, name, _meth) ->
      let impl_label = match get_method_impl c name with
        | Some (impl_class, _) -> method_label impl_class { meth_name = name; meth_type = Tvoid; meth_params = []; meth_ofs = 0 }
        | None -> "0"  (* should not happen *)
      in
      code ++ address [impl_label]
    ) nop methods
  end

let generate_all_descriptors (tfile : tfile) =
  generate_descriptor class_Object ++
  generate_descriptor class_String ++
  List.fold_left (fun code (c, _) ->
    if c != class_Object && c != class_String then
      code ++ generate_descriptor c
    else code
  ) nop tfile

(* ========================================================================= *)
(* String Literals                                                           *)
(* ========================================================================= *)

let generate_string_literals () =
  Hashtbl.fold (fun content lbl code ->
    code ++
    label lbl ++
    address ["D_String"] ++
    string content
  ) string_literals nop

(* ========================================================================= *)
(* Expression Compilation                                                    *)
(* ========================================================================= *)

(* Compile expression, result in %rax *)
let rec compile_expr (e : expr) : text =
  match e.expr_desc with
  | Econstant (Cint n) ->
    (* All Mini-Java ints fit in 32 bits, but we use 64-bit internally *)
    movq (imm32 n) !%rax

  | Econstant (Cbool true) ->
    movq (imm 1) !%rax

  | Econstant (Cbool false) ->
    xorq !%rax !%rax

  | Econstant (Cstring s) ->
    let lbl = register_string s in
    leaq (lab lbl) rax

  | Enull ->
    xorq !%rax !%rax

  | Ethis ->
    movq (ind ~ofs:16 rbp) !%rax

  | Evar v ->
    movq (ind ~ofs:v.var_ofs rbp) !%rax

  | Eassign_var (v, e) ->
    compile_expr e ++
    movq !%rax (ind ~ofs:v.var_ofs rbp)

  | Eattr (e1, attr) ->
    compile_expr e1 ++
    movq !%rax !%rdi ++
    call "_check_null" ++
    movq (ind ~ofs:attr.attr_ofs rax) !%rax

  | Eassign_attr (e1, attr, e2) ->
    compile_expr e2 ++
    pushq !%rax ++
    compile_expr e1 ++
    movq !%rax !%rdi ++
    call "_check_null" ++
    popq rdx ++
    movq !%rdx (ind ~ofs:attr.attr_ofs rax) ++
    movq !%rdx !%rax  (* assignment returns the value *)

  | Eunop (Uneg, e1) ->
    compile_expr e1 ++
    negq !%rax

  | Eunop (Unot, e1) ->
    compile_expr e1 ++
    xorq (imm 1) !%rax

  | Eunop (Ustring_of_int, e1) ->
    compile_expr e1 ++
    movq !%rax !%rdi ++
    call "_int_to_string"

  | Ebinop (Badd, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    addq !%rbx !%rax

  | Ebinop (Bsub, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    subq !%rax !%rbx ++
    movq !%rbx !%rax

  | Ebinop (Bmul, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    imulq !%rbx !%rax

  | Ebinop (Bdiv, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    movq !%rax !%rcx ++  (* divisor in rcx *)
    call "_check_div_zero" ++
    popq rax ++  (* dividend in rax *)
    cqto ++
    idivq !%rcx  (* quotient in rax *)

  | Ebinop (Bmod, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    movq !%rax !%rcx ++  (* divisor in rcx *)
    call "_check_div_zero" ++
    popq rax ++  (* dividend in rax *)
    cqto ++
    idivq !%rcx ++
    movq !%rdx !%rax  (* remainder in rdx *)

  | Ebinop (Beq, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    sete (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Bneq, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    setne (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Blt, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    setl (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Ble, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    setle (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Bgt, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    setg (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Bge, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    popq rbx ++
    cmpq !%rax !%rbx ++
    setge (reg al) ++
    movzbq (reg al) rax

  | Ebinop (Band, e1, e2) ->
    let lbl_false = fresh_label "and_false" in
    let lbl_end = fresh_label "and_end" in
    compile_expr e1 ++
    testq !%rax !%rax ++
    jz lbl_false ++
    compile_expr e2 ++
    testq !%rax !%rax ++
    jz lbl_false ++
    movq (imm 1) !%rax ++
    jmp lbl_end ++
    label lbl_false ++
    xorq !%rax !%rax ++
    label lbl_end

  | Ebinop (Bor, e1, e2) ->
    let lbl_true = fresh_label "or_true" in
    let lbl_end = fresh_label "or_end" in
    compile_expr e1 ++
    testq !%rax !%rax ++
    jnz lbl_true ++
    compile_expr e2 ++
    testq !%rax !%rax ++
    jnz lbl_true ++
    xorq !%rax !%rax ++
    jmp lbl_end ++
    label lbl_true ++
    movq (imm 1) !%rax ++
    label lbl_end

  | Ebinop (Badd_s, e1, e2) ->
    compile_expr e1 ++
    pushq !%rax ++
    compile_expr e2 ++
    movq !%rax !%rsi ++
    popq rdi ++
    call "_concat_strings"

  | Enew (c, args) ->
    (* Allocate object *)
    let size = get_object_size c in
    movq (imm size) !%rdi ++
    call "my_malloc" ++
    (* Set descriptor *)
    pushq !%rax ++
    leaq (lab (descriptor_label c)) rdi ++
    movq !%rdi (ind rax) ++
    (* Initialize attributes to 0 *)
    (let init_code = ref nop in
     for i = 1 to (size / 8) - 1 do
       init_code := !init_code ++ movq (imm 0) (ind ~ofs:(i*8) rax)
     done;
     !init_code) ++
    (* Push arguments in reverse order *)
    (let push_args = List.fold_right (fun arg code ->
      compile_expr arg ++
      pushq !%rax ++
      code
    ) args nop in
    push_args) ++
    (* Push 'this' *)
    pushq (ind rsp ~ofs:(8 * List.length args)) ++
    (* Call constructor *)
    call (constructor_label c) ++
    (* Pop arguments + this *)
    addq (imm (8 * (List.length args + 1))) !%rsp ++
    (* Return object pointer *)
    popq rax

  | Ecall (e_receiver, meth, args) ->
    (* Push arguments in reverse order *)
    let push_args = List.fold_right (fun arg code ->
      compile_expr arg ++
      pushq !%rax ++
      code
    ) args nop in
    push_args ++
    (* Compile receiver and push as 'this' *)
    compile_expr e_receiver ++
    pushq !%rax ++
    (* Null check *)
    movq !%rax !%rdi ++
    call "_check_null" ++
    (* Get descriptor and method pointer *)
    movq (ind rax) !%rcx ++  (* descriptor *)
    movq (ind ~ofs:meth.meth_ofs rcx) !%rcx ++  (* method pointer *)
    (* Call method *)
    call_star !%rcx ++
    (* Pop arguments + this *)
    addq (imm (8 * (List.length args + 1))) !%rsp

  | Ecast (c, e1) ->
    compile_expr e1 ++
    movq !%rax !%rdi ++
    leaq (lab (descriptor_label c)) rsi ++
    call "_check_cast"

  | Einstanceof (e1, class_name) ->
    let c = Hashtbl.find Typing.class_table class_name in
    compile_expr e1 ++
    movq !%rax !%rdi ++
    leaq (lab (descriptor_label c)) rsi ++
    call "_instanceof"

  | Eprint e1 ->
    compile_expr e1 ++
    (* String pointer in rax, actual string at offset 8 *)
    leaq (ind ~ofs:8 rax) rdi ++
    xorq !%rax !%rax ++
    call "my_printf" ++
    xorq !%rax !%rax  (* print returns void *)

(* ========================================================================= *)
(* Statement Compilation                                                     *)
(* ========================================================================= *)

let rec compile_stmt (s : stmt) : text =
  match s with
  | Sexpr e ->
    compile_expr e

  | Svar (v, init) ->
    compile_expr init ++
    movq !%rax (ind ~ofs:v.var_ofs rbp)

  | Sif (cond, s1, s2) ->
    let lbl_else = fresh_label "else" in
    let lbl_end = fresh_label "endif" in
    compile_expr cond ++
    testq !%rax !%rax ++
    jz lbl_else ++
    compile_stmt s1 ++
    jmp lbl_end ++
    label lbl_else ++
    compile_stmt s2 ++
    label lbl_end

  | Sreturn None ->
    leave ++
    ret

  | Sreturn (Some e) ->
    compile_expr e ++
    leave ++
    ret

  | Sblock stmts ->
    List.fold_left (fun code s -> code ++ compile_stmt s) nop stmts

  | Sfor (init, cond, update, body) ->
    let lbl_loop = fresh_label "for_loop" in
    let lbl_end = fresh_label "for_end" in
    compile_stmt init ++
    label lbl_loop ++
    compile_expr cond ++
    testq !%rax !%rax ++
    jz lbl_end ++
    compile_stmt body ++
    compile_stmt update ++
    jmp lbl_loop ++
    label lbl_end

(* ========================================================================= *)
(* Local Variable Offset Assignment                                          *)
(* ========================================================================= *)

(* Count and assign offsets to local variables in a statement *)
let rec assign_local_offsets (s : stmt) (next_ofs : int) : int =
  match s with
  | Svar (v, _) ->
    v.var_ofs <- next_ofs;
    next_ofs - 8
  | Sblock stmts ->
    List.fold_left (fun ofs stmt -> assign_local_offsets stmt ofs) next_ofs stmts
  | Sif (_, s1, s2) ->
    let ofs1 = assign_local_offsets s1 next_ofs in
    assign_local_offsets s2 ofs1
  | Sfor (init, _, update, body) ->
    let ofs1 = assign_local_offsets init next_ofs in
    let ofs2 = assign_local_offsets update ofs1 in
    assign_local_offsets body ofs2
  | Sexpr _ | Sreturn _ ->
    next_ofs

(* ========================================================================= *)
(* Method and Constructor Compilation                                        *)
(* ========================================================================= *)

let compile_method (c : class_) (m : method_) (body : stmt) : text =
  (* Assign parameter offsets: this at +16, param1 at +24, etc. *)
  List.iteri (fun i param ->
    param.var_ofs <- 24 + i * 8
  ) m.meth_params;

  (* Assign local variable offsets *)
  let last_ofs = assign_local_offsets body (-8) in
  let locals_size = -(last_ofs + 8) in
  (* Round up to multiple of 16 for alignment *)
  let locals_size = ((locals_size + 15) / 16) * 16 in

  label (method_label c m) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  (if locals_size > 0 then subq (imm locals_size) !%rsp else nop) ++
  compile_stmt body ++
  (* Implicit return for void methods *)
  (if m.meth_type = Tvoid then
    leave ++ ret
  else
    nop)

let compile_constructor (c : class_) (params : var list) (body : stmt) : text =
  (* Assign parameter offsets: this at +16, param1 at +24, etc. *)
  List.iteri (fun i param ->
    param.var_ofs <- 24 + i * 8
  ) params;

  (* Assign local variable offsets *)
  let last_ofs = assign_local_offsets body (-8) in
  let locals_size = -(last_ofs + 8) in
  let locals_size = ((locals_size + 15) / 16) * 16 in

  label (constructor_label c) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  (if locals_size > 0 then subq (imm locals_size) !%rsp else nop) ++
  compile_stmt body ++
  leave ++
  ret

(* Default constructor: does nothing *)
let compile_default_constructor (c : class_) : text =
  label (constructor_label c) ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  leave ++
  ret

(* ========================================================================= *)
(* Main Entry Point                                                          *)
(* ========================================================================= *)

let compile_main (main_body : stmt) : text =
  (* Assign local variable offsets for main *)
  let last_ofs = assign_local_offsets main_body (-8) in
  let locals_size = -(last_ofs + 8) in
  let locals_size = ((locals_size + 15) / 16) * 16 in

  globl "main" ++
  label "main" ++
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  (if locals_size > 0 then subq (imm locals_size) !%rsp else nop) ++
  compile_stmt main_body ++
  xorq !%rax !%rax ++  (* return 0 *)
  leave ++
  ret

(* ========================================================================= *)
(* Main Compilation Function                                                 *)
(* ========================================================================= *)

let file ?debug:(b=false) (tfile : Ast.tfile) : X86_64.program =
  debug := b;

  (* Reset state *)
  label_counter := 0;
  Hashtbl.clear string_literals;
  Hashtbl.clear object_sizes;

  (* Compute offsets *)
  compute_attr_offsets tfile;
  compute_method_offsets tfile;

  (* Find Main class and its main body *)
  let main_body = ref (Sblock []) in
  let all_methods = ref nop in

  List.iter (fun (c, decls) ->
    if c.class_name = "Main" then begin
      (* Main class - find main method body *)
      List.iter (fun decl ->
        match decl with
        | Dmethod (m, body) when m.meth_name = "main" ->
          main_body := body
        | _ -> ()
      ) decls
    end else if c != class_Object && c != class_String then begin
      (* Regular class - compile methods and constructor *)
      let has_constructor = ref false in
      List.iter (fun decl ->
        match decl with
        | Dmethod (m, body) ->
          all_methods := !all_methods ++ compile_method c m body
        | Dconstructor (params, body) ->
          has_constructor := true;
          all_methods := !all_methods ++ compile_constructor c params body
      ) decls;
      (* Add default constructor if none defined *)
      if not !has_constructor then
        all_methods := !all_methods ++ compile_default_constructor c
    end
  ) tfile;

  (* Generate program - sequence text before data to ensure strings are registered *)
  let text_code =
    generate_runtime () ++
    compile_main !main_body ++
    !all_methods
  in
  let data_code =
    generate_runtime_data () ++
    generate_all_descriptors tfile ++
    generate_string_literals ()
  in
  { text = text_code; data = data_code }

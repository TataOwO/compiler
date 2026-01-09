# Mini-Java Compiler Implementation Plan

## Project Overview

Build a compiler for Mini-Java targeting x86-64 assembly. The lexer, parser, AST definitions, and x86-64 library are provided. We need to implement:

1. **Type Checking** (`typing.ml`)
2. **Code Generation** (`compile.ml`)

---

## Phase 1: Type Checking (`typing.ml`)

The type checker transforms `Ast.pfile` (parsed AST) into `Ast.tfile` (typed AST).

### 1.1 Data Structures Setup

**Goal:** Create the infrastructure for type checking.

```ocaml
(* Global class table: name -> class_ *)
let class_table : (string, class_) Hashtbl.t = Hashtbl.create 17

(* Predefined classes *)
let class_Object : class_
let class_String : class_
```

**Tasks:**
- [x] Create `class_Object` with no parent, no attributes, no methods
- [x] Create `class_String` extending `Object` with `equals(String): boolean` method
- [x] Add helper functions:
  - `find_class : string -> class_` (lookup with error)
  - `is_subtype : typ -> typ -> bool` (τ₁ ⊑ τ₂)
  - `are_compatible : typ -> typ -> bool` (τ₁ ≡ τ₂)
  - `typ_of_pexpr_typ : pexpr_typ -> typ` (convert parsed type to typed type)

### 1.2 First Pass: Collect Class Names

**Goal:** Register all class names and check uniqueness.

```ocaml
let collect_classes (pfile: pfile) : unit
```

**Tasks:**
- [x] Iterate over all `pclass` in `pfile`
- [x] For each class, check name is not already defined
- [x] Check class is not named `Object` or `String` (reserved)
- [x] Create `class_` record with empty methods/attributes hashtables
- [x] Add to `class_table`
- [x] Verify `Main` class exists

### 1.3 Second Pass: Build Inheritance Hierarchy

**Goal:** Resolve extends relationships and check for cycles.

```ocaml
let resolve_inheritance () : unit
```

**Tasks:**
- [x] For each class, resolve `extends` to actual `class_` reference
- [x] Default to `Object` if no extends clause
- [x] Check parent class exists
- [x] Check not extending `String`
- [x] Detect inheritance cycles using DFS/visited set
- [x] Build topological order for processing

### 1.4 Third Pass: Collect Members

**Goal:** Register attributes, constructors, and methods for each class.

```ocaml
let collect_members (c: class_) (decls: pdecl list) : unit
```

**Tasks:**
- [x] For each `PDattribute`:
  - Check type is well-formed
  - Check name not already declared in this class
  - Create `attribute` record, add to `class_attributes`

- [x] For each `PDconstructor`:
  - Check at most one constructor
  - Check name matches class name
  - Check parameter names are distinct
  - Check parameter types are well-formed

- [x] For each `PDmethod`:
  - Check name not already declared in this class
  - Check parameter names are distinct
  - Check parameter/return types are well-formed
  - If overriding parent method: check same signature
  - Create `method_` record, add to `class_methods`

### 1.5 Type Checking Expressions

**Goal:** Implement `typecheck_expr : env -> pexpr -> expr`

The typing environment `env` maps variable names to `var` records.

```ocaml
type env = {
  vars: (string, var) Hashtbl.t;  (* local variables *)
  current_class: class_;           (* C₀ in the spec *)
  this_var: var;                   (* "this" binding *)
}
```

**Tasks for each expression type:**

- [x] `PEconstant` → Return typed constant with appropriate type
- [x] `PEthis` → Return `Ethis` with type `Tclass current_class`
- [x] `PEnull` → Return `Enull` with type `Tnull`
- [x] `PEident` → Look up in env, then in class attributes
- [x] `PEdot` → Type check receiver, look up attribute in its class
- [x] `PEassign_ident` → Check variable exists, check subtype
- [x] `PEassign_dot` → Type check receiver, find attribute, check subtype
- [x] `PEunop Uneg` → Check operand is `int`, return `int`
- [x] `PEunop Unot` → Check operand is `boolean`, return `boolean`
- [x] `PEbinop` → Handle all cases:
  - `+,-,*,/,%` on `int` → `int`
  - `<,<=,>,>=` on `int` → `boolean`
  - `==,!=` on compatible types → `boolean`
  - `&&,||` on `boolean` → `boolean`
  - `+` with `String` → `String` (use `Badd_s`, insert `Ustring_of_int` if needed)
- [x] `PEnew` → Find class, find constructor, check argument types
- [x] `PEcall` → Type check receiver, find method, check argument types
- [x] `PEcast` → Check types are compatible, return cast type
- [x] `PEinstanceof` → Check receiver is class/null type, return `boolean`

### 1.6 Type Checking Statements

**Goal:** Implement `typecheck_stmt : env -> pstmt -> stmt * env`

Statements can extend the environment (variable declarations).

**Tasks:**

- [x] `PSexpr` → Type check expression, return `Sexpr`
- [x] `PSvar` → Check variable not already in scope, check type well-formed, optionally check initializer subtype, extend env
- [x] `PSif` → Check condition is `boolean`, type check both branches (scopes don't leak)
- [x] `PSfor` → Type check init/cond/update/body (scope of body doesn't leak)
- [x] `PSblock` → Type check statements sequentially, threading env
- [x] `PSreturn` → Record return type for later checking

### 1.7 Type Checking Methods & Constructors

**Goal:** Type check bodies with proper return checking.

**Tasks:**

- [x] For constructors:
  - Build env with `this` + parameters
  - Type check body

- [x] For methods:
  - Build env with `this` + parameters
  - Type check body
  - If return type is not `void`: verify all paths return
  - Verify all `return` statements have correct subtype

### 1.8 Return Path Analysis

**Goal:** Ensure non-void methods always return.

```ocaml
let always_returns : stmt -> bool
```

**Tasks:**
- [x] `Sreturn _` → true
- [x] `Sif (_, s1, s2)` → `always_returns s1 && always_returns s2`
- [x] `Sblock stmts` → any statement always returns
- [x] `Sfor` → false (loop might not execute)
- [x] `Sexpr`, `Svar` → false

---

## Phase 2: Code Generation (`compile.ml`)

The code generator transforms `Ast.tfile` into `X86_64.program`.

### 2.1 Setup & Data Structures

**Tasks:**
- [x] Label generation: `let fresh_label prefix = ...`
- [x] String literals table: collect all strings, generate labels
- [x] Class descriptor labels: `"D_" ^ class_name`
- [x] Method labels: `class_name ^ "_" ^ method_name`

### 2.2 Calculate Offsets

**Goal:** Assign memory offsets to attributes and methods.

**Attribute offsets (within object):**
```
Object layout: [class_descriptor_ptr | attr1 | attr2 | ... | attrN]
               offset 0               offset 8  offset 16  ...
```

**Tasks:**
- [x] Process classes in inheritance order (parent before child)
- [x] Each class inherits parent's attributes at same offsets
- [x] New attributes get offsets after parent's last attribute
- [x] Set `attr.attr_ofs` for each attribute

**Method offsets (within class descriptor):**
```
Descriptor: [parent_descriptor_ptr | method1_ptr | method2_ptr | ...]
            offset 0                offset 8       offset 16     ...
```

**Tasks:**
- [x] Process classes in inheritance order
- [x] Inherited methods keep same offset
- [x] Overridden methods use parent's offset
- [x] New methods get next available offset
- [x] Set `meth.meth_ofs` for each method

### 2.3 Generate Class Descriptors

**Goal:** Generate `.data` section with class descriptors.

```asm
D_MyClass:
    .quad D_ParentClass     # pointer to parent descriptor
    .quad MyClass_method1   # method at offset 8
    .quad MyClass_method2   # method at offset 16
    ...
```

**Tasks:**
- [x] Generate `D_Object` descriptor (parent = 0)
- [x] Generate `D_String` descriptor
- [x] For each user class, generate descriptor with:
  - Pointer to parent descriptor
  - Method pointers (own or inherited)

### 2.4 Generate String Literals

**Tasks:**
- [x] Collect all string constants from the program
- [x] Generate labeled strings in `.data`:
  ```asm
  S_0:
      .quad D_String
      .string "hello\n"
  ```

### 2.5 Compile Expressions

**Goal:** `compile_expr : expr -> text`

Expressions leave their result in `%rax`.

**Tasks for each expression:**

- [x] `Econstant (Cint n)` → `movq $n, %rax`
- [x] `Econstant (Cbool b)` → `movq $0/%1, %rax`
- [x] `Econstant (Cstring s)` → `leaq S_label, %rax`
- [x] `Enull` → `movq $0, %rax`
- [x] `Ethis` → `movq this_offset(%rbp), %rax`
- [x] `Evar v` → `movq v.var_ofs(%rbp), %rax`
- [x] `Eattr (e, attr)` → compile e, `movq attr.attr_ofs(%rax), %rax`
- [x] `Eassign_var (v, e)` → compile e, `movq %rax, v.var_ofs(%rbp)`
- [x] `Eassign_attr (e1, attr, e2)` → compile both, store
- [x] `Eunop Uneg` → compile e, `negq %rax`
- [x] `Eunop Unot` → compile e, `xorq $1, %rax`
- [x] `Ebinop` → compile e1, push, compile e2, pop, operate
- [x] `Enew` → call malloc, set descriptor, call constructor
- [x] `Ecall` → push args, push this, indirect call via vtable
- [x] `Ecast` → compile e, check at runtime (or skip if statically safe)
- [x] `Einstanceof` → compile e, call runtime check routine
- [x] `Eprint` → compile string arg, call printf wrapper

### 2.6 Compile Statements

**Goal:** `compile_stmt : stmt -> text`

**Tasks:**

- [x] `Sexpr e` → compile_expr e (discard result)
- [x] `Svar (v, e)` → compile e, store at `v.var_ofs(%rbp)`
- [x] `Sif (e, s1, s2)` → compile condition, conditional jump, compile branches
- [x] `Sfor (init, cond, update, body)` → compile with loop labels
- [x] `Sblock stmts` → compile each statement
- [x] `Sreturn None` → `leave; ret`
- [x] `Sreturn (Some e)` → compile e, `leave; ret`

### 2.7 Compile Methods & Constructors

**Method compilation:**
```asm
ClassName_methodName:
    pushq %rbp
    movq %rsp, %rbp
    subq $locals_size, %rsp    # allocate locals
    ... body ...
    leave
    ret
```

**Constructor compilation:**
- Similar to methods
- Initialize attributes to default values (0/null)
- `this` is passed as first argument

**Tasks:**
- [x] Calculate stack frame size (locals + temporaries)
- [x] Set `var.var_ofs` for parameters (positive offsets from %rbp)
- [x] Set `var.var_ofs` for locals (negative offsets from %rbp)
- [x] Generate prologue/epilogue
- [x] Compile body

### 2.8 Main Function

**Goal:** Generate entry point that calls `Main.main()`.

```asm
main:
    pushq %rbp
    movq %rsp, %rbp
    ... compile Main's main body ...
    movq $0, %rax
    leave
    ret
```

### 2.9 Runtime Support

**Tasks:**

- [x] **Stack-aligned wrappers** for libc functions:
  ```ocaml
  aligned_call_wrapper ~f:"malloc" ~newf:"my_malloc"
  aligned_call_wrapper ~f:"printf" ~newf:"my_printf"
  ```

- [x] **Cast checking routine:**
  ```asm
  _check_cast:
      # Input: %rdi = object, %rsi = target class descriptor
      # Output: %rax = object (or exit with error)
      testq %rdi, %rdi
      jz .cast_ok           # null is ok
      movq (%rdi), %rax     # get object's descriptor
  .loop:
      cmpq %rax, %rsi
      je .cast_ok
      movq (%rax), %rax     # go to parent
      testq %rax, %rax
      jnz .loop
      # cast failed - print error and exit
      ...
  .cast_ok:
      movq %rdi, %rax
      ret
  ```

- [x] **Instanceof routine:**
  ```asm
  _instanceof:
      # Input: %rdi = object, %rsi = target class descriptor
      # Output: %rax = 0 or 1
      ...similar to cast but returns bool...
  ```

- [x] **Null pointer check** (for method calls):
  ```asm
  _check_null:
      testq %rdi, %rdi
      jz .null_error
      ret
  .null_error:
      ... print error, exit(1) ...
  ```

---

## Phase 3: Testing & Refinement

### 3.1 Unit Testing

- [x] Test type checker with `typing/good/*.java`
- [x] Test type errors with `typing/bad/*.java`
- [x] Verify parse errors with `syntax/bad/*.java`

### 3.2 Integration Testing

- [x] Test code generation with `exec/*.java`
- [x] Compare output with `.out` files
- [x] Test runtime errors with `exec-fail/*.java`

### 3.3 Edge Cases

- [x] Empty classes
- [x] Deep inheritance chains
- [x] Method overriding
- [x] String concatenation with integers
- [x] Null handling
- [x] Cast failures
- [x] Division by zero (if required)

---

## Implementation Order

### Week 1: Type Checking Foundation
1. Setup data structures (1.1)
2. First pass - collect classes (1.2)
3. Second pass - inheritance (1.3)
4. Third pass - members (1.4)

### Week 2: Type Checking Completion
5. Type check expressions (1.5)
6. Type check statements (1.6)
7. Type check methods (1.7)
8. Return analysis (1.8)

### Week 3: Code Generation Foundation
9. Setup & offsets (2.1, 2.2)
10. Class descriptors (2.3)
11. String literals (2.4)

### Week 4: Code Generation Completion
12. Compile expressions (2.5)
13. Compile statements (2.6)
14. Methods & main (2.7, 2.8)
15. Runtime support (2.9)

### Week 5: Testing & Polish
16. Run test suite
17. Fix bugs
18. Write report

---

## Key References

- `ast.ml:81-116` - Typed tree definitions (`typ`, `class_`, `method_`, `var`)
- `ast.ml:118-152` - Typed expression/statement types
- `x86_64.mli` - Assembly generation API
- `mini-java.pdf` Section 2 - Type rules
- `mini-java.pdf` Section 3 - Code generation hints

---

## Useful OCaml Patterns

### Error reporting
```ocaml
let error ?(loc=dummy_loc) fmt =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ fmt ^^ "@]")

(* Usage: *)
error ~loc:e.pexpr_loc "type mismatch: expected %s, got %s"
  (string_of_typ expected) (string_of_typ actual)
```

### Hashtable with default
```ocaml
let find_or_add tbl key default =
  try Hashtbl.find tbl key
  with Not_found ->
    let v = default () in
    Hashtbl.add tbl key v; v
```

### Fold with environment threading
```ocaml
let rec typecheck_stmts env = function
  | [] -> ([], env)
  | s :: rest ->
    let (s', env') = typecheck_stmt env s in
    let (rest', env'') = typecheck_stmts env' rest in
    (s' :: rest', env'')
```

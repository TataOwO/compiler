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
- [ ] Create `class_Object` with no parent, no attributes, no methods
- [ ] Create `class_String` extending `Object` with `equals(String): boolean` method
- [ ] Add helper functions:
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
- [ ] Iterate over all `pclass` in `pfile`
- [ ] For each class, check name is not already defined
- [ ] Check class is not named `Object` or `String` (reserved)
- [ ] Create `class_` record with empty methods/attributes hashtables
- [ ] Add to `class_table`
- [ ] Verify `Main` class exists

### 1.3 Second Pass: Build Inheritance Hierarchy

**Goal:** Resolve extends relationships and check for cycles.

```ocaml
let resolve_inheritance () : unit
```

**Tasks:**
- [ ] For each class, resolve `extends` to actual `class_` reference
- [ ] Default to `Object` if no extends clause
- [ ] Check parent class exists
- [ ] Check not extending `String`
- [ ] Detect inheritance cycles using DFS/visited set
- [ ] Build topological order for processing

### 1.4 Third Pass: Collect Members

**Goal:** Register attributes, constructors, and methods for each class.

```ocaml
let collect_members (c: class_) (decls: pdecl list) : unit
```

**Tasks:**
- [ ] For each `PDattribute`:
  - Check type is well-formed
  - Check name not already declared in this class
  - Create `attribute` record, add to `class_attributes`

- [ ] For each `PDconstructor`:
  - Check at most one constructor
  - Check name matches class name
  - Check parameter names are distinct
  - Check parameter types are well-formed

- [ ] For each `PDmethod`:
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

- [ ] `PEconstant` → Return typed constant with appropriate type
- [ ] `PEthis` → Return `Ethis` with type `Tclass current_class`
- [ ] `PEnull` → Return `Enull` with type `Tnull`
- [ ] `PEident` → Look up in env, then in class attributes
- [ ] `PEdot` → Type check receiver, look up attribute in its class
- [ ] `PEassign_ident` → Check variable exists, check subtype
- [ ] `PEassign_dot` → Type check receiver, find attribute, check subtype
- [ ] `PEunop Uneg` → Check operand is `int`, return `int`
- [ ] `PEunop Unot` → Check operand is `boolean`, return `boolean`
- [ ] `PEbinop` → Handle all cases:
  - `+,-,*,/,%` on `int` → `int`
  - `<,<=,>,>=` on `int` → `boolean`
  - `==,!=` on compatible types → `boolean`
  - `&&,||` on `boolean` → `boolean`
  - `+` with `String` → `String` (use `Badd_s`, insert `Ustring_of_int` if needed)
- [ ] `PEnew` → Find class, find constructor, check argument types
- [ ] `PEcall` → Type check receiver, find method, check argument types
- [ ] `PEcast` → Check types are compatible, return cast type
- [ ] `PEinstanceof` → Check receiver is class/null type, return `boolean`

### 1.6 Type Checking Statements

**Goal:** Implement `typecheck_stmt : env -> pstmt -> stmt * env`

Statements can extend the environment (variable declarations).

**Tasks:**

- [ ] `PSexpr` → Type check expression, return `Sexpr`
- [ ] `PSvar` → Check variable not already in scope, check type well-formed, optionally check initializer subtype, extend env
- [ ] `PSif` → Check condition is `boolean`, type check both branches (scopes don't leak)
- [ ] `PSfor` → Type check init/cond/update/body (scope of body doesn't leak)
- [ ] `PSblock` → Type check statements sequentially, threading env
- [ ] `PSreturn` → Record return type for later checking

### 1.7 Type Checking Methods & Constructors

**Goal:** Type check bodies with proper return checking.

**Tasks:**

- [ ] For constructors:
  - Build env with `this` + parameters
  - Type check body

- [ ] For methods:
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
- [ ] `Sreturn _` → true
- [ ] `Sif (_, s1, s2)` → `always_returns s1 && always_returns s2`
- [ ] `Sblock stmts` → any statement always returns
- [ ] `Sfor` → false (loop might not execute)
- [ ] `Sexpr`, `Svar` → false

---

## Phase 2: Code Generation (`compile.ml`)

The code generator transforms `Ast.tfile` into `X86_64.program`.

### 2.1 Setup & Data Structures

**Tasks:**
- [ ] Label generation: `let fresh_label prefix = ...`
- [ ] String literals table: collect all strings, generate labels
- [ ] Class descriptor labels: `"D_" ^ class_name`
- [ ] Method labels: `class_name ^ "_" ^ method_name`

### 2.2 Calculate Offsets

**Goal:** Assign memory offsets to attributes and methods.

**Attribute offsets (within object):**
```
Object layout: [class_descriptor_ptr | attr1 | attr2 | ... | attrN]
               offset 0               offset 8  offset 16  ...
```

**Tasks:**
- [ ] Process classes in inheritance order (parent before child)
- [ ] Each class inherits parent's attributes at same offsets
- [ ] New attributes get offsets after parent's last attribute
- [ ] Set `attr.attr_ofs` for each attribute

**Method offsets (within class descriptor):**
```
Descriptor: [parent_descriptor_ptr | method1_ptr | method2_ptr | ...]
            offset 0                offset 8       offset 16     ...
```

**Tasks:**
- [ ] Process classes in inheritance order
- [ ] Inherited methods keep same offset
- [ ] Overridden methods use parent's offset
- [ ] New methods get next available offset
- [ ] Set `meth.meth_ofs` for each method

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
- [ ] Generate `D_Object` descriptor (parent = 0)
- [ ] Generate `D_String` descriptor
- [ ] For each user class, generate descriptor with:
  - Pointer to parent descriptor
  - Method pointers (own or inherited)

### 2.4 Generate String Literals

**Tasks:**
- [ ] Collect all string constants from the program
- [ ] Generate labeled strings in `.data`:
  ```asm
  S_0:
      .quad D_String
      .string "hello\n"
  ```

### 2.5 Compile Expressions

**Goal:** `compile_expr : expr -> text`

Expressions leave their result in `%rax`.

**Tasks for each expression:**

- [ ] `Econstant (Cint n)` → `movq $n, %rax`
- [ ] `Econstant (Cbool b)` → `movq $0/%1, %rax`
- [ ] `Econstant (Cstring s)` → `leaq S_label, %rax`
- [ ] `Enull` → `movq $0, %rax`
- [ ] `Ethis` → `movq this_offset(%rbp), %rax`
- [ ] `Evar v` → `movq v.var_ofs(%rbp), %rax`
- [ ] `Eattr (e, attr)` → compile e, `movq attr.attr_ofs(%rax), %rax`
- [ ] `Eassign_var (v, e)` → compile e, `movq %rax, v.var_ofs(%rbp)`
- [ ] `Eassign_attr (e1, attr, e2)` → compile both, store
- [ ] `Eunop Uneg` → compile e, `negq %rax`
- [ ] `Eunop Unot` → compile e, `xorq $1, %rax`
- [ ] `Ebinop` → compile e1, push, compile e2, pop, operate
- [ ] `Enew` → call malloc, set descriptor, call constructor
- [ ] `Ecall` → push args, push this, indirect call via vtable
- [ ] `Ecast` → compile e, check at runtime (or skip if statically safe)
- [ ] `Einstanceof` → compile e, call runtime check routine
- [ ] `Eprint` → compile string arg, call printf wrapper

### 2.6 Compile Statements

**Goal:** `compile_stmt : stmt -> text`

**Tasks:**

- [ ] `Sexpr e` → compile_expr e (discard result)
- [ ] `Svar (v, e)` → compile e, store at `v.var_ofs(%rbp)`
- [ ] `Sif (e, s1, s2)` → compile condition, conditional jump, compile branches
- [ ] `Sfor (init, cond, update, body)` → compile with loop labels
- [ ] `Sblock stmts` → compile each statement
- [ ] `Sreturn None` → `leave; ret`
- [ ] `Sreturn (Some e)` → compile e, `leave; ret`

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
- [ ] Calculate stack frame size (locals + temporaries)
- [ ] Set `var.var_ofs` for parameters (positive offsets from %rbp)
- [ ] Set `var.var_ofs` for locals (negative offsets from %rbp)
- [ ] Generate prologue/epilogue
- [ ] Compile body

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

- [ ] **Stack-aligned wrappers** for libc functions:
  ```ocaml
  aligned_call_wrapper ~f:"malloc" ~newf:"my_malloc"
  aligned_call_wrapper ~f:"printf" ~newf:"my_printf"
  ```

- [ ] **Cast checking routine:**
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

- [ ] **Instanceof routine:**
  ```asm
  _instanceof:
      # Input: %rdi = object, %rsi = target class descriptor
      # Output: %rax = 0 or 1
      ...similar to cast but returns bool...
  ```

- [ ] **Null pointer check** (for method calls):
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

- [ ] Test type checker with `typing/good/*.java`
- [ ] Test type errors with `typing/bad/*.java`
- [ ] Verify parse errors with `syntax/bad/*.java`

### 3.2 Integration Testing

- [ ] Test code generation with `exec/*.java`
- [ ] Compare output with `.out` files
- [ ] Test runtime errors with `exec-fail/*.java`

### 3.3 Edge Cases

- [ ] Empty classes
- [ ] Deep inheritance chains
- [ ] Method overriding
- [ ] String concatenation with integers
- [ ] Null handling
- [ ] Cast failures
- [ ] Division by zero (if required)

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

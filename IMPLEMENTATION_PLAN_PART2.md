# Mini-Java Compiler - Part 2: Code Generation

## Overview

This plan details the implementation of code generation for the Mini-Java compiler.
The code generator transforms `Ast.tfile` (typed AST) into `X86_64.program`.

**Input:** `tfile = tclass list` where `tclass = class_ * decl list`
**Output:** `{ text: text; data: data }` - x86-64 assembly program

---

## Memory Layout

### Object Layout
```
┌─────────────────┬────────┬────────┬─────┬────────┐
│ descriptor_ptr  │ attr_1 │ attr_2 │ ... │ attr_n │
└─────────────────┴────────┴────────┴─────┴────────┘
   offset 0          8        16            8*(n)
```
- Each slot is 8 bytes (64-bit word)
- First word points to class descriptor
- Inherited attributes keep same offsets as parent

### String Layout
```
┌─────────────────┬──────────────────────────┐
│ descriptor_ptr  │ 0-terminated ASCII chars │
└─────────────────┴──────────────────────────┘
   offset 0          8 bytes onward (n+1 bytes for string of length n)
```

### Class Descriptor Layout
```
┌─────────────────┬──────────┬──────────┬─────┬──────────┐
│ parent_desc_ptr │ method_1 │ method_2 │ ... │ method_n │
└─────────────────┴──────────┴──────────┴─────┴──────────┘
   offset 0           8          16            8*(n)
```
- First word points to parent's descriptor (0 for Object)
- Method pointers in vtable order
- Overridden methods keep same offset as in parent

### Stack Frame Layout
```
     │ parameter n    │ +8*(n+1) from %rbp
     │ ...            │
     │ parameter 1    │ +24 from %rbp
     │ this           │ +16 from %rbp
     │ return address │ +8 from %rbp
     ├────────────────┤
%rbp │ saved %rbp     │ 0 from %rbp
     ├────────────────┤
     │ local var 1    │ -8 from %rbp
     │ local var 2    │ -16 from %rbp
     │ ...            │
     │ temporaries    │
%rsp │                │
     └────────────────┘
```

---

## Implementation Phases

### Phase 2.1: Infrastructure & Utilities

**Goal:** Set up helper functions and data structures for code generation.

```ocaml
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
```

**Tasks:**
- [ ] `fresh_label : string -> string` - generate unique labels
- [ ] String literal table and registration
- [ ] Class descriptor label: `"D_" ^ class_name`
- [ ] Method label: `class_name ^ "_" ^ method_name`
- [ ] Constructor label: `class_name ^ "_constructor"`

---

### Phase 2.2: Calculate Offsets

**Goal:** Assign `attr_ofs`, `meth_ofs`, and `var_ofs` values.

**Attribute Offsets:**
Process classes in topological order (parents before children).
```
Object: no attributes
String: no user attributes (just descriptor + string data)
Class A { int x; int y; }:
  x.attr_ofs = 8
  y.attr_ofs = 16
Class B extends A { int z; }:
  inherits x at 8, y at 16
  z.attr_ofs = 24
```

**Method Offsets:**
```
Object: no methods -> next_offset = 8
String: equals at offset 8 -> next_offset = 16
Class A { void f(); void g(); }:
  f.meth_ofs = 8
  g.meth_ofs = 16
Class B extends A { void f(); void h(); }:  // f overrides
  f.meth_ofs = 8  (same as parent!)
  g inherited at 16
  h.meth_ofs = 24 (new method)
```

**Tasks:**
- [ ] `compute_attr_offsets : tfile -> unit` - set `attr.attr_ofs` for all attributes
- [ ] `compute_method_offsets : tfile -> unit` - set `meth.meth_ofs` for all methods
- [ ] Track object size per class for `malloc` calls
- [ ] Track descriptor size per class

---

### Phase 2.3: Generate Class Descriptors (Data Section)

**Goal:** Generate `.data` section with class descriptors.

```asm
D_Object:
    .quad 0              # no parent
    # no methods

D_String:
    .quad D_Object       # parent is Object
    .quad String_equals  # equals method at offset 8

D_MyClass:
    .quad D_ParentClass
    .quad MyClass_method1   # or ParentClass_method1 if inherited
    .quad MyClass_method2
    ...
```

**Tasks:**
- [ ] Generate `D_Object` descriptor
- [ ] Generate `D_String` descriptor with `equals` method
- [ ] For each user class, generate descriptor with:
  - Parent descriptor pointer
  - Method table (own implementation or inherited)
- [ ] Helper to get method implementation label for a class

---

### Phase 2.4: Generate String Literals (Data Section)

**Goal:** Generate labeled string constants.

```asm
S_1:
    .quad D_String       # String descriptor pointer
    .string "hello\n"    # 0-terminated string content
```

**Tasks:**
- [ ] Collect all string literals during expression compilation
- [ ] Generate data section entries after compilation
- [ ] Each string is a "String object" with descriptor pointer

---

### Phase 2.5: Runtime Support Routines

**Goal:** Implement runtime helper routines in assembly.

**Required Routines:**

1. **Aligned libc wrappers** (use `aligned_call_wrapper`):
   - `my_malloc`, `my_printf`, `my_sprintf`, `my_strlen`
   - `my_strcmp`, `my_strcpy`, `my_strcat`, `my_exit`

2. **Null check:**
```asm
_check_null:
    testq %rdi, %rdi
    jz _null_error
    ret
_null_error:
    # print "error" and exit(1)
    leaq error_msg(%rip), %rdi
    xorq %rax, %rax
    call my_printf
    movq $1, %rdi
    call my_exit
```

3. **Cast check:**
```asm
_check_cast:
    # Input: %rdi = object, %rsi = target descriptor
    # Output: %rax = object (or exit with error)
    testq %rdi, %rdi
    jz .cast_null_ok        # null can be cast to anything
    movq (%rdi), %rax       # object's descriptor
.cast_loop:
    cmpq %rax, %rsi
    je .cast_ok
    movq (%rax), %rax       # parent descriptor
    testq %rax, %rax
    jnz .cast_loop
    # cast failed
    jmp _cast_error
.cast_null_ok:
.cast_ok:
    movq %rdi, %rax
    ret
```

4. **Instanceof check:**
```asm
_instanceof:
    # Input: %rdi = object, %rsi = target descriptor
    # Output: %rax = 0 or 1
    testq %rdi, %rdi
    jz .instanceof_false    # null instanceof X = false
    movq (%rdi), %rax       # object's descriptor
.instanceof_loop:
    cmpq %rax, %rsi
    je .instanceof_true
    movq (%rax), %rax       # parent descriptor
    testq %rax, %rax
    jnz .instanceof_loop
.instanceof_false:
    xorq %rax, %rax
    ret
.instanceof_true:
    movq $1, %rax
    ret
```

5. **String concatenation:**
```asm
_concat_strings:
    # Input: %rdi = string1 ptr, %rsi = string2 ptr
    # Output: %rax = new string ptr
    # Implementation: malloc(8 + len1 + len2 + 1), copy both strings
```

6. **Integer to string:**
```asm
_int_to_string:
    # Input: %rdi = integer
    # Output: %rax = new string ptr
    # Implementation: malloc buffer, sprintf("%ld", n)
```

**Tasks:**
- [ ] Generate aligned wrappers using `aligned_call_wrapper`
- [ ] Implement `_check_null` routine
- [ ] Implement `_check_cast` routine
- [ ] Implement `_instanceof` routine
- [ ] Implement `_concat_strings` routine
- [ ] Implement `_int_to_string` routine
- [ ] Generate error message strings in data section

---

### Phase 2.6: Compile Expressions

**Goal:** `compile_expr : expr -> text`

Convention: Result is left in `%rax`.

| Expression | Compilation |
|------------|-------------|
| `Econstant (Cint n)` | `movq $n, %rax` |
| `Econstant (Cbool true)` | `movq $1, %rax` |
| `Econstant (Cbool false)` | `movq $0, %rax` |
| `Econstant (Cstring s)` | `leaq S_label(%rip), %rax` |
| `Enull` | `xorq %rax, %rax` |
| `Ethis` | `movq 16(%rbp), %rax` |
| `Evar v` | `movq v.var_ofs(%rbp), %rax` |
| `Eattr (e, attr)` | compile e; null check; `movq attr.attr_ofs(%rax), %rax` |
| `Eassign_var (v, e)` | compile e; `movq %rax, v.var_ofs(%rbp)` |
| `Eassign_attr (e1, attr, e2)` | compile e2; push; compile e1; null check; pop; store |

**Binary Operations:**
```ocaml
(* General pattern for binop *)
compile_expr e1 ++
pushq !%rax ++
compile_expr e2 ++
popq rbx ++
(* now: rbx = e1, rax = e2 *)
(* perform operation, result in rax *)
```

| Binop | Code after pop |
|-------|----------------|
| `Badd` | `addq %rbx, %rax` |
| `Bsub` | `subq %rax, %rbx; movq %rbx, %rax` |
| `Bmul` | `imulq %rbx, %rax` |
| `Bdiv` | `movq %rbx, %rax; cqto; idivq %rcx` (with div-by-zero check) |
| `Bmod` | similar, result in `%rdx` |
| `Blt/Ble/Bgt/Bge` | `cmpq; setl/setle/setg/setge %al; movzbq %al, %rax` |
| `Beq/Bneq` | `cmpq; sete/setne %al; movzbq %al, %rax` |
| `Band` | short-circuit evaluation |
| `Bor` | short-circuit evaluation |
| `Badd_s` | call `_concat_strings` |

**Short-circuit && and ||:**
```ocaml
(* e1 && e2 *)
let lbl_false = fresh_label "and_false" in
let lbl_end = fresh_label "and_end" in
compile_expr e1 ++
testq !%rax !%rax ++
jz lbl_false ++
compile_expr e2 ++
jmp lbl_end ++
label lbl_false ++
xorq !%rax !%rax ++
label lbl_end
```

**Method Call `Ecall (e, meth, args)`:**
```
1. Compile and push arguments right-to-left
2. Compile receiver e, push as 'this'
3. Null check on 'this'
4. Get descriptor from object: movq (%rax), %rcx
5. Get method from vtable: movq meth.meth_ofs(%rcx), %rcx
6. call *%rcx
7. Pop arguments + this: addq $(8*(n+1)), %rsp
```

**Object Creation `Enew (cls, args)`:**
```
1. Call malloc(object_size)
2. Save pointer, set descriptor: movq $D_Class, (%rax)
3. Initialize all attributes to 0
4. Push arguments for constructor
5. Push 'this'
6. Call constructor
7. Pop arguments
8. Return saved object pointer
```

**Tasks:**
- [ ] Implement `compile_expr` for all expression variants
- [ ] Handle division by zero check
- [ ] Implement short-circuit evaluation for && and ||
- [ ] Implement string operations (concat, int-to-string conversion)
- [ ] Implement method calls with vtable dispatch
- [ ] Implement object construction
- [ ] Implement cast and instanceof

---

### Phase 2.7: Compile Statements

**Goal:** `compile_stmt : stmt -> text`

| Statement | Compilation |
|-----------|-------------|
| `Sexpr e` | `compile_expr e` (discard result) |
| `Svar (v, init)` | `compile_expr init; movq %rax, v.var_ofs(%rbp)` |
| `Sreturn None` | `leave; ret` |
| `Sreturn (Some e)` | `compile_expr e; leave; ret` |
| `Sblock stmts` | compile each statement in sequence |

**If Statement:**
```ocaml
let lbl_else = fresh_label "else" in
let lbl_end = fresh_label "endif" in
compile_expr cond ++
testq !%rax !%rax ++
jz lbl_else ++
compile_stmt then_branch ++
jmp lbl_end ++
label lbl_else ++
compile_stmt else_branch ++
label lbl_end
```

**For Loop `Sfor (init, cond, update, body)`:**
```ocaml
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
```

**Tasks:**
- [ ] Implement `compile_stmt` for all statement variants
- [ ] Handle control flow correctly

---

### Phase 2.8: Compile Methods & Constructors

**Goal:** Generate code for methods and constructors.

**Stack Frame Setup:**
1. Count local variables in body (traverse statements)
2. Assign offsets to parameters: `this` at +16, param1 at +24, param2 at +32, ...
3. Assign offsets to locals: local1 at -8, local2 at -16, ...

**Method Template:**
```asm
ClassName_methodName:
    pushq %rbp
    movq %rsp, %rbp
    subq $locals_size, %rsp   # allocate locals (must be multiple of 16)
    ... compile body ...
    # implicit return for void methods
    leave
    ret
```

**Constructor Template:**
```asm
ClassName_constructor:
    pushq %rbp
    movq %rsp, %rbp
    subq $locals_size, %rsp
    # 'this' is at 16(%rbp), already allocated by caller
    ... compile body ...
    leave
    ret
```

**Tasks:**
- [ ] `count_locals : stmt -> int` - count local variable declarations
- [ ] `assign_param_offsets : var list -> unit` - set var_ofs for parameters
- [ ] `assign_local_offsets : stmt -> int -> int` - set var_ofs for locals, return count
- [ ] `compile_method : class_ -> method_ -> stmt -> text`
- [ ] `compile_constructor : class_ -> var list -> stmt -> text`

---

### Phase 2.9: Main Entry Point

**Goal:** Generate the `main` function that runs `Main.main()`.

The `Main` class is special - it has a static `main` method with no `this`.

```asm
    .globl main
main:
    pushq %rbp
    movq %rsp, %rbp
    subq $locals_size, %rsp
    ... compile Main.main body ...
    xorq %rax, %rax          # return 0
    leave
    ret
```

**Note:** Main.main has no `this` and no constructor call.

**Tasks:**
- [ ] Find Main class and its main method body
- [ ] Compile main body without `this` reference
- [ ] Ensure exit code 0 on success

---

### Phase 2.10: Putting It All Together

**Goal:** Implement `Compile.file : tfile -> program`

```ocaml
let file ?debug:(b=false) (tfile: tfile) : program =
  debug := b;
  (* 1. Compute offsets *)
  compute_all_offsets tfile;

  (* 2. Generate runtime support *)
  let runtime_text = generate_runtime () in
  let runtime_data = generate_runtime_data () in

  (* 3. Generate class descriptors *)
  let descriptors = generate_descriptors tfile in

  (* 4. Compile all methods and constructors *)
  let methods_text = compile_all_methods tfile in

  (* 5. Compile Main.main *)
  let main_text = compile_main tfile in

  (* 6. Generate string literals *)
  let strings_data = generate_string_literals () in

  { text = runtime_text ++ main_text ++ methods_text;
    data = runtime_data ++ descriptors ++ strings_data }
```

---

## Implementation Order (Suggested)

### Step 1: Minimal Working Compiler
1. Infrastructure (labels, string table)
2. Main function compilation (just Main.main body)
3. Basic expressions: constants, variables, arithmetic
4. Basic statements: Sexpr, Svar, Sblock, Sreturn
5. Print statement (critical for testing!)

**Test with:** `arith-int.java`, basic print programs

### Step 2: Control Flow
1. If/else statements
2. For loops
3. Short-circuit && and ||
4. Comparison operators

**Test with:** `arith-bool*.java`, `for*.java`, `while.java`

### Step 3: Objects (No Inheritance)
1. Attribute and method offset computation
2. Class descriptors (single class)
3. Object allocation (`new`)
4. Attribute access
5. Method calls (simple dispatch)
6. Constructors

**Test with:** `call1.java`, `call2.java`, `constr*.java`

### Step 4: Inheritance & Polymorphism
1. Inherited attributes/methods with correct offsets
2. Class descriptor chains
3. Virtual method dispatch
4. Cast and instanceof

**Test with:** `dispatch*.java`, `instanceof.java`, `cast*.java`

### Step 5: Strings
1. String literals in data section
2. String concatenation
3. Integer to string conversion
4. String.equals method

**Test with:** `string*.java`, `big_string.java`

### Step 6: Runtime Errors
1. Null pointer check
2. Division by zero check
3. Cast failure

**Test with:** `exec-fail/*.java`

---

## Key Considerations

### Null Safety
- Check for null before:
  - Attribute access (`e.x`)
  - Method call (`e.m()`)
  - Attribute assignment (`e.x = ...`)

### Division by Zero
- Check divisor before `idivq`
- Print error and exit(1) if zero

### Stack Alignment
- Before calling libc functions, stack must be 16-byte aligned
- Use `aligned_call_wrapper` for all libc calls

### Callee-Saved Registers
- If using `%rbx`, `%r12`-`%r15`, save/restore them
- `%rax`, `%rcx`, `%rdx`, `%rsi`, `%rdi`, `%r8`-`%r11` are caller-saved

### Object Initialization
- All attributes initialized to 0 (works for int, boolean, null pointers)

---

## Testing Strategy

```bash
# Run Part 3 tests (code generation)
cd tests && bash ../test -3 ../mini-java-ocaml/_build/default/minijava.exe

# Test individual file
../mini-java-ocaml/_build/default/minijava.exe file.java
gcc -no-pie file.s -o file
./file
```

Expected: All `exec/*.java` produce correct output, all `exec-fail/*.java` exit with code 1.

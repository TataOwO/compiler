# Mini-Java Compiler Report

## Project Overview

This project implements a compiler for Mini-Java, a subset of Java, targeting x86-64 assembly. The compiler consists of two main phases:

1. **Type Checking** (`typing.ml`) - Transforms parsed AST into typed AST
2. **Code Generation** (`compile.ml`) - Transforms typed AST into x86-64 assembly

All 72 test cases pass with 100% success rate on compilation, code generation, and runtime behavior.

---

## Technical Choices

### Type Checking (`typing.ml`)

#### Multi-Pass Architecture
The type checker uses a three-pass approach as recommended in the specification:

1. **Pass 1: Collect Classes** - Registers all class names, checks uniqueness, and verifies reserved names (`Object`, `String`) are not redefined.

2. **Pass 2: Build Inheritance & Collect Members** - Resolves `extends` relationships, detects inheritance cycles using DFS, and collects attributes/methods for each class.

3. **Pass 3: Type Check Bodies** - Type checks constructor and method bodies with proper environment handling.

#### Key Data Structures
- `class_table`: Global hashtable mapping class names to `class_` records
- `class_constructors`: Stores constructor parameter lists for each class
- Typing environment (`env`): Contains local variables, current class reference, and return type

#### Subtyping Implementation
- Primitive types (`int`, `boolean`) are only subtypes of themselves
- Class types use the inheritance chain (`C1 → C2`)
- `typenull` is a subtype of any class type
- Type compatibility (`τ1 ≡ τ2`) checks if either is a subtype of the other

### Code Generation (`compile.ml`)

#### Memory Layout

**Objects:**
```
[descriptor_ptr | attr1 | attr2 | ... | attrN]
 offset 0        offset 8  offset 16  ...
```

**Class Descriptors:**
```
[parent_desc_ptr | method1_ptr | method2_ptr | ...]
 offset 0         offset 8       offset 16     ...
```

**Strings:**
```
[descriptor_ptr | null-terminated string bytes]
 offset 0        offset 8
```

#### Stack Frame Layout
```
parameter n      (higher addresses)
...
parameter 1
this
return address
saved %rbp       <- %rbp points here
local variables
temporary values <- %rsp points here (lower addresses)
```

- `this` is at offset `+16` from `%rbp`
- Parameters are at offsets `+24`, `+32`, etc.
- Local variables are at negative offsets from `%rbp`

#### Offset Computation
Classes are processed in **topological order** (parents before children) to ensure:
- Attribute offsets are inherited correctly
- Method offsets in vtables maintain the prefix property for virtual dispatch

#### Virtual Method Dispatch
Method calls use indirect calls through the vtable:
1. Load object's descriptor pointer
2. Load method pointer from descriptor at `meth.meth_ofs`
3. Call through register (`call *%rcx`)

#### Runtime Support
Implemented in assembly and concatenated to generated code:
- `_check_null`: Null pointer check for method calls
- `_check_cast`: Runtime cast verification walking the inheritance chain
- `_instanceof`: Returns 0/1 based on type hierarchy check
- `_concat_strings`: String concatenation allocating new string
- `_int_to_string`: Integer to string conversion using `sprintf`
- Stack-aligned wrappers (`my_malloc`, `my_printf`, etc.) for libc calls

---

## Challenges and Solutions

### 1. Parameter/Variable Offset Sharing
**Problem:** The typing phase created duplicate `var` objects - one set stored in method records and another for the body environment. This caused parameter offsets set during compilation to not affect variables used in the body.

**Solution:** Modified `typing.ml` to reuse the same `var` objects from pass 1 when type-checking bodies in pass 2.

### 2. Argument Push Order
**Problem:** Arguments were pushed in the wrong order, causing parameter values to be swapped in method/constructor calls.

**Solution:** Reverse the argument list before pushing so the first argument ends up at the lowest stack offset (closest to `this`).

### 3. Class Processing Order
**Problem:** Method offsets were computed incorrectly when child classes were processed before their parents.

**Solution:** Added `sort_by_depth` function to sort classes by inheritance depth, ensuring parents are always processed first.

### 4. Void Return Exit Code
**Problem:** `return;` statements in `main` left garbage in `%rax`, causing non-zero exit codes.

**Solution:** Set `%rax` to 0 for all void returns (`Sreturn None`).

### 5. Caller-Saved Register Clobbering
**Problem:** After calling `_check_null`, the object pointer in `%rax` was clobbered.

**Solution:** Push object to stack before null check, reload from stack after the call.

---

## Testing Results

| Test Suite | Result |
|------------|--------|
| Syntax (Part 1) | 100% |
| Type Checking (Part 2) | 100% |
| Code Generation (Part 3) - Compilation | 72/72 (100%) |
| Code Generation (Part 3) - Generated Code | 72/72 (100%) |
| Code Generation (Part 3) - Code Behavior | 72/72 (100%) |

---

## Files Structure

```
mini-java-ocaml/
├── Makefile        # Builds minijava and minijava.exe
├── ast.ml          # AST definitions (provided)
├── lexer.mll       # Lexer (provided)
├── parser.mly      # Parser (provided)
├── typing.ml       # Type checker implementation
├── compile.ml      # Code generator implementation
├── x86_64.ml/mli   # x86-64 assembly library (provided)
└── minijava.ml     # Main driver (provided)
```

---

## Usage

```bash
# Build the compiler
make

# Compile a Mini-Java program
./minijava program.java        # Produces program.s

# Type check only
./minijava --type-only program.java

# Parse only
./minijava --parse-only program.java

# Assemble and run
gcc -no-pie program.s -o program
./program
```

---

## Known Limitations

- No garbage collection (memory allocated with `malloc` is never freed)
- No register allocation optimization (all intermediate values use the stack)
- Arrays are not supported (not part of Mini-Java specification)

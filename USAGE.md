# Mini-Java Compiler - Usage Guide

## Building the Compiler

```bash
cd mini-java-ocaml
make
```

This produces:
- `minijava.exe` - The compiler executable
- `minijava` - Symlink to `minijava.exe`

## Using the Compiler

### Basic Compilation
```bash
./minijava program.java    # Produces program.s
```

### Command Line Options
```bash
./minijava --parse-only program.java   # Parse only, exit 0 if successful
./minijava --type-only program.java    # Type check only, exit 0 if successful
./minijava program.java                # Full compilation to assembly
```

### Running Compiled Programs
```bash
./minijava program.java           # Produces program.s
gcc -no-pie program.s -o program  # Assemble and link
./program                         # Run the program
```

---

## Running the Test Suite

### Prerequisites

The test script requires Unix line endings. **If you're on Windows/WSL**, you may encounter errors like:
```
$'\r': command not found
```

### Fix: Convert Line Endings (WSL/Windows)

**Option 1: Using dos2unix**
```bash
sudo apt install dos2unix
dos2unix tests/test
```

**Option 2: Using sed**
```bash
sed -i 's/\r$//' tests/test
```

**Option 3: Using tr**
```bash
tr -d '\r' < tests/test > tests/test.tmp && mv tests/test.tmp tests/test
chmod +x tests/test
```

### Running Tests

```bash
cd tests

# Run all tests
./test -all ../mini-java-ocaml/minijava.exe

# Run specific parts
./test -1 ../mini-java-ocaml/minijava.exe    # Syntax tests only
./test -2 ../mini-java-ocaml/minijava.exe    # Type checking tests only
./test -3 ../mini-java-ocaml/minijava.exe    # Code generation tests only
```

### Expected Output
```
Part 1:
...

Part 2:
...

Part 3:
Compilation : 72/72 : 100%
Generated Code : 72/72 : 100%
Code Behavior : 72/72 : 100%
```

---

## Troubleshooting

### Error: `$'\r': command not found`
**Cause:** Windows line endings (CRLF) in shell scripts
**Fix:** Run `dos2unix tests/test` or `sed -i 's/\r$//' tests/test`

### Error: Tests fail with "bad output" (WSL/Windows)
**Cause:** The `.out` expected output files have Windows line endings (CRLF), but our compiler produces Unix line endings (LF). The `cmp` comparison fails due to this mismatch.
**Fix:** Convert all test files to Unix line endings:
```bash
# Install dos2unix if needed
sudo apt install dos2unix

# Convert all test-related files
dos2unix tests/test
find tests -name "*.out" -exec dos2unix {} \;
```

Or using sed:
```bash
sed -i 's/\r$//' tests/test
find tests -name "*.out" -exec sed -i 's/\r$//' {} \;
```

### Error: `/usr/bin/ld: warning: missing .note.GNU-stack section`
**Cause:** Normal warning from linker, can be safely ignored
**Fix:** Not required, program runs correctly

### Error: `gcc: command not found`
**Cause:** GCC not installed
**Fix:** `sudo apt install gcc` (Ubuntu/Debian) or `sudo yum install gcc` (RHEL/Fedora)

### Error: `dune: command not found`
**Cause:** OCaml/dune not installed
**Fix:** Install OPAM and dune:
```bash
sudo apt install opam
opam init
opam install dune menhir
eval $(opam env)
```

---

## Git Line Ending Configuration (Recommended for Teams)

To prevent CRLF issues in the future, configure git:

```bash
# On Linux/Mac/WSL
git config --global core.autocrlf input

# On Windows (if not using WSL)
git config --global core.autocrlf true
```

Or add a `.gitattributes` file to the repository:
```
* text=auto
*.sh text eol=lf
tests/test text eol=lf
```

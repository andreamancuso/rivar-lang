# Rivar

**Rivar** is a minimal, contract-first, class-based programming language inspired by Eiffel.

It is being built from scratch in OCaml, designed to bring native **Design by Contract (DbC)** to the modern developer's toolbox.

---

## Quick Start

### Prerequisites

- OCaml (recommended via [opam](https://opam.ocaml.org))
- `dune`
- `menhir`

```bash
opam install dune menhir
```

### Build

```bash
dune build
```

### Run

- Generate C code from Rivar source: `./_build/default/src/main.exe greeter.rivar`
- Compile generated C code: `gcc -std=c99 -o test test-greeter.c ../out.c -lgc`


### Example

```rivar
class ACCOUNT

feature
    balance: INTEGER

    deposit(amount: INTEGER)
        require amount > 0
        do
            balance := balance + amount
        ensure
            balance = old balance + amount
        end

end
```

---

## Project Structure

```
rivar/
├── src/
│   ├── ast.ml          # Core AST types
│   ├── lexer.mll       # OCamllex lexer
│   ├── parser.mly      # Menhir parser
│   ├── typecheck.ml    # Type checker (WIP)
│   ├── codegen.ml      # C code generation
│   └── main.ml         # CLI entry point
├── runtime/
│   └── runtime.c       # Contract violation handler
└── account.rivar       # Sample test file
```

---

## Philosophy

Rivar isn't about speed or syntax gimmicks. It's about **truth** in code.  
Contracts aren't optional—they are the code.

---

## License

MIT



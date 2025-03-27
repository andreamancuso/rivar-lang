# Rivar

**Rivar** is a minimal, contract-first, class-based programming language inspired by Eiffel.

It is being built from scratch in OCaml, designed to bring native **Design by Contract (DbC)** to the modern developer's toolbox.

## Why Rivar?

Rivar is a minimal, principled programming language rooted in Design by Contract.

> Its name evokes the image of a river shaped by its banks—
> fluid, powerful, and guided with purpose.
> In Rivar, code flows clearly within the boundaries of contracts,
> ensuring correctness by design and elegance in execution.

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

- Generate C code from Rivar source: `./_build/default/src/main.exe account.rivar`
- Compile generated C code: `gcc -std=c99 -o account account.c -I. librivar.c`


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



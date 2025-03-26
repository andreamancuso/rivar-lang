# Rivar

gcc -std=c99 -shared -fPIC -o librivar.so out.c


**Rivar** is a minimal, contract-first, class-based programming language inspired by Eiffel.  
It is being built from scratch in OCaml with LLVM as the backend, designed to bring native **Design by Contract (DbC)** to the modern developer’s toolbox.

> _"Write what must be true. Let the compiler enforce the rest."_

---

## Features (WIP)

- Class definitions
- Typed fields (`INTEGER`, `BOOLEAN`)
- Parsing with Menhir + OCamllex
- AST-based architecture
- Contract support (`require`, `ensure`, `invariant`)
- LLVM IR codegen
- Native compilation and runtime enforcement of contracts

---

## Quick Start

### Prerequisites

- OCaml (recommended via [opam](https://opam.ocaml.org))
- `dune`
- `menhir`
- LLVM (tested with LLVM 13+)

```bash
opam install dune menhir llvm
```

### Build

```bash
dune build
```

### Run

```bash
./_build/default/src/main.exe account.rivar
```

### Example

```rivar
class ACCOUNT

feature
    balance: INTEGER

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
│   ├── codegen.ml      # LLVM IR generation (WIP)
│   └── main.ml         # CLI entry point
├── runtime/
│   └── runtime.c       # Contract violation handler
└── account.rivar       # Sample test file
```

---

## Philosophy

Rivar isn’t about speed or syntax gimmicks. It’s about **truth** in code.  
Contracts aren’t optional—they are the code.

---

## Coming Soon

- Full method parsing with `require`/`do`/`ensure`
- Control flow + expressions
- Invariant support
- LLVM IR emission
- Self-hosted examples

---

## License

MIT



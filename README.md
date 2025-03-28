# Rivar

**Rivar** is a minimal, contract-first, class-based, [garbage-collected](https://en.wikipedia.org/wiki/Boehm_garbage_collector) programming language inspired by Eiffel.

It is being [built from scratch in OCaml](https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html), designed to bring native **Design by Contract (DbC)** to the modern developer's toolbox.

---

## Quick Start

So far this has been tested only on Ubuntu 24.04.

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

#### Install Boehm GC

```bash
sudo apt-get install libgc-dev
```

#### Compile and Test

- Generate C code from Rivar source:
  ```bash
  ./_build/default/src/main.exe examples/greeter.rivar
  ```
- Compile generated C code with Boehm GC:
  ```bash
  gcc -std=c99 -o test examples/test-greeter.c out.c -lgc
  ```

---

## Example

```rivar
class GREETER

feature
    name: STRING

    set_name(new_name: STRING)
        require new_name != ""
        do
            this->name = new_name
        ensure
            this->name != old this->name
        end

    greet()
        require this->name != ""
        do
            print(this->name)
        ensure
            result == true
        end

end
```

---

## Project Structure

```
rivar/
├── src/
│   ├── ast.ml             # Core [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) definitions
│   ├── ir.ml              # IR types ([Intermediate Representation](https://en.wikipedia.org/wiki/Intermediate_representation))
│   ├── ir.mli             # IR interface
│   ├── irgen.ml           # AST -> IR lowering
│   ├── lexer.mll          # OCamllex lexer
│   ├── parser.mly         # Menhir parser
│   ├── codegen.ml         # IR -> C code generation
│   ├── typecheck.ml       # Type checker (WIP)
│   └── main.ml            # CLI compiler entry point (`rivarc`)
├── examples/
│   ├── account.rivar      # Sample Rivar class
│   ├── greeter.rivar      # Sample Rivar class with strings
│   ├── test-account.c     # Test harness for `account.rivar`
│   └── test-greeter.c     # Test harness for `greeter.rivar`
├── out.c                  # Generated C output (from `rivarc`)
├── out.h                  # Generated C header
├── dune-project           # Dune project file
├── .gitignore             # Ignores compiled artifacts and test binaries
└── README.md              # You're looking at it
```

---

## Philosophy

Rivar isn't about speed or syntax gimmicks. It's about **truth** in code.  
Contracts aren't optional: they are the code.

---

## License

MIT


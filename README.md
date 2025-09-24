# Peano Compiler (bplang_pest)

Peano is an experimental, statically typed programming language and compiler implemented in Rust. The project currently focuses on establishing a practical end-to-end toolchain: parsing a Rust and Haskell-inspired surface syntax, performing semantic analysis, and emitting LLVM IR that can be linked into native executables. The repository includes an embryonic standard library and an extensive roadmap that tracks the march toward a self-hosting compiler.

## Quick Start

```bash
cp -n .env.example .env   # optional: configure LLVM installation paths
make setup                 # mark helper scripts executable
make build                 # cargo build
make run -- simple_test.pn # compile & run the sample program (defaults to src.pn)
make test                  # run the compiler test suite (clang required for codegen tests)
```

All helper scripts (`scripts/*.sh`) automatically source `.env` if it exists, so you can point the build at a custom LLVM installation (see `DEVELOPMENT.md` for details).

## Toolchain Requirements

- **Rust toolchain** — `rustup`, `cargo`, and the Rust 2021 edition.
- **LLVM 18** — exposed through `inkwell` (`llvm18-1` feature). Configure `LLVM_SYS_181_PREFIX` in `.env` if LLVM is in a custom location.
- **Clang** — used by `src/main.rs`, tests, and scripts to link generated object files.

## Language at a Glance

```pn
# functions use `name :: (params) => expr_or_block`
main :: () => {
    message := "Hello, Peano!"
    println(message)

    pair := (40, 2)
    total := match pair { (a, b) => a + b }
    println(total)  # prints 42

    Point :: struct { x: i64, y: i64 }
    origin := { x: 0, y: 0 }

    Result :: enum {
        Ok: i64,
        Err: string,
    }

    impl Result for Result {
        to_string :: (self: &Result) -> string => {
            match self {
                Result_Ok(value) => "ok";
                Result_Err(msg) => msg;
            }
        }
    }
}
```

Key surface syntax features visible in the current implementation include:

- **Pattern matching** with tuples, enums, and range expressions (`src/parser/mod.rs`, `src/ast.rs`).
- **Structs, enums, and traits** with associated types and method declarations.
- **Pointers, optional (`?T`), and result (`!T`) types** alongside first-class function types.
- **Matrix literals and slices** (prototype) for numerical experimentation.
- **Module & import system** via `mod name;` and `use foo::bar`, expanded recursively by `expand_modules` in `src/main.rs`.

## Architecture Overview

- **Parser** — Pest grammar (`src/parser/grammar.pest`) produces a rich AST (`src/ast.rs`).
- **Semantic analysis** — `src/semantic.rs` performs multi-pass symbol collection, scope management, trait/impl validation, and type checking with descriptive diagnostics.
- **Code generation** — `src/codegen.rs` lowers the checked AST into LLVM IR using `inkwell`, defines helper intrinsics (e.g., `println`, `slice_*`), and can emit object files or a freestanding `_start` entrypoint.
- **Driver** — `src/main.rs` wires the stages together, links the resulting object with `clang`, and executes the produced binary.
- **Standard library** — Minimal runtime utilities live in `stdlib/` (see `stdlib/prelude.pn`).
- **Tests** — `tests/` contains parser, semantic, and full codegen integration tests that compile snippets, link them with clang, and assert on the resulting executable output (see `tests/run_tuple_patterns.rs` for an example).

## Repository Layout

- **`src/`** — Core compiler crates (`ast`, `parser`, `semantic`, `codegen`, `main`).
- **`stdlib/`** — Early draft of the Peano prelude and runtime helpers.
- **`tests/`** — Rust-based integration tests covering language features and codegen.
- **`scripts/`** — Convenience wrappers for build, run, test, and fmt tasks.
- **`plans/`** — Roadmaps broken down by feature area (syntax, traits, concurrency, I/O, etc.).
- **`simple_test.pn`, `src.pn`** — Sample Peano programs for experimentation.

## Current Status & Roadmap

The compiler successfully parses and type-checks a sizable subset of the language, including tuples, structs, enums, traits, loops, and basic pattern matching, and can generate runnable executables through LLVM. Major upcoming milestones are tracked in `next_steps.md` and the detailed `plans/` hierarchy. Highlights from the near-term roadmap include:

- **Language completeness** — character literals, richer integer semantics, tuple pattern exhaustiveness, generic parameters, and async lowering.
- **Trait system** — derive support, trait objects, standard trait implementations, and trait-bound diagnostics.
- **Runtime & stdlib** — collections (`Vec`, hash maps), IO abstractions, error handling ergonomics, and logging.
- **Concurrency** — threads, atomics, channels, and async executor work.

Refer to `next_steps.md` for granular progress tracking and pseudocode sketches of the intended implementations.

## Developing & Testing

```bash
cargo fmt                 # formatting
cargo clippy --all-targets # linting (optional, gated by LLVM availability)
cargo test                # runs unit + integration tests
```

Many tests require `clang` and a linkable LLVM toolchain. If you lack clang locally, you can still run parser/semantic tests by filtering:

```bash
cargo test parse_ -- --ignored
```

## Contributing & Feedback

This repository is under active development and evolving rapidly. Contributions are welcome—open issues or discussion threads describing desired language features, compiler improvements, or documentation gaps. See the roadmap files in `plans/` to understand ongoing priorities before proposing large changes.

_The project has not yet declared a license; consult the repository maintainers before redistributing substantial portions of the code._

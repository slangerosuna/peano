# Peano Language and Standard Library Requirements

Legend:
- [x] Implemented
- [~] Partial / prototype
- [ ] Not yet

This document tracks the foundations and expected batteries for the Peano language and its standard library/runtime. Peano aims for Rust-ish core semantics with first-class ECS10. Math helpers in stdlib. [x]
  - Notes: Added rem_i64(a,b), abs_i64(x), min_i64(a,b), max_i64(x) to prelude; tests run_math_rem.rs and run_math_abs_min_max.rs pass.
11. Add match destructuring for enum payloads (bind payload variables in patterns). [x]
  - Notes: Implemented in semantic analysis (bind variables) and codegen (load payload into variables); test run_enum_match_destructure.rs passes.
11. Add match destructuring for enum payloads (bind payload variables in patterns). [ ]GPU, Web, and Matrix/Numerics capabilities—not bolted-on libraries.

## Positioning and pillars
- See plan: [Positioning and pillars](plans/positioning-and-pillars/plan.md)
- Rust-ish core: expressions, traits, strong static types, zero-cost abstractions.
- First-class domains: ECS (in-memory RDBMS), GPU/compute shaders, Web/Networking, and Matrices/Linear Algebra.
- Practical standard traits and utilities, with clear panic/error semantics.

---

## Tier 1: Core language semantics (Rust-ish)
- See plan: [Tier 1 core language semantics](plans/tier-1-core-language-semantics/plan.md)

- Types and memory
  - [~] Signed/unsigned integers (i8 … i64, u8 … u64)
    - Notes: i32/i64 fully supported; unsigned lower as i64 today. Literal constants now leverage suffix metadata during codegen, fixing range loop lowering (`tests/run_integer_widths.rs`). See plan: [Tier 1 core language semantics](plans/tier-1-core-language-semantics/plan.md).
  - [~] Floating-point (f32, f64)
  - [x] bool
  - [x] char (Unicode scalar/code unit)
    - Notes: Parser, semantics, and codegen now recognize `'c'` literals (with escapes/Unicode). Test `tests/run_char_literal.rs` exercises println + variable usage.
  - [~] Arrays (fixed-size)
    - Notes: Vector/matrix literals lower to stack arrays and are iterable.
  - [~] Slices/views (pointer + length)
    - Notes: Built-in `slice_i64` with for-loop iteration and helpers (`slice_len`, `slice_is_empty`, `slice_get`).
  - [~] Strings (UTF-8 by default)
    - Notes: literals, printing; len/streq/contains/starts_with/ends_with.
  - [~] Tuples and algebraic data types
    - Notes: Enums available; variant static-path references (Type::Variant) lower to i64 tag with deterministic order. Tuples pending.
  - [~] Safe allocation/deallocation wrappers
    - Notes: externs for `malloc`/`free`; higher-level safety pending.
  - [ ] Generics

- Traits and generics
  - [x] Traits / interfaces (impls, static dispatch)
  - [ ] Trait Objects (dynamic dispatch via V-Tables)
  - [ ] Generics for types/functions
  - [ ] Associated types with trait-bounded generics

- Control flow and expressions
  - [x] Loops, ranges (exclusive upper), if/else
  - [~] Pattern matching/switch
  - [ ] Async/await (language-level)

---

## Standard traits (first-class)
- See plan: [Standard traits](plans/standard-traits/plan.md)
- Equality and ordering
  - [~] Eq/PartialEq
  - [~] Ord/PartialOrd
  - [ ] Hash
- Conversion and defaults
  - [ ] From/Into
  - [ ] AsRef/AsMut
  - [ ] Default
  - [ ] Clone/Copy
- Formatting and debugging
  - [~] Display
- Iteration and collections
  - [~] IntoIterator/Iterator
  - [ ] Borrow/BorrowMut
- Error handling
  - [ ] Error (trait)
- [ ] `!`/`?` ergonomics (pattern-matching helpers)

Notes: Today equality/ordering exist for numeric expressions. Display via println works for core scalars/strings; trait-based formatting is pending.

---

{{ ... }}
## Runtime, errors, and panics
- See plan: [Runtime, errors, and panics](plans/runtime-errors-and-panics/plan.md)
- Error handling
  - [ ] `!T` propagation ergonomics
  - [ ] Error trait and downcasting
- Panics
  - [x] panic(message) intrinsic
    - Notes: Implemented as stdlib function printing "Assertion failed: " + message and calling exit(1).
  - [x] assert!/assert_eq! macros (or equivalents)
    - Notes: assert(cond, msg) and assert_eq_i64(a, b, msg) in stdlib; tests pass.
  - [ ] Backtraces (opt-in), panic hook
  - [ ] Unwind vs abort semantics (configurable per build profile)

---

## Core utilities (batteries)
- See plan: [Core utilities](plans/core-utilities/plan.md)
- Strings and text
  - [~] UTF-8 strings: len, compare, contains, prefix/suffix, find
  - [ ] Builders/rope, encoding conversions
- Collections
  - [ ] Dynamic arrays (Vec)
  - [ ] Maps/Sets (hash, ordered)
  - [ ] Queues/stacks/deques
- Slices and arrays
  - [~] slice_i64: len, is_empty, get, for-loop iteration
  - [ ] Generic slices, constructors, iter methods
- Time and randomness
  - [ ] Monotonic/Wall clocks, sleep
  - [ ] PRNG utilities
- IO and FS
  - [ ] Files: open/read/write/seek/remove/metadata
  - [ ] Paths
- Process and environment
  - [~] exit(code)
  - [ ] spawn/wait/kill, env, args
- Math
  - [~] Basic integer math
    - Notes: Modulo operator not implemented; prelude provides rem_i64(a,b) as a stopgap.
  - [ ] Float math: pow, sqrt, trig, log
- Diagnostics and devx
  - [ ] Logging framework
  - [ ] Assertions and debugging helpers
  - [ ] Testing framework (in-language unit tests)

---

## Tier 1: First-class domains
- See plan: [Tier 1 first-class domains](plans/tier-1-first-class-domains/plan.md)

### ECS / In-memory RDBMS
- Data model
  - [ ] Components (structs/enums) and resources
  - [ ] Columnar storage with tight memory layout
  - [ ] O(1) entity access; stable IDs/handles
- Query & systems
  - [ ] Declarative queries with borrow safety (mutable/exclusive and immutable/shared)
  - [ ] System scheduling with conflict detection and parallel execution
  - [ ] Cross-thread safety guarantees for components/resources
- Async/task integration
  - [ ] Async systems execution model

### GPU / Graphics / Compute
- Shaders and pipelines
  - [ ] Peano-to-SPIR-V subset for vertex/fragment/compute
  - [ ] Strongly-typed bindings (UBO/SSBO), descriptors, pipeline layouts
- Runtime
  - [ ] Vulkan/portable abstraction (creation, queues, command buffers)
  - [ ] Resource lifecycle: buffers, images, samplers
  - [ ] Synchronization primitives and barriers
- Interop
  - [ ] Matrix/Vector types mapped to GPU layouts
  - [ ] CPU-GPU data transfer utilities

### Web / Networking
- Server-side
  - [ ] HTTP/1.1, HTTP/2, HTTP/3
  - [ ] HTTPS/TLS
  - [ ] Routing primitives for web apps
  - [ ] WebSockets
- Client-side & WASM
  - [ ] WASM target (optional)

### Matrices / Numerics (first-class)
- Core math types
  - [ ] Fixed-size matrices/vectors (f32/f64/iNN)
  - [ ] n-D arrays (optional follow-up)
- Operations
  - [ ] Add/Sub/Mul (matrix×matrix, matrix×vector, scalar×matrix)
  - [ ] Determinant, inverse, transpose
  - [ ] Decompositions (LU/QR/SVD) (stretch goal)
- Acceleration
  - [ ] CPU-optimized kernels; hooks for GPU offload

---

## Concurrency and parallelism
- See plan: [Concurrency and parallelism](plans/concurrency-and-parallelism/plan.md)

- [ ] Threads
- [ ] Mutex/RwLock/Condvar; Rc/Arc equivalents
- [ ] Atomics and memory ordering
- [ ] Channels/message passing
- [ ] Futures/async tasks

---

## IO, platform, and networking
- See plan: [IO, platform, and networking](plans/io-platform-and-networking/plan.md)

- [ ] File system (open, close, read, write, seek, delete, metadata)
- [~] Standard streams (stdin, stdout, stderr)
  - Notes: Basic stdout via `printf`/`puts` externs is wired. No stdin/stderr helpers yet.
- [ ] Signals/interrupts
- [ ] Shared memory and memory-mapped files
- [ ] Cross-platform abstraction layer
- [ ] Networking primitives (TCP, UDP)
- [ ] Time (wall-clock, monotonic, sleep)

---

## Serialization and parsing
- See plan: [Serialization and parsing](plans/serialization-and-parsing/plan.md)
- [ ] JSON, XML, binary formats
- [ ] CLI argument parsing helpers

---

## Security and cryptography
- See plan: [Security and cryptography](plans/security-and-cryptography/plan.md)
- [ ] Common encryption algorithms

---

## Advanced language features
- See plan: [Advanced language features](plans/advanced-language-features/plan.md)

- [ ] Procedural Macros
  - [ ] function-like
  - [ ] attribute-like
  - [ ] derive-like
- [ ] Borrow Checker
  - Notes: *T is owned and non-none (equivalent to Box), &T/&mut T are borrowed
- [ ] Compile-time evaluation
- [ ] Detect and memoize pure functions
- [ ] Lazy evaluation

---

## Luxuries
- See plan: [Luxuries](plans/luxuries/plan.md)

### Advanced Data Structures
- [ ] BigInt / arbitrary precision arithmetic
- [ ] Bignum rational/decimal types
- [ ] Bitsets
- [ ] Graph utilities

### Text and Encoding
- [ ] Regex engine
- [ ] Encoding/decoding utilities (UTF-16, Latin-1, etc)
- [ ] String builder/rope for efficient text manipulation

### Serialization / Parsing
- [ ] JSON, XML, and binary serialization formats
- [ ] Serialization integration with RDBMS types
- [ ] Command-line argument parsing helpers

### Safety & Utility
- [ ] Assertions and debugging utilities
- [ ] Logging framework
- [ ] Testing framework (unit test harness in-language)

### Cybersecurity
- [ ] Support for various common encryption algorithms

### Networking (extended)
- [ ] HTTP/1.1, HTTP/2, HTTP/3
- [ ] HTTPS
- [ ] Routing utilities for webdev
- [ ] QUIC
- [ ] WebRTC via STUN/TURN, etc.
- [ ] Sync ECS resources over the network

## OS and Platform
- See plan: [OS and platform](plans/os-and-platform/plan.md)

- [ ] File locking
- [ ] Shared memory, memory-mapped files
- [ ] Signals/interrupt handling
- [ ] Cross-platform abstractions for filesystem/network
- [~] Freestanding runtime entrypoint
  - Notes: Codegen supports a runtime mode that emits `_start` and wires `exit`; switch remains driver-controlled.

## Meta (project-wide)
- See plan: [Meta (project-wide)](plans/meta-project-wide/plan.md)

- [ ] Portability: consistent APIs across platforms (define behavior when OS features don’t exist)
- [ ] Performance guarantees: document big-O for collections, latency expectations for concurrency
- [ ] Safety guarantees: make UB impossible without explicit unsafe code
- [x] Extensibility: traits/interfaces so users can extend with custom types

---

## Current scope snapshot (as implemented here)
- See plan: [Current scope snapshot](plans/current-scope-snapshot/plan.md)

- Parser/AST/Semantic analyzer with structs, enums, traits, impls, functions/closures, ranges, loops, arrays/vectors, basic operators, and method dispatch.
- Codegen (LLVM via inkwell) supporting:
  - Numeric/boolean operations, ranges with exclusive upper bound, loops, vector iteration.
  - Structs (field access, method calls), trait method resolution, function.bind wrappers.
  - `println` for core scalars, vectors, basic 2D literal pretty-print; placeholder for filter path.
  - LLVM module verification prior to object emission.
- Runtime/stdlib scaffolding:
  - Module system (Rust-style `use`/`mod`), a loader that expands external modules from `.pn` files.
  - `stdlib/prelude.pn` and `stdlib/rt.pn` with stubs (to be expanded).
  - Externs for `malloc`, `free`, and `exit`; runtime mode can emit `_start`.
  - Tests: Added integration tests exercising prelude (print/id, len, streq) and rt coexistence with hosted main.

## Near-term priorities
- See plan: [Near-term priorities](plans/near-term-priorities/plan.md)
1. Make println work with traits (Display) instead of compiler codegen. [ ]
2. Turn placeholder printing paths (filter, matrix identifiers) into robust implementations that use Iterator and IntoIterator traits (including changing for-loops to use these). [ ]
3. Implement generics that works like the following [ ]:
```pn
my_generic_struct :: <T> {
    field: T,
}

my_generic_function :: <T: Iterator<Item: i64>> (iter: T) -> i64 {
    acc := 0
    for i in iter {
        acc += i
    }
    acc
}
```
4. Add support for trait objects via V-Tables
5. Add generic dynamic arrays (Vec) in stdlib with push/pop/len/iter, then maps/sets. [~]
  - Notes: Added Vec_i64 type and new_vec_i64 constructor (temporary due to generics unimplemented)
6. Introduce slice type (ptr + length) and generic iteration over slices. [~]
  - Notes: Implemented minimal `slice_i64`, loop support, and helpers `slice_len`/`slice_is_empty` and `slice_get(s, idx)`. Bounds checks added to slice_get. Next: generic slices, constructors (e.g., `slice_from(arr)`), and an `iter` method.
7. Flesh out file IO and process APIs; add error types and `!` ergonomics. [ ]
8. Namespaced modules (avoid flattening) and import resolver improvements (dedupe, cycles, diagnostics). [ ]
9. Solidify string type and UTF-8 APIs; make string printing first-class. [~]
  - Progress: len(string) implemented and tested; println for strings validated incl. UTF-8; stdlib tests verify via prelude; added streq/contains/starts_with/ends_with built-ins with tests.
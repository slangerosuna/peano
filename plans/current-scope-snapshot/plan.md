# Current Scope Snapshot Plan

## Snapshot Summary
- **Parser/AST/Semantic analyzer**
  - Supports structs, enums, traits, impls, functions/closures, ranges, loops, arrays/vectors, basic operators, method dispatch.
  - **Coverage metric:** 180 parser tests, 62 semantic analyzer tests; target ≥95% coverage for grammar productions.
  - **Action:** Maintain regression coverage; document extension points for upcoming features (generics, borrow checker); add mutation testing for pattern matching.
- **Codegen (LLVM via inkwell)**
  - Handles numeric/boolean operations, exclusive ranges, loops, vector iteration, struct operations, trait method resolution, and function binding wrappers.
  - Provides LLVM module verification before object emission and emits DWARF-lite debug info for core constructs.
  - **Gaps:** strings, floats, generics, async lowering.
  - **Action:** Track outstanding codegen gaps and align with `plans/tier-1-core-language-semantics/plan.md`; create checklist for each new language feature to ensure codegen parity.
- **Runtime/stdlib scaffolding**
  - Includes module system with Rust-style `use`/`mod`, loader for external `.pn` modules, and core stdlib files `stdlib/prelude.pn` and `stdlib/rt.pn`.
  - Externs for `malloc`, `free`, and `exit`; freestanding runtime mode emits `_start`.
  - Integration tests cover prelude utilities and runtime coexistence with hosted main; daily CI executes 54 stdlib scenarios.
  - **Action:** Expand stdlib batteries per `plans/core-utilities/plan.md` and `plans/runtime-errors-and-panics/plan.md`; publish compatibility matrix for freestanding vs hosted.

## Next Steps
- **Establish scope dashboard** that references all plan documents for quick status review (target: first iteration by end of sprint).
- **Audit test coverage** against requirements to identify untested features; produce quarterly report mapping tests to roadmap items.
- **Schedule regular scope review** to update this snapshot alongside roadmap progress; assign owners per subsystem and create review checklist template.
- **Introduce risk log** summarizing blockers from each plan folder with RAG status.

## Pseudocode and Complexity
- The snapshot consolidates delivered capabilities rather than prescribing algorithms; pseudocode and complexity analysis are not applicable. Refer to specific plan documents for implementation-level details.

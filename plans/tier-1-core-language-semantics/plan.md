# Tier 1: Core Language Semantics Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| Signed/unsigned integers (i8 … i64, u8 … u64) | [~] | `i32`/`i64` fully supported; unsigned types currently alias `i64`. | Implement true unsigned types and smaller integer widths; align codegen and type checker; add constant folding. |
| Floating-point (f32, f64) | [~] | Partial support available. | Flesh out operations, literals, intrinsic functions across pipeline, and IEEE rounding modes. |
| bool | [x] | Fully supported. | Maintain regression coverage. |
| char (Unicode scalar/code unit) | [x] | Char literals parsed/typed/codegen as 32-bit scalar; println uses `%c`. | Add scenarios covering comparisons, casting, and ensure unicode escapes round-trip in runtime. |
| Arrays (fixed-size) | [~] | Vector/matrix literals lower to stack arrays and are iterable. | Formalize array syntax and semantics; ensure generic iteration support and const-eval initialization. |
| Slices/views (pointer + length) | [~] | Built-in `slice_i64` with helpers (`slice_len`, `slice_is_empty`, `slice_get`). | Generalize slices to arbitrary element types; integrate borrow semantics and slicing syntax. |
| Strings (UTF-8) | [~] | Literals and basic operations implemented. | Expand API (mutation, builders) and ensure performance characteristics with rope integration. |
| Tuples and algebraic data types | [~] | Enums available; tuples pending. | Design tuple syntax/type system entries; ensure pattern matching support and tuple traits. |
| Safe allocation/deallocation wrappers | [~] | `malloc`/`free` externs exist; higher-level safety pending. | Build RAII-style abstractions and integrate with borrow checker roadmap; add `Drop` semantics. |
| Generics | [ ] | Absent in language today. | Implement type parameter parsing, monomorphization/codegen, trait bounds, and specialization roadmap. |
| Pattern matching/switch | [~] | Core matching available; payload destructuring implemented. | Complete exhaustiveness checking, guard clauses, ergonomics, and refutable pattern diagnostics. |
| Async/await | [ ] | Not available. | Define async runtime model and syntax; integrate with concurrency plans and cancellation tokens. |
| Reflection/introspection | [ ] | Needed for Debug/serde. | Provide limited type metadata for traits and diagnostics. |

## Near-Term Tasks
- **Finalize integer type matrix** including unsigned widths and literal parsing.
- Track: integer literal codegen now respects suffix metadata for loop bounds (`tests/run_integer_widths.rs`).
- **Advance generics prototype** to support simple generic structs/functions ahead of trait-bound support.
- **Expand slice/string coverage** with iterator traits as dependencies unblock.
- **Draft tuple syntax RFC** and pattern-matching rules.
- **Prototype async lowering** stub aligning with executor plan.

## Milestones
- **M1 — Numeric & String Foundations (3 weeks):** integer matrix completion, floating API audit, char type design, string API updates.
- **M2 — Structural Types (4 weeks):** tuples, arrays formalization, slice generalization, safe allocation wrappers.
- **M3 — Generics & Patterns (5 weeks):** generics parser/semantics MVP, pattern matching exhaustiveness, Drop semantics alignment.
- **M4 — Async & Reflection (3 weeks):** async syntax prototype, reflection metadata plan, integration tests with concurrency.

## Dependencies
- Trait system enhancements from `plans/standard-traits/plan.md`.
- Runtime ergonomics in `plans/runtime-errors-and-panics/plan.md` for async error flows.

## Pseudocode and Complexity

### Generic Monomorphization
```pn
instantiate_generic :: (definition: GenericDef, type_args: Array<TypeId>) -> LlvmIr => {
	signature := canonical_signature(definition.id, type_args)
	if let some(hit) = cache.get(signature) {
		ret hit
	}
	subst_map := zip(definition.type_params, type_args)
	specialized_ast := substitute_types(definition.ast, subst_map)
	analyzed := semantic_check(specialized_ast)
	llvm_ir := codegen(analyzed)
	cache.insert(signature, llvm_ir)
	ret llvm_ir
}
```
- **Time:** O(|AST|) per instantiation; dominated by semantic analysis and codegen after substitution.
- **Space:** O(|AST|) for the duplicated specialized AST; cache storage proportional to number of instantiated signatures.

### Slice Bounds Check
```pn
slice_get :: (slice: Slice<T>, index: i64) -> T => {
	if index < 0 or index >= slice.len {
		panic("slice index out of bounds")
	}
	ret *(slice.ptr + index)
}
```
- **Time:** O(1) per access.
- **Space:** O(1) auxiliary space.

### Async to State Machine Lowering
```pn
lower_async :: (fn: AsyncFn) -> (StateEnum, StateMachine) => {
	state_enum := []
	builder := new_state_machine()
	for await_point in fn.body.await_points {
		state_enum.push(label(await_point))
		builder.emit_suspend(await_point)
	}
	builder.finalize()
	ret (state_enum, builder.machine)
}
```
- **Time:** O(N) where N is the number of statements in the async function.
- **Space:** O(A) where A is the number of `await` points (states) generated.

### Tuple Pattern Exhaustiveness Check
```pn
check_tuple_patterns :: (patterns: [TuplePattern], arity: i64) -> bool => {
	covered := CoverageMatrix::new(arity)
	for pattern in patterns {
		covered.mark(pattern)
	}
	ret covered.is_complete()
}
```
- **Time:** O(P · A) where P patterns, A arity.
- **Space:** O(A) for coverage matrix per dimension.

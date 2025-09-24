# Runtime, Errors, and Panics Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| `!T` propagation ergonomics | [ ] | Not yet implemented. | Define canonical `!T` usage, propagation syntax (`?` equivalent), and runtime helpers with span capture. |
| Error trait and downcasting | [ ] | Absent today. | Design `Error` trait, implement `source()`/message APIs, runtime type information for downcasting, and conversion traits. |
| panic(message) intrinsic | [x] | Implemented via stdlib function calling `exit(1)`. | Expand diagnostics (file/line), integrate with panic hooks, add payload formatting. |
| assert!/assert_eq! macros | [x] | Provided via stdlib helpers. | Extend to richer assertions, generic types, and custom failure messages. |
| Backtraces (opt-in), panic hook | [ ] | Not supported. | Implement stack unwinding metadata, opt-in hooks configurable at runtime, and symbolication integration. |
| Unwind vs abort semantics | [ ] | Absent. | Introduce build-profile configuration, runtime support for unwind strategy, and FFI boundary guidelines. |
| Structured logging errors | [ ] | Need error reporting pipeline. | Emit JSON/structured payloads for errors/panics consumable by tooling. |

## Near-Term Tasks
- **Draft `!`/`?` ergonomics** in alignment with language syntax proposals.
- **Prototype enhanced panic reporting** including source location capture.
- **Define panic strategy configuration** to guide runtime design decisions.
- **Design structured error payload schema** shared with logging framework.
- **Spike backtrace symbolication** using DWARF info emitted by codegen.

## Milestones
- **M1 — `!` & `?` Ergonomics (3 weeks):** error/option patterns, `?` lowering, error trait skeleton.
- **M2 — Panic Enhancements (2 weeks):** enriched diagnostics, panic hook API, structured logging integration.
- **M3 — Backtrace & Strategy (3 weeks):** unwind metadata, abort/unwind selection, symbolication CLI tool.

## Dependencies
- Trait definitions from `plans/standard-traits/plan.md` for `Error` trait.
- Runtime entrypoint work in `plans/os-and-platform/plan.md` for hook registration.

## Pseudocode and Complexity

### `!` Propagation (`?`-like operator)
```pn
try_expr :: (value: !T) -> !T => {
	match value {
		!err => ret !err,
		result => ret result,
	}
}
```
- **Time:** O(1) per propagation; depends on pattern match constant cost.
- **Space:** O(1) auxiliary.

### Panic Hook Invocation
```pn
panic :: (message: string, location: SourceLocation) -> none => {
	if HOOK.is_some() {
		HOOK?(message, location)
	}
	print("Assertion failed at", location, ":", message)
	perform_strategy()
}

perform_strategy :: () -> none => {
	if strategy == PanicStrategy::Abort {
		exit(1)
	} else {
		begin_unwind()
	}
}
```
- **Time:** O(B + H) where B cost of printing/backtrace and H runtime hook work.
- **Space:** O(1) aside from backtrace buffers (implementation-defined).

### Error Downcasting
```pn
downcast :: (error: &ErrorObject, target_type: TypeId) -> ?&ErrorObject => {
	if error.type_id == target_type {
		ret some(cast(error))
	}
	match error.source {
		none => ret none,
		some(inner) => ret downcast(inner, target_type),
	}
}
```
- **Time:** O(D) where D is depth of error source chain.
- **Space:** O(1) additional space with recursion; iterative approach avoids stack growth.

### Structured Error Emission
```pn
emit_error :: (error: &ErrorObject, context: &ErrorContext) -> none => {
	payload := JsonObject::new()
	payload.insert("type", error.type_name())
	payload.insert("message", error.message())
	payload.insert("location", context.location.to_string())
	payload.insert("backtrace", context.backtrace.format())
	if context.tags.not_empty() {
		payload.insert("tags", context.tags)
	}
	LOG_SINK.write(payload.to_string())
}
```
- **Time:** O(F) where F fields serialized.
- **Space:** O(F) for payload buffer.

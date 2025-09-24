# Standard Traits Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| Eq/PartialEq | [~] | Implemented for numeric expressions. | Generalize to user-defined types and collections; provide derive integration. |
| Ord/PartialOrd | [~] | Numeric comparison available. | Extend to custom types and provide trait-driven ordering APIs, ensure NaN semantics. |
| Hash | [ ] | Not yet implemented. | Design hashing trait, integrate with collection roadmap, and document security considerations. |
| From/Into | [ ] | Not yet implemented. | Establish conversion trait semantics; align with generics rollout and `TryFrom` variants. |
| AsRef/AsMut | [ ] | Not yet implemented. | Provide borrow-based conversion patterns post borrow-checker work. |
| Default | [ ] | Not yet implemented. | Add trait semantics, derive support, and default values for core types. |
| Clone/Copy | [ ] | Not yet implemented. | Define ownership semantics for duplication; depends on borrow checker and move semantics. |
| Display | [~] | println supports core scalars/strings; trait-based formatting missing. | Implement `Display` trait integration, trait-object support, and formatting macros. |
| Debug | [ ] | Not yet implemented. | Provide structured debug formatting once reflection primitives land. |
| IntoIterator/Iterator | [~] | Partial support for loops over slices/vectors. | Formalize trait definitions, implement for core collections, and extend to streaming iterators. |
| Borrow/BorrowMut | [ ] | Not yet implemented. | Align with borrow checker design, document lifetime contracts. |
| Error (trait) | [ ] | Not yet implemented. | Define error trait for interoperability with result ergonomics and structured logging. |
| `!`/`?` ergonomics | [ ] | Not yet implemented. | Add helper traits/methods for pattern-based control flow and combinators. |
| Formatter traits (Write/Formatter) | [ ] | Absent. | Introduce writer abstractions enabling formatting engine reuse. |

## Near-Term Tasks
- **Complete Display trait integration** to unblock `println` improvements.
- **Draft Hash trait specification** in collaboration with `plans/core-utilities/plan.md` (maps/sets).
- **Prototype Iterator trait** usage for slices/vectors to validate trait resolution.
- **Author Clone/Copy semantics RFC** capturing move/borrow interactions.
- **Define Formatter abstractions** to share code between Display/Debug implementations.

## Milestones
- **M1 — Formatting Suite (3 weeks):** Display/Debug traits, formatter abstractions, println integration.
- **M2 — Collections Traits (3 weeks):** IntoIterator/Borrow traits across core containers, Hash spec complete.
- **M3 — Ownership Traits (4 weeks):** Clone/Copy/Default semantics finalized with borrow checker integration.
- **M4 — Ergonomics Enhancements (2 weeks):** `!`/`?` combinators, From/Into implementations, derive support draft.

## Dependencies
- Generics implementation from `plans/tier-1-core-language-semantics/plan.md`.
- Borrow checker initiatives under `plans/advanced-language-features/plan.md`.

## Pseudocode and Complexity

### Trait Method Resolution
```pn
resolve_trait_method :: (receiver_type: TypeId, trait: TraitId, method_name: string) -> ?MethodRef => {
	candidate_impls := lookup_impls(trait, receiver_type)
	for impl in candidate_impls {
		if impl.defines(method_name) {
			if satisfies_where_clauses(impl, receiver_type) {
				ret some(impl.method(method_name))
			}
		}
	}
	report_error("method not found for type")
	ret none
}
```
- **Time:** O(I · W) where I is number of candidate impls and W cost of evaluating where-clauses.
- **Space:** O(1) auxiliary; relies on pre-indexed impl tables.

### Iterator `next` Contract
```pn
trait Iterator<T> {
	next :: (&mut self) -> ?T
}

for_loop :: (iterator: &mut dyn Iterator<T>, body: (T) -> none) -> none => {
	loop {
		next_val := iterator.next()
		match next_val {
			none => ret,
			some(value) => body(value),
		}
	}
}
```
- **Time:** O(N) for N yielded items; each `next` should be O(1).
- **Space:** O(1) additional space aside from yielded items.

### Hash Trait Default Implementation Skeleton
```pn
trait Hash {
	hash :: (&self, state: &mut Hasher) -> none
}

impl Hash for i64 {
	hash :: (&self, state: &mut Hasher) -> none => {
		state.write_i64(*self)
	}
}

finalize_hash :: (value: T, registry: &HasherRegistry) -> u64 where T: Hash => {
	hasher := registry.acquire()
	value.hash(&mut hasher)
	ret hasher.finish()
}
```
- **Time:** O(H) where H hashing cost for the value (typically proportional to size).
- **Space:** O(1) auxiliary; hasher retains state.

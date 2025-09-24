# Near-term Priorities Plan

## Priority Items
| # | Description | Status | Notes | Planned Actions |
| --- | --- | --- | --- | --- |
| 1 | Make `println` work with `Display` trait | [ ] | Currently routed through compiler codegen. | Implement trait-based dispatch, macro hygiene, and buffered stdout with tests. |
| 2 | Replace placeholder printing paths with iterator-driven implementations | [ ] | For loops should consume `IntoIterator`. | Extend trait system, refactor for-loop lowering, add borrow-checker-aware iterators. |
| 3 | Implement generics (structs/functions with trait bounds) | [ ] | Language support absent. | Deliver parsing, semantic analysis, monomorphization, codegen support, and instantiation cache. |
| 4 | Support trait objects via V-Tables | [ ] | Requires runtime representation. | Design vtable layout, adjust codegen, add dynamic dispatch tests, document drop/clone semantics. |
| 5 | Add generic dynamic arrays (`Vec`) then maps/sets | [~] | Temporary `Vec_i64` exists. | Generalize to `Vec<T>`, then build hash map/set on top; ensure iterator + borrow integration. |
| 6 | Introduce slice type (ptr + length) and generic iteration | [~] | `slice_i64` implemented. | Generalize slices, integrate iterator traits, and add bounds-check policy. |
| 7 | Flesh out file IO and process APIs with `!` ergonomics | [ ] | APIs missing. | Define stdlib interfaces leveraging `!`/`Error`, align with platform detection. |
| 8 | Improve module resolver (namespaces, dedupe, cycles, diagnostics) | [ ] | Current resolver flattens modules. | Implement namespace-aware resolver, produce cycle diagnostics with fix suggestions. |
| 9 | Solidify string type and UTF-8 APIs | [~] | Core operations exist. | Expand APIs, ensure performance, integrate with builder/rope plans, add normalization toggles. |
| 10 | Establish structured logging and assertions | [ ] | Diagnostics limited. | Coordinate with `plans/luxuries/plan.md` to ship logging facade and configurable assertions. |

## Execution Strategy
- **Quarterly roadmap**: Align these priorities with quarterly deliverables and staffing allocations.
- **Dependency tracking**: Reference relevant plan documents to unblock cross-cutting concerns (traits, runtime, IO).
- **Milestone reviews**: Establish checkpoints to measure progress and adjust sequencing as dependencies land.
- **Risk management**: Maintain risk register covering borrow checker, generics complexity, and tooling readiness.

## Milestones
- **M1 — Language Ergonomics (4 weeks):** `Display` trait integration, iterator-driven printing, string API enhancement.
- **M2 — Type System Foundations (5 weeks):** generics parser+semantics, slice generalization, initial trait object support.
- **M3 — Collections & IO (4 weeks):** `Vec<T>`, hash map/set prototypes, file/process APIs with `!` ergonomics.
- **M4 — Module & Diagnostics (3 weeks):** module resolver overhaul, logging/assertion framework, coverage reporting.

## Pseudocode and Complexity

### Trait-backed `println`
```pn
println :: (value: Display) -> none => {
	formatter := resolve_trait_method(type_of(value), DisplayTrait, "format")?
	buffer := ScratchBuffer::acquire()
	formatter.write(value, &mut buffer)
	buffer.push('\n')
	stdout := Stdout::lock()
	stdout.write(buffer.as_slice())
	ScratchPool::release(buffer)
}
```
- **Time:** O(L) where L length of formatted output; locking overhead O(1).
- **Space:** O(L) for formatting buffer (pooled reuse).

### For-loop Desugaring to `IntoIterator`
```pn
lower_for_loop :: (iter_expr: IntoIterator<T>, body: (T) -> none) -> none => {
	iterator := into_iterator(iter_expr)
	loop {
		next_val := iterator.next()
		match next_val {
			none => break,
			some(value) => body(value),
		}
	}
}
```
- **Time:** O(N) for N yielded items; `next` expected O(1).
- **Space:** O(1) auxiliary.

### V-Table Dispatch
```pn
call_trait_object :: (obj: TraitObject, method_index: i64, args: Args) -> Value => {
	vtable := obj.vtable
	target := vtable[method_index]
	ret target(obj.data, args)
}
```
- **Time:** O(1) dispatch overhead.
- **Space:** O(1) auxiliary.

### Module Resolver Graph Walk with Namespaces
```pn
resolve_modules :: (root: ModuleId) -> none => {
	visited := Set<ModuleId>()
	stack := Set<ModuleId>()
	namespace_map := NamespaceTable::new()

	visit :: (module: ModuleId) -> none => {
		if stack.contains(module) {
			report_cycle(module)
			ret
		}
		if visited.contains(module) {
			ret
		}
		stack.insert(module)
		namespace_map.push_scope(module)
		for dep in module.imports {
			resolved := namespace_map.resolve(module, dep)
			visit(resolved)
		}
		namespace_map.pop_scope(module)
		stack.remove(module)
		visited.insert(module)
	}

	visit(root)
}
```
- **Time:** O(V + E) for module graph, plus namespace lookups O(log K) per import for K entries.
- **Space:** O(V) for recursion stack and visited set; O(K) for namespace table.

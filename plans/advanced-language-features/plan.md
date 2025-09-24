# Advanced Language Features Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| Procedural macros (function-like, attribute-like, derive-like) | [ ] | No macro system today. | Design macro AST representation, parsing hooks, hygienic expansion model, and caching story for incremental rebuilds. |
| Borrow checker (2-phase mutable borrows, NLL) | [ ] | Ownership semantics partially described; enforcement absent. | Define borrow rules, region inference, 2-phase reborrows, and diagnostics with fix-it hints. |
| Region & lifetime inference | [ ] | Lifetime graph design blocked by borrow checker. | Formalize region constraints solver, leak checking, and lifetime visualization tooling. |
| Compile-time evaluation | [ ] | Not implemented. | Specify const-eval execution environment, sandboxing, and side-effect guardrails. |
| Detect and memoize pure functions | [ ] | No infrastructure. | Introduce purity annotations, side-effect analysis, and optimizer passes for memoization. |
| Lazy evaluation | [ ] | Absent. | Determine language semantics for laziness, thunk representation, and integration with ownership/borrowing. |
| Metaprogram diagnostics | [ ] | Macro errors surface poorly. | Build structured diagnostics pipeline with span tracking and macro backtraces. |

## Near-Term Tasks
- **Finalize borrow checker RFC** to unblock generics, traits, and concurrency work.
- **Prototype function-like macro expansion** focusing on incremental compiler integration.
- **Assess const-eval requirements** in collaboration with codegen/runtime teams.
- **Spike lifetime visualizer tooling** to validate constraint graph introspection UX.
- **Draft macro hygiene test matrix** covering nested expansions and conflicting identifiers.

## Milestones
- **M1 — Borrow Checker Core (4 weeks):** region inference solver, borrow graph updates on MIR, baseline diagnostics.
- **M2 — Macro MVP (3 weeks):** function-like macros with cache invalidation, hygiene resolution, failure reporting.
- **M3 — Const-Eval Sandbox (2 weeks):** interpreter harness executing whitelisted ops, panic-on-impure detection.
- **M4 — Lazy Thunks Prototype (2 weeks):** arena-backed thunk storage, forced evaluation hooks, and destructor semantics.

## Dependencies
- Trait and generics support from `plans/tier-1-core-language-semantics/plan.md`.
- Diagnostics infrastructure defined in `plans/runtime-errors-and-panics/plan.md` for borrow checker messaging.

## Pseudocode and Complexity

### Borrow Checker Region Inference
```pn
check_function :: (fn: FunctionAst) -> none => {
	mir := lower_to_mir(fn)
	regions := solve_regions(mir)
	borrow_graph := BorrowGraph::with_capacity(mir.local_count)
	queue := Deque<(BlockId, StmtId)>::from(mir.statements())
	while let some((block, stmt)) = queue.pop_front() {
		constraint := derive_borrow_constraint(mir[block][stmt], regions)
		match borrow_graph.apply(constraint) {
			Conflict(conflict) => report_conflict(conflict, suggest_fix(conflict)),
			Updated(dependents) => queue.extend_back(dependents),
			Stable => continue,
		}
	}
}

solve_regions :: (mir: Mir) -> RegionSolution => {
	worklist := Deque<Constraint>()
	pending := BitSet::new()
	solution := RegionSolution::default()
	for constraint in initialize_constraints(mir) {
		worklist.push_back(constraint)
		pending.insert(constraint.id)
	}
	while let some(constraint) = worklist.pop_front() {
		pending.remove(constraint.id)
		if propagate(constraint, &mut solution) {
			for dep in dependents(constraint) {
				if pending.insert(dep.id) {
					worklist.push_back(dep)
				}
			}
		}
	}
	ret solution
}
```
- **Time:** O(S + E + C) where S statements, E edges in borrow graph, C constraint propagations.
- **Space:** O(V + E) for borrow graph vertices/edges plus O(R) for region solutions.

### Procedural Macro Expansion Pipeline
```pn
expand_macro :: (invocation: MacroInvocation, cache: &mut MacroCache) -> Ast => {
	macro_def := resolve_macro(invocation.path)
	if cache.contains(invocation, macro_def.version_hash) {
		ret cache.get(invocation)
	}
	input := normalize_tokens(invocation.tokens)
	guard := hygiene_context::enter(invocation.span)
	expanded_stream := host_call(macro_def.dispatch, input)
	validate(expanded_stream, invocation.span)?
	parsed_ast := parse(expanded_stream)
	cache.insert(invocation, macro_def.version_hash, parsed_ast.clone())
	guard.exit()
	ret parsed_ast
}
```
- **Time:** O(T + P + V) where T size of input tokens, P parsing cost, V validation/hygiene overhead; cached invocation amortizes to O(1).
- **Space:** O(T + P) for token buffers and AST plus O(C) for cache entries.

### Const-Eval Execution
```pn
const_eval :: (expr: Expr, env: ConstEnv) -> Value => {
	match expr {
		Expr::Literal(value) => ret value,
		Expr::Binary(op, lhs, rhs) => {
			left := const_eval(lhs, env)
			right := const_eval(rhs, env)
			ret apply(op, left, right)
		},
		Expr::If(cond, then_branch, else_branch) => {
			if const_eval(cond, env) {
				ret const_eval(then_branch, env)
			}
			ret const_eval(else_branch, env)
		},
		Expr::Match(scrutinee, arms) => {
			value := const_eval(scrutinee, env)
			arm := select_match_arm(value, arms)?
			ret const_eval(arm.body, env.extend(arm.bindings, value))
		},
		Expr::Call(fn_ref, args) => {
			if ~fn_ref.is_const || fn_ref.may_panic {
				error("non-const function")
			}
			fn_env := env.extend_with_args(fn_ref.params, args.map(|a| const_eval(a, env)))
			ret interpret(fn_ref.body, fn_env)
		},
	}
}
```
- **Time:** O(N + M) where N nodes evaluated and M matching arm scans; may short-circuit on branches.
- **Space:** O(D + B) where D recursion depth and B binding frames.

### Lazy Thunk Forcing
```pn
force :: (thunk: &Thunk<T>) -> T => {
	match thunk.state.load() {
		ThunkState::Evaluated(value) => ret value.clone(),
		ThunkState::Pending => {
			if thunk.lock.try_acquire() == false {
				wait_for(thunk.lock)
				ret force(thunk)
			}
			value := (thunk.compute)()
			thunk.state.store(ThunkState::Evaluated(value.clone()))
			thunk.lock.release()
			ret value
		},
	}
}
```
- **Time:** O(1) after first evaluation; O(K) for K compute cost on first execution.
- **Space:** O(1) auxiliary beyond thunk payload.

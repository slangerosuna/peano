# Legend

- [ ] Not started
- [~] In progress
- [x] Completed

# Next Steps Toward a Self-Hosting Compiler

1. **Finalize core syntax and semantics** (`plans/tier-1-core-language-semantics/plan.md`)
   - **[1.1]** Add `char` literal parsing, type checking, and codegen. [x]
     - **Plan:** Update `parser/grammar.pest` to accept escaped Unicode scalars, extend `ast::LiteralKind`, validate scalar range in semantic analyzer, and emit LLVM `i32` codegen with UTF-32 storage. Add tests in `tests/run_char_literal.rs`.
     - **Pseudocode:**
```pn
parse_char_literal :: (tokens: TokenStream) -> !Expr => {
	token := tokens.expect(TokenKind::CharLiteral)?
	value := decode_escape_sequences(token.lexeme)
	if ~is_valid_unicode_scalar(value) {
		ret !ParseError::InvalidChar(token.span)
	}
	ret Expr::Literal(LiteralKind::Char(value), token.span)
}

codegen_char_literal :: (ctx: &mut CodegenCtx, value: u32) -> LlvmValue => {
	ret ctx.builder.const_int(ctx.types.i32, value)
}
```
   - **[1.2]** Implement remaining integer widths and unsigned semantics across parser, semantic analysis, and backend.
     - **Plan:** Add tokenization for suffixes, expand type table with metadata, adjust numeric literal inference, ensure unsigned wrap semantics in IR, and add regression tests for overflow behavior. [x]
     - **Pseudocode:**
```pn
lower_int_literal :: (lit: ParsedInt, target: IntType) -> !TypedLiteral => {
	bits := target.bit_width()
	if lit.value > max_value(bits, target.signed) {
		diag := Diagnostic::new("integer literal out of range", lit.span)
		diag.add_note(format("max value for {} is {}", target, max_value(bits, target.signed)))
		ret !diag
	}
	ret TypedLiteral { ty: target, value: lit.value & mask(bits) }
}

emit_int_arith :: (op: IntOp, lhs: LlvmValue, rhs: LlvmValue, ty: IntType) -> LlvmValue => {
	match (op, ty.signed) {
		(Add, _) => builder.add(lhs, rhs),
		(Sub, _) => builder.sub(lhs, rhs),
		(Mul, _) => builder.mul(lhs, rhs),
		(Div, true) => builder.sdiv(lhs, rhs),
		(Div, false) => builder.udiv(lhs, rhs),
		(Rem, true) => builder.srem(lhs, rhs),
		(Rem, false) => builder.urem(lhs, rhs),
	}
}
```
   - **[1.3]** Generalize arrays/tuples (including tuple pattern binding and tuple constructor lowering).
     - **Plan:** Introduce tuple type descriptors, update pattern binder to handle destructuring, generate aggregate LLVM constants, and ensure borrow-safe indexes. Add parser grammar for tuple literals and patterns. [x]
     - **Pseudocode:**
```pn
analyze_tuple_pattern :: (pat: TuplePattern, expected: TypeId) -> !TupleBinding => {
	elems := expected.as_tuple()?.elements
	if pat.items.len() ~= elems.len() {
		ret !diagnostic_tuple_arity(pat.span, elems.len())
	}
	bindings := []
	for (item, elem_ty) in zip(pat.items, elems) {
		bindings.push(analyze_pattern(item, elem_ty)?)
	}
	ret TupleBinding { bindings }
}

lower_tuple_literal :: (ctx: &mut CodegenCtx, exprs: Vec<ExprId>) -> LlvmValue => {
	values := exprs.map(|id| lower_expr(ctx, id))
	ret ctx.builder.const_struct(values, /*packed=*/false)
}
```
   - **[1.4]** Expand slice support from `slice_i64` to generic slices with borrow-aware iteration.
     - **Plan:** Define generic slice struct `{ ptr: *T, len: usize }`, parameterize functions over element type, integrate borrow checker views, and implement iterator trait bridging. [ ]
     - **Pseudocode:**
```pn
slice_from_vec :: (vec: &Vec<T>) -> Slice<T> => {
	ret Slice { ptr: vec.data.as_ptr(), len: vec.len }
}

slice_iter_next :: (iter: &mut SliceIter<T>) -> ?T => {
	if iter.index >= iter.slice.len {
		ret none
	}
	value := unsafe_load(iter.slice.ptr, iter.index)
	iter.index += 1
	ret some(value)
}
```
   - **[1.5]** Introduce generic parameter parsing in AST and type resolver (structs, enums, functions).
     - **Plan:** Extend grammar for `<T, U>` parameter lists, store generics in AST nodes, update resolver to maintain scope stack, and emit diagnostics for missing arguments. [ ]
     - **Pseudocode:**
```pn
parse_generics :: (tokens: TokenStream) -> !GenericParams => {
	params := []
	if ~tokens.peek_is(TokenKind::Lt) {
		ret GenericParams::empty()
	}
	tokens.consume(TokenKind::Lt)
	loop {
		ident := tokens.expect_ident()? 
		params.push(GenericParam { name: ident.text, bounds: parse_bounds(tokens)? })
		match tokens.expect_one_of([TokenKind::Comma, TokenKind::Gt])? {
			TokenKind::Comma => continue,
			TokenKind::Gt => break,
		}
	}
	ret GenericParams::new(params)
}
```
   - **[1.6]** Implement monomorphization cache per `instantiate_generic` pseudocode, including signature hashing and duplication avoidance.
     - **Plan:** Create cache keyed by normalized type signature, use deterministic hashing, and deduplicate IR modules. Add metrics logging when cache hits occur. [ ]
     - **Pseudocode:**
```pn
instantiate_mono :: (def: GenericDefId, args: Vec<TypeId>) -> !LlvmValue => {
	signature := make_signature(def, args)
	if let some(cached) = mono_cache.get(signature) {
		ret cached
	}
	specialized := substitute(def, args)?
	analyzed := analyze(specialized)?
	llvm := codegen(analyzed)
	mono_cache.insert(signature, llvm)
	ret llvm
}
```
   - **[1.7]** Enforce trait bounds during instantiation and surface diagnostics referencing `plans/standard-traits/plan.md`.
     - **Plan:** During type substitution, evaluate where clauses, query trait implementation tables, collect missing bounds, and emit actionable diagnostics with suggestions. [ ]
     - **Pseudocode:**
```pn
check_trait_bounds :: (impl_scope: &ImplScope, args: Vec<TypeId>, bounds: Vec<WhereClause>) -> !none => {
	for bound in bounds {
		if ~impl_scope.satisfies(bound, args) {
			d := Diagnostic::new("trait bound not satisfied", bound.span)
			d.add_note(format("{} must implement {}", bound.ty, bound.trait_id))
			ret !d
		}
	}
	ret none
}
```
   - **[1.8]** Complete pattern matching upgrades (exhaustiveness, guards, ergonomic sugar) with tests mirroring plan requirements.
     - **Plan:** Implement decision tree builder, integrate guard evaluation short-circuiting, add diagnostics for uncovered cases, and support or-pattern sugar. [ ]
     - **Pseudocode:**
```pn
check_match_exhaustiveness :: (arms: Vec<MatchArm>, scrut_ty: TypeId) -> !none => {
	cov := CoverageMatrix::new(scrut_ty)
	for arm in arms {
		cov.mark(arm.pattern)
	}
	if ~cov.is_complete() {
		missing := cov.first_missing()
		d := Diagnostic::new("non-exhaustive patterns", missing.span)
		d.add_note(format("missing pattern: {}", missing.render()))
		ret !d
	}
	ret none
}
```
   - **[1.9]** Lower async functions to state machines and wire to executor hooks from `plans/concurrency-and-parallelism/plan.md`.
     - **Plan:** Translate async AST to MIR with yield points, generate enums for states, plug into executor `register_waker`, and ensure borrow checking across await points. [ ]
     - **Pseudocode:**
```pn
lower_async_fn :: (fn_id: FnId) -> AsyncArtifact => {
	mir := build_mir(fn_id)
	states := collect_await_points(mir)
	state_enum := build_state_enum(states)
	machine := lower_state_machine(mir, state_enum)
	ret AsyncArtifact { state_enum, machine }
}
```
   - **[1.10]** Backfill regression tests in `tests/` for each newly supported construct.
     - **Plan:** Create test templates per feature, integrate into CI, add golden outputs, and ensure coverage thresholds update. [ ]
     - **Pseudocode:**
```pn
add_regression_test :: (name: string, source: string, expected: ExpectedResult) -> none => {
	path := format("tests/{}.pn", name)
	write_file(path, source)
	run_result := run_compiler(path)
	assert_eq(run_result, expected)
}
```

2. **Complete trait system and polymorphism** (`plans/standard-traits/plan.md`, `plans/near-term-priorities/plan.md`)
   - **[2.1]** Finish implementations for `Eq`/`PartialEq` on user-defined structs/enums.
     - **Plan:** Autogenerate equality based on field comparison, integrate derive macro stubs, and ensure structural equality semantics. [ ]
     - **Pseudocode:**
```pn
derive_partial_eq :: (ty: TypeId, fields: Vec<FieldId>) -> ImplBlock => {
	body := []
	for field in fields {
		body.push(Expr::eq(FieldAccess::new("self", field), FieldAccess::new("other", field)))
	}
	ret ImplBlock::new(ty, Trait::PartialEq, combine_with_and(body))
}
```
   - **[2.2]** Extend `Ord`/`PartialOrd` to tuple and enum variant ordering.
     - **Plan:** Implement lexicographic comparison for tuples and discriminant-first ordering for enums, ensuring fallback to field comparisons. [ ]
     - **Pseudocode:**
```pn
compare_tuple :: (lhs: TupleValue, rhs: TupleValue) -> Ordering => {
	for i in 0:lhs.len {
		cmp := compare(lhs[i], rhs[i])
		if cmp ~= Ordering::Equal {
			ret cmp
		}
	}
	ret Ordering::Equal
}
```
   - **[2.3]** Ship `Hash` trait and link to hash map plan items (`plans/core-utilities/plan.md`).
     - **Plan:** Provide default implementations for primitives, allow custom stateful hashers, and integrate with `HashMap`. [ ]
     - **Pseudocode:**
```pn
hash_struct :: (value: &StructValue, state: &mut Hasher) -> none => {
	for field in value.fields {
		field.value.hash(state)
	}
}
```
   - **[2.4]** Implement `Display` trait plumbing for `println` (moving away from compiler intrinsic).
     - **Plan:** Create trait object path for formatting, update macro lowering, reuse scratch buffers, and ensure thread-safe stdout locking. [ ]
     - **Pseudocode:**
```pn
display_write :: (value: &dyn Display, writer: &mut Formatter) -> !none => {
	value.fmt(writer)?
	ret none
}
```
   - **[2.5]** Add `Debug` formatting hooks referencing compiler diagnostics.
     - **Plan:** Extend formatting engine with structured output, provide derived debug for enums/structs, and integrate with panic payloads. [ ]
     - **Pseudocode:**
```pn
debug_struct :: (name: string, fields: Vec<DebugField>, f: &mut Formatter) -> !none => {
	f.write_str(name)?
	f.write_char('{')?
	for (i, field) in fields.enumerate() {
		if i > 0 { f.write_str(", ")? }
		f.write_str(field.name)?
		f.write_str(": ")?
		field.value.fmt(f)?
	}
	f.write_char('}')?
	ret none
}
```
   - **[2.6]** Stabilize `Iterator`/`IntoIterator` traits and ensure for-loop desugaring uses them.
     - **Plan:** Finalize trait definitions, add blanket impls for collections, and update lowering to use `next` contract. [ ]
     - **Pseudocode:**
```pn
for_loop_lowering :: (iterable: ExprId, body: ExprId) -> Lowered => {
	iter := call_into_iterator(iterable)
	loop_label := new_label()
	emit_label(loop_label)
	next_val := call_iterator_next(iter)
	match next_val {
		Some(value) => {
			execute_body(body, value)
			emit_jump(loop_label)
		},
		None => emit_break(),
	}
}
```
   - **[2.7]** Provide `From`/`Into` and `AsRef`/`AsMut` conversions for standard types.
     - **Plan:** Define trait signatures, implement conversions for numeric widening, string slices, and reference views. [ ]
     - **Pseudocode:**
```pn
impl From<String> for Vec<u8> {
	from :: (value: String) -> Vec<u8> => {
		ret Vec::from_slice(value.as_bytes())
	}
}
```
   - **[2.8]** Define `Default`, `Clone`, `Copy`, `Borrow`, `Error` traits with derive support roadmap.
     - **Plan:** Provide default implementations for primitives, design clone semantics referencing ownership, and stub derive macro hooks. [ ]
     - **Pseudocode:**
```pn
clone_impl :: (value: &T) -> T where T: Clone => {
	ret value.clone_inner()
}

default_impl :: (ty: TypeId) -> Value => {
	match ty {
		Type::Bool => false,
		Type::Int(_) => 0,
		Type::Struct(id) => struct_default(id),
	}
}
```
   - **[2.9]** Harden trait resolution caches/lookups with conflict diagnostics.
     - **Plan:** Add caching with versioning snapshots, detect overlapping impls, and issue targeted diagnostics. [ ]
     - **Pseudocode:**
```pn
resolve_trait :: (trait_id: TraitId, ty: TypeId) -> !ImplId => {
	key := TraitCacheKey { trait_id, ty }
	if let some(entry) = trait_cache.get(key) {
		ret entry
	}
	candidates := lookup_impls(trait_id, ty)
	impl := disambiguate(candidates)?
	trait_cache.insert(key, impl)
	ret impl
}
```
   - **[2.10]** Implement trait objects: vtable layout, `dyn Trait` parsing, runtime dispatch.
     - **Plan:** Introduce trait object type representation, construct vtables during codegen, and update runtime dispatch call sites. [ ]
     - **Pseudocode:**
```pn
make_trait_object :: (data_ptr: Ptr, vtable: &VTable) -> TraitObject => {
	ret TraitObject { data: data_ptr, vtable }
}

dispatch_trait :: (obj: TraitObject, slot: usize, args: Args) -> Value => {
	target := obj.vtable.slots[slot]
	ret target(obj.data, args)
}
```

3. **Establish error handling and diagnostics** (`plans/runtime-errors-and-panics/plan.md`)
   - **[3.1]** Provide error propagation on `!T`/`E!T` result types with `?`-like flow and helper combinators.
     - **Plan:** Implement syntactic sugar lowering for `!` values and ensure control flow analysis handles early returns. [ ]
     - **Pseudocode:**
```pn
lower_try_operator :: (expr: ExprId) -> Lowered => {
	match lower_expr(expr) {
		!err => emit_return(err),
		value => value,
	}
}
```
   - **[3.2]** Add error/option convenience helpers (`unwrap_or`, `map_err`) in stdlib.
     - **Plan:** Implement in `stdlib/prelude.pn`, ensure zero-cost inline semantics, and add doc tests. [ ]
     - **Pseudocode:**
```pn
unwrap_or :: (value: !T, fallback: T) -> T => {
	match value {
		!_ => fallback,
		result => result,
	}
}
```
   - **[3.3]** Implement `Error` trait with `source()` traversal and downcasting helper.
     - **Plan:** Define trait methods, integrate with dynamic type metadata, and support nested errors. [ ]
     - **Pseudocode:**
```pn
error_chain :: (err: &dyn Error) -> Iterator<&dyn Error> => {
	current := some(err)
	ret Iterator::new(|yield| {
		while let some(e) = current {
			yield(e)
			current = e.source()
		}
	})
}
```
   - **[3.4]** Capture file/line metadata during panic and integrate with compiler diagnostics.
     - **Plan:** Use macros to capture location, embed into panic payload, and align with diagnostic renderer. [ ]
     - **Pseudocode:**
```pn
panic_with_location :: (message: string, location: SourceLocation) -> none => {
	payload := PanicPayload { message, location, backtrace: None }
	trigger_panic(payload)
}
```
   - **[3.5]** Expose panic hook registration API and default logging behavior.
     - **Plan:** Implement atomic pointer to hook, provide setter/getter, call hook before default action. [ ]
     - **Pseudocode:**
```pn
set_panic_hook :: (hook: PanicHook) -> PanicHook => {
	old := PANIC_HOOK.swap(hook)
	ret old
}
```
   - **[3.6]** Implement backtrace collection (gated) and store in panic payloads.
     - **Plan:** On supported platforms, collect frame pointers via unwinder, serialize to string representation, store in payload. [ ]
     - **Pseudocode:**
```pn
capture_backtrace :: () -> Backtrace => {
	frames := []
	stack_walk(|frame| frames.push(frame))
	ret Backtrace { frames }
}
```
   - **[3.7]** Add configuration to choose abort vs unwind per build profile, with runtime plumbing.
     - **Plan:** Extend build config parser, set global strategy, branch in panic handler accordingly. [ ]
     - **Pseudocode:**
```pn
perform_panic_strategy :: (strategy: PanicStrategy) -> none => {
	match strategy {
		PanicStrategy::Abort => os_exit(1),
		PanicStrategy::Unwind => start_unwind(),
	}
}
```

4. **Build foundational collections and utilities** (`plans/core-utilities/plan.md`, `plans/luxuries/plan.md`)
   - **[4.1]** Implement `Vec<T>` allocation growth, `push`, `pop`, `len`, `iter`, and `IntoIterator`.
     - **Plan:** Use doubling capacity strategy, ensure safe reallocations, provide iterator structs, and integrate into prelude. [ ]
     - **Pseudocode:**
```pn
vec_push :: (vec: &mut Vec<T>, item: T) -> none => {
	if vec.len == vec.cap {
		vec.grow()
	}
	vec.data[vec.len] = move(item)
	vec.len += 1
}

vec_pop :: (vec: &mut Vec<T>) -> ?T => {
	if vec.len == 0 {
		ret none
	}
	vec.len -= 1
	ret some(move(vec.data[vec.len]))
}

vec_len :: (&Vec<T>) -> usize => vec.len

vec_into_iter :: (vec: Vec<T>) -> VecIntoIter<T> => {
	ret VecIntoIter { data: vec.data, len: vec.len, index: 0 }
}

impl Iterator<T> for VecIntoIter<T> {
	next :: (&mut self) -> ?T => {
		if self.index == self.len {
			ret none
		}
		value := move(self.data[self.index])
		self.index += 1
		ret some(value)
	}
}
```
   - **[4.2]** Provide `Vec<T>` slicing, reserve, shrink, and capacity APIs.
     - **Plan:** Add `ensure_capacity`, `shrink_to_fit`, and slicing constructors interacting with `Slice<T>` type. [ ]
     - **Pseudocode:**
```pn
vec_reserve :: (vec: &mut Vec<T>, additional: usize) -> none => {
	needed := vec.len + additional
	if needed > vec.cap {
		vec.reallocate(next_capacity(needed))
	}
}

vec_slice :: (&Vec<T>, range: Range<usize>) -> Slice<T> => {
	assert(range.end <= vec.len)
	ret Slice { ptr: vec.data.as_ptr() + range.start, len: range.end - range.start }
}

vec_shrink_to_fit :: (vec: &mut Vec<T>) -> none => {
	if vec.len == vec.cap {
		ret none
	}
	new_cap := max(vec.len, 4)
	vec.reallocate(new_cap)
}
```
```
   - **[4.3]** Create hash map/set scaffolding leveraging `Hash` trait and open-addressing pseudocode.
     - **Plan:** Implement probe sequence, lazy deletion, resizing strategy, and integrate `Hash` trait stateful hashing. [ ]
     - **Pseudocode:**
```pn
hashmap_insert :: (map: &mut HashMap<K, V>, key: K, value: V) -> none => {
	idx := map.find_slot(&key)
	map.slots<idx> = Slot::occupied(key, value)
	map.len += 1
	if map.load_factor() > 0.7 {
		map.resize(map.cap * 2)
	}
}
```
   - **[4.4]** Extend string utilities: builders, rope concatenation/indexing, substring search.
     - **Plan:** Implement `StringBuilder` with chunked buffers, integrate rope tree balancing, and add Boyer-Moore substring search. [ ]
     - **Pseudocode:**
```pn
rope_concat :: (left: RopeNode, right: RopeNode) -> RopeNode => {
	ret RopeNode::Branch { left, right, weight: left.len }
}

bm_search :: (haystack: string, needle: string) -> ?usize => {
	table := build_bad_char_table(needle)
	i := 0
	while i <= haystack.len - needle.len {
		j := needle.len - 1
		while j >= 0 && needle<j> == haystack<i + j> {
			j -= 1
		}
		if j < 0 { ret some(i) }
		i += max(1, j - table<haystack<i + j>>)
	}
	ret none
}
```
   - **[4.5]** Add Unicode-aware string diagnostics aligned with `plans/core-utilities/plan.md`.
     - **Plan:** Integrate normalization checks, detect invalid grapheme clusters, and emit detailed diagnostics via `Diagnostic::add_note`. [ ]
     - **Pseudocode:**
```pn
validate_utf8 :: (input: string, span: Span) -> !none => {
	if ~is_normalized_nfc(input) {
		d := Diagnostic::new("string not NFC-normalized", span)
		d.add_note("consider calling normalize_nfc()")
		ret !d
	}
	ret none
}
```
   - **[4.6]** Implement logging macros and async sink worker from `plans/luxuries/plan.md`.
     - **Plan:** Provide macros expanding to log queue pushes, spawn background worker tied to executor, and allow sink configuration. [ ]
     - **Pseudocode:**
```pn
log :: (level: LogLevel, message: string) -> none => {
	entry := LogEntry::new(level, message, timestamp_now())
	LOG_QUEUE.push(entry)
}

log_worker :: (queue: &LogQueue, sink: &mut LogSink) -> none => {
	while RUNNING.load() {
		if let some(entry) = queue.pop() {
			sink.write(entry)
		} else {
			park_thread()
		}
	}
}
```
   - **[4.7]** Provide in-language testing harness with assertion/reporting integration.
     - **Plan:** Build test runner that discovers annotated functions, captures results, and prints colored summaries. [ ]
     - **Pseudocode:**
```pn
run_tests :: (registry: &TestRegistry) -> TestSummary => {
	summary := TestSummary::default()
	for test in registry.tests {
		match catch_unwind(|| test.func()) {
			!panic_info => {
				summary.fail += 1
				print_failure(test.name, panic_info)
			},
			_ => summary.pass += 1,
		}
	}
	ret summary
}
```
   - **[4.8]** Flesh out PRNG helpers (deterministic + secure placeholders) for tooling tests.
     - **Plan:** Provide deterministic algorithms like XorShift and stub secure RNG hooking to platform entropy. [ ]
     - **Pseudocode:**
```pn
xor_shift32 :: (state: &mut u32) -> u32 => {
	x := *state
	x ^= x << 13
	x ^= x >> 17
	x ^= x << 5
	*state = x
	ret x
}
```

5. **Rework module system and internal compiler architecture** (`plans/near-term-priorities/plan.md`, `plans/tier-1-first-class-domains/plan.md`, `plans/advanced-language-features/plan.md`)
   - **[5.1]** Redesign module resolver to maintain namespace tree and detect cycles.
     - **Plan:** Build namespace table, maintain stack for DFS, record resolved modules, and produce diagnostics on cycles. [ ]
     - **Pseudocode:**
```pn
resolve_modules :: (root: ModuleId) -> none => {
	visited := Set::new()
	stack := Vec::new()
	visit(root, &mut visited, &mut stack)
}

visit :: (module: ModuleId, visited: &mut Set<ModuleId>, stack: &mut Vec<ModuleId>) -> none => {
	if stack.contains(module) {
		report_cycle(module, stack)
		return
	}
	if visited.contains(module) {
		return
	}
	stack.push(module)
	for dep in module.imports {
		visit(dep, visited, stack)
	}
	stack.pop()
	visited.insert(module)
}
```
   - **[5.2]** Implement import de-duplication and conflict diagnostics.
     - **Plan:** Track imported symbols in per-module map, detect duplicates, and provide fix-it suggestions. [ ]
     - **Pseudocode:**
```pn
record_import :: (module: ModuleId, name: Symbol, span: Span) -> !none => {
	entry := import_table[module]
	if let some(prev) = entry.insert(name, span) {
		d := Diagnostic::new("duplicate import", span)
		d.add_note(format("previous import at {}", prev))
		ret !d
	}
	ret none
}
```
   - **[5.3]** Introduce stable handle table for AST/IR nodes (inspired by ECS plan).
     - **Plan:** Allocate handles with generation counters, store nodes in arena, and provide typed references. [ ]
     - **Pseudocode:**
```pn
	allocate_handle :: (arena: &mut Arena<T>) -> Handle<T> => {
```
	index := arena.storage.len
	arena.storage.push(None)
	gen := arena.generations<index>
	ret Handle { index, generation: gen }
}
```
   - **[5.4]** Provide query iteration utilities over IR collections for passes.
     - **Plan:** Design query API returning iterators, allow filtering by predicate, and ensure borrow-safe access. [ ]
     - **Pseudocode:**
```pn
query_ir :: (ctx: &IrCtx, filter: QueryFilter) -> Iterator<NodeId> => {
	ret Iterator::new(|yield| {
		for node in ctx.nodes {
			if filter.matches(node) {
				yield(node.id)
			}
		}
	})
}
```
   - **[5.5]** Implement borrow checker core inference algorithm and diagnostic emitter.
     - **Plan:** Build MIR borrow graph, apply dataflow analysis, and emit conflict diagnostics with suggestions. [ ]
     - **Pseudocode:**
```pn
analyze_borrows :: (mir: Mir) -> !none => {
	graph := BorrowGraph::new()
	for block in mir.blocks {
		for stmt in block.statements {
			graph.update(stmt)
			if let some(conflict) = graph.detect_conflict(stmt) {
				ret !make_borrow_diag(conflict)
			}
		}
	}
	ret none
}
```
   - **[5.6]** Deliver procedural macro host interface and expansion pipeline.
     - **Plan:** Provide IPC boundary for host compiler, manage caching, validate hygiene contexts, and integrate with incremental builds. [ ]
     - **Pseudocode:**
```pn
expand_macro :: (invocation: MacroInvocation) -> !Ast => {
	def := resolve_macro(invocation.path)
	input := normalize_tokens(invocation.tokens)
	response := host_call(def.handle, input)
	ast := parse_macro_output(response.tokens)?
	ret ast
}
```
   - **[5.7]** Implement const-eval interpreter for compile-time expressions (const functions, literals).
     - **Plan:** Build interpreter with sandboxed environment, support integer/float operations, branch handling, and recursion limits. [ ]
     - **Pseudocode:**
```pn
const_eval :: (expr: ExprId, env: &mut ConstEnv) -> !Value => {
	match expr.kind {
		ExprKind::Literal(lit) => ret Value::from_literal(lit),
		ExprKind::Binary(op, lhs, rhs) => {
			left := const_eval(lhs, env)?
			right := const_eval(rhs, env)?
			ret apply_const_binop(op, left, right)
		},
		ExprKind::Call(fn_id, args) => ret eval_const_fn(fn_id, args, env)?,
	}
}
```
   - **[5.8]** Integrate pure-function memoization hooks using const-eval framework.
     - **Plan:** Detect pure functions via annotations, hash arguments, check memo cache, and reuse results. [ ]
     - **Pseudocode:**
```pn
memoize_pure_call :: (fn_id: FnId, args: [Value]) -> Value => {
	key := hash_args(fn_id, args)
	if let some(result) = memo_cache.get(key) {
		ret result
	}
	result := eval_pure_fn(fn_id, args)
	memo_cache.insert(key, result.clone())
	ret result
}
```

6. **Deliver IO and platform abstractions** (`plans/io-platform-and-networking/plan.md`, `plans/os-and-platform/plan.md`)
   - **[6.1]** Build `File` abstraction with buffered read/write/seek and metadata access.
     - **Plan:** Encapsulate file descriptor, provide buffered reader/writer, implement error conversions, and ensure RAII closing. [ ]
     - **Pseudocode:**
```pn
file_read :: (file: &mut File, buf: &mut Vec<u8>) -> !usize => {
	if file.buffer.is_empty() {
		file.buffer.fill_from_os(file.fd)?
	}
	ret file.buffer.copy_into(buf)
}
```
   - **[6.2]** Implement `Path` type with conversion between OS strings and Peano strings.
     - **Plan:** Store normalized segments, support UTF-8/OS string conversions, handle relative/absolute variants. [ ]
     - **Pseudocode:**
```pn
path_from_os :: (os_str: OsString) -> !Path => {
	segments := normalize_segments(split_components(os_str))
	ret Path { segments, is_absolute: os_str.is_absolute() }
}
```
   - **[6.3]** Add stdin/stdout/stderr handles with error propagation.
     - **Plan:** Provide global lazy handles, support locking, and integrate with logging. [ ]
     - **Pseudocode:**
```pn
stdout_write :: (bytes: Slice<u8>) -> !none => {
	lock := STDOUT.lock()
	os_write(lock.fd, bytes)?
	ret none
}
```
   - **[6.4]** Provide process control APIs: spawn, wait, kill, exit codes, env vars, args.
     - **Plan:** Wrap platform spawn functions, manage pipes, return `ProcessHandle` with wait/kill operations. [ ]
     - **Pseudocode:**
```pn
spawn_process :: (spec: CommandSpec) -> !ProcessHandle => {
	setup_stdio(spec.stdio)?
	pid := os_spawn(spec.executable, spec.args, spec.env)?
	ret ProcessHandle { pid }
}
```
   - **[6.5]** Implement capability detection registry (mmap, signals, threads) using `detect_capabilities()` pseudocode.
     - **Plan:** Probe OS features at startup, cache results, expose query API for conditional code paths. [ ]
     - **Pseudocode:**
```pn
capabilities_init :: () -> CapabilityMap => {
	map := CapabilityMap::new()
	map.insert("mmap", os_supports("mmap"))
	map.insert("signals", os_supports("sigaction"))
	map.insert("threads", os_supports("pthread_create"))
	ret map
}
```
   - **[6.6]** Wrap memory-mapped file lifecycle (`mmap_file`, `munmap`).
     - **Plan:** Provide safe wrapper struct with Drop implementation releasing resources. [ ]
     - **Pseudocode:**
```pn
open_mmap :: (path: &Path, len: usize, mode: MmapMode) -> !MmapHandle => {
	fd := os_open(path, mode.flags)?
	ptr := os_mmap(fd, len, mode.prot)?
	ret MmapHandle { fd, ptr, len }
}
```
   - **[6.7]** Expose signal registration helpers with safe callbacks.
     - **Plan:** Provide API to register signals with closure capturing limited data, queue to async executor if necessary. [ ]
     - **Pseudocode:**
```pn
register_signal :: (sig: Signal, handler: SignalHandler) -> !none => {
	unsafe_register(sig, handler.raw_ptr())?
	ret none
}
```
   - **[6.8]** Supply monotonic timer, sleep API, interval scheduling utilities.
     - **Plan:** Create wrapper around `clock_gettime`, implement sleep with parking, add interval timer storing callbacks. [ ]
     - **Pseudocode:**
```pn
sleep :: (duration: Duration) -> none => {
	target := monotonic_now() + duration
	while monotonic_now() < target {
		os_nanosleep(target - monotonic_now())
	}
}
```
   - **[6.9]** Implement TCP/UDP socket wrappers with blocking + async integration stubs.
     - **Plan:** Wrap socket creation, provide read/write, integrate with executor via readiness notifications, and support non-blocking mode. [ ]
     - **Pseudocode:**
```pn
tcp_connect :: (addr: SocketAddr) -> !TcpStream => {
	fd := os_socket(AF_INET, SOCK_STREAM)?
	os_connect(fd, addr)?
	ret TcpStream { fd }
}
```

7. **Enable concurrency primitives** (`plans/concurrency-and-parallelism/plan.md`)
   - **[7.1]** Expose `spawn_thread`, join handles, and thread-local storage.
     - **Plan:** Wrap OS thread APIs, manage TLS map, propagate panics across join. [ ]
     - **Pseudocode:**
```pn
spawn_thread :: (entry: fn(Arg) -> (), arg: Arg) -> ThreadHandle => {
	stack := allocate_stack(DEFAULT_STACK)
	id := os_thread_create(entry, arg, stack)
	register_tls(id)
	ret ThreadHandle { id }
}
```
   - **[7.2]** Implement `Mutex`, `RwLock`, `Condvar`, reference-counted `Rc`/`Arc` semantics.
     - **Plan:** Build lock structs using atomics, integrate with parking lot, and ensure `Arc` uses atomic ref counts. [ ]
     - **Pseudocode:**
```pn
arc_clone :: (arc: &Arc<T>) -> Arc<T> => {
	arc.count.fetch_add(1, Ordering::Relaxed)
	ret Arc { ptr: arc.ptr, count: arc.count }
}
```
   - **[7.3]** Finalize atomic integer/boolean APIs with memory ordering enum.
     - **Plan:** Provide wrappers over platform atomics, support orderings (Relaxed, Acquire, Release, SeqCst). [ ]
     - **Pseudocode:**
```pn
atomic_compare_exchange :: (atom: &AtomicUsize, current: usize, new: usize, order: Ordering) -> bool => {
	ret os_cmpxchg(atom.ptr, current, new, order)
}
```
   - **[7.4]** Implement bounded MPMC channel per slot-state pseudocode.
     - **Plan:** Use ring buffer with atomic indices, implement blocking semantics via condition variables or async integration. [ ]
     - **Pseudocode:**
```pn
channel_push :: (chan: &BoundedChannel<T>, item: T) -> none => {
	loop {
		tail := chan.tail.load(Ordering::Acquire)
		slot := &chan.buffer<tail % chan.cap>
		if slot.state.compare_exchange(Empty, Writing) {
			slot.value.write(item)
			slot.state.store(Full, Ordering::Release)
			chan.tail.fetch_add(1, Ordering::AcqRel)
			signal_not_empty(chan)
			break
		}
		wait_not_full(chan)
	}
}
```
   - **[7.5]** Deliver async executor loop with task queue integration.
     - **Plan:** Implement task queue, poll tasks until completion, reschedule pending tasks, integrate timers. [ ]
     - **Pseudocode:**
```pn
executor_run :: (queue: &mut TaskQueue) -> none => {
	loop {
		task := queue.pop()
		match task {
			none => break,
			some(mut t) => {
				if t.poll() == Pending {
					queue.push(t)
				}
			}
		}
	}
}
```
   - **[7.6]** Add futures/async task waker primitives aligning with executor design.
     - **Plan:** Define `Waker` struct referencing queue, implement wake/wake_by_ref, and integrate with runtime registration. [ ]
     - **Pseudocode:**
```pn
create_waker :: (queue: TaskQueuePtr, task_id: TaskId) -> Waker => {
	ret Waker {
		wake: | | queue.push(task_id),
		wake_by_ref: | | queue.push(task_id),
	}
}
```

8. **Integrate serialization and CLI tooling** (`plans/serialization-and-parsing/plan.md`, `plans/meta-project-wide/plan.md`)
   - **[8.1]** Implement CLI parser handling flags, options, subcommands, and validation.
     - **Plan:** Build token iterator, support flag variations, validate required arguments, and emit helpful errors. [ ]
     - **Pseudocode:**
```pn
parse_cli :: (tokens: Vec<string>, spec: CliSpec) -> !ParsedArgs => {
	args := ParsedArgs::new()
	iter := tokens.iter()
	while iter.has_next() {
		token := iter.next()
		if token.starts_with("--") {
			handle_long_option(token, iter, spec, &mut args)?
		} else if token.starts_with("-") {
			handle_short_flags(token, iter, spec, &mut args)?
		} else {
			args.positionals.push(token)
		}
	}
	ret args
}
```
   - **[8.2]** Provide JSON encoder/decoder targeting compiler configuration and metadata files.
     - **Plan:** Implement streaming encoder/decoder, handle numbers, strings, arrays, objects, and integrate with `!` errors. [ ]
     - **Pseudocode:**
```pn
encode_json :: (value: JsonValue, writer: &mut Writer) -> !none => {
	match value {
		JsonValue::Null => writer.write("null")?,
		JsonValue::Bool(b) => writer.write(if b { "true" } else { "false" })?,
		JsonValue::Number(n) => writer.write(n.to_string())?,
		JsonValue::String(s) => write_string(writer, s)?,
		JsonValue::Array(arr) => write_array(writer, arr)?,
		JsonValue::Object(obj) => write_object(writer, obj)?,
	}
	ret none
}
```
   - **[8.3]** Add binary serialization for incremental compilation caches.
     - **Plan:** Design schema, implement endian-aware readers/writers, support versioning, and ensure checksum validation. [ ]
     - **Pseudocode:**
```pn
write_cache_record :: (writer: &mut BinaryWriter, record: CacheRecord) -> !none => {
	writer.write_u32(record.version)?
	writer.write_bytes(record.key)?
	writer.write_bytes(record.payload)?
	writer.write_u64(hash(record.payload))?
	ret none
}
```
   - **[8.4]** Integrate CLI and serialization error paths with `Error` trait.
     - **Plan:** Implement conversions to `Error`, provide context stacking, and ensure CLI errors produce exit codes. [ ]
     - **Pseudocode:**
```pn
cli_error :: (msg: string, code: i32) -> CliError => {
	ret CliError { message: msg, exit_code: code }
}

impl Error for CliError {
	source :: (&self) -> ?&dyn Error => none
}
```
   - **[8.5]** Feed logging/metrics events into governance dashboard per meta plan.
     - **Plan:** Emit JSON payloads for key events, push to metrics sink, and surface in dashboard aggregator. [ ]
     - **Pseudocode:**
```pn
record_metric :: (event: MetricsEvent) -> none => {
	payload := serde_json::to_string(event)
	METRICS_QUEUE.push(payload)
}
```

9. **Advance higher-level language features** (`plans/advanced-language-features/plan.md`, `plans/luxuries/plan.md`, `plans/security-and-cryptography/plan.md`)
   - **[9.1]** Implement function-like procedural macros and hygiene model.
     - **Plan:** Design token normalization, assign hygiene IDs, call host process, and parse output into AST. [ ]
     - **Pseudocode:**
```pn
apply_hygiene :: (tokens: TokenStream, scope: HygieneScope) -> TokenStream => {
	ret tokens.map(|t| t.with_scope(scope.fresh_id()))
}
```
   - **[9.2]** Extend macros to attribute/derive forms with incremental compilation support.
     - **Plan:** Register attribute hooks, transform AST nodes, ensure re-run triggers on file changes, and cache derived impls. [ ]
     - **Pseudocode:**
```pn
run_attribute_macro :: (attr: Attribute, item: AstNode) -> !AstNode => {
	ctx := MacroContext::new(attr, item)
	output := host_call(attr.path, serialize(ctx))
	ret parse_ast(output)?
}
```
   - **[9.3]** Introduce lazy evaluation primitives and adjust borrow checker accordingly.
     - **Plan:** Implement thunk type, enforce thread-safe initialization, and integrate with drop semantics. [ ]
     - **Pseudocode:**
```pn
force_thunk :: (thunk: &Thunk<T>) -> T => {
	if let Evaluated(value) = thunk.state.load(Ordering::Acquire) {
		ret value.clone()
	}
	lock := thunk.lock.lock()
	match thunk.state.load(Ordering::Relaxed) {
		Evaluated(value) => value.clone(),
		Pending => {
			value := (thunk.compute)()
			thunk.state.store(Evaluated(value.clone()), Ordering::Release)
			ret value
		},
	}
}
```
   - **[9.4]** Detect pure functions and add memoization cache per optimizer needs.
     - **Plan:** Static analyze AST for side effects, annotate pure functions, integrate with const-eval caches. [ ]
     - **Pseudocode:**
```pn
is_pure_fn :: (fn_id: FnId) -> bool => {
	attrs := get_fn_attrs(fn_id)
	if attrs.contains("pure") { ret true }
	return analyze_effects(fn_id).effects.is_empty()
}
```
   - **[9.5]** Deliver BigInt arithmetic (addition, multiplication, conversion) for literal parsing.
     - **Plan:** Implement limb-based representation, support addition/multiplication, and integrate with literal parser. [ ]
     - **Pseudocode:**
```pn
bigint_mul :: (a: BigInt, b: BigInt) -> BigInt => {
	result := BigInt::with_capacity(a.len + b.len)
	for i in 0:a.len {
		carry := 0
		for j in 0:b.len {
			prod := a[i] * b[j] + result<i + j> + carry
			result<i + j> = prod % BASE
			carry = prod / BASE
		}
		result<i + b.len> += carry
	}
	ret result.trim()
}
```
   - **[9.6]** Implement regex engine per NFA pseudocode and integrate with lexer diagnostics.
     - **Plan:** Build parser from regex AST to NFA, implement simulation with epsilon closures, and embed into lexer for diagnostics. [ ]
     - **Pseudocode:**
```pn
regex_match :: (nfa: Nfa, input: string) -> bool => {
	current := epsilon_closure({ nfa.start })
	for ch in input {
		next := Set::new()
		for state in current {
			next.union(epsilon_closure(move(state, ch)))
		}
		current = next
	}
	ret current.contains(nfa.accept)
}
```
   - **[9.7]** Provide QUIC/WebRTC scaffolding as future runtime extensions.
     - **Plan:** Define API surfaces, integrate with async runtime, stub handshake logic, and implement basic packet processing. [ ]
     - **Pseudocode:**
```pn
quic_send_packet :: (conn: &mut QuicConnection, payload: Slice<u8>) -> !none => {
	packet := build_quic_packet(conn.next_packet_number(), payload)
	encrypt_in_place(&mut packet, conn.keys)?
	udp_send(conn.socket, packet)?
	ret none
}
```
   - **[9.8]** Add AES/HMAC, secure randomness, and key management helpers for package signing.
     - **Plan:** Integrate AES/HMAC implementations, expose key store, and support deterministic signing workflows. [ ]
     - **Pseudocode:**
```pn
sign_payload :: (payload: Slice<u8>, key: &SigningKey) -> Signature => {
	hash := hmac_sha256(key.mac_key, payload)
	ret Signature::new(aes_encrypt(key.enc_key, hash))
}
```

10. **Maintain governance and visibility** (`plans/meta-project-wide/plan.md`, `plans/current-scope-snapshot/plan.md`, `plans/near-term-priorities/plan.md`)
    - **[10.1]** Publish project charter detailing portability, performance, safety goals.
      - **Plan:** Draft charter document, review with stakeholders, store in `docs/charter.md`, and link from README. [ ]
      - **Pseudocode:**
```pn
publish_charter :: (doc: Document) -> none => {
	write_file("docs/charter.md", doc.render())
	update_index("README.md", doc.summary())
}
```
    - **[10.2]** Update scope snapshot and dashboards after each milestone completion.
      - **Plan:** Integrate CI step updating `plans/current-scope-snapshot/plan.md`, refresh dashboard data, and notify team. [ ]
      - **Pseudocode:**
```pn
update_dashboard :: (milestone: MilestoneId, status: Status) -> none => {
	dashboard := load_dashboard()
	dashboard.update(milestone, status)
	save_dashboard(dashboard)
}
```
    - **[10.3]** Schedule quarterly roadmap reviews using `plans/near-term-priorities/plan.md` and adjust sequencing.
      - **Plan:** Create calendar events, gather metrics, prepare agenda referencing plan progress, and capture action items. [ ]
      - **Pseudocode:**
```pn
schedule_review :: (quarter: Quarter) -> Meeting => {
	agenda := collect_agenda_items(quarter)
	metrics := gather_metrics()
	ret Meeting::new(date_for_quarter(quarter), agenda, metrics)
}
```
    - **[10.4]** Track dependency completion state and reflect in `requirements.md` references.
      - **Plan:** Build dependency matrix generator, update `requirements.md` with status badges, and run weekly. [ ]
      - **Pseudocode:**
```pn
sync_requirements :: () -> none => {
	matrix := compute_dependency_matrix()
	content := render_requirements(matrix)
	write_file("requirements.md", content)
}
```

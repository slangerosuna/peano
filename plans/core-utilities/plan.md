# Core Utilities Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| UTF-8 strings helpers | [~] | Core operations implemented; builders missing. | Design builder/rope APIs, Unicode normalization, and encode/decode utilities. |
| Builders/rope | [ ] | No current support. | Develop rope structure to handle large text edits efficiently and expose incremental diff APIs. |
| Dynamic arrays (Vec) | [ ] | Planned via near-term priorities; partial `Vec_i64` exists. | Implement generic `Vec<T>` with push/pop/len/iter, capacity heuristics, and iterator trait impls. |
| Maps/Sets | [ ] | Absent. | Introduce hash map/set once hashing trait is defined, including load-factor tuning and iteration order guarantees. |
| Queues/stacks/deques | [ ] | Absent. | Define interfaces and back data structures with `Vec` or linked nodes; ensure lock-free variants align with concurrency plan. |
| slice_i64 helpers | [~] | Iteration and basic helpers exist. | Expand to generics and ensure safety invariants with borrow checker hooks. |
| Generic slices | [ ] | Not yet implemented. | Build generic slice type with constructors, iterator integration, and slicing syntax sugar. |
| Time utilities | [ ] | No clocks/sleep. | Provide monotonic/wall-clock APIs with platform abstraction and timer wheel integration. |
| PRNG utilities | [ ] | Absent. | Implement deterministic RNG and secure RNG (stretch) with seeding story. |
| File IO | [ ] | Not available. | Align with IO/platform plans to supply wrappers, buffered readers/writers, and path integration. |
| Paths | [ ] | Absent. | Define `Path` abstraction with conversions, normalization, and string interop. |
| Process utilities | [~] | `exit` available. | Add spawn/wait/kill and environment interfaces with streaming IO options. |
| Math utilities | [~] | Basic integer math; modulo via helper. | Add float math (pow, sqrt, trig), refine integer ops, vectorized helpers. |
| Diagnostics/devx | [ ] | Logging, assertions, test harness missing. | Coordinate with diagnostics plan in `plans/luxuries/plan.md` and surface structured logging macros. |
| Configuration/loading utilities | [ ] | No support. | Provide TOML/JSON config loader building atop serialization plan. |

## Near-Term Tasks
- **Stabilize generic `Vec<T>` design** leveraging forthcoming generics work.
- **Define file I/O API surface** in coordination with `plans/io-platform-and-networking/plan.md`.
- **Draft logging and diagnostics spec** to avoid divergence across standard library modules.
- **Prototype `Path` normalization** with cross-platform fixtures.
- **Evaluate RNG algorithm candidates** (XorShift, PCG, ChaCha-based) and document trade-offs.

## Milestones
- **M1 — Collections Core (3 weeks):** generic `Vec<T>`, slice generalization, iterator trait integration, baseline tests.
- **M2 — Text Tooling (3 weeks):** string builder, rope prototype, Unicode normalization utilities.
- **M3 — IO Utilities (4 weeks):** `Path`, file IO wrappers, buffered reader/writer, process spawn API.
- **M4 — Diagnostics & RNG (2 weeks):** logging facade, assertion extensions, deterministic RNG with seed APIs.

## Dependencies
- Hash trait from `plans/standard-traits/plan.md` for maps/sets.
- Platform abstractions from `plans/os-and-platform/plan.md` for clocks and process APIs.

## Pseudocode and Complexity

### `Vec<T>` Push/Pop
```pn
vec_push :: (vec: &mut Vec<T>, item: T) -> none => {
	reserve_additional(vec, 1)
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

reserve_additional :: (vec: &mut Vec<T>, additional: usize) -> none => {
	required := vec.len + additional
	if required <= vec.capacity {
		ret none
	}
	new_capacity := max(next_pow2(required), 4)
	new_data := allocate_buffer[T](new_capacity)
	copy_nonoverlapping(new_data, vec.data, vec.len)
	deallocate_buffer(vec.data)
	vec.data = new_data
	vec.capacity = new_capacity
}
```
- **Time:** `vec_push` amortized O(1); worst-case O(N) on growth. `vec_pop` O(1).
- **Space:** Growth allocates O(N) new buffer.

### Iterator Integration
```pn
impl Iterator<T> for VecIter<T> {
	next :: (&mut self) -> ?T => {
		if self.index == self.slice.len {
			ret none
		}
		value := self.slice[self.index]
		self.index += 1
		ret some(value)
	}
}
```
- **Time:** O(1) per `next` call.
- **Space:** O(1) iterator state.

### Hash Map Insert (open addressing)
```pn
map_insert :: (map: &mut HashMap<K, V>, key: K, value: V) -> none => {
	if load_factor(map) > 0.75 {
		resize(map)
	}
	mask := map.capacity - 1
	idx := hash(key) & mask
	first_tombstone := none
	loop {
		slot := &mut map.slots[idx]
		match slot.state {
			SlotState::Empty => {
				target := first_tombstone.unwrap_or(idx)
				map.slots[target] = Slot::occupied(key, value)
				map.size += 1
				ret none
			},
			SlotState::Tombstone => {
				if first_tombstone.is_none() {
					first_tombstone = some(idx)
				}
			},
			SlotState::Occupied(existing) => {
				if existing.key == key {
					existing.value = value
					ret none
				}
			}
		}
		idx = (idx + 1) & mask
	}
}
```
- **Time:** Expected O(1); worst-case O(N) with clustering.
- **Space:** O(N) for slots plus resizing buffer.

### Rope Concatenation
```pn
rope_concat :: (left: RopeNode, right: RopeNode) -> RopeNode => {
		left: left,
		right: right,
		weight: left.length,
	}
}
```pn
rope_index :: (node: RopeNode, index: i64) -> char => {
	current := node
	mut idx := index
	loop {
		match current.kind {
			RopeKind::Leaf(text) => ret text[idx],
			RopeKind::Branch(inner) => {
				if idx < inner.weight {
					current = inner.left
					idx -= inner.weight
					continue
				}
				current = inner.right
			}
		}
	}
}

### Path Normalization
```pn
normalize_path :: (path: Path) -> Path => {
	segments := []
	for segment in path.split('/') {
		match segment {
			"" | "." => continue,
			".." => { if segments.not_empty() { segments.pop() } },
			_ => segments.push(segment),
		}
	}
	ret Path::from_segments(path.is_absolute, segments)
}
```
- **Time:** O(S) where S number of path segments.
- **Space:** O(S) for normalized segment buffer.

### PRNG Next (XorShift example)
```pn
xor_shift32 :: (state: &mut u32) -> u32 => {
	x := *state
	x = x ^ (x << 13)
	x = x ^ (x >> 17)
	x = x ^ (x << 5)
	*state = x
	ret x
}
```
- **Time:** O(1) per number.
- **Space:** O(1) state.

### Buffered Reader Fill
```pn
fill_buf :: (reader: &mut BufReader) -> &[u8] => {
	if reader.pos == reader.cap {
		bytes_read := os_read(reader.fd, &mut reader.buf)
		reader.pos = 0
		reader.cap = bytes_read
		if bytes_read == 0 {
			ret []
		}
	}
	ret reader.buf[reader.pos:reader.cap]
}
```
- **Time:** O(1) per call; underlying read O(N) for bytes fetched.
- **Space:** O(B) buffer size retained.

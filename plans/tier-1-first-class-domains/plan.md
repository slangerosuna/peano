# Tier 1: First-class Domains Plan

## ECS / In-memory RDBMS
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Components/resources | [ ] | Design component/resource declarations, runtime storage metadata, and versioning story. |
| Columnar storage | [ ] | Implement columnar ECS storage with cache-friendly layouts and hot-swap archetypes. |
| O(1) entity access with stable IDs | [ ] | Provide allocator/handle system ensuring stable identifiers, generation counter enforcement. |
| Declarative queries with borrow safety | [ ] | Create query builder enforcing mutable/immutable borrowing guarantees and change detection. |
| System scheduling with conflict detection | [ ] | Build scheduler detecting borrow conflicts, enabling parallel execution, and providing instrumentation hooks. |
| Cross-thread safety | [ ] | Define thread-safe storage primitives, trait bounds, and send/sync equivalents. |
| Async systems execution model | [ ] | Align scheduler with async runtime; define task wake semantics and cancellation. |
| Deterministic replay | [ ] | Offer deterministic scheduler replay for debugging and netcode.
| Serialization bridge | [ ] | Integrate ECS data serialization with `plans/serialization-and-parsing/plan.md`.

## GPU / Graphics / Compute
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Peano-to-SPIR-V subset | [ ] | Define shader subset and implement compiler backend translation with validation layers. |
| Strongly-typed bindings | [ ] | Model descriptors/pipeline layouts with type-safe APIs and reflection data. |
| Vulkan abstraction | [ ] | Create portable runtime over Vulkan (and future backends) with graceful fallback. |
| Resource lifecycle management | [ ] | Provide buffer/image/sampler abstractions with lifetime tracking and leak detection. |
| Synchronization primitives | [ ] | Implement barriers, semaphores, fences with safe defaults and validation passes. |
| Matrix/vector GPU interop | [ ] | Align numeric types with GPU layouts and std140 equivalents; provide conversion helpers. |
| CPU-GPU data transfer utilities | [ ] | Supply staging buffer helpers, async transfer workflows, and profiling instrumentation. |
| Shader hot-reload | [ ] | Enable live shader reloading for rapid iteration.

## Web / Networking
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| HTTP stack | [ ] | Implement HTTP/1.1 first, then layer HTTP/2/3 support with observability hooks. |
| HTTPS/TLS | [ ] | Integrate TLS libraries with portable abstraction and certificate automation. |
| Routing primitives | [ ] | Create declarative routing DSL, middleware support, and typed request extractors. |
| WebSockets | [ ] | Build ws server and client interfaces consistent with async model and backpressure. |
| WASM target | [ ] | Define WASM codegen path, stdlib compatibility surface, and host bindings. |
| Edge deployment | [ ] | Provide lightweight runtime mode for edge/IoT, aligning with OS plan. |

## Matrices / Numerics
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Fixed-size matrices/vectors | [ ] | Establish core math types with generic dimensions and constexpr validation. |
| n-D arrays | [ ] | Explore optional multi-dimensional array support built over slices with stride metadata. |
| Matrix operations | [ ] | Implement BLAS-like operations, operator overloading, and auto-vectorization. |
| Determinant/inverse/transpose | [ ] | Provide algorithms optimized for small fixed sizes with tolerance controls. |
| Decompositions (stretch) | [ ] | Target LU/QR/SVD as advanced extensions with fallback precision. |
| CPU-optimized kernels | [ ] | Integrate SIMD/intrinsics; plan GPU offload hooks in tandem with GPU roadmap.
| Automatic differentiation | [ ] | Evaluate forward/reverse-mode autodiff support as stretch goal.

## Milestones
- **M1 — ECS Core (4 weeks):** component metadata, stable handles, query builder prototype, deterministic replay MVP.
- **M2 — GPU Foundations (5 weeks):** shader subset, typed bindings, resource lifecycle manager, hot-reload pipeline.
- **M3 — Web/Networking Stack (4 weeks):** HTTP stack MVP, routing DSL, TLS abstraction, WebSocket integration.
- **M4 — Numerics Expansion (4 weeks):** matrix library core, BLAS wrappers, autodiff feasibility study, GPU interop tests.

## Pseudocode and Complexity

### ECS Stable Entity Handles
```pn
create_entity :: (world: &mut World) -> EntityHandle => {
	id := if world.free_ids.is_empty() {
		index := world.entities.len
		world.entities.push(EntitySlot::Empty)
		index
	} else {
		world.free_ids.pop()
	}
	generation := world.generations[id]
	ret EntityHandle { id: id, generation: generation }
}

destroy_entity :: (world: &mut World, handle: EntityHandle) -> none => {
	if world.generations[handle.id] ~= handle.generation {
		ret
	}
	world.generations[handle.id] = world.generations[handle.id] + 1
	world.free_ids.push(handle.id)
}
```
- **Time:** O(1) per create/destroy.
- **Space:** O(N) for entity arrays and free-id stack.

### ECS Columnar Query Iteration
```pn
query :: (world: &World, component_set: ComponentMask) -> Iterator[QueryRow] => {
	matching := world.archetypes.filter((arch) => arch.contains_all(component_set))
	ret Iterator::new((yield_fn) => {
		for archetype in matching {
			for row in 0:archetype.len {
				yield_fn(archetype.extract_row(component_set, row))
			}
		}
	})
}
```
- **Time:** O(E_q) where E_q is number of matching entities.
- **Space:** O(1) auxiliary iteration state.

### GPU Pipeline Creation
```pn
create_pipeline :: (device: &GpuDevice, shader_modules: [ShaderModule], layout_desc: PipelineLayoutDesc) -> Pipeline => {
	layout := device.create_pipeline_layout(layout_desc)
	pipeline := device.create_graphics_pipeline({
		layout: layout,
		shaders: shader_modules,
		render_state: default_state(),
	})
	ret pipeline
}
```
- **Time:** O(S) where S cost of validating shader modules and descriptors (driver dependent).
- **Space:** O(1) client-side metadata; GPU driver allocates pipeline objects.

### HTTP Request Handling
```pn
handle_connection :: (conn: &mut TcpStream) -> none => {
	buffer := read_until_double_crlf(conn)
	request := parse_http_request(buffer)
	response := route_request(request)
	write_response(conn, response)
}
```
- **Time:** O(H) parsing cost proportional to header size; routing cost depends on handler.
- **Space:** O(H) to store request buffer.

### Matrix Multiplication (Naïve)
```pn
mat_mul :: (A: Matrix[m, k], B: Matrix[k, n]) -> Matrix[m, n] => {
	C := zero_matrix(m, n)
	for i in 0:m {
		for j in 0:n {
			sum := 0
			for t in 0:k {
				sum = sum + A[i][t] * B[t][j]
			}
			C[i][j] = sum
		}
	}
	ret C
}
```
- **Time:** O(m · n · k).
- **Space:** O(m · n) for result matrix (additional beyond inputs).

### Deterministic Replay Scheduler
```pn
replay_schedule :: (trace: ScheduleTrace, world: &mut World) -> none => {
	for event in trace.events {
		apply_event(world, event)
		while world.pending_tasks().not_empty() {
			task := world.pending_tasks().pop_front()
			task.run()
		}
	}
}
```
- **Time:** O(E + T) where E events, T tasks per event.
- **Space:** O(T) for pending task queue.

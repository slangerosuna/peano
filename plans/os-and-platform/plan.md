# OS and Platform Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| File locking | [ ] | No abstractions today. | Determine cross-platform API surface, including advisory vs. mandatory locks, and document deadlock detection guidance. |
| Shared memory / memory-mapped files | [ ] | Functionality absent. | Implement shared memory primitives with safety wrappers, lifetime management, and cross-process synchronization. |
| Signals/interrupt handling | [ ] | No support. | Provide signal registration API with safe handler patterns, masking, and deferred execution queue. |
| Cross-platform filesystem/network abstractions | [ ] | Undefined. | Design capability matrix, fallback behaviors, and compile-time feature flags for unsupported platforms. |
| Freestanding runtime entrypoint | [~] | `_start` mode exists; driver-controlled toggle. | Formalize freestanding configuration options, document ABI expectations, and ensure panic/error pathways. |
| Dynamic loader integration | [ ] | Need to support dynamic libraries/plugins. | Define `dlopen`/`dlsym` wrappers and capability detection. |
| Platform configuration registry | [ ] | No centralized config store. | Build registry for per-platform overrides consumed by compiler/runtime. |

## Near-Term Tasks
- **Standardize platform capability detection** to gate APIs at compile time.
- **Document runtime entry expectations** for hosted vs. freestanding targets.
- **Coordinate with `plans/io-platform-and-networking/plan.md`** to ensure consistent abstractions across filesystem/network modules.
- **Prototype file locking facade** with advisory locks on Linux/macOS.
- **Draft signal safety guidelines** covering reentrancy and async-signal-safe APIs.

## Milestones
- **M1 — Capability Baseline (2 weeks):** platform registry, capability matrix, compile-time gating.
- **M2 — Memory & Signals (3 weeks):** shared memory primitives, signal handling API with tests.
- **M3 — Runtime Modes (2 weeks):** freestanding/hosted docs, panic strategy alignment, loader configuration.
- **M4 — Dynamic Integration (2 weeks):** dynamic loader wrappers, plugin discovery hooks.

## Dependencies
- Runtime configuration hooks from `plans/runtime-errors-and-panics/plan.md`.
- Concurrency primitives from `plans/concurrency-and-parallelism/plan.md` for shared memory synchronization.

## Pseudocode and Complexity

### Capability Detection
```pn
detect_capabilities :: () -> CapabilityMap => {
	caps := CapabilityMap::new()
	caps["mmap"] = os_supports("mmap")
	caps["signals"] = os_supports("signals")
	caps["threads"] = os_supports("threads")
	caps["dlopen"] = os_supports("dlopen")
	caps["timerfd"] = os_supports("timerfd")
	ret caps
}
```
- **Time:** O(F) for number of feature probes.
- **Space:** O(F) to store capability map.

### Memory-mapped File Open
```pn
mmap_file :: (path: Path, length: i64, mode: MMapMode) -> MMapHandle => {
	fd := os_open(path, mode)
	ptr := os_mmap(fd, length, mode)
	ret MMapHandle { fd: fd, ptr: ptr, length: length }
}

munmap :: (handle: MMapHandle) -> none => {
	os_munmap(handle.ptr, handle.length)
	os_close(handle.fd)
}
```
- **Time:** O(1) for mapping/unmapping (OS dependent).
- **Space:** O(1) auxiliary; maps `length` bytes into address space.

### Signal Registration
```pn
register_signal :: (signal: Signal, handler: SignalHandler) -> SignalHandler => {
	old := os_signal(signal, handler)
	ret old
}
```
- **Time:** O(1) per registration.
- **Space:** O(1) auxiliary.

### Platform Registry Lookup
```pn
platform_config :: (target: TargetTriple, key: string) -> ?ConfigValue => {
	registry := load_platform_registry()
	if registry.contains(target, key) {
		ret some(registry.get(target, key))
	}
	parent := registry.find_parent(target)
	match parent {
		none => ret none,
		some(p) => ret platform_config(p, key),
	}
}
```
- **Time:** O(L) where L number of fallback levels.
- **Space:** O(1) auxiliary; registry stored globally.

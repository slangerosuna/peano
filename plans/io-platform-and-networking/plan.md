# IO, Platform, and Networking Plan

## Requirements Snapshot
| Requirement | Status | Notes | Planned Actions |
| --- | --- | --- | --- |
| File system APIs | [ ] | Lacking open/read/write/seek/delete/metadata wrappers. | Define `File` abstraction with buffered and unbuffered modes; design error taxonomy and lifetime tracking. |
| Standard streams | [~] | Basic stdout wired via `printf`/`puts`; stdin/stderr missing. | Implement standard stream handles with buffered IO, error propagation, and line buffering controls. |
| Signals/interrupts | [ ] | No support today. | Introduce signal handling abstraction with safe callbacks and deferred execution queues. |
| Shared memory & mmap | [ ] | Missing primitives. | Provide memory-mapped file APIs, copy-on-write support, and synchronization helpers. |
| Cross-platform abstraction | [ ] | Not yet defined. | Establish capability flags, OS-specific backends, and fallback shims for portability. |
| Networking primitives | [ ] | No TCP/UDP support. | Implement socket APIs with async integration, TLS hooks, and DNS resolution. |
| Time utilities | [ ] | Absent. | Provide wall-clock/monotonic timers, sleep, interval scheduling, and timer wheel integration. |
| Process management | [ ] | Spawn/exec missing. | Add process builder, environment management, and IPC pipes. |
| Platform capability reporting | [ ] | Need structured reporting for feature detection. | Emit `PlatformReport` used by higher-level plans. |

## Near-Term Tasks
- **Specify file and path API** to unblock stdlib development in `plans/core-utilities/plan.md`.
- **Prototype TCP client/server** surfaces aligned with future async runtime.
- **Integrate platform detection** to report unsupported features gracefully.
- **Model signal handling safety** with runtime hooks and document reentrancy guarantees.
- **Draft process builder RFC** capturing spawn/exec, environment, and stdio redirection with `!` ergonomics.

## Milestones
- **M1 — Filesystem Foundations (3 weeks):** `File` abstraction, metadata API, buffered reader/writer, capability error taxonomy.
- **M2 — Signals & Timing (2 weeks):** signal registration, timer APIs, integration tests on Linux/macOS.
- **M3 — Networking MVP (4 weeks):** TCP/UDP sockets, DNS resolver, async integration, TLS stub.
- **M4 — Process & IPC (3 weeks):** process builder, pipes, shared memory primitives, cross-platform reporting.

## Dependencies
- Error handling from `plans/runtime-errors-and-panics/plan.md`.
- Async/concurrency primitives from `plans/concurrency-and-parallelism/plan.md`.

## Pseudocode and Complexity

### File Read (Buffered)
```pn
file_read :: (file: &File, buffer: &mut [u8], len: i64) -> i64 => {
	total := 0
	while total < len {
		bytes := os_read(file.fd, buffer + total, len - total)
		if bytes == 0 {
			break
		}
		total = total + bytes
	}
	ret total
}
```
- **Time:** O(N) for N bytes read.
- **Space:** O(1) auxiliary aside from provided buffer.

### Capability Report Generation
```pn
probe_platform :: () -> PlatformReport => {
	report := PlatformReport::default()
	report.filesystem = os_supports("openat") && os_supports("fstat")
	report.sockets = os_supports("socket")
	report.mmap = os_supports("mmap")
	report.signals = os_supports("sigaction")
	report.high_res_timer = os_supports("clock_gettime")
	ret report
}
```
- **Time:** O(F) where F number of feature probes.
- **Space:** O(1) report storage.

### TCP Accept Loop
```pn
serve_tcp :: (listener: &TcpListener, handler: (TcpStream) -> none) -> none => {
	loop {
		socket := listener.accept()
		spawn_thread(handler, socket)
	}
}
```
- **Time:** Accept O(1) per connection plus handler cost.
- **Space:** O(1) per loop; total resource footprint depends on active connections.

### Async DNS Resolution (stub)
```pn
resolve_host :: (host: string, cache: &mut DnsCache) -> Future<!IpAddr> => {
	ret Future::new(move || {
		if let some(entry) = cache.lookup(host) {
			ret entry
		}
		req := build_dns_query(host)
		reply := await send_udp_request(DNS_SERVER, req)?
		parsed := parse_dns_reply(reply)?
		cache.insert(host, parsed.address, parsed.ttl)
		ret parsed.address
	})
}
```
- **Time:** O(R) where R network round-trip latency plus parsing cost.
- **Space:** O(1) auxiliary; cache storage O(H) entries.

### Timer Sleep (Monotonic Clock)
```pn
sleep :: (duration: Duration) -> none => {
	target := monotonic_now() + duration
	while monotonic_now() < target {
		os_park_thread(target - monotonic_now())
	}
}
```
- **Time:** O(1) overhead; dominated by OS scheduler wake latency.
- **Space:** O(1) auxiliary.

### Process Spawn Pipeline
```pn
spawn_process :: (cmd: CommandSpec) -> !ProcessHandle => {
	setup_stdio(cmd.stdio)?
	child_env := build_env(cmd.env)
	pipes := allocate_pipes(cmd.ipc_channels)?
	pid := os_spawn(cmd.executable, cmd.args, child_env, pipes)?
	ret ProcessHandle { pid: pid, pipes: pipes }
}
```
- **Time:** O(A + E) where A argument count, E environment size; OS spawn dominates.
- **Space:** O(P) for pipe descriptors.

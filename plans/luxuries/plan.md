# Luxuries Plan

## Advanced Data Structures
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| BigInt / arbitrary precision | [ ] | Implement arbitrary-precision integers with performant algorithms (Karatsuba, FFT) and FFI hooks for acceleration; add constant folding hooks. |
| Bignum rational/decimal | [ ] | Design rational/decimal types for financial computations with exactness guarantees and configurable rounding policies. |
| Bitsets | [ ] | Provide dense/sparse bitset structures with set operation APIs, SIMD acceleration, and serialization support. |
| Graph utilities | [ ] | Develop graph data structures, traversal algorithms, and graph-specific iterators with concurrency-safe traversal options. |

## Text and Encoding
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Regex engine | [ ] | Build RE2-like engine with linear-time guarantees, capture groups, and compile-time optimization. |
| Encoding/decoding utilities | [ ] | Support UTF-16, Latin-1, and conversion helpers with streaming adapters. |
| String builder/rope | [ ] | Coordinate with `plans/core-utilities/plan.md` to deliver efficient text manipulation structures and editor-friendly APIs. |

## Serialization / Parsing
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| JSON/XML/binary serialization | [ ] | Extend base serialization traits with advanced formats, schema validation, and streaming encoders. |
| RDBMS serialization integration | [ ] | Map ECS/DB types to serialization formats seamlessly; support migrations. |
| CLI argument parsing | [ ] | Expand base CLI helpers with derive macros for ergonomics and shell completion generation. |

## Safety & Utility
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Assertions/debugging utilities | [ ] | Provide richer debugging macros, conditional compilation flags, and telemetry toggles. |
| Logging framework | [ ] | Design structured logging with sinks, filters, async writers, and correlation IDs. |
| Testing framework | [ ] | Implement in-language test harness with assertions, property testing, and reporting. |

## Cybersecurity
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Encryption algorithms | [ ] | Expand security primitives with higher-level protocols, key rotation tooling, and compliance guidance. |

## Networking (Extended)
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Advanced HTTP stack | [ ] | Layer caching, middleware, load balancing, and observability hooks. |
| HTTPS | [ ] | Strengthen TLS capabilities with certificate management, ACME, and session resumption. |
| Routing utilities | [ ] | Provide high-level router DSL, plugin ecosystem, and typed extractors. |
| QUIC | [ ] | Implement QUIC transport aligned with async runtime and congestion control tuning. |
| WebRTC | [ ] | Deliver media/data channels with NAT traversal support and codec strategy. |
| Sync ECS resources | [ ] | Build real-time synchronization layer atop network primitives with delta compression. |

## Milestones
- **M1 — Advanced Numerics (4 weeks):** BigInt core, rational prototype, vectorized bitsets, initial benchmarks.
- **M2 — Developer Ergonomics (3 weeks):** regex engine MVP, encoding adapters, string builder integration.
- **M3 — Diagnostics & Testing (3 weeks):** logging framework, assertion macros, property-testing harness.
- **M4 — Networking Luxuries (4 weeks):** HTTP stack layer, QUIC draft, WebRTC signaling prototype.

## Pseudocode and Complexity

### BigInt Addition (Limb-based)
```pn
bigint_add :: (a: BigInt, b: BigInt) -> BigInt => {
	carry := 0
	result := BigInt::new()
	for i in 0:max(len(a), len(b)) {
		sum := limb(a, i) + limb(b, i) + carry
		result[i] = sum % BASE
		carry = sum / BASE
	}
	if carry ~= 0 {
		result.push(carry)
	}
	ret result
}
```
- **Time:** O(n) where n number of limbs.
- **Space:** O(n) for result.

### Regex NFA Simulation
```pn
regex_match :: (nfa: Nfa, input: string) -> bool => {
	current := epsilon_closure({ nfa.start })
	for ch in input {
		next_states := Set[NfaState]()
		for state in current {
			next_states.union_in_place(epsilon_closure(move(state, ch)))
		}
		current = next_states
	}
	ret current.contains(nfa.accept)
}
```
- **Time:** O(|input| · |states|) in worst-case.
- **Space:** O(|states|) to store active set.

### Logging Framework (Async Sink)
```pn
log :: (level: LogLevel, message: string) -> none => {
	entry := LogEntry {
		level: level,
		timestamp: timestamp_now(),
		message: message,
	}
	LOG_QUEUE.push(entry)
}

logging_worker :: (queue: &LogQueue, sink: &mut LogSink) -> none => {
	while running() {
		entry := queue.pop()
		sink.write(format(entry))
	}
}
```
- **Time:** `log` O(1) enqueue; worker throughput depends on sink latency.
- **Space:** O(N) bounded by queue size.

### Property-based Test Runner
```pn
run_property :: (property: PropertyFn, generator: Generator) -> TestResult => {
	for case in 0:MAX_CASES {
		input := generator.next()
		result := property(input)
		if result == TestResult::Fail {
			shrunk := shrink(input, generator)
			ret TestResult::FailWith(shrunk)
		}
	}
	ret TestResult::Pass
}
```
- **Time:** O(C · P) where C number of cases, P cost per property evaluation.
- **Space:** O(1) aside from shrink trail.

### QUIC Packet Encryption (High-level)
```pn
quic_encrypt :: (packet: QuicPacket, key: Key, nonce: Nonce, packet_number: u64) -> QuicPacket => {
	ad := build_aead_associated_data(packet_number, headers(packet))
	ciphertext := aead_seal(key, nonce, ad, packet.payload)
	packet.payload = ciphertext
	ret packet
}
```
- **Time:** O(P) where P payload length (depends on AEAD implementation).
- **Space:** O(P) for ciphertext output.

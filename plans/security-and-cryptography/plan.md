# Security and Cryptography Plan

## Requirements Snapshot
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| Common encryption algorithms | [ ] | Identify priority algorithms (AES, ChaCha20, RSA, ECC) and determine implementation approach (pure Peano vs. FFI bindings). |
| Hashing & MACs | [ ] | Establish SHA-2, SHA-3, BLAKE3 implementations with incremental APIs and MAC integration. |
| Key management | [ ] | Provide key generation, storage, and rotation utilities with secure randomness requirements. |
| TLS integration | [ ] | Define TLS abstraction layer leveraging platform or bundled implementations. |
| Secure randomness | [ ] | Expose CSPRNG interface backed by OS entropy or hardware RNG. |
| Audit & compliance tooling | [ ] | Supply linting/check tools ensuring cryptographic best practices. |

## Near-Term Tasks
- **Define cryptography API surfaces** including key management, streaming modes, and error handling.
- **Evaluate dependency strategy** (stdlib vs. optional modules) with security review process.
- **Catalog regulatory requirements** (FIPS, GDPR) influencing implementation strategy.
- **Prototype RNG facade** backed by platform entropy sources.

## Milestones
- **M1 — Foundations (3 weeks):** RNG facade, hashing primitives, error taxonomy, API sketches.
- **M2 — Symmetric & Auth (4 weeks):** AES/ChaCha implementations, HMAC integration, streaming cipher modes.
- **M3 — Key Management (3 weeks):** key storage abstraction, rotation hooks, compliance checklist.
- **M4 — TLS & Auditing (4 weeks):** TLS layer abstraction, configuration validation tool, documentation pass.

## Dependencies
- Error/reporting infrastructure from `plans/runtime-errors-and-panics/plan.md`.
- Platform primitives for secure randomness from `plans/io-platform-and-networking/plan.md`.

## Pseudocode and Complexity

### AES-CTR Encrypt Block
```pn
aes_ctr_encrypt :: (key: AesKey, nonce: Nonce, plaintext: [u8]) -> [u8] => {
	counter := 0
	ciphertext := []
	while !plaintext.is_empty() {
		keystream := aes_encrypt_block(key, nonce || counter)
		block := plaintext.take(16)
		ciphertext.push(block ^ keystream)
		counter = counter + 1
	}
	ret ciphertext
}
```
- **Time:** O(B) where B number of 16-byte blocks.
- **Space:** O(1) additional aside from output buffer.

### HMAC-SHA256
```pn
hmac_sha256 :: (key: [u8], message: [u8]) -> [u8] => {
	mutable_key := if len(key) > BLOCK_SIZE {
		sha256(key)
	} else {
		key
	}
	if len(mutable_key) < BLOCK_SIZE {
		mutable_key = pad(mutable_key, BLOCK_SIZE)
	}
	o_key_pad := mutable_key ^ 0x5c5c...
	i_key_pad := mutable_key ^ 0x3636...
	ret sha256(o_key_pad || sha256(i_key_pad || message))
}
```
- **Time:** O(K + M) where K key length, M message length.
- **Space:** O(1) extra buffers.

### Key Rotation Scheduler
```pn
rotate_keys :: (store: &KeyStore, policy: RotationPolicy) -> none => {
	for entry in store.list_keys() {
		if policy.is_expired(entry.metadata) {
			new_key := generate_key(entry.algorithm, policy.security_level)
			store.replace(entry.id, new_key)
			log_rotation(entry.id, current_time())
		}
	}
}
```
- **Time:** O(N) where N keys managed; key generation cost depends on algorithm.
- **Space:** O(1) auxiliary per iteration.

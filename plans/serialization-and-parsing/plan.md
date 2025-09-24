# Serialization and Parsing Plan

## Requirements Snapshot
| Requirement | Status | Planned Actions |
| --- | --- | --- |
| JSON/XML/binary formats | [ ] | Define serialization traits and runtime adapters targeting core formats with streaming support. |
| CLI argument parsing helpers | [ ] | Design parsing utilities for command-line interfaces with validation and auto-complete metadata. |
| Schema definition & validation | [ ] | Introduce schema language for struct validation and versioning. |
| Derive macros for serialization | [ ] | Provide proc-macro-like derives once macro system lands. |
| Binary protocol toolkit | [ ] | Offer readers/writers for endian-aware binary formats with zero-copy slices. |

## Near-Term Tasks
- **Outline serialization trait interfaces** to align with `!`/`Error` ergonomics.
- **Survey existing AST tooling** to enable reuse for binary/text parsing helpers.
- **Draft schema evolution strategy** covering version bumps and compatibility checks.
- **Prototype CLI parser DSL** integrating with standard traits.

## Milestones
- **M1 — Trait Foundations (3 weeks):** base serialization traits, error types, JSON encoder/decoder skeleton.
- **M2 — Schema & CLI (2 weeks):** schema draft implementation, CLI parser DSL with validation hooks.
- **M3 — Binary Toolkit (3 weeks):** endian-aware readers/writers, zero-copy utilities, stress tests.
- **M4 — Derive & Integration (3 weeks):** derive macros (pending macro infra), schema validation in CI, documentation.

## Dependencies
- Error handling support from `plans/runtime-errors-and-panics/plan.md`.
- Standard trait implementations from `plans/standard-traits/plan.md` (e.g., `IntoIterator`, `Display`).

## Pseudocode and Complexity

### JSON Encoding (Object)
```pn
encode_json_object :: (map: Map[string, JsonValue]) -> string => {
	buffer := "{"
	first := true
	for (key, value) in map {
		if ~first {
			buffer = buffer + ","
		}
		buffer = buffer + quote_string(key)
		buffer = buffer + ":"
		buffer = buffer + encode_json_value(value)
		first = false
	}
	buffer = buffer + "}"
	ret buffer
}
```
- **Time:** O(M) where M total characters emitted.
- **Space:** O(M) for output buffer.

### Recursive Descent Parser (CLI Args)
```pn
parse_args :: (tokens: [string], spec: CliSpec) -> !ParsedArgs => {
	args := ParsedArgs::new()
	iter := tokens.iter()
	while iter.has_next() {
		token := iter.next()
		if token.starts_with("--") {
			option := lookup(spec, token)
			if option.takes_value {
				value := iter.next()?
				args.insert(option.name, value)
			} else {
				args.insert(option.name, "true")
			}
		} else {
			args.positionals.push(token)
		}
	}
	ret args
}
```
- **Time:** O(T) where T number of tokens.
- **Space:** O(T) to store parsed arguments.

### Binary Reader (Endian-aware)
```pn
read_u32 :: (reader: &mut BinaryReader, endian: Endian) -> !u32 => {
	bytes := reader.consume(4)?
	match endian {
		Endian::Little => ret u32_from_le(bytes)
		Endian::Big => ret u32_from_be(bytes)
	}
}
```
- **Time:** O(1) per read.
- **Space:** O(1) auxiliary.

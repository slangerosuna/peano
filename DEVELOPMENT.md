# Development

This repo includes simple developer tools for a smoother workflow.

## Prereqs
- Rust toolchain (rustup, cargo)
- LLVM installed and discoverable. If you use a non-system LLVM, copy `.env.example` to `.env` and set the appropriate `LLVM_SYS_*_PREFIX` variable to your LLVM install prefix.

## Quick start
```bash
cp -n .env.example .env # then edit if needed
make setup
make build
make run
make test
make fmt  # fmt + (optional) clippy
```

You can also call the scripts directly:

- `./scripts/build.sh`
- `./scripts/run.sh [-- ARGS]`
- `./scripts/test.sh [-- TEST_FILTERS]`
- `./scripts/fmt.sh` (runs rustfmt and clippy if available)

## Environment
The scripts will source `.env` if present. Use it to configure LLVM and toolchain discovery, for example:

```bash
export LLVM_SYS_39_PREFIX="$HOME/llvm-3.9.0"
export PATH="$HOME/llvm-3.9.0/bin:$PATH"
export LD_LIBRARY_PATH="$HOME/llvm-3.9.0/lib:${LD_LIBRARY_PATH:-}"
```

On newer llvm-sys crates the env var uses the crate's versioned name, e.g. `LLVM_SYS_191_PREFIX`. (make sure to also update the features in `Cargo.toml`)

## VS Code tasks
Tasks are provided under `.vscode/tasks.json` to build, run, test, and format from the Command Palette.

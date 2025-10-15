# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Repository Overview

Buck2 is a fast, hermetic, multi-language build system written in Rust. This is
Meta's internal version of Buck2, located at `fbcode/buck2` within the fbsource
monorepo.

## Building and Development

### Validate changes

**ALWAYS** run this after changing files in `app/` or `fbcode/buck2/app/`:

```bash
arc rust-check fbcode//buck2/app/...
```

**Using Buck2 (self-bootstrap):**

```bash
./buck2.py           # Compile and run local buck2 binary
```

Follow by normal buck2 commands, e.g. `./buck2.py build fbcode//buck2:buck2` to
using local changed buck2 binary to build buck2

### Testing

Buck2 has extensive test suites located in the `tests/` directory:

**tests/core/** - Core integration tests

- Tests for individual Buck2 subsystems and features
- Covers: analysis, audit commands, build system, BXL, configurations, DICE,
  query language, etc.

**tests/e2e/** - End-to-end tests

- Full workflow tests that exercise Buck2 as users would
- Tests for: audit, build, BXL scripts, configurations, test command, etc.

**Running tests:**

```bash
# Run all tests in a Python file (e.g., tests/core/analysis/test_cmd_args.py)
buck2 test fbcode//buck2/tests/core/analysis:test_cmd_args

# Run a specific test function within a Python file
# (e.g., test_output_artifact_in_relative_to in tests/core/analysis/test_cmd_args.py)
buck2 test fbcode//buck2/tests/core/analysis:test_cmd_args -- test_output_artifact_in_relative_to

# Run all tests in a directory
buck2 test fbcode//buck2/tests/core/analysis/...
```

## Code Architecture

### Major Components

**app/** - Main Buck2 application code

- `buck2` - Main binary entry point
- `buck2_client` - Client-side CLI handling
- `buck2_server` - Server/daemon implementation
- `buck2_server_commands` - Server command implementations
- `buck2_build_api` - Core build system APIs
- `buck2_interpreter` - Starlark interpreter integration
- `buck2_execute` - Action execution framework
- `buck2_query` - Query language implementation
- `buck2_node` - Build graph node representation
- `buck2_artifact` - Artifact handling
- `buck2_bxl` - Buck Extension Language (BXL) support
- `buck2_test` - Test runner framework

**dice/** - Incremental computation engine

- DICE (Deterministic Incremental Computation Engine) powers Buck2's incremental
  builds
- Handles dependency tracking and change detection
- `dice/dice` - Core DICE implementation
- `dice/dice_error` - Error types

**starlark-rust/** - Starlark language implementation

- Buck2 uses Starlark (a Python-like language) for build file definitions
- `starlark` - Core language implementation
- `starlark_lsp` - Language Server Protocol support
- `starlark_syntax` - Parser and syntax tree
- `starlark_map` - Optimized map data structure

**prelude/** - Standard build rules

- Contains the same prelude code used internally at Meta
- Default build rules for various languages (C++, Rust, Python, etc.)
- Platform configurations

**gazebo/** - Utility libraries

- `gazebo` - General utilities with `str_pattern_extensions`
- `dupe` - Cheap cloning trait for reference-counted types
- `strong_hash` - Type-safe hashing

**shed/** - Additional utility crates

- `static_interner` - String interning
- `lock_free_hashtable` - Lock-free concurrent data structures
- `provider` - Provider pattern implementations

**remote_execution/** - Remote execution client

- Implements Remote Execution API for distributed builds
- OSS version differs from internal Meta version

**superconsole/** - Terminal UI

- Rich terminal output and progress display

### Key Concepts

**Buck Extension Language (BXL):**

- Allows self-introspection of the build system
- Used for automation tools, LSPs, and compilation databases
- Can inspect and run actions in the build graph

**Multi-language Support:**

- Language-agnostic core with scriptable rule definitions
- Users can implement language support in Starlark
- Support for dependencies across languages

## Coding Conventions

- Follow standard `rustfmt` conventions
- Use `gazebo` utilities, especially `dupe` trait
- Prefer `to_owned` over `.to_string()` for `&str` to `String`
- Use `derivative` library for `PartialEq`/`Hash` when ignoring fields
- Prefer `use crate::foo::bar` over `use super::bar`
- Modules should have either submodules OR types/functions, not both

## Internal vs OSS Differences

- Some code uses `@oss-enable` or `@oss-disable` markers
- `is_open_source()` function controls configuration differences
- Internal RE client differs from OSS version
- Internal version has additional Meta-specific integrations (Scribe, etc.)

## Protobuf Handling

Buck2 uses Protocol Buffers extensively. On Linux/macOS/Windows, prebuilt
`protoc` binaries are used automatically.

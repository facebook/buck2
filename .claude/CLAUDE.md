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

## Error Handling

Buck2 uses a custom error handling system via `buck2_error` instead of `anyhow`.
All error handling in Buck2 should follow these patterns:

### Result Type

Always use `buck2_error::Result<T>` instead of `anyhow::Result<T>`:

```rust
fn my_function() -> buck2_error::Result<String> {
    // ...
}
```

### Defining Custom Error Types

Use `#[derive(Debug, buck2_error::Error)]` instead of `thiserror::Error`. Every
error must be tagged with an `ErrorTag`:

```rust
#[derive(Debug, buck2_error::Error)]
#[error("My error message: {field}")]
#[buck2(tag = Input)]  // or other appropriate tag
struct MyError {
    field: String,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum MyErrors {
    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("Missing required field: {0}")]
    MissingField(String),
}
```

### Error Tags

Common error tags (from `buck2_data::error::ErrorTag`):

- `Input` - User input errors (invalid arguments, malformed build files, etc.)
- `Tier0` - Critical infrastructure failures
- `Environment` - External environment issues (system configuration, external
  services, network/certificates, filesystem)
- Create new/meaningful/distinct error tag whenever possible in
  `app/buck2_data/error.proto`, and if the error is generic, use Input, Tier0,
  and Environment

### Creating Ad-Hoc Errors

Use the `buck2_error!` macro to create errors without defining a type:

```rust
use buck2_error::buck2_error;

if some_condition {
    return Err(buck2_error!(
        buck2_error::ErrorTag::Input,
        "Invalid value: expected {}, got {}",
        expected,
        actual
    ));
}
```

### Internal Errors

For bugs in Buck2 code, use `internal_error!` macro:

```rust
use buck2_error::internal_error;

let value = map.get(key).internal_error("Key must exist")?;

// Or:
return Err(internal_error!(
    "Unexpected state: {} should not be empty",
    collection_name
));
```

### Adding Context to Errors

Use `BuckErrorContext` trait for adding context:

```rust
use buck2_error::BuckErrorContext;

// Add context to Results
result.buck_error_context("Failed to process file")?;

// Add context with formatted message
result.with_buck_error_context(|| format!("Failed to process file: {}", path))?;

// For internal errors
value.internal_error("This should never be None")?;
value.with_internal_error(|| format!("Missing key: {}", key))?;
```

### Error Conversion

Buck2's error system provides automatic conversion:

```rust
// From std::io::Error, std::fmt::Error, etc.
std::fs::read_to_string(path)?  // Automatically converts to buck2_error::Error

// From custom error types (if they implement std::error::Error)
my_custom_error?  // Works if error derives buck2_error::Error

// Manual conversion with tags
use buck2_error::conversion::from_any_with_tag;

some_result.map_err(|e| from_any_with_tag(e, ErrorTag::Tier0))?;
```

### Common Patterns

**Function returning Result:**

```rust
fn process_artifact(&self, artifact: &Artifact) -> buck2_error::Result<()> {
    let path = artifact.path()
        .buck_error_context("Failed to get artifact path")?;

    if !path.exists() {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "Artifact does not exist: {}",
            path
        ));
    }

    Ok(())
}
```

**Unwrapping with internal_error:**

```rust
// Instead of .unwrap() or .expect()
let value = option_value.internal_error("Value must be set")?;

// For collections
let item = collection.get(index)
    .with_internal_error(|| format!("Missing item at index {}", index))?;
```

### Key Differences from anyhow

1. **No `anyhow!` macro** - Use `buck2_error!` instead
2. **No `.context()`** - Use `.buck_error_context()` instead
3. **Tags required** - All errors must be categorized with an `ErrorTag`
4. **Type is `buck2_error::Result`** - Not `anyhow::Result`
5. **Derive `buck2_error::Error`** - Not `thiserror::Error`

## Internal vs OSS Differences

- Some code uses `@oss-enable` or `@oss-disable` markers
- `is_open_source()` function controls configuration differences
- Internal RE client differs from OSS version
- Internal version has additional Meta-specific integrations (Scribe, etc.)

## Protobuf Handling

Buck2 uses Protocol Buffers extensively. On Linux/macOS/Windows, prebuilt
`protoc` binaries are used automatically.

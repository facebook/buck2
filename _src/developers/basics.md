---
oncalls: ['build_infra']
---

# Buck2 Developer Basics

This file is `docs/developers/basics.md`. It is required reading for working on buck2 itself.

This file is optimized for both humans and LLMs and must be kept short. Detailed explanations belong
in adjacent files in this directory or other documentation from which humans or LLMs can pull as
needed.

## Validation

buck2 is built with buck2 internally at Meta. Cargo builds are primarily for OSS. To validate
changes:

```bash
# Check that things compile
# Required for LLMs making changes to `buck2/app`
arc rust-check fbcode//buck2/app/...
# Run clippy
arc rust-clippy fbcode//buck2/app/...
# Run lints and apply fixes
arc lint -a
# Format code. Usually unnecessary, performed by IDEs and hooks
arc f
```

Buck2 has standard Rust unittests and integration tests at `tests/core`.

```bash
# Run an integration test
buck2 test fbcode//buck2/tests/core/analysis:test_cmd_args
# Discover more information about writing and executing integration tests
cat tests/core/README.md
# Run some unittests
buck2 test fbcode//buck2/app/buck2_core:buck2_core
```

In OSS, standard cargo tooling mostly applies. Exceptions are that integration tests do not run in
OSS and clippy has some atypical configuration requiring use of `python3 test.py --get --lint-only`

## Coding conventions

Most important of all: Most questions can be answered by matching the conventions and style
of nearby code.

Standard `rustfmt` conventions apply. Beyond that:

- **HashMaps**: use `buck2_hash::BuckHashMap`, not `fxhash::FxHashMap`.
- **Cloning**: prefer `.dupe()` over `.clone()` for types that implement `Dupe`
  (e.g. `Arc`-wrapped types). Use `gazebo` utilities — particularly `dupe` —
  where they fit.
- **String conversion**: prefer `.to_owned()` over `.to_string()` for `&str` →
  `String`.
- **Imports**: use `use crate::foo::bar`, not `use super::bar`. Place all `use`
  statements at the module level — never inside a function or block. Test
  modules may use `use super::*;` at the top.
- **Modules**: a module should contain either submodules OR types/functions, not
  both.
- **PartialEq/Hash with ignored fields**: use the `derivative` crate.

### Error message style

- Names (variables, targets, files, ...) should be quoted with backticks, e.g.
  ``Variable `x` not defined``.
- Lists should use square brackets, e.g. ``Available targets: [`aa`, `bb`]``.
- Error messages should start with an upper case letter and should not end with
  a period.

## Error handling

Buck2 uses `buck2_error` replacing both `anyhow` and `thiserror`. The must-knows:

- Return `buck2_error::Result<T>`.
- Define error types with `#[derive(Debug, buck2_error::Error)]` and tag them
  with `#[buck2(tag = ...)]` (no `thiserror::Error`).
- Use the `buck2_error!` macro for ad-hoc errors.
- `.expect()`, `.unwrap()`, etc. are ok for file-local invariant violations/"this should never
  happen" cases. If not file-local, prefer `internal_error!()`, `.internal_error("...")?` or
  `.with_internal_error(|| ...)` if possible.
- Inspecting or creating errors in non-error codepaths is strongly discouraged. Represent states
  that are not errors using types that are not errors.

For more details including about defining errors, tagging, conversion, and context see [Error
Handling](./error_handling.md).

## Internal vs open source

Code is generally the same internally and externally, exceptions will be locally self-explanatory.

## Rust Dependencies

When modifying dependencies, change both BUCK and Cargo.toml.

At Meta, common third party Rust libraries are generally just available.

# Debugging

```bash
# Build buck2
buck2 build @fbcode//mode/opt fbcode//buck2:buck2 --out /tmp/buck2_dest
# Build buck2 from source and run a command with it in a different isolation dir
./buck2.py build :foo
```

Further information at [debugging.md](./debugging.md)

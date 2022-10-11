## Typed distributed plugin registration

[<img alt="github" src="https://img.shields.io/badge/github-dtolnay/inventory-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/dtolnay/inventory)
[<img alt="crates.io" src="https://img.shields.io/crates/v/inventory.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/inventory)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-inventory-66c2a5?style=for-the-badge&labelColor=555555&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/inventory)
[<img alt="build status" src="https://img.shields.io/github/workflow/status/dtolnay/inventory/CI/master?style=for-the-badge" height="20">](https://github.com/dtolnay/inventory/actions?query=branch%3Amaster)

This crate provides a way to set up a plugin registry into which plugins can be
registered from any source file linked into your application. There does not
need to be a central list of all the plugins.

```toml
[dependencies]
inventory = "0.1"
```

*Supports rustc 1.31+*

<br>

# Examples

Suppose we are writing a command line flags library and want to allow any source
file in the application to register command line flags that are relevant to it.

This is the flag registration style used by [gflags] and is better suited for
large scale development than maintaining a single central list of flags, as the
central list would become an endless source of merge conflicts in an application
developed simultaneously by thousands of developers.

[gflags]: https://gflags.github.io/gflags/

### Instantiating the plugin registry

Let's use a `struct Flag` as the plugin type, which will contain the short name
of the flag like `-v`, the full name like `--verbose`, and maybe other
information like argument type and help text. We instantiate a plugin registry
with an invocation of `inventory::collect!`.

```rust
pub struct Flag {
    short: char,
    name: &'static str,
    /* ... */
}

impl Flag {
    pub fn new(short: char, name: &'static str) -> Self {
        Flag { short, name }
    }
}

inventory::collect!(Flag);
```

This `collect!` call must be in the same crate that defines the plugin type.
This macro does not "run" anything so place it outside of any function body.

### Registering plugins

Now any crate with access to the `Flag` type can register flags as a plugin.
Plugins can be registered by the same crate that declares the plugin type, or by
any downstream crate.

```rust
inventory::submit! {
    Flag::new('v', "verbose")
}
```

The `submit!` macro does not "run" anything so place it outside of any function
body. In particular, note that all `submit!` invocations across all source files
linked into your application all take effect simultaneously. A `submit!`
invocation is not a statement that needs to be called from `main` in order to
execute.

### Iterating over plugins

The value `inventory::iter::<T>` is an iterator with element type `&'static T`
that iterates over all plugins registered of type `T`.

```rust
for flag in inventory::iter::<Flag> {
    println!("-{}, --{}", flag.short, flag.name);
}
```

There is no guarantee about the order that plugins of the same type are visited
by the iterator. They may be visited in any order.

<br>

## How it works

Inventory is built on the [`ctor`] crate which provides module initialization
functions for Rust similar to `__attribute__((constructor))` in C. Each call to
`inventory::submit!` produces a shim that evaluates the given expression and
registers it into a registry of its corresponding type. This registration
happens dynamically as part of life-before-main for statically linked elements.
Elements brought in by a dynamically loaded library are registered at the time
that dlopen occurs.

[`ctor`]: https://github.com/mmastrac/rust-ctor

Platform support includes Linux, macOS, iOS, FreeBSD, Android, and Windows.
Other platforms will simply find that no plugins have been registered. Support
for other targets would need to be added in the `ctor` crate.

For a different approach to plugin registration that *does not* involve
life-before-main, see the [`linkme`] crate.

[`linkme`]: https://github.com/dtolnay/linkme

<br>

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

# Tips and tricks for hacking on Buck2

You might have been lead here by reading [CONTRIBUTING.md](/CONTRIBUTING.md). If
not, please read that as well! That will give you the high level overview; this
document is all about the needed elbow grease you'll have to apply.

## Building the code

Buck2 is written in Rust, and **currently requires a nightly toolchain**. You
need **[rustup](https://rustup.rs)** installed to provision the necessary
version of `rustc` and `cargo` as a result.

You can either build `buck2` from a clone of the Buck2 repo (this will cause
rustup/cargo to install the right nightly version of `rustc`):

```sh
git clone https://github.com/facebook/buck2.git
cd buck2/
cargo install --path=app/buck2
```

Or, alternatively, install it directly from GitHub:

```sh
rustup install nightly-2025-10-25
cargo +nightly-2025-10-25 install --git https://github.com/facebook/buck2.git buck2
```

### Side note: using [Nix] to compile the source

> **NOTE**: You shouldn't need to do this if you already have a tool such as
> `rustup` installed. The following instructions are for Nix and NixOS users
> specifically.

Most [Nix] users provision tools directly with Nix itself, rather than rustup;
the Buck2 source code includes a `flake.nix` file, which can be used to compile
Buck2 itself via `cargo`:

```sh
git clone https://github.com/facebook/buck2.git
cd buck2/
nix develop . # add 'rustc' and 'cargo' to $PATH
cargo build --release --bin=buck2
```

A Nix package (e.g. `nix build .#buck2`) does not yet exist; see `buck2` in
nixpkgs for inspiration for writing one.

An `.envrc` file using the Nix flake is provided for `direnv` users:
`direnv allow` will give a usable development environment.

[Nix]: https://nixos.org/nix

### Side note: `protoc` binaries and alternative operating systems

> **NOTE**: You can probably skip this if you're on a typical "Tier 1" operating
> system like aarch64/x86_64 Linux, Windows, or macOS.

Buck2 uses Protocol Buffers quite extensively in its internals, and also for
communication with remote systems for tasks like Remote Execution. Therefore
there are `.proto` files in the codebase that need to be compiled to Rust code.
Due to the architecture of how Protocol Buffers works, you'll need the `protoc`
compiler available in order to do this.

For the 3 major operating systems &mdash; Linux, Windows, and macOS &mdash; the
`cargo` build uses prebuilt `protoc` binaries from
[protoc-bin-vendored](https://crates.io/crates/protoc-bin-vendored) crate to
accomplish this. So everything should transparently work with no intervention
needed and just running `cargo build` is enough.

But there are non-Tier-1 operating systems where Buck2 might be used, and the
above crate won't work, and you'll need to override it. You will need to install
`protoc` from some other source. Doing that is out of scope for this document.
But once you've done so, you can use the following two environment variables
**before running `cargo build`** to override the crate:

- `BUCK2_BUILD_PROTOC`, the path to the `protoc` binary
- `BUCK2_BUILD_PROTOC_INCLUDE`, the path to the protocol buffers header file
  directory

Assuming you have your protocol buffers installation located in `/opt/protobuf`,
you can do the following:

```bash
export BUCK2_BUILD_PROTOC=/opt/protobuf/bin/protoc
export BUCK2_BUILD_PROTOC_INCLUDE=/opt/protobuf/include
```

Buck2 should then build with `cargo` using the steps above.

### Building buck2 with buck2

See [Bootstrapping] for details; the gist is: use
`reindeer --third-party-dir shim/third-party/rust buckify` to generate BUCK
files for Cargo dependencies, then `buck2 build //:buck2` will work.

[Bootstrapping]: ./docs/about/bootstrapping.md

## Running tests and lints

It's possible to run buck2's test suite with `cargo test`.

[Currently][clippy-bug], `cargo clippy` will generate spurious warnings as the
canonical lint configuration is in `lint_levels.bzl` rather than `Cargo.toml`,
so it's recommended to use `test.py` instead.

[clippy-bug]: https://github.com/facebook/buck2/issues/943

To run the build, tests, rustdoc, and lints in the same way as buck2's OSS CI,
run:

```
python3 test.py --git
```

Various checks can be run individually with `--lint-only`, `--test-only` and
similar; see `python3 test.py --help` for details.

N.B. This command will not run Starlark lints since the Starlark linter is
[disabled due to Git gremlins][Git gremlins] that may be fixable with enough
effort. FIXME: It may be possible to use Sapling on OSS Buck2 to get a working
Starlark linter, but the instructions to do so would need to be written.

[Git gremlins]:
  https://github.com/facebook/buck2/commit/54f986c0329f4f60e9057d7e86f3d361f1b5e1bf

## Coding conventions

Beyond the obvious (well-tested, easy to read) we prefer guidelines that are
automatically enforced, e.g. through Rustfmt, Clippy or the custom linter we
have written. Some rules:

- Follow standard `rustfmt` conventions.
- Use the utilities from Gazebo where they are useful, in particular, `dupe`.
- Prefer `to_owned` to convert `&str` to `String`.
- Qualify `anyhow::Result` rather than `use anyhow::Result`.
- Most errors should be returned as `anyhow::Result`. Inspecting errors outside
  tests and the top-level error handler is strongly discouraged.
- Most errors should be constructed with `thiserror` deriving `enum` values, not
  raw `anyhow!`.
- We use the `derivative` library to derive the `PartialEq` and `Hash` traits
  when some fields should be ignored.
- Prefer `use crate::foo::bar` over `use super::bar` or `use crate::foo::*`,
  apart from test modules which often have `use super::*` at the top.
- Modules should either have submodules or types/functions/constants, but not
  both.
- Prefer `anyhow::Error` for checking internal invariants that are maintained
  between multiple files, while `panic!`/`unreachable!` are reasonable if the
  invariant is file-local.

### Error messages

- Names (of variables, targets, files, etc) should be quoted with backticks,
  e.g. ``Variable `x` not defined``.
- Lists should use square brackets, e.g. ``Available targets: [`aa`, `bb`]``.
- Error messages should start with an upper case letter. Error messages should
  not end with a period.

## Open-source differences

Most code is shared as-is between open source and the internal Meta version of
Buck2. However, there are some exceptions:

- The open-source remote execution client is different, because our internal one
  works with custom servers/infrastructure that is not publicly available.
- There are places controlled with `is_open_source()` which change configuration
  between the internal and open source versions.
- Some places use `@oss-enable` or `@oss-disable` to comment/uncomment lines of
  code. The internal code is visible, but the comment markers are moved during
  export/import of code.

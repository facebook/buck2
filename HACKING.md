# Tips and tricks for hacking on Buck2

You might have been lead here by reading [CONTRIBUTING.md](/CONTRIBUTING.md). If
not, please read that as well! That will give you the high level overview; this
is the document is all about the needed elbow grease you'll have to apply.

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
rustup install nightly-2023-05-28
cargo +nightly-2023-05-28 install --git https://github.com/facebook/buck2.git buck2
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
cargo build --release --path=app/buck2
```

A Nix package (e.g. `nix build .#buck2`) does not yet exist.

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

## Unstable `rustc` features

<!-- [tag:unstable-rustc] -->

TL;DR: Buck2 developers **are free to use unstable `rustc` features**. Please
submit patches to change this if you are interested in building buck2 with a
stable Rust compiler. Follow issue https://github.com/facebook/buck2/issues/265
for more information and changes to this policy.

Buck2 currently uses unstable features of `rustc` freely. Actually, building and
using Buck2 requires an exact version of `rustc` that is checked at build time;
this required version is identical within Meta and outside. No other version is
supported or allowed.

However, there is some outside desire to move away from using unstable Rust
features; the primary motivators for this being:

- Consumption by parties who may not allow unstable toolchains when procuring
  tools; and
- Compiler stability (occasionally we find ICEs in some nightly versions that
  are likely surfaced in part due to unstable features)

See https://github.com/facebook/buck2/issues/265 for some discussion and
examples.

But this is a very low priority, since most users will end up using `buck2` as a
binary; they are isolated from such details. As a result, there is currently
**NO** commitment to keep the codebase working with the stable `rustc` release.
If such a thing happens, it will be decided and &mdash; most likely, very loudly
&mdash; communicated and enforced beforehand.

If you want to build buck2 with a stable Rust compiler, please help out and
submit patches. If you are _not_ concerned with this, then carry on as usual,
though it would be nice to avoid "gratuitous" unstable features if you can
manage it, perhaps at the expense of few small extra lines of code or something.

**NOTE**: If you want to look around the codebase for notes on unstable feature
usage, and to keep track of progress, please keep track of issue #265 above; you
can also use the **[tagref]** tool to search for references to this task in the
codebase with the following command and learn more or help:

[tagref]: https://github.com/stepchowfun/tagref

```bash
tagref list-refs | grep ref:unstable-rustc
```

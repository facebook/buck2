# Buck2

**WARNING:** This project is not yet polished. We are continuing to develop it in the open, but don't expect it to be suitable for most people until Feb/Mar/Apr 2023 (at which point we'll properly announce it). If you try and use it, you will probably have a bad time. If you are willing to work closely with us, please give it a go and [let us know](https://github.com/facebookincubator/buck2/issues) what is blocking you.

This repo contains the code for the Buck2 build system - the successor to the original [Buck build system](https://buck.build). To understand why it might be interesting, see [this explainer](docs/why.md). For the moment, we only test it on Linux, and don't recommend running benchmarks as features like the disk cache are not entirely implemented in the open source build.

## Getting started

### Building Buck2

To build Buck2 type `cargo build --bin=buck2 --release` from this directory and copy the resulting binary (probably `target/release/buck2`) to your `$PATH`. Typing `buck2 --help` should now work.

### Building sample targets

__FIXME(marwhal): This section needs to be made to work__

If you `cd examples/prelude` and type `buck2 build ...` that will build a number of targets in a variety of languages. Doing so requires that `python3` and `clang` are both on your `$PATH`.

### Bootstrapping Buck2

To build Buck2 using Buck2:

* Install [protobuf](https://github.com/protocolbuffers/protobuf#protocol-compiler-installation):
  * On Ubuntu Linux 20.04: Install protoc from [this link](https://github.com/protocolbuffers/protobuf/releases/download/v21.10/protoc-21.10-linux-x86_64.zip)
  * On Ubuntu Linux 22.04: `apt-get install protobuf-compiler` (check `protoc --version` is 3.12 or higher)
  * On Mac: `brew install protobuf`
* Install [`reindeer`](https://github.com/facebookincubator/reindeer), which is used to make Buck targets for Rust libraries.
* Run `reindeer --third-party-dir shim/third-party/rust vendor`
* Run `reindeer --third-party-dir shim/third-party/rust buckify --stdout > shim/third-party/rust/BUCK_OSS`
* Run `buck2 build :buck2`

Note that the resulting binary will be compiled without optimisations or [jemalloc](https://github.com/jemalloc/jemalloc), so we recommend using the Cargo-produced binary in further development.

### Making your own project

A Buck2 project requires:

* A `.buckconfig` file in the root which has a `[repositories]` section listing out interesting cells. We recommend copying from `examples/prelude` to ensure it contains the necessary fields.
* A `prelude` directory, which should be produced with `git submodule add https://github.com/facebookincubator/buck2-prelude.git prelude`
* A `toolchains` directory, which specifies where to find the relevant toolchains. We recommend copying from `examples/prelude` to start, but you may wish to use alternative toolchains.
* Some `BUILD` files that specify the targets specific to your project.

## Terminology conventions

* A _target_, e.g. `fbcode//buck2:buck2`, is something a user defines that is an instance of a _rule_, which can be built.
* A _rule_, e.g. `cxx_library`, is an implementation of how something is built.
* _Loading_ a `TARGETS`/`BUCK` file involves evaluating the Starlark and doing attribute coercion/resolution. It can be done with `buck2 cquery fbcode//buck2:buck2` or `buck2 cquery 'deps(fbcode//buck2:buck2)'` to do it recursively.
* _Analysing_ a _target_ involves running the associated _rule_ to produce the _providers_. It can be done with `buck2 audit providers fbcode//buck2:buck2`.
* _Building_ a _target_ involves demanding the _artifacts_ from a _provider_ (e.g. `DefaultInfo`). It can be done with `buck2 build fbcode//buck2:buck2`.

## Coding conventions

Beyond the obvious (well-tested, easy to read) we prefer guidelines that are automatically enforced, e.g. through `rust fmt`, Clippy or the custom linter we have written. Some rules:

* Use the utilities from Gazebo where they are useful, in particular, `dupe`.
* Prefer `to_owned` to convert `&str` to `String`.
* Qualify `anyhow::Result` rather than `use anyhow::Result`.
* Most errors should be returned as `anyhow::Result`. Inspecting errors outside tests and the top-level error handler is strongly discouraged.
* Most errors should be constructed with `thiserror` deriving `enum` values, not raw `anyhow!`.
* We use the `derivative` library to derive the `PartialEq` and `Hash` traits when some fields should be ignored.
* Prefer `use crate::foo::bar` over `use super::bar` or `use crate::foo::*`, apart from test modules which often have `use super::*` at the top.
* Modules should either have submodules or types/functions/constants, but not both.

### Error messages

* Names (of variables, targets, files, etc) should be quoted with backticks,
  e.g. ``Variable `x` not defined``.
* Lists should use square brackets, e.g. ``Available targets: [`aa`, `bb`]``.
* Error messages should start with an upper case letter.
  Error messages should not end with a period.

## License

Buck2 is both MIT and Apache License, Version 2.0 licensed, as found in the [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) files.

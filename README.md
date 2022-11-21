# Buck2

This repo contains the code for the Buck2 build system - the successor to the original Buck build system.

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

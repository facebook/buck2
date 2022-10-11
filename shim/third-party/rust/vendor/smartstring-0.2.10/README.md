# smartstring

[![Travis CI](https://travis-ci.org/bodil/smartstring.svg?branch=master&status=passed)](https://travis-ci.org/github/bodil/smartstring)

Compact inlined strings.

## tl;dr

String type that's source compatible with `std::string::String`, uses exactly the same amount of
space, doesn't heap allocate for short strings (up to 23 bytes on 64-bit archs) by storing them in
the space a `String` would have taken up on the stack, making strings go faster overall.

## Overview

This crate provides a wrapper for Rust's standard `String` which uses the space a `String` occupies
on the stack to store inline string data, automatically promoting it to a `String` when it grows
beyond the inline capacity. This has the advantage of avoiding heap allocations for short strings as
well as improving performance thanks to keeping the strings on the stack.

This is all accomplished without the need for an external discriminant, so a `SmartString` is
exactly the same size as a `String` on the stack, regardless of whether it's inlined or not, and
when not inlined it's pointer compatible with `String`, meaning that you can safely coerce a
`SmartString` to a `String` using `std::mem::replace()` or `pointer::cast()` and go on using it as
if it had never been a `SmartString`. (But please don't do that, there's an `Into<String>`
implementation that's much safer.)

## Supported architectures
`smartstring` currently doesn't run on 32-bit big endian architectures like `powerpc`, so its use
in any crates that intend to run on those architectures should ideally be gated behind a
[platform specific dependency](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#platform-specific-dependencies)
in your `Cargo.toml`, like so:
```toml
[target.'cfg(not(all(target_endian = "big", target_pointer_width = "32")))'.dependencies]
smartstring = "0.2"
```

This will ensure that `cargo` does not try to build `smartstring` on these unsupported
architectures, which will otherwise [always fail](https://github.com/bodil/smartstring/blob/v0.2.9/src/config.rs#L91-L93).

## Caveat

The way `smartstring` gets by without a discriminant is dependent on the memory layout of the
`std::string::String` struct, which isn't something the Rust compiler and standard library make any
guarantees about. `smartstring` makes an assumption about how it's been laid out, which has held
basically since rustc came into existence, but is nonetheless not a safe assumption to make, and if
the layout ever changes, `smartstring` will stop working properly (at least on little-endian
architectures, the assumptions made on big-endian archs will hold regardless of the actual memory
layout). Its test suite does comprehensive validation of these assumptions, and as long as the
[CI build](https://travis-ci.org/github/bodil/smartstring) is passing for any given rustc version,
you can be sure it will do its job properly on all tested architectures. You can also check out the
`smartstring` source tree yourself and run `cargo test` to validate it for your particular
configuration.

As an extra precaution, some runtime checks are made as well, so that if the memory layout
assumption no longer holds, `smartstring` will not work correctly, but there should be no security
implications and it should crash early.

## Documentation

-   [API docs](https://docs.rs/smartstring)

## Licence

Copyright 2020 Bodil Stokke

This software is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL
was not distributed with this file, You can obtain one at <http://mozilla.org/MPL/2.0/>.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct][coc]. By
participating in this project you agree to abide by its terms.

[immutable.rs]: https://immutable.rs/
[coc]: https://github.com/bodil/sized-chunks/blob/master/CODE_OF_CONDUCT.md

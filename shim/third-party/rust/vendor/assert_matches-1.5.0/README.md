# assert_matches

Provides a macro, `assert_matches`, which tests whether a value
matches a given pattern, causing a panic if the match fails.

[Documentation](https://docs.rs/assert_matches/)

```rust
#[macro_use] extern crate assert_matches;

#[derive(Debug)]
enum Foo {
    A(i32),
    B(i32),
}

let a = Foo::A(1);

assert_matches!(a, Foo::A(_));

assert_matches!(a, Foo::A(i) if i > 0);
```

To include in your project, only when tests are compiled, add the following
to your Cargo.toml:

```toml
[dev-dependencies]
assert_matches = "1.5"
```

And add the following to your crate root:

```rust
#[cfg(test)] #[macro_use]
extern crate assert_matches;
```

## License

`assert_matches` is distributed under the terms of both the MIT license and the
Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.

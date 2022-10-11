[![Crates.io](https://img.shields.io/crates/v/test-case.svg)](https://crates.io/crates/test-case)
[![Docs.rs](https://docs.rs/test-case/badge.svg)](https://docs.rs/test-case)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/rust-lang/docs.rs/master/LICENSE)
[![Build Status](https://github.com/frondeus/test-case/workflows/Test/badge.svg)](https://github.com/frondeus/test-case/actions)
![Maintenance](https://img.shields.io/badge/maintenance-activly--developed-brightgreen.svg)

# Test Case

## Overview
This crate provides `#[test_case]` procedural macro attribute that generates multiple parametrized tests using one body with different input parameters.
A test is generated for each data set passed in `test_case` attribute.
Under the hood, all test cases that share same body are grouped into `mod`, giving clear and readable test results.

## Getting Started

First of all you have to add this dependency to your `Cargo.toml`:

```toml
[dev-dependencies]
test-case = "1.2.3"
```

Additionally, you have to import the procedural macro with `use` statement:

```rust
use test_case::test_case;
```

## Example usage:

```rust
// The next two lines are not needed for 2018 edition or newer
#[cfg(test)]
extern crate test_case;

#[cfg(test)]
mod tests {
    use test_case::test_case;

    // Not needed for this example, but useful in general
    use super::*;

    #[test_case(4,  2  ; "when operands are swapped")]
    #[test_case(-2, -4 ; "when both operands are negative")]
    #[test_case(2,  4  ; "when both operands are positive")]
    fn multiplication_tests(x: i8, y: i8) {
        let actual = (x * y).abs();

        assert_eq!(8, actual)
    }

    // You can still use regular tests too
    #[test]
    fn addition_test() {
        let actual = -2 + 8;
        assert_eq!(6, actual)
    }
}
```

Output from `cargo test` for this example:

```sh
$ cargo test

running 4 tests
test tests::addition_test ... ok
test tests::multiplication_tests::when_both_operands_are_negative ... ok
test tests::multiplication_tests::when_both_operands_are_positive ... ok
test tests::multiplication_tests::when_operands_are_swapped ... ok

test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Examples

If your only assertion is just `assert_eq!`, you can pass the expectation as macro attribute using `=>` syntax:

```rust
#[test_case( 2 => 2 ; "returns given number for positive input")]
#[test_case(-2 => 2 ; "returns opposite number for non-positive input")]
#[test_case( 0 => 0 ; "returns 0 for 0")]
fn abs_tests(x: i8) -> i8 {
   if x > 0 { x } else { -x }
}
```

Which is equivalent to

```rust
#[test_case( 2, 2 ; "returns given number for positive input")]
#[test_case(-2, 2 ; "returns opposite number for non-positive input")]
#[test_case( 0, 0 ; "returns 0 for 0")]
fn abs_tests(x: i8, expected: i8){
   let actual = if x > 0 { x } else { -x };

   assert_eq!(expected, actual);
}
```

Attributes and expectation may be any expresion unless they contain `=>`, e.g.

```rust
#[test_case(None,        None    => 0 ; "treats none as 0")]
#[test_case(Some(2),     Some(3) => 5)]
#[test_case(Some(2 + 3), Some(4) => 2 + 3 + 4)]
fn fancy_addition(x: Option<i8>, y: Option<i8>) -> i8 {
    x.unwrap_or(0) + y.unwrap_or(0)
}
```

Note: in fact, `=>` is not prohibited, but the parser will always treat last `=>` sign as beginning of expectation definition.

Test case names are optional. They are set using `;` followed by string literal at the end of macro attributes.

Example generated code:

```rust
mod fancy_addition {
    #[allow(unused_imports)]
    use super::*;

    fn fancy_addition(x: Option<i8>, y: Option<i8>) -> i8 {
        x.unwrap_or(0) + y.unwrap_or(0)
    }

    #[test]
    fn treats_none_as_0() {
        let expected = 0;
        let actual = fancy_addition(None, None);

        assert_eq!(expected, actual);
    }

    #[test]
    fn some_2_some_3() {
        let expected = 5;
        let actual = fancy_addition(Some(2), Some(3));

        assert_eq!(expected, actual);
    }

    #[test]
    fn some_2_3_some_4() {
        let expected = 2 + 3 + 4;
        let actual = fancy_addition(Some(2 + 3), Some(4));

        assert_eq!(expected, actual);
    }
}
```

## Modifiers

### inconclusive

#### Context ignored test cases (deprecated, will be dropped in 2.0.0)

If test case name (passed using `;` syntax described above) contains a word "inconclusive", generated test will be marked with `#[ignore]`.

#### Keyword 'inconclusive'

If test expectation is preceded by keyword `inconclusive` the test will be ignored as if it's description would contain word `inconclusive`

```rust
#[test_case("42")]
#[test_case("XX" ; "inconclusive - parsing letters temporarily doesn't work, but it's ok")]
#[test_case("na" => inconclusive ())]
fn parses_input(input: &str) {
    // ...
}
```

Generated code:
```rust
mod parses_input {
    // ...

    #[test]
    pub fn _42() {
        // ...
    }

    #[test]
    #[ignore]
    pub fn inconclusive_parsing_letters_temporarily_doesn_t_work_but_it_s_ok() {
        // ...
    }

```
### matches

If test expectation is preceded by `matches` keyword, the result will be tested whether it fits within provided pattern.

```rust
#[test_case("foo", "bar" => matches ("foo", _) ; "first element of zipped tuple is correct")]
#[test_case("foo", "bar" => matches (_, "bar") ; "second element of zipped tuple is correct")]
fn zip_test<'a>(left: &'a str, right: &'a str) -> (&'a str, &'a str) {
    (left, right)
}
```

### panics

If test case expectation is preceded by `panics` keyword and the expectation itself is `&str` **or** expresion that evaluates to `&str` then test case will be expected to panic during execution.

```rust

#[test_case("foo" => panics "invalid input")]
#[test_case("bar")]
fn test_panicking(input: &str) {
    if input == "foo" {
        panic!("invalid input")
    }
}
```

### is|it (feature = "hamcrest_assertions")

This feature requires addition of hamcrest2 crate to your Cargo.toml:

```toml
test-case = { version = "1.1.0", features = ["hamcrest_assertions"] }
hamcrest2 = "0.3.0"
```

After that you can use test cases with new keywords `is` and `it` which will allow you to use hamcrest2 assertions ([doc](https://docs.rs/hamcrest2/0.3.0/hamcrest2/))

```rust

#[test_case(&[1, 3] => is empty())]
#[test_case(&[2, 3] => it contains(2))]
#[test_case(&[2, 3] => it not(contains(3)))]
#[test_case(&[2, 4] => it contains(vec!(2, 4)))]
#[test_case(&[2, 3] => is len(1))]
fn removes_odd_numbers(collection: &[u8]) -> &Vec<u8> {
    Box::leak(Box::new(collection.into_iter().filter(|x| *x % 2 == 0).copied().collect()))
}
```

## async in test cases

Test cases can work with `tokio`, `async-std` and other runtimes, provided `#[test...]` attribute from mentioned libraries is used as a last attribute.

eg.

```rust

#[test_case("Hello, world" => true)]
#[tokio::test]
async fn runs_async_task(input: &str) -> bool {
    some_async_fn(input).await
}
```

## Porting from Rust `#[test]`s with `Result` types

It is important to note that test-case does not support the [Rust 2018+ idiom](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html#tests-and-) of failing tests by returning a `Result<T, E>` with an error type.
The simplest way to remedy this is to append `... => Ok(_)` to each `#[test-case(...)` expression, e.g:

```rust
#[test_case( 0 => Ok(_) ; "Test with 0")]
```

Previously, tests relying on the return error being checked would silently pass; as of 1.2.2 attempting to return a `Result<>` without an appropriate return check in the expression will result in a compilation error. However if you wish to keep the old behaviour for some reason the feature flag `allow_result` will disable the check.


## License

Licensed under of MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

### Contribution

All contributions and comments are more than welcome! Don't be afraid to open an issue or PR whenever you find a bug or have an idea to improve this crate.

Recommended tools:
* `cargo readme` - to regenerate README.md based on template and lib.rs comments
* `cargo insta`  - to review test snapshots
* `cargo edit`   - to add/remove dependencies
* `cargo fmt`    - to format code
* `cargo clippy` - for all insights and tips
* `cargo fix`    - for fixing warnings

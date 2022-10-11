//! # Overview
//! This crate provides `#[test_case]` procedural macro attribute that generates multiple parametrized tests using one body with different input parameters.
//! A test is generated for each data set passed in `test_case` attribute.
//! Under the hood, all test cases that share same body are grouped into `mod`, giving clear and readable test results.
//!
//! # Getting Started
//!
//! First of all you have to add this dependency to your `Cargo.toml`:
//!
//! ```toml
//! [dev-dependencies]
//! test-case = "1.2.3"
//! ```
//!
//! Additionally, you have to import the procedural macro with `use` statement:
//!
//! ```rust
//! use test_case::test_case;
//! ```
//!
//! # Example usage:
//!
//! ```rust
//! // The next two lines are not needed for 2018 edition or newer
//! #[cfg(test)]
//! extern crate test_case;
//!
//! #[cfg(test)]
//! mod tests {
//!     use test_case::test_case;
//!
//!     // Not needed for this example, but useful in general
//!     use super::*;
//!
//!     #[test_case(4,  2  ; "when operands are swapped")]
//!     #[test_case(-2, -4 ; "when both operands are negative")]
//!     #[test_case(2,  4  ; "when both operands are positive")]
//!     fn multiplication_tests(x: i8, y: i8) {
//!         let actual = (x * y).abs();
//!
//!         assert_eq!(8, actual)
//!     }
//!
//!     // You can still use regular tests too
//!     #[test]
//!     fn addition_test() {
//!         let actual = -2 + 8;
//!         assert_eq!(6, actual)
//!     }
//! }
//! ```
//!
//! Output from `cargo test` for this example:
//!
//! ```sh
//! $ cargo test
//!
//! running 4 tests
//! test tests::addition_test ... ok
//! test tests::multiplication_tests::when_both_operands_are_negative ... ok
//! test tests::multiplication_tests::when_both_operands_are_positive ... ok
//! test tests::multiplication_tests::when_operands_are_swapped ... ok
//!
//! test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
//! ```
//!
//! # Examples
//!
//! If your only assertion is just `assert_eq!`, you can pass the expectation as macro attribute using `=>` syntax:
//!
//! ```rust
//! # use test_case::test_case;
//! #[test_case( 2 => 2 ; "returns given number for positive input")]
//! #[test_case(-2 => 2 ; "returns opposite number for non-positive input")]
//! #[test_case( 0 => 0 ; "returns 0 for 0")]
//! fn abs_tests(x: i8) -> i8 {
//!    if x > 0 { x } else { -x }
//! }
//! ```
//!
//! Which is equivalent to
//!
//! ```rust
//! # use test_case::test_case;
//! #[test_case( 2, 2 ; "returns given number for positive input")]
//! #[test_case(-2, 2 ; "returns opposite number for non-positive input")]
//! #[test_case( 0, 0 ; "returns 0 for 0")]
//! fn abs_tests(x: i8, expected: i8){
//!    let actual = if x > 0 { x } else { -x };
//!
//!    assert_eq!(expected, actual);
//! }
//! ```
//!
//! Attributes and expectation may be any expresion unless they contain `=>`, e.g.
//!
//! ```rust
//! # use test_case::test_case;
//! #[test_case(None,        None    => 0 ; "treats none as 0")]
//! #[test_case(Some(2),     Some(3) => 5)]
//! #[test_case(Some(2 + 3), Some(4) => 2 + 3 + 4)]
//! fn fancy_addition(x: Option<i8>, y: Option<i8>) -> i8 {
//!     x.unwrap_or(0) + y.unwrap_or(0)
//! }
//! ```
//!
//! Note: in fact, `=>` is not prohibited, but the parser will always treat last `=>` sign as beginning of expectation definition.
//!
//! Test case names are optional. They are set using `;` followed by string literal at the end of macro attributes.
//!
//! Example generated code:
//!
//! ```rust
//! mod fancy_addition {
//!     #[allow(unused_imports)]
//!     use super::*;
//!
//!     fn fancy_addition(x: Option<i8>, y: Option<i8>) -> i8 {
//!         x.unwrap_or(0) + y.unwrap_or(0)
//!     }
//!
//!     #[test]
//!     fn treats_none_as_0() {
//!         let expected = 0;
//!         let actual = fancy_addition(None, None);
//!
//!         assert_eq!(expected, actual);
//!     }
//!
//!     #[test]
//!     fn some_2_some_3() {
//!         let expected = 5;
//!         let actual = fancy_addition(Some(2), Some(3));
//!
//!         assert_eq!(expected, actual);
//!     }
//!
//!     #[test]
//!     fn some_2_3_some_4() {
//!         let expected = 2 + 3 + 4;
//!         let actual = fancy_addition(Some(2 + 3), Some(4));
//!
//!         assert_eq!(expected, actual);
//!     }
//! }
//! ```
//!
//! # Modifiers
//!
//! ## inconclusive
//!
//! ### Context ignored test cases (deprecated, will be dropped in 2.0.0)
//!
//! If test case name (passed using `;` syntax described above) contains a word "inconclusive", generated test will be marked with `#[ignore]`.
//!
//! ### Keyword 'inconclusive'
//!
//! If test expectation is preceded by keyword `inconclusive` the test will be ignored as if it's description would contain word `inconclusive`
//!
//! ```rust
//! # use test_case::test_case;
//! #[test_case("42")]
//! #[test_case("XX" ; "inconclusive - parsing letters temporarily doesn't work, but it's ok")]
//! #[test_case("na" => inconclusive ())]
//! fn parses_input(input: &str) {
//!     // ...
//! }
//! ```
//!
//! Generated code:
//! ```ignore
//! mod parses_input {
//!     // ...
//!
//!     #[test]
//!     pub fn _42() {
//!         // ...
//!     }
//!
//!     #[test]
//!     #[ignore]
//!     pub fn inconclusive_parsing_letters_temporarily_doesn_t_work_but_it_s_ok() {
//!         // ...
//!     }
//!
//! ```
//! ## matches
//!
//! If test expectation is preceded by `matches` keyword, the result will be tested whether it fits within provided pattern.
//!
//! ```rust
//! # use test_case::test_case;
//! #[test_case("foo", "bar" => matches ("foo", _) ; "first element of zipped tuple is correct")]
//! #[test_case("foo", "bar" => matches (_, "bar") ; "second element of zipped tuple is correct")]
//! fn zip_test<'a>(left: &'a str, right: &'a str) -> (&'a str, &'a str) {
//!     (left, right)
//! }
//! ```
//!
//! ## panics
//!
//! If test case expectation is preceded by `panics` keyword and the expectation itself is `&str` **or** expresion that evaluates to `&str` then test case will be expected to panic during execution.
//!
//! ```rust
//! # use test_case::test_case;
//!
//! #[test_case("foo" => panics "invalid input")]
//! #[test_case("bar")]
//! fn test_panicking(input: &str) {
//!     if input == "foo" {
//!         panic!("invalid input")
//!     }
//! }
//! ```
//!
//! ## is|it (feature = "hamcrest_assertions")
//!
//! This feature requires addition of hamcrest2 crate to your Cargo.toml:
//!
//! ```toml
//! test-case = { version = "1.1.0", features = ["hamcrest_assertions"] }
//! hamcrest2 = "0.3.0"
//! ```
//!
//! After that you can use test cases with new keywords `is` and `it` which will allow you to use hamcrest2 assertions ([doc](https://docs.rs/hamcrest2/0.3.0/hamcrest2/))
//!
//! ```rust
//! # use test_case::test_case;
//!
//! #[test_case(&[1, 3] => is empty())]
//! #[test_case(&[2, 3] => it contains(2))]
//! #[test_case(&[2, 3] => it not(contains(3)))]
//! #[test_case(&[2, 4] => it contains(vec!(2, 4)))]
//! #[test_case(&[2, 3] => is len(1))]
//! fn removes_odd_numbers(collection: &[u8]) -> &Vec<u8> {
//!     Box::leak(Box::new(collection.into_iter().filter(|x| *x % 2 == 0).copied().collect()))
//! }
//! ```
//!
//! # async in test cases
//!
//! Test cases can work with `tokio`, `async-std` and other runtimes, provided `#[test...]` attribute from mentioned libraries is used as a last attribute.
//!
//! eg.
//!
//! ```rust
//! # use test_case::test_case;
//!
//! #[test_case("Hello, world" => true)]
//! #[tokio::test]
//! async fn runs_async_task(input: &str) -> bool {
//!     some_async_fn(input).await
//! }
//! ```
//!
//! # Porting from Rust `#[test]`s with `Result` types
//!
//! It is important to note that test-case does not support the [Rust 2018+ idiom](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html#tests-and-) of failing tests by returning a `Result<T, E>` with an error type.
//! The simplest way to remedy this is to append `... => Ok(_)` to each `#[test-case(...)` expression, e.g:
//!
//! ```rust
//! #[test_case( 0 => Ok(_) ; "Test with 0")]
//! ```
//!
//! Previously, tests relying on the return error being checked would silently pass; as of 1.2.2 attempting to return a `Result<>` without an appropriate return check in the expression will result in a compilation error. However if you wish to keep the old behaviour for some reason the feature flag `allow_result` will disable the check.
//!

extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, ItemFn};

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_quote;
use syn::spanned::Spanned;
use test_case::TestCase;

mod expected;
mod test_case;
mod utils;

/// Generates tests for given set of data
///
/// In general, test case consists of four elements:
///
/// 1. _(Required)_ Arguments passed to test body
/// 2. _(Optional)_ Expected result
/// 3. _(Optional)_ Test case name
/// 4. _(Required)_ Test body
///
///  When _expected result_ is provided, it is compared against the actual value generated with _test body_ using `assert_eq!`.
/// _Test cases_ that don't provide _expected result_ should contain custom assertions inside _test body_.
///
/// # Examples
///
/// - Without result and name
///
/// ```rust
/// # use test_case::test_case;
/// #[test_case(5)]
/// #[test_case(10)]
/// fn is_positive(x: i8) {
///     assert!(x > 0)
/// }
/// ```
///
/// - With name, without result
///
/// ```rust
/// # use test_case::test_case;
/// #[test_case(1   ; "little number")]
/// #[test_case(100 ; "big number")]
/// #[test_case(5)] // some tests may use default name generated from arguments list
/// fn is_positive(x: i8) {
///     assert!(x > 0)
/// }
/// ```
///
/// - With result, without name
///
/// ```rust
/// # use test_case::test_case;
/// #[test_case(1,   2 =>  3)]
/// #[test_case(-1, -2 => -3)]
/// fn addition(x: i8, y: i8) -> i8 {
///     x + y
/// }
/// ```
///
/// - With result and name
///
/// ```rust
/// # use test_case::test_case;
/// #[test_case(1,   2 =>  3 ; "both numbers possitive")]
/// #[test_case(-1, -2 => -3 ; "both numbers negative")]
/// fn addition(x: i8, y: i8) -> i8 {
///     x + y
/// }
/// ```
#[proc_macro_attribute]
pub fn test_case(args: TokenStream, input: TokenStream) -> TokenStream {
    let test_case = parse_macro_input!(args as TestCase);
    let mut item = parse_macro_input!(input as ItemFn);

    let mut test_cases = vec![test_case];
    let mut attrs_to_remove = vec![];
    for (idx, attr) in item.attrs.iter().enumerate() {
        if attr.path == parse_quote!(test_case) || attr.path == parse_quote!(test_case::test_case) {
            let test_case = match attr.parse_args::<TestCase>() {
                Ok(test_case) => test_case,
                Err(err) => {
                    return syn::Error::new(
                        attr.span(),
                        format!("cannot parse test_case arguments: {:?}", err),
                    )
                    .to_compile_error()
                    .into()
                }
            };

            test_cases.push(test_case);
            attrs_to_remove.push(idx);
        }
    }

    cfg_if::cfg_if! {
        if #[cfg(not(feature = "allow_result"))] {
            for test_case in test_cases.iter() {
                if let Err(err) = check_for_result(test_case, &item) {
                    return err;
                }
            }
        }
    }

    for i in attrs_to_remove.into_iter().rev() {
        item.attrs.swap_remove(i);
    }

    render_test_cases(&test_cases, item)
}

#[cfg(not(feature = "allow_result"))]
fn check_for_result(test_case: &TestCase, item: &ItemFn) -> Result<(), TokenStream> {
    use syn::ReturnType;

    let fn_ret = &item.sig.output;
    match fn_ret {
        ReturnType::Type(_, ret_type) if  !test_case.expects_return() =>  {
            Err(syn::Error::new(
                ret_type.span(),
                format!("Test function {} has a return-type but no exected clause in the test-case. This is currently unsupported. See test-case documentation for more details.", item.sig.ident),
            )
            .to_compile_error()
            .into())
        },
        _ => Ok(())
    }
}

#[allow(unused_mut)]
fn render_test_cases(test_cases: &[TestCase], mut item: ItemFn) -> TokenStream {
    let mut rendered_test_cases = vec![];

    for test_case in test_cases {
        rendered_test_cases.push(test_case.render(item.clone()));
    }

    let mod_name = item.sig.ident.clone();

    let mut additional_usings: Vec<TokenStream2> = vec![];

    cfg_if::cfg_if! {
        if #[cfg(feature="hamcrest_assertions")] {
            additional_usings.push(quote! {
                #[allow(unused_imports)]
                use hamcrest2::*;
            })
        }
    }

    // We don't want any external crate to alter main fn code, we are passing them to each sub-function
    item.attrs.clear();

    let output = quote! {
        mod #mod_name {
            #[allow(unused_imports)]
            use super::*;

            #(#additional_usings)*

            #[allow(unused_attributes)]
            #item

            #(#rendered_test_cases)*
        }
    };

    output.into()
}

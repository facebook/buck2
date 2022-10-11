# Changelog

## V1.2.3
* Fix regression where `panics` and `inconclusive` were not allowed on `test_cases` returning a value
* Fix case where `test_case` would allow to return a type when only single attribute was used

## V1.2.2
* `test-case` no longer allows returning values from tested function without `=>` pattern (thanks to @tarka)
    * Behaviour can be reenabled via `allow_result` feature 

## V1.2.1
* Disabled clippy warning when test-case was generating `assert_eq(bool, bool)` expression.

## V1.2.0
### New features
* Allow usage of fully qualified attribute `#[test_case::test_case]` (thanks to @tomprince)

### Improvements
* Stopped code from emmiting unneded `()` expression in test cases with `expected` fragment (thanks to @martinvonz)

## V1.1.0
### New features
* Added support for using `hamcrest2` assertions with test case
* Enabled support of `async` via tokio or similar
* Enabled attribute passthrough for test cases - it means that you can combine `test-case` with other testing frameworks,
  given at least one `#[test_case]` attribute appears before mentioned framework in testing function
  
### Deprecation
* `inconclusive` inside test case name will not be supported starting `2.0.0`

## V1.0.0
### New features
* Added support for three new keywords: `panics`, `matches` and `inconclusive` which can be applied after `=>` token.

  `matches` gives possibility to test patterns, like:
  ```rust
  #[test_case("foo" => matches Some(("foo", _)))]
  ```

  `panics` gives `should_panic(expected="...")` for one `test_case`:
  ```rust
  #[test_case(true  => panics "Panic error message" ; "This should panic")]
  #[test_case(false => None                         ; "But this should return None")]
  ```

  `inconclusive` ignores one specific test case.- thanks to @luke-biel
  ```rust
  #[test_case("42")]
  #[test_case("XX" ; "inconclusive - parsing letters temporarily doesn't work, but it's ok")]
  #[test_case("na" => inconclusive ())]
  ```

### Major improvements
* Added extra unit tests - thanks to @luke-biel
* Replace `parented_test_case` with parsing `test_case` directly from args - thanks to @luke-biel
* Added keeping trailing underscores in names - thanks to @rzumer
### Minor improvements
* Moved `lazy-static` dependency to `dev-dependencies`
* Fixed README - thanks to @luke-biel and @drwilco
### Upgraded dependencies
* Upgraded `insta` to `0.12.0`

## v0.3.3
### Bugfixes
* Fixed "inconclusive" feature with different cases.

## v0.3.2
### Bugfixes
* Added support for `impl Trait` - it worked in v2.x crate.
### Minor improvements
* Added extra test cases
### Upgraded dependencies
* Upgraded `version_check` to `v0.9.1`

## v0.3.1
### Minor improvements:
* Refreshed readme
* Added CI for stable version of Rust. - thanks to @macisamuele
* Limited crate to Rust 1.29+ - thanks to @macisamuele
### Upgraded dependencies:
* Upgraded `syn`, `quote` and `proc-macro-2` to `v1`
* Upgraded `lazy-static` to `1.4.0`
* Upgraded `insta` to `0.11.0`

## v0.3.0
### Breaking changes
* Crate has new maintainer: Wojciech Polak :hand: :tada:
* Crate has new name, as `test-case-derive` had no meaning for `derive` part.
* Delimiter for test case description is `;` instead of `::`.

  Reason: `::` is valid part of expression and rustc treats const variable as path
### New features
* Proper error propagation :tada:
  When there is for example a typo in function body, rustc can now show location
  of it instead of `test_case` location.
* Internally for tests crate uses `cargo insta` for snapshot testing
* Attribute is now compatible all other attributes like `#[should_panic]` 

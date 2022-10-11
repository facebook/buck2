# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.7.1] - 2022-07-10

### Changed
* Updated documentation to be easier to follow.

## [1.7.0] - 2022-03-22

### Added
* Added `Clone` implementation for `Box<RelativePath>` with `RelativePathBuf::into_boxed_relative_path` ([#37]).

[#37]: https://github.com/udoprog/relative-path/pull/37

## [1.6.1] - 2022-02-07

### Changed
* Updated documentation.

## [1.6.0] - 2021-12-03

### Added
* Added `RelativePath::is_normalized` to check for normalization ([#28]).
* Added `impl From<&RelativePath> for Box<RelativePath>` ([#26]).
* Added `impl From<RelativePathBuf> for Box<RelativePath>` ([#26]).
* Added `impl From<&RelativePath> for Arc<RelativePath>` ([#26]).
* Added `impl From<RelativePathBuf> for Arc<RelativePath>` ([#26]).
* Added `impl From<&RelativePath> for Rc<RelativePath>` ([#26]).
* Added `impl From<RelativePathBuf> for Rc<RelativePath>` ([#26]).

### Fixed
* Changed `to_path` and `to_logical_path` to treat empty paths better ([#29]).

[#29]: https://github.com/udoprog/relative-path/pull/29
[#28]: https://github.com/udoprog/relative-path/pull/28
[#26]: https://github.com/udoprog/relative-path/pull/26

## [1.5.0] - 2021-07-29

### Added
* Implement Extend and FromIterator for RelativePathBuf ([#25]).

[#25]: https://github.com/udoprog/relative-path/pull/25

## [1.4.0] - 2021-05-04

### Added
* `to_logical_path` as an alternative method of converting into a path.

### Changed
* `set_extension` no longer needs to allocate.
* `set_file_name` implementation to more closely match
  `std::path::PathBuf::set_file_name`.

## [1.3.1], [1.3.2] - 2020-07-12

### Changed
* Minor documentation fixes.

## [1.3.0] - 2020-07-12

### Fixed
* Changed `to_path` to ignore platform-specific absolute components ([#18]).

[#18]: https://github.com/udoprog/relative-path/pull/18

## [1.2.1] - 2020-06-16

### Changed
* Change signature of `RelativePath::strip_prefix` to be the same as `std::path::Path` ([#16]).

## [1.2.0] - 2020-06-13

### Added
* Added `FromPathError::kind` to get more detailed error information ([#15]).

### Changed
* Marked `FromPathErrorKind` `#[non_exhaustive]` which technically is a breaking
  change. But since it was not accessible from API code of this library, anyone
  who used it outside are on their own.

## [1.1.1] - 2020-06-13

### Changed
* Deprecated use of `std::error::Error::description` in favor of just having a `std::fmt::Display` impl.

## [1.1.0] - 2020-06-13

### Added
* Added `RelativePath::relative` to render a path relative from one path to another ([#14]).

[#16]: https://github.com/udoprog/relative-path/pull/16
[#15]: https://github.com/udoprog/relative-path/pull/15
[#14]: https://github.com/udoprog/relative-path/pull/14

[Unreleased]: https://github.com/udoprog/relative-path/compare/1.7.1...master
[1.7.1]: https://github.com/udoprog/relative-path/compare/1.7.0...1.7.1
[1.7.0]: https://github.com/udoprog/relative-path/compare/1.6.1...1.7.0
[1.6.1]: https://github.com/udoprog/relative-path/compare/1.6.0...1.6.1
[1.6.0]: https://github.com/udoprog/relative-path/compare/1.5.0...1.6.0
[1.5.0]: https://github.com/udoprog/relative-path/compare/1.4.0...1.5.0
[1.4.0]: https://github.com/udoprog/relative-path/compare/1.3.2...1.4.0
[1.3.2]: https://github.com/udoprog/relative-path/compare/1.3.1...1.3.2
[1.3.1]: https://github.com/udoprog/relative-path/compare/1.3.0...1.3.1
[1.3.0]: https://github.com/udoprog/relative-path/compare/1.2.1...1.3.0
[1.2.1]: https://github.com/udoprog/relative-path/compare/1.2.0...1.2.1
[1.2.0]: https://github.com/udoprog/relative-path/compare/1.1.1...1.2.0
[1.1.1]: https://github.com/udoprog/relative-path/compare/1.1.0...1.1.1
[1.1.0]: https://github.com/udoprog/relative-path/compare/1.0.0...1.1.0

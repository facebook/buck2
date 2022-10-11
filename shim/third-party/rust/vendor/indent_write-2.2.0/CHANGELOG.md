# Changelog

## 2.2.0

### Added

- Added `indentable` module, which contains utilities for indenting objects via `Display`, rather than adapting writers.
- Add `std` feature, which is enabled by default. When disabled, `indent-write` operates in `no_std` mode.

## 2.1.0

### Added

- Added a new constructor, `new_skip_indent`, to `fmt::IndentWriter` and `io::IndentWriter`. This constructor configures the write to omit the indent on the initial line.

## 2.0.0

### Removed

- Removed `IndentedWrite`, which was left over from a previous (unreleased) version of `indent-write` which I simply forgot to delete from the code before releasing

## 1.0.0

Initial release!

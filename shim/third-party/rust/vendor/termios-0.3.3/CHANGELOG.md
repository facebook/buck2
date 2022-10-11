# Change Log

## 0.3.2 (2020-04-05)
### Added
* ([#22](https://github.com/dcuddeback/termios-rs/pull/22))
  Added support for NetBSD.
* ([#21](https://github.com/dcuddeback/termios-rs/pull/21))
  Added support for illumos.

## 0.3.1 (2018-10-08)
### Added
* ([#19](https://github.com/dcuddeback/termios-rs/pull/19))
  Added support for Android.

### Changed
* Minimum supported version of Rust is now 1.13.0.

## 0.3.0 (2017-12-03)
### Added
* ([#6](https://github.com/dcuddeback/termios-rs/pull/6))
  Added DragonflyBSD support.

### Removed
* Removed `TAB3` from top-level export.

## 0.2.2 (2016-01-26)
### Added
* ([#5](https://github.com/dcuddeback/termios-rs/pull/5))
  Added OpenBSD support.

## 0.2.1 (2015-11-04)
### Changed
* Bumped `libc` dependency to v0.2.0.

## 0.2.0 (2015-10-15)
### Added
* ([#3](https://github.com/dcuddeback/termios-rs/pull/3))
  Added FreeBSD support.

### Removed
* Removed top-level export for `c_oflag` values that are not supported globally.

## 0.1.0 (2015-05-02)
### Added
* Added safe wrapper for `tcgetsid()`.
* Converted `Termios` to a wrapper struct with `Deref` and `DerefMut` implementations to access the
  inner `termios` struct.
* Export target-specific modules as `os::{platform}`.

### Changed
* Export target OS module as `os::target` instead of `os`.

### Removed
* Removed `Default` implementation for `Termios`.
* Removed `Termios::zeroed()`.

### Changed
* Replaced `c_int` with `RawFd`.

## 0.0.5 (2015-04-07)
### Fixed
* ([#2](https://github.com/dcuddeback/termios-rs/pull/2))
  Updated for compatibility with the new Rust beta channel:
  - `libc` is now an external crate.

## 0.0.4 (2015-04-03)
### Fixed
* Updated for compatibility with 1.0.0-beta:
  - Implementing `Copy` now requires implementing `Clone`.

## 0.0.3 (2015-03-29)
### Changed
* Migrated from `std::old_io` to `std::io`.

## 0.0.2 (2015-02-10)
### Added
* ([#1](https://github.com/dcuddeback/termios-rs/pull/1))
  Implement `PartialEq` and `Eq` for `Termios` struct.

### Fixed
* ([#1](https://github.com/dcuddeback/termios-rs/pull/1))
  Updated for compatibility with latest Rust nightly:
  - `std::io` renamed to `std::old_io`.
  - `Show` renamed to `Debug`.

## 0.0.1 (2015-02-01)
### Addded
* Added Linux support.
* Added OS X support.
* Added safe wrappers for termios API.

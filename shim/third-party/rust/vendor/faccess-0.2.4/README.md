[![Cargo](https://img.shields.io/crates/v/faccess.svg)][crate]
[![Documentation](https://docs.rs/faccess/badge.svg)][docs]
[![CI](https://github.com/Freaky/faccess/workflows/build/badge.svg)][ci]

# faccess

Basic cross-platform file accessibility checks for Rust.

## Synopsis

```rust
pub trait PathExt {
    fn access(&self, mode: AccessMode) -> std::io::Result<()>;
    fn readable(&self) -> bool;
    fn writable(&self) -> bool;
    fn executable(&self) -> bool;
}

impl PathExt for std::path::Path;
```

## Description

`faccess` provides an extension trait for `std::path::Path` which adds an
`access` method for checking the accessibility of a path for the given access
permissions — a bitwise-inclusive OR of one or more `AccessMode` flags
(`EXISTS`, `READ`, `WRITE`, `EXECUTE`).

It also provides convenience methods `readable`, `writable`, and `executable`
if only a single permission needs to be checked in a simple boolean fashion.

## Example

```rust
use std::path::Path;
use faccess::{AccessMode, PathExt};

let path = Path::new("/bin/ls");

assert!(path.access(AccessMode::READ | AccessMode::EXECUTE).is_ok());
assert!(path.readable());
assert!(!path.writable());
assert!(path.executable());
```

## Platform-specific Behaviour

On Unix platforms, `access` directly maps to [`faccessat(2)`], with the
`AT_EACCESS` flag used where available to test against the effective user and
group ID's.

On Windows, a complex custom implementation is used to approximate these
semantics in a best-effort fashion, using a mixture of file extension checks,
simply attempting to open a file, [`GetNamedSecurityInfoW`], and [`AccessCheck`],
depending on the permissions being checked.  This is similar to implementations
found in other languages.

On other platforms it simply proxies to `exists()` and `readonly()` as appropriate.

## Caveats

There is a history of time-of-check to time-of-use ([TOCTOU]) bugs with this
class of function, particularly with set-user-ID programs relying on them to
validate effective user/group permissions prior to accessing files on behalf
of other users.  They should not be relied upon in a security context.

[`faccessat(2)`]: https://pubs.opengroup.org/onlinepubs/9699919799/functions/access.html
[`GetNamedSecurityInfoW`]: https://docs.microsoft.com/en-us/windows/win32/api/aclapi/nf-aclapi-getnamedsecurityinfow
[`AccessCheck`]: https://docs.microsoft.com/en-us/windows/win32/api/securitybaseapi/nf-securitybaseapi-accesscheck
[TOCTOU]: https://en.wikipedia.org/wiki/Time-of-check_to_time-of-use
[crate]: https://crates.io/crates/faccess
[docs]: https://docs.rs/faccess
[ci]: https://github.com/Freaky/faccess/actions?query=workflow%3Abuild

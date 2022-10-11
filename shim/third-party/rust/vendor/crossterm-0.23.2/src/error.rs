//! Module containing error handling logic.

use std::io;

/// The `crossterm` result type.
pub type Result<T> = std::result::Result<T, ErrorKind>;

pub type ErrorKind = io::Error;

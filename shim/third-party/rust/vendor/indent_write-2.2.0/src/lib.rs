#![cfg_attr(not(feature = "std"), no_std)]

//! Simple indentation adapters for [`io::Write`][std::io::Write],
//! [`fmt::Write`][std::fmt::Write], and [`Display`][std::fmt::Display]. Each
//! adapter wraps a writer or writable object, and inserts an indentation at
//! the front of each non-empty line.
//!
//! See [`fmt::IndentWriter`], [`io::IndentWriter`], and
//! [`indentable::Indentable`] for examples.

pub mod fmt;
pub mod indentable;

#[cfg(feature = "std")]
pub mod io;

trait Inspect<T> {
    fn inspect(self, func: impl FnOnce(&T)) -> Self;
}

impl<T> Inspect<T> for Option<T> {
    #[inline]
    fn inspect(self, func: impl FnOnce(&T)) -> Self {
        if let Some(ref value) = self {
            func(value)
        }

        self
    }
}

impl<T, E> Inspect<T> for Result<T, E> {
    #[inline]
    fn inspect(self, func: impl FnOnce(&T)) -> Self {
        if let Ok(ref value) = self {
            func(value)
        }

        self
    }
}

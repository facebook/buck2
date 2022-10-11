/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Indented writer. Not a part of the public API.
//! Partially copied from [`indenter`](https://github.com/yaahc/indenter/blob/master/src/lib.rs)
//! crate by Jane Losare-Lusby <jlusby42@gmail.com>.

use std::fmt;

/// Helper struct for efficiently indenting multi line display implementations
///
/// # Explanation
///
/// This type will never allocate a string to handle inserting indentation. It instead leverages
/// the `write_str` function that serves as the foundation of the `std::fmt::Write` trait. This
/// lets it intercept each piece of output as its being written to the output buffer. It then
/// splits on newlines giving slices into the original string. Finally we alternate writing these
/// lines and the specified indentation to the output buffer.
pub(crate) struct Indented<'a, D: ?Sized> {
    inner: &'a mut D,
    needs_indent: bool,
    indentation: &'static str,
}

impl<T> fmt::Write for Indented<'_, T>
where
    T: fmt::Write + ?Sized,
{
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for (ind, line) in s.split('\n').enumerate() {
            if ind > 0 {
                self.inner.write_char('\n')?;
                self.needs_indent = true;
            }

            if self.needs_indent {
                // Don't render the line unless its actually got text on it
                if line.is_empty() {
                    continue;
                }

                write!(self.inner, "{}", self.indentation)?;
                self.needs_indent = false;
            }

            self.inner.write_fmt(format_args!("{}", line))?;
        }

        Ok(())
    }
}

/// Helper function for creating a default indenter
pub(crate) fn indented<'a, D: ?Sized>(f: &'a mut D, indentation: &'static str) -> Indented<'a, D> {
    Indented {
        inner: f,
        needs_indent: true,
        indentation,
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Write as _;

    use super::*;

    #[test]
    fn no_digits() {
        let input = "verify\nthis";
        let expected = "    verify\n    this";
        let mut output = String::new();

        indented(&mut output, "    ").write_str(input).unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn with_str() {
        let input = "verify\nthis";
        let expected = "...verify\n...this";
        let mut output = String::new();

        indented(&mut output, "...").write_str(input).unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn dyn_write() {
        let input = "verify\nthis";
        let expected = "    verify\n    this";
        let mut output = String::new();
        let writer: &mut dyn fmt::Write = &mut output;

        indented(writer, "    ").write_str(input).unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn nice_api_2() {
        let input = "verify\nthis";
        let expected = "  verify\n  this";
        let output = &mut String::new();

        write!(indented(output, "  "), "{}", input).unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn trailing_newlines() {
        let input = "verify\nthis\n";
        let expected = "  verify\n  this\n";
        let output = &mut String::new();

        write!(indented(output, "  "), "{}", input).unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn several_interpolations() {
        let input = "verify\nthis\n";
        let expected = "  verify\n  this\n   and verify\n  this\n";
        let output = &mut String::new();

        write!(indented(output, "  "), "{} and {}", input, input).unwrap();

        assert_eq!(expected, output);
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

/// Indent a text block: split text into lines, prepend each line with `indent`,
/// and join the lines back together by appending a newline to each line.
pub fn indent<'a>(indent: &'a str, text: &'a str) -> impl Display + 'a {
    struct Indent<'a> {
        indent: &'a str,
        text: &'a str,
    }

    impl<'a> Display for Indent<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for line in self.text.lines() {
                writeln!(f, "{}{}", self.indent, line)?;
            }
            Ok(())
        }
    }

    Indent { indent, text }
}

#[cfg(test)]
mod tests {
    use crate::indent::indent;

    #[test]
    fn test_indent() {
        assert_eq!("", indent("  ", "").to_string());
        assert_eq!("  foo\n", indent("  ", "foo").to_string());
        assert_eq!("  foo\n", indent("  ", "foo\n").to_string());
        assert_eq!("  foo\n  bar\n", indent("  ", "foo\nbar").to_string());
        assert_eq!("  foo\n  bar\n", indent("  ", "foo\nbar\n").to_string());
    }
}

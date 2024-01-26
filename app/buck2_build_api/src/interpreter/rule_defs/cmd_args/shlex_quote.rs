/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

/// Quote string for shell.
///
/// This is copy-paste from [`shlex`](https://github.com/comex/rust-shlex/) 1.0.
///
/// Generally shell quoting is unspecified, for example `a` can be quoted as:
/// - `a`
/// - `"a"`
/// - `'a'`
/// - and many more
///
/// to be used in shell. But we use shell quoting to generate arguments for argfiles.
/// And certain programs expect certain quoting style (for example, `cl.exe` expect double-quoted).
/// Additionally, we probably also incorrectly use shell quoting for `cmd.exe`.
///
/// Long story short, we should not depend on possible correct behavior change in `shlex` crate.
pub(crate) fn shlex_quote(in_str: &str) -> Cow<str> {
    if in_str.is_empty() {
        "\"\"".into()
    } else if in_str.bytes().any(|c| match c as char {
        '|' | '&' | ';' | '<' | '>' | '(' | ')' | '$' | '`' | '\\' | '"' | '\'' | ' ' | '\t'
        | '\r' | '\n' | '*' | '?' | '[' | '#' | '~' | '=' | '%' => true,
        _ => false,
    }) {
        let mut out: Vec<u8> = Vec::new();
        out.push(b'"');
        for c in in_str.bytes() {
            match c as char {
                '$' | '`' | '"' | '\\' => out.push(b'\\'),
                _ => (),
            }
            out.push(c);
        }
        out.push(b'"');
        unsafe { String::from_utf8_unchecked(out) }.into()
    } else {
        in_str.into()
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::rule_defs::cmd_args::shlex_quote::shlex_quote;

    #[test]
    fn test_quote() {
        assert_eq!(shlex_quote("foobar"), "foobar");
        assert_eq!(shlex_quote("foo bar"), "\"foo bar\"");
        assert_eq!(shlex_quote("\""), "\"\\\"\"");
        assert_eq!(shlex_quote(""), "\"\"");
    }
}

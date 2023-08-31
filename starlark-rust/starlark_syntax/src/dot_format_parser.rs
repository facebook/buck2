/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::mem;
use std::ops::Deref;

/// Parser for `.format()` arguments.
pub struct FormatParser<'a> {
    view: StringView<'a>,
}

/// Token in the format string.
#[derive(Debug, PartialEq)]
pub enum FormatToken<'a> {
    /// Text to copy verbatim to the output.
    Text(&'a str),
    Capture {
        /// Format part inside curly braces.
        capture: &'a str,
        /// The position of this capture. This does not include the curly braces.
        pos: usize,
    },
    Escape(EscapeCurlyBrace),
}

/// Emitted when processing an escape (`{{` or `}}`).
#[derive(Debug, PartialEq)]
pub enum EscapeCurlyBrace {
    Open,
    Close,
}

impl EscapeCurlyBrace {
    /// Get what this represents.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Open => "{",
            Self::Close => "}",
        }
    }

    /// Get back the escaped form for this.
    pub fn back_to_escape(&self) -> &'static str {
        match self {
            Self::Open => "{{",
            Self::Close => "}}",
        }
    }
}

impl<'a> FormatParser<'a> {
    #[inline]
    pub fn new(s: &'a str) -> Self {
        Self {
            view: StringView::new(s),
        }
    }

    /// Parse the next token from the format string.
    pub fn next(&mut self) -> anyhow::Result<Option<FormatToken<'a>>> {
        let mut i = 0;

        while i < self.view.len() {
            match self.view.as_bytes()[i] {
                b'{' | b'}' if i != 0 => {
                    let text = self.view.eat(i);
                    return Ok(Some(FormatToken::Text(text)));
                }
                b'{' => {
                    assert!(i == 0);
                    if self.view.starts_with("{{") {
                        self.view.eat(2);
                        return Ok(Some(FormatToken::Escape(EscapeCurlyBrace::Open)));
                    }
                    i = 1;
                    while i < self.view.len() {
                        match self.view.as_bytes()[i] {
                            b'}' => {
                                let pos = self.view.pos();
                                let capture = self.view.eat(i + 1); // Grab the closing brace.
                                return Ok(Some(FormatToken::Capture {
                                    capture: &capture[1..i],
                                    pos: pos + 1,
                                }));
                            }
                            b'{' => {
                                break;
                            }
                            _ => i += 1,
                        }
                    }
                    return Err(anyhow::anyhow!(
                        "Unmatched '{{' in format string `{}`",
                        self.view.original()
                    ));
                }
                b'}' => {
                    assert!(i == 0);
                    if self.view.starts_with("}}") {
                        self.view.eat(2);
                        return Ok(Some(FormatToken::Escape(EscapeCurlyBrace::Close)));
                    }
                    return Err(anyhow::anyhow!(
                        "Standalone '}}' in format string `{}`",
                        self.view.original()
                    ));
                }
                _ => i += 1,
            }
        }

        if i == 0 {
            Ok(None)
        } else {
            Ok(Some(FormatToken::Text(mem::take(&mut self.view).rem())))
        }
    }
}

/// A String and an index pointing into this string. This behaves as if you had just the part
/// starting at this index, and you can use `eat(n)` to advance.
#[derive(Default)]
struct StringView<'a> {
    /// The string we're viewing.
    s: &'a str,
    /// The current offset in bytes.
    i: usize,
}

impl<'a> StringView<'a> {
    fn new(s: &'a str) -> Self {
        Self { s, i: 0 }
    }

    fn eat(&mut self, n: usize) -> &'a str {
        let ret = &self.s[self.i..self.i + n];
        self.i += n;
        ret
    }

    fn pos(&self) -> usize {
        self.i
    }

    /// Get the current string.
    fn rem(&self) -> &'a str {
        &self.s[self.i..]
    }

    /// Get the original string.
    fn original(&self) -> &'a str {
        self.s
    }
}

impl<'a> Deref for StringView<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.rem()
    }
}

#[cfg(test)]
mod tests {
    use crate::dot_format_parser::FormatParser;
    use crate::dot_format_parser::FormatToken;

    #[test]
    fn test_parser_position() {
        let s = "foo{x}bar{yz}baz";
        let mut parser = FormatParser::new(s);
        assert_eq!(parser.next().unwrap(), Some(FormatToken::Text("foo")));
        assert_eq!(
            parser.next().unwrap(),
            Some(FormatToken::Capture {
                capture: "x",
                pos: 4,
            })
        );
        assert_eq!(parser.next().unwrap(), Some(FormatToken::Text("bar")));
        assert_eq!(
            parser.next().unwrap(),
            Some(FormatToken::Capture {
                capture: "yz",
                pos: 10,
            })
        );
        assert_eq!(parser.next().unwrap(), Some(FormatToken::Text("baz")));
        assert_eq!(parser.next().unwrap(), None);
    }
}

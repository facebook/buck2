/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use crossterm::style::Attribute;
use crossterm::style::Color;
use crossterm::style::ContentStyle;
use crossterm::style::ResetColor;
use crossterm::style::SetAttributes;
use crossterm::style::SetBackgroundColor;
use crossterm::style::SetForegroundColor;
use crossterm::style::StyledContent;
use crossterm::Command;
use termwiz::cell;
use termwiz::cell::Hyperlink;
use unicode_segmentation::Graphemes;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, thiserror::Error)]
enum SpanError {
    #[error("Word {0} contains non-space whitespace")]
    InvalidWhitespace(String),
}

/// A `Span` is a segment of text that may or may not have [`style`](crate::style) applied to it.
/// One may think of this as a unit of text.  It must be [`valid`](Span::valid) to be constructed.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Span {
    pub(crate) content: Cow<'static, str>,
    pub style: ContentStyle,
    pub hyperlink: Option<Hyperlink>,
}

/// Test whether a char is permissable to be inside a Span.
/// Whitespace is not allowed, except for spaces.
pub(crate) fn char_valid(c: char) -> bool {
    c == ' ' || !c.is_whitespace()
}

/// Strip invalid characters from the string.
pub(crate) fn sanitize<S: std::fmt::Display>(stringlike: S) -> String {
    let mut content = stringlike.to_string();
    content.retain(char_valid);
    content
}

impl Span {
    #[inline]
    // This could be const fn, but `crossterm::Attributes` constructor is not const.
    pub fn dash() -> Span {
        Span {
            content: Cow::Borrowed("-"),
            style: ContentStyle::default(),
            hyperlink: None,
        }
    }

    /// `Spans` are only valid if they do not contain any non whitespace characters other than a space.
    /// This is because the `superconsole` expects content to be mono-spaced and newlines, tabs, etc., violate this.
    pub fn valid(stringlike: &str) -> bool {
        !stringlike.contains(|c: char| !char_valid(c))
    }

    pub fn sanitized<S: std::fmt::Display>(string: S) -> Self {
        let content = sanitize(string);
        Span {
            content: Cow::Owned(content),
            style: ContentStyle::default(),
            hyperlink: None,
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    /// Create an unstyled span with the specified amount of whitespace padding.
    pub fn padding(amount: usize) -> Self {
        Self {
            content: Cow::Owned(format!("{:<width$}", "", width = amount)),
            style: ContentStyle::default(),
            hyperlink: None,
        }
    }

    /// Determine if this span is mergeable with another span, i.e. if they
    /// are equal except for content.
    pub fn is_mergeable_with(&self, other: &Span) -> bool {
        self.style == other.style && self.hyperlink == other.hyperlink
    }

    /// Attempt to create a new, unstyled span equivalent to the underlying stringlike.
    /// This will fail if the input string is not [`valid`](Span::valid).
    pub fn new_unstyled<S: std::fmt::Display>(stringlike: S) -> anyhow::Result<Self> {
        let owned = stringlike.to_string();
        if Self::valid(&owned) {
            Ok(Self {
                content: Cow::Owned(owned),
                style: ContentStyle::default(),
                hyperlink: None,
            })
        } else {
            Err(SpanError::InvalidWhitespace(owned).into())
        }
    }

    pub fn new_unstyled_lossy<S: std::fmt::Display>(stringlike: S) -> Self {
        let content = sanitize(stringlike);
        Self {
            content: Cow::Owned(content),
            style: ContentStyle::default(),
            hyperlink: None,
        }
    }

    /// Equivalent to [`new_unstyled`](Span::new_unstyled), except with styling.
    // TODO(brasselsprouts): Does this have to be a `String`? probably not.
    pub fn new_styled(content: StyledContent<String>) -> anyhow::Result<Self> {
        if Self::valid(content.content()) {
            Ok(Self {
                content: Cow::Owned(content.content().clone()),
                style: *content.style(),
                hyperlink: None,
            })
        } else {
            Err(SpanError::InvalidWhitespace(content.content().to_owned()).into())
        }
    }

    /// Equivalent to [`Self::new_styled`] except it will sanitize the content.
    pub fn new_styled_lossy(span: StyledContent<String>) -> Self {
        let content = sanitize(span.content());
        Self {
            content: Cow::Owned(content),
            style: *span.style(),
            hyperlink: None,
        }
    }

    pub fn new_colored(text: &str, color: Color) -> anyhow::Result<Self> {
        Self::new_styled(StyledContent::new(
            ContentStyle {
                foreground_color: Some(color),
                ..ContentStyle::default()
            },
            text.to_owned(),
        ))
    }

    pub fn new_colored_lossy(text: &str, color: Color) -> Self {
        Self::new_styled_lossy(StyledContent::new(
            ContentStyle {
                foreground_color: Some(color),
                ..ContentStyle::default()
            },
            text.to_owned(),
        ))
    }

    pub fn with_hyperlink(self, hyperlink: Option<Hyperlink>) -> Self {
        Self { hyperlink, ..self }
    }

    /// Returns the number of graphemes in the span.
    pub fn len(&self) -> usize {
        // Pulled this dep from another FB employee's project - better unicode support for terminal column widths.
        cell::unicode_column_width(&self.content, None)
    }

    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }

    /// Iterates over each [`Grapheme`](Graphemes) in the [`Span`].
    /// Applies the stylization of the `Span` to each `Grapheme`.
    /// Because a `Grapheme` is represented as another string, the sub-`Span` is represented as a `Span`.
    /// This `panics` if it encounters unicode that it doesn't know how to deal with.
    pub fn iter(&self) -> impl Iterator<Item = Span> + '_ {
        SpanIterator(&self.style, self.content.graphemes(true), &self.hyperlink)
    }

    pub(crate) fn render(&self, f: &mut impl fmt::Write) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }

        let mut reset_background = false;
        let mut reset_foreground = false;
        let mut reset_hyperlink = false;
        let mut reset = false;

        if let Some(bg) = self.style.background_color {
            SetBackgroundColor(bg).write_ansi(f)?;
            reset_background = true;
        }
        if let Some(fg) = self.style.foreground_color {
            SetForegroundColor(fg).write_ansi(f)?;
            reset_foreground = true;
        }
        if !self.style.attributes.is_empty() {
            SetAttributes(self.style.attributes).write_ansi(f)?;
            reset = true;
        }
        if let Some(hy) = &self.hyperlink {
            // TODO: IDs aren't supported here. This is a very naive implementation
            // ideally this should be supported by crossterm
            write!(f, "\x1B]8;;{}\x1B\\", hy.uri())?;
            reset_hyperlink = true;
        }

        write!(f, "{}", self.content)?;

        if reset_hyperlink {
            write!(f, "\x1B]8;;\x1B\\")?;
        }

        if reset {
            ResetColor.write_ansi(f)?;
        } else {
            if reset_background {
                SetBackgroundColor(Color::Reset).write_ansi(f)?;
            }
            if reset_foreground {
                SetForegroundColor(Color::Reset).write_ansi(f)?;
            }
        }

        Ok(())
    }

    pub fn fmt_for_test(&self) -> impl Display + '_ {
        fn to_snake_case(s: &str) -> String {
            let mut result = String::new();
            for c in s.chars() {
                if c.is_uppercase() {
                    if !result.is_empty() {
                        result.push('_');
                    }
                    result.push(c.to_ascii_lowercase());
                } else {
                    result.push(c);
                }
            }
            result
        }

        fn fmt_color(color: Color) -> impl Display {
            struct Impl(Color);
            impl Display for Impl {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    match self.0 {
                        Color::Reset
                        | Color::Black
                        | Color::Red
                        | Color::Green
                        | Color::Yellow
                        | Color::Blue
                        | Color::Magenta
                        | Color::Cyan
                        | Color::White
                        | Color::Grey
                        | Color::DarkGrey
                        | Color::DarkRed
                        | Color::DarkGreen
                        | Color::DarkYellow
                        | Color::DarkBlue
                        | Color::DarkMagenta
                        | Color::DarkCyan => {
                            write!(f, "{}", to_snake_case(&format!("{:?}", self.0)))
                        }
                        Color::Rgb { r, g, b } => write!(f, "rgb({}, {}, {})", r, g, b),
                        Color::AnsiValue(v) => write!(f, "ansi({})", v),
                    }
                }
            }
            Impl(color)
        }

        struct Impl<'a>(&'a Span);
        impl<'a> Display for Impl<'a> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let style_is_default = self.0.style.foreground_color.is_none()
                    && self.0.style.background_color.is_none()
                    && self.0.style.attributes.is_empty();
                if style_is_default {
                    write!(f, "{}", self.0.content)
                } else {
                    write!(f, "<span")?;
                    if let Some(fg) = self.0.style.foreground_color {
                        write!(f, " fg={}", fmt_color(fg))?;
                    }
                    if let Some(bg) = self.0.style.background_color {
                        write!(f, " bg={}", fmt_color(bg))?;
                    }
                    if !self.0.style.attributes.is_empty() {
                        let mut a = self.0.style.attributes;
                        for known in Attribute::iterator() {
                            if a.has(known) {
                                write!(f, " {}", to_snake_case(&format!("{:?}", known)))?;
                                a.unset(known);
                            }
                        }
                        if !a.is_empty() {
                            write!(f, " unknown_attributes={:?}", a)?;
                        }
                    }
                    write!(f, ">")?;
                    write!(f, "{}", self.0.content)?;
                    write!(f, "</span>")?;
                    Ok(())
                }
            }
        }

        Impl(self)
    }
}

impl TryFrom<String> for Span {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new_unstyled(value)
    }
}

impl TryFrom<&str> for Span {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new_unstyled(value)
    }
}

impl TryFrom<StyledContent<String>> for Span {
    type Error = anyhow::Error;

    fn try_from(value: StyledContent<String>) -> Result<Self, Self::Error> {
        Self::new_styled(value)
    }
}

pub(crate) struct SpanIterator<'a>(&'a ContentStyle, Graphemes<'a>, &'a Option<Hyperlink>);

impl<'a> Iterator for SpanIterator<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        let content = self.1.next();
        let style = self.0;
        let hyperlink = self.2;
        content.map(|content| Span {
            style: *style,
            content: Cow::Owned(content.to_owned()),
            hyperlink: hyperlink.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crossterm::style::Attributes;
    use crossterm::style::Stylize;

    use super::*;

    const BAD_WORD: &str = "i'm really gonna do it\n汉字";
    #[test]
    fn invalid_span() {
        assert!(Span::new_unstyled(BAD_WORD).is_err());
    }

    #[test]
    fn invalid_sanitized() {
        let sanitized = Span::sanitized(BAD_WORD);
        assert_eq!(sanitized.content, "i'm really gonna do it汉字");
        assert!(Span::new_unstyled(sanitized.content).is_ok());
    }

    #[test]
    fn multi_column_character() {
        let foot = "\u{1f9b6}";
        let span = Span::new_unstyled(foot).unwrap();
        assert_eq!(span.len(), 2);
    }

    #[test]
    fn test_padding_equality() {
        let lhs = Span::new_styled_lossy("   ".to_owned().red());
        let rhs = Span::new_styled_lossy("   ".to_owned().yellow());

        assert_ne!(lhs, rhs);

        let lhs = Span::new_styled_lossy("   ".to_owned().red().on_yellow());
        let rhs = Span::new_styled_lossy("   ".to_owned().yellow().on_green());

        assert_ne!(lhs, rhs);
    }

    #[test]
    fn test_inequality() {
        let lhs = Span::new_styled_lossy("hello".to_owned().red());
        let rhs = Span::new_styled_lossy("world".to_owned().yellow());

        assert_ne!(lhs, rhs);
    }

    #[test]
    fn test_equality() {
        let lhs = Span::new_styled_lossy("hello".to_owned().red().on_yellow());
        let rhs = Span::new_styled_lossy("hello".to_owned().red().on_yellow());

        assert_eq!(lhs, rhs);
    }

    #[test]
    fn test_fmt_for_test() {
        let span = Span::new_styled(StyledContent::new(
            ContentStyle {
                foreground_color: Some(Color::Cyan),
                background_color: None,
                underline_color: None,
                attributes: Attributes::from(Attribute::Bold) | Attributes::from(Attribute::Italic),
            },
            "fish".to_owned(),
        ))
        .unwrap();
        assert_eq!(
            "<span fg=cyan bold italic>fish</span>",
            span.fmt_for_test().to_string()
        );
    }
}

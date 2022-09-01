/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use crossterm::queue;
use crossterm::style::ContentStyle;
use crossterm::style::Print;
use crossterm::style::PrintStyledContent;
use crossterm::style::StyledContent;
use termwiz::cell;
use unicode_segmentation::Graphemes;
use unicode_segmentation::UnicodeSegmentation;

use crate::Error;

/// A `Span` is a segment of text that may or may not have [`style`](crate::style) applied to it.
/// One may think of this as a unit of text.  It must be [`valid`](Span::valid) to be constructed.
#[derive(Clone, Debug, Eq)]
#[non_exhaustive]
pub struct Span {
    pub(crate) content: String,
    pub stylization: Option<ContentStyle>,
}

impl PartialEq for Span {
    /// Spans are sequences of text with a common styling applied to them.
    /// In our model here, two spans are considered to be equal
    /// if they have the same text and the same styling.
    /// Styling is entirely a function of appearance - if they look the same,
    /// they have the same styling.
    /// Our underlying crossterm dep allows for an "unstyled" styling,
    /// where the text is marked as styled but doesn't actually appear different
    ///  than if no styling had been applied at all.
    /// Therefore, spans where there is no styling
    /// and spans where the styling is empty are visually equivalent.
    /// Additionally, if the text entirely consists of whitespaces, then foreground styling is ignored when comparing equality.
    fn eq(&self, other: &Self) -> bool {
        // this assumes single character content
        fn stylizations_eq(lhs: &Span, rhs: &Span) -> bool {
            let ignore_foreground_color = || lhs.is_padding() || rhs.is_padding();

            match (&lhs.stylization, &rhs.stylization) {
                (None, None) => true,
                (None, Some(other)) | (Some(other), None) => {
                    Span::is_unstyled(other, ignore_foreground_color())
                }
                (Some(lhs), Some(rhs)) => {
                    (lhs.foreground_color == rhs.foreground_color || ignore_foreground_color())
                        && lhs.background_color == rhs.background_color
                        && lhs.attributes == rhs.attributes
                }
            }
        }
        self.iter()
            .zip(other.iter())
            .all(|(lhs, rhs)| lhs.content == rhs.content && stylizations_eq(&lhs, &rhs))
    }
}

/// Test whether a char is permissable to be inside a Span.
/// Whitespace is not allowed, except for spaces.
pub fn char_valid(c: char) -> bool {
    c == ' ' || !c.is_whitespace()
}

/// Strip invalid characters from the string.
pub fn sanitize<S: std::fmt::Display>(stringlike: S) -> String {
    let mut content = stringlike.to_string();
    content.retain(char_valid);
    content
}

impl Span {
    /// `Spans` are only valid if they do not contain any non whitespace characters other than a space.
    /// This is because the `superconsole` expects content to be mono-spaced and newlines, tabs, etc., violate this.
    pub fn valid(stringlike: &str) -> bool {
        !stringlike.contains(|c: char| !char_valid(c))
    }

    fn is_unstyled(style: &ContentStyle, is_padding: bool) -> bool {
        style.attributes.is_empty()
            && style.background_color.is_none()
            && (style.foreground_color.is_none() || is_padding)
    }

    fn is_padding(&self) -> bool {
        self.content.as_bytes().iter().all(u8::is_ascii_whitespace)
    }

    pub fn sanitized<S: std::fmt::Display>(string: S) -> Self {
        let content = sanitize(string);
        Span {
            content,
            stylization: None,
        }
    }

    pub fn content(&self) -> Cow<str> {
        Cow::from(&self.content)
    }

    /// Create an unstyled span with the specified amount of whitespace padding.
    pub fn padding(amount: usize) -> Self {
        Self {
            content: format!("{:<width$}", "", width = amount),
            stylization: None,
        }
    }

    /// Attempt to create a new, unstyled span equivalent to the underlying stringlike.
    /// This will fail if the input string is not [`valid`](Span::valid).
    pub fn new_unstyled<S: std::fmt::Display>(stringlike: S) -> anyhow::Result<Self> {
        let owned = stringlike.to_string();
        if Self::valid(&owned) {
            Ok(Self {
                content: owned,
                stylization: None,
            })
        } else {
            Err(Error::InvalidWhitespace(owned).into())
        }
    }

    pub fn new_unstyled_lossy<S: std::fmt::Display>(stringlike: S) -> Self {
        let content = sanitize(stringlike);
        Self {
            content,
            stylization: None,
        }
    }

    /// Equivalent to [`new_unstyled`](Span::new_unstyled), except with styling.
    // TODO(brasselsprouts): Does this have to be a `String`? probably not.
    pub fn new_styled(content: StyledContent<String>) -> anyhow::Result<Self> {
        if Self::valid(content.content()) {
            Ok(Self {
                content: content.content().clone(),
                stylization: Some(*content.style()),
            })
        } else {
            Err(Error::InvalidWhitespace(content.content().to_owned()).into())
        }
    }

    /// Equivalent to [`Self::new_styled`] except it will sanitize the content.
    pub fn new_styled_lossy(span: StyledContent<String>) -> Self {
        let content = sanitize(span.content());
        Self {
            content,
            stylization: Some(*span.style()),
        }
    }

    /// Returns the number of graphemes in the span.
    pub fn len(&self) -> usize {
        // Pulled this dep from another FB employee's project - better unicode support for terminal column widths.
        cell::unicode_column_width(&self.content, None)
    }

    pub fn is_empty(self) -> bool {
        self.content.is_empty()
    }

    /// Iterates over each [`Grapheme`](Graphemes) in the [`Span`].
    /// Applies the stylization of the `Span` to each `Grapheme`.
    /// Because a `Grapheme` is represented as another string, the sub-`Span` is represented as a `Span`.
    /// This `panics` if it encounters unicode that it doesn't know how to deal with.
    pub fn iter(&self) -> SpanIterator {
        SpanIterator(&self.stylization, self.content.graphemes(true))
    }

    pub(crate) fn render(self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        match self.stylization {
            Some(style) => {
                queue!(
                    writer,
                    PrintStyledContent(StyledContent::new(style, self.content))
                )?;
            }
            None => {
                queue!(writer, Print(self.content))?;
            }
        }

        Ok(())
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

pub struct SpanIterator<'a>(&'a Option<ContentStyle>, Graphemes<'a>);

impl<'a> Iterator for SpanIterator<'a> {
    type Item = Span;

    fn next(&mut self) -> Option<Self::Item> {
        let content = self.1.next();
        content.map(|content| Span {
            stylization: *self.0,
            content: content.to_owned(),
        })
    }
}

#[cfg(test)]
mod tests {
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

        assert_eq!(lhs, rhs);

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
}

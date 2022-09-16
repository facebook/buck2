/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;

use crossterm::cursor::MoveToColumn;
use crossterm::queue;
use crossterm::style::Print;
use crossterm::terminal::Clear;
use crossterm::terminal::ClearType;
use unicode_segmentation::UnicodeSegmentation;

use crate::Span;

/// A `Line` is an abstraction for a collection of stylized or unstylized strings.
/// Since each `Span` denotes a portion of a single line, an ordered collection represents a single line of text.
#[derive(Default, Clone, Debug, Eq)]
pub struct Line(pub Vec<Span>);

impl PartialEq for Line {
    /// This equality merges spans with the same styles and checks for semantic equality.
    /// Semantic equality includes things like:
    /// - Spaces with different foreground colors appear the same.
    /// - Spans which are unstyled vs have idempotent styling.
    /// - Visually identical lines which are chunked into Spans differently.
    fn eq(&self, other: &Self) -> bool {
        // iterate grapheme by grapheme
        let lhs = self.0.iter().flat_map(Span::iter);
        let rhs = other.0.iter().flat_map(Span::iter);
        lhs.eq(rhs)
    }
}

impl Line {
    /// Constructor.
    #[allow(clippy::should_implement_trait)] // Because this is not a generic collection.
    pub fn from_iter<I: IntoIterator<Item = Span>>(iter: I) -> Line {
        Line(Vec::from_iter(iter))
    }

    pub fn unstyled(text: &str) -> anyhow::Result<Line> {
        Ok(Line::from_iter([Span::new_unstyled(text)?]))
    }

    pub fn sanitized(text: &str) -> Line {
        Line::from_iter([Span::sanitized(text)])
    }

    /// Return the length of the all words in the line added together.
    pub fn len(&self) -> usize {
        self.0.iter().map(Span::len).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Adds padding to the right side of the line.
    /// This adds a new unstyled word consisting entirely of the appropriate number of spaces.
    /// If no padding is requested, then no word is added.
    pub fn pad_right(&mut self, amount: usize) {
        if amount == 0 {
            return;
        }

        self.0.push(Span::padding(amount));
    }

    /// Same behavior as pad right, but the pad is on the left
    pub fn pad_left(&mut self, amount: usize) {
        if amount == 0 {
            return;
        }

        self.0.insert(0, Span::padding(amount));
    }

    /// Truncates the right side of the line until it is no longer than `max_width`.
    /// This will delete words entirely if they cannot fit.
    /// If the line is padded to 0, then it will become an empty line.
    pub fn truncate_line(&mut self, max_width: usize) {
        let mut cur_width = 0;

        for (index, span) in self.0.iter_mut().enumerate() {
            if cur_width >= max_width {
                self.0.truncate(index);
                break;
            }
            let word = span.content.graphemes(true);
            let word_len = word.clone().count();
            // if the line is going to overflow
            if word_len + cur_width > max_width {
                let word = word
                    // cut off the extra graphemes
                    .take(max_width.saturating_sub(cur_width) as usize)
                    .collect();

                // overwrite the current word
                // unfortunately, there is no way to mutably update the word, seemingly.
                span.content = word;

                // drop the remaining words
                self.0.truncate(index + 1);

                break;
            }
            cur_width += word_len;
        }
    }

    /// Slices out some middle subline of the Line. Removes the first `start` characters and
    /// keeps `width` characters after that.
    pub fn trim_ends(&mut self, mut start: usize, mut width: usize) {
        let mut owned = Vec::new();
        std::mem::swap(&mut owned, &mut self.0);

        for mut span in owned.into_iter() {
            // TODO(cjhopman): Other code here uses a mix of graphemes count and span.len() for computing the length of
            // a span, but these are totally different approaches. span.len() supposedly uses a more accurate approach,
            // so we should consider switching to that throughout. But, to keep this code self-consistent it uses only
            // the graphemes approach.
            let chars = span.content.graphemes(true);
            let len = chars.clone().count();

            if start > 0 && len < start {
                start -= len;
                continue;
            }

            let end = std::cmp::min(len, start + width);

            if start != 0 || end != len {
                span.content = chars.skip(start).take(end - start).collect();
            }
            self.0.push(span);

            width -= end - start;
            start = 0;

            if width == 0 {
                break;
            }
        }
    }

    /// Either calls [`pad_right`](Line::pad_right) or [`truncate_line`](Line::truncate_line) until the line is the exact width specified.
    /// This call acts on the right side of the `Line`.
    pub fn to_exact_width(&mut self, exact_width: usize) {
        let len = self.len();
        match len.cmp(&exact_width) {
            Ordering::Less => {
                self.pad_right(exact_width - len);
            }
            Ordering::Equal => {}
            Ordering::Greater => {
                self.truncate_line(exact_width);
            }
        }
    }

    /// Renders the formatted content of the line to `stdout`.
    /// The buffer must be flushed to produce output.
    pub fn render(self, writer: &mut Vec<u8>) -> anyhow::Result<()> {
        for word in self.0 {
            word.render(writer)?;
        }
        queue!(
            writer,
            Clear(ClearType::UntilNewLine),
            Print("\n"),
            MoveToColumn(0),
        )?;

        Ok(())
    }
}

impl FromIterator<Span> for Line {
    fn from_iter<T: IntoIterator<Item = Span>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl TryFrom<Vec<String>> for Line {
    type Error = anyhow::Error;

    fn try_from(other: Vec<String>) -> Result<Self, Self::Error> {
        other
            .into_iter()
            .map(Span::new_unstyled)
            .collect::<anyhow::Result<Line>>()
    }
}

impl TryFrom<Vec<&str>> for Line {
    type Error = anyhow::Error;

    fn try_from(other: Vec<&str>) -> Result<Self, Self::Error> {
        other
            .into_iter()
            .map(Span::new_unstyled)
            .collect::<anyhow::Result<Line>>()
    }
}

#[cfg(test)]
mod tests {
    use crossterm::style::Stylize;

    use super::*;

    #[test]
    fn test_words_len() {
        let normal = Line(vec![
            "test".try_into().unwrap(),
            "hello".try_into().unwrap(),
            "world".try_into().unwrap(),
        ]);

        assert_eq!(normal.len(), 14);

        assert_eq!(Line::default().len(), 0);
    }

    #[test]
    fn test_pad_line_right() {
        let mut test = Line(vec!["test".try_into().unwrap(), "ok".try_into().unwrap()]);
        let mut new_test: Line = test.clone();
        test.0.push(" ".repeat(4).try_into().unwrap());
        new_test.pad_right(4);
        assert_eq!(test, new_test);

        new_test.pad_right(6);
        test.0.push(" ".repeat(6).try_into().unwrap());
        assert_eq!(test, new_test);

        new_test.pad_right(10);
        test.0.push(" ".repeat(10).try_into().unwrap());
        assert_eq!(test, new_test);
    }

    #[test]
    fn test_pad_line_left() -> anyhow::Result<()> {
        let mut test: Line = Line(vec!["test".try_into()?, "ok".try_into()?]);
        let mut new_test: Line = test.clone();
        test.0.insert(0, " ".repeat(4).try_into()?);
        new_test.pad_left(4);
        assert_eq!(test, new_test);

        new_test.pad_left(6);
        test.0.insert(0, " ".repeat(6).try_into()?);
        assert_eq!(test, new_test);

        new_test.pad_left(10);
        test.0.insert(0, " ".repeat(10).try_into()?);
        assert_eq!(test, new_test);

        Ok(())
    }

    #[test]
    fn test_truncate_line() -> anyhow::Result<()> {
        let mut test: Line = vec!["test", "ok"].try_into()?;
        let mut new_test: Line = test.clone();
        test.truncate_line(10);
        assert_eq!(test, new_test);

        new_test.truncate_line(6);
        assert_eq!(test, new_test);

        new_test.truncate_line(5);
        test.0[1] = "o".try_into()?;
        assert_eq!(test, new_test);

        new_test.truncate_line(4);
        test.0.remove(1);
        assert_eq!(test, new_test);

        new_test.truncate_line(0);
        assert_eq!(new_test, Line::default());

        Ok(())
    }

    #[test]
    fn test_trim_ends() -> anyhow::Result<()> {
        let line = |spans: &[&str]| -> anyhow::Result<Line> { spans.to_vec().try_into() };
        let mut test = line(&["hello", "cat", "world"])?;
        test.trim_ends(0, 15);
        assert_eq!(test, line(&["hello", "cat", "world"])?);

        test.trim_ends(0, 13);
        assert_eq!(test, line(&["hello", "cat", "world"])?);

        let mut test = line(&["hello", "cat", "world"])?;
        test.trim_ends(2, 10);
        assert_eq!(test, line(&["llo", "cat", "worl"])?);

        let mut test = line(&["hello", "cat", "world"])?;
        test.trim_ends(6, 2);
        assert_eq!(test, line(&["at"])?);

        let mut test = line(&["hello", "cat", "world"])?;
        test.trim_ends(9, 2);
        assert_eq!(test, line(&["or"])?);

        Ok(())
    }

    #[test]
    fn test_equality() {
        let lhs = Line(vec![
            Span::new_styled_lossy("te".to_owned().dark_yellow()),
            Span::new_styled_lossy("st".to_owned().dark_yellow()),
            Span::new_styled_lossy("world".to_owned().dark_red()),
        ]);
        let rhs = Line(vec![
            Span::new_styled_lossy("test".to_owned().dark_yellow()),
            Span::new_styled_lossy("world".to_owned().dark_red()),
        ]);

        assert_eq!(lhs, rhs);
    }

    #[test]
    fn test_equality_unequal() {
        let lhs = Line(vec![Span::new_styled_lossy(
            "     xxx     ".to_owned().dark_yellow(),
        )]);
        let rhs = Line(vec![
            Span::new_styled_lossy("     ".to_owned().black()),
            Span::new_styled_lossy("xxx".to_owned().dark_yellow()),
            Span::new_styled_lossy("     ".to_owned().red()),
        ]);
        assert_eq!(lhs, rhs);
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    cmp::{self, Ordering},
    iter,
};

use crossterm::style::{Attribute, Attributes, Color};
use termwiz::{
    cell::Intensity,
    color::ColorSpec,
    escape::{
        csi::{Sgr, CSI},
        Action,
    },
};

use crate::{
    style::{ContentStyle, StyledContent},
    Dimensions, Line, Span,
};

pub type Lines = Vec<Line>;

/// Creates an instance of [`Lines`] with a style applied from a single multiline string
pub fn lines_from_multiline_string(multiline_string: &str, style: ContentStyle) -> Lines {
    multiline_string
        .lines()
        .map(|line| {
            let styled = StyledContent::new(style, line.to_owned());
            crate::line![Span::new_styled_lossy(styled)]
        })
        .collect()
}

/// State container that's used to parse strings with ANSI color codes in them.
#[derive(Default)]
struct ColoredStringParser {
    foreground_color: Option<Color>,
    background_color: Option<Color>,
    attributes: Attributes,
    line_buffer: String,
    spans: Vec<Span>,
}

impl ColoredStringParser {
    fn push_current(&mut self) {
        let sc = StyledContent::new(
            ContentStyle {
                foreground_color: self.foreground_color,
                background_color: self.background_color,
                attributes: self.attributes,
            },
            std::mem::take(&mut self.line_buffer),
        );
        self.spans.push(Span::new_styled_lossy(sc));
    }

    fn spec_to_color(spec: ColorSpec) -> Option<Color> {
        match spec {
            ColorSpec::Default => None,
            ColorSpec::PaletteIndex(idx) => Some(Color::AnsiValue(idx)),
            ColorSpec::TrueColor(tc) => Some(Color::from(tc.to_tuple_rgb8())),
        }
    }

    /// Given a line w/ some ANSI encoded color characters, turn it into a list of spans.
    fn parse_line(&mut self, parser: &mut termwiz::escape::parser::Parser, s: &str) -> Line {
        // Because we only stick "printable" characters onto the buffer, and skip any other
        // class of characters that we don't recognize, this should be a
        // roughly sanitized string. We do one more pass when we create the Span, just to
        // make sure, but that is a pretty fast check.
        parser.parse(s.as_bytes(), |a| match a {
            Action::Print(c) => {
                self.line_buffer.push(c);
            }
            Action::CSI(CSI::Sgr(Sgr::Reset)) => {
                self.push_current();
                self.foreground_color = None;
                self.background_color = None;
                self.attributes = Attributes::default();
            }
            Action::CSI(CSI::Sgr(Sgr::Intensity(intensity))) => {
                self.push_current();
                self.attributes = match intensity {
                    Intensity::Normal => Attributes::default(),
                    Intensity::Bold => Attributes::from(Attribute::Bold),
                    Intensity::Half => Attributes::from(Attribute::Dim),
                };
            }
            Action::CSI(CSI::Sgr(Sgr::Foreground(spec))) => {
                self.push_current();
                self.foreground_color = Self::spec_to_color(spec);
            }

            Action::CSI(CSI::Sgr(Sgr::Background(spec))) => {
                self.push_current();
                self.background_color = Self::spec_to_color(spec);
            }
            _ => {}
        });
        self.push_current();
        Line(std::mem::take(&mut self.spans))
    }
}

/// Takes a multiline string that might contain ANSI color codes, and returns a set of lines
/// that include spans representing those color codes.
///
/// Note that any other types of control characters are omitted, and certain whitespace
/// characters are also disallowed / replaced.
pub fn colored_lines_from_multiline_string(multiline_string: &str) -> Lines {
    let mut parser = termwiz::escape::parser::Parser::new();
    let mut color_parser = ColoredStringParser::default();
    multiline_string
        .lines()
        .map(|s| color_parser.parse_line(&mut parser, s))
        .collect()
}

/// Set of helper methods for `Vec<Line>`, that manipulate on each line individually.
pub trait LinesExt {
    /// truncates all lines to the same max width.
    fn truncate_lines(&mut self, max_width: usize);

    /// Returns the max column width of any line
    fn max_line_length(&self) -> usize;

    /// Given a set of lines, inserts the specified amount of padding to the longest line.
    /// However, the entire block of lines will remain a rectangle afterwards.
    /// This means that shorter lines will receive extra paddding such that it has the same length as the longest line
    fn pad_lines_right(&mut self, amount: usize);

    /// Prepends a fixed `amount` of padding to each row.
    /// Unlike `pad_lines_right`, this method does not add extra padding to shorter lines.
    /// The right end of the padding remains ragged.
    fn pad_lines_left(&mut self, amount: usize);

    /// Adds padding to each line dependent on its length so that each line is the same length.
    /// The line is left justified always.  This is because right or center aligning lines inherently justifies them.
    /// Therefore, there is no explicit method for justifying text any other way.
    fn justify(&mut self);

    /// Given a set of lines, set them all to the exact width
    fn set_lines_to_exact_width(&mut self, exact_width: usize);

    /// Extends the Lines list by the given length, adding empty lines at the bottom
    fn pad_lines_bottom(&mut self, num_lines_to_add: usize);

    /// Same functionality as `pad_lines_bottom` but on the top.
    fn pad_lines_top(&mut self, num_lines_to_add: usize);

    /// Truncates the line list to the given length, removing entries at the end.
    fn truncate_lines_bottom(&mut self, desired_length: usize);

    /// Sets the line list to the given length, padding or truncating from the bottom.
    fn set_lines_to_exact_length(&mut self, desired_width: usize);

    /// Truncates columns and rows that do not fit within a bounding box
    fn shrink_lines_to_dimensions(&mut self, dimensions: Dimensions);

    /// Formats and renders all lines to `stdout`.
    /// Notably, this *queues* the lines for rendering.  You must flush the buffer.
    /// If a limit is specified, no more than that amount will be drained.
    /// The limit is on the number of *lines*, **NOT** the number of *bytes*.
    /// Care should be taken with calling a limit of 0 - this will cause no lines to render and the buffer to never be drained.
    fn render(&mut self, writer: &mut Vec<u8>, limit: Option<usize>) -> anyhow::Result<()>;

    /// Returns the maximum line width and the number of lines.
    /// This corresponds to how much space a justified version of the output would take.
    fn dimensions(&self) -> anyhow::Result<Dimensions>;

    /// Sets the lines to the exact dimensions specified below, truncating or padding as necessary.
    fn set_lines_to_exact_dimensions(&mut self, desired_dimensions: Dimensions);
}

impl LinesExt for Vec<Line> {
    fn truncate_lines(&mut self, max_width: usize) {
        self.iter_mut()
            .for_each(|line| line.truncate_line(max_width));
    }

    fn max_line_length(&self) -> usize {
        // for each line in the LHS
        self.iter()
            // add the length of each word in the line
            .map(|line| line.len())
            // pick the maximum line length
            .max()
            // or set it to 0 if there are no lines on the left side
            .unwrap_or_default()
    }

    fn pad_lines_right(&mut self, amount: usize) {
        if amount == 0 {
            return;
        }

        let longest_len = self.max_line_length();
        for line in self.iter_mut() {
            let len = line.len();
            line.pad_right(amount + (longest_len - len));
        }
    }

    fn justify(&mut self) {
        let longest_len = self.max_line_length();
        for line in self.iter_mut() {
            let len = line.len();
            line.pad_right(longest_len - len);
        }
    }

    fn pad_lines_left(&mut self, amount: usize) {
        if amount == 0 {
            return;
        }

        self.iter_mut().for_each(|line| {
            line.pad_left(amount);
        });
    }

    fn set_lines_to_exact_width(&mut self, exact_width: usize) {
        self.iter_mut()
            .for_each(|line| line.to_exact_width(exact_width));
    }

    fn pad_lines_bottom(&mut self, amount: usize) {
        let mut extender = iter::repeat(Line::default()).take(amount);
        self.extend(&mut extender);
    }

    fn pad_lines_top(&mut self, amount: usize) {
        let extender = iter::repeat(Line::default()).take(amount);

        self.splice(0..0, extender);
    }

    fn truncate_lines_bottom(&mut self, desired_length: usize) {
        self.truncate(desired_length);
    }

    fn set_lines_to_exact_length(&mut self, desired_length: usize) {
        match self.len().cmp(&desired_length) {
            Ordering::Less => {
                self.pad_lines_bottom(desired_length - self.len());
            }
            Ordering::Equal => {}
            Ordering::Greater => {
                self.truncate_lines_bottom(desired_length);
            }
        }
    }

    fn shrink_lines_to_dimensions(&mut self, dimensions: Dimensions) {
        self.iter_mut()
            .for_each(|line| line.truncate_line(dimensions.x as usize));
        self.truncate(dimensions.y as usize);
    }

    fn render(&mut self, writer: &mut Vec<u8>, limit: Option<usize>) -> anyhow::Result<()> {
        let limit = limit.unwrap_or(self.len());
        let amt = cmp::min(limit, self.len());
        for line in self.drain(..amt) {
            line.render(writer)?;
        }

        Ok(())
    }

    fn dimensions(&self) -> anyhow::Result<Dimensions> {
        let x = self.max_line_length();
        let y = self.len();

        Ok((x, y).into())
    }

    fn set_lines_to_exact_dimensions(&mut self, Dimensions { x, y }: Dimensions) {
        self.set_lines_to_exact_length(y);
        self.set_lines_to_exact_width(x);
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use crossterm::style::{Attribute, Color};
    use gazebo::prelude::*;

    use super::*;

    #[test]
    fn truncate_lines() -> anyhow::Result<()> {
        let mut test: Vec<Line> = vec![
            vec!["test", "line"].try_into()?,
            vec!["another one"].try_into()?,
        ];

        let mut new_test = test.clone();
        new_test.truncate_lines(5);
        test[0] = vec!["test", "l"].try_into()?;
        test[1] = vec!["anoth"].try_into()?;
        assert_eq!(test, new_test);

        let mut empty = vec![];
        empty.truncate_lines(5);
        assert_eq!(empty, []);

        Ok(())
    }

    #[test]
    fn test_max_line_length() -> anyhow::Result<()> {
        let test = vec![
            Line::default(),
            vec!["test", "line"].try_into()?,
            vec!["another one"].try_into()?,
        ];

        assert_eq!(test.max_line_length(), 11);
        assert_eq!(vec![].max_line_length(), 0);

        Ok(())
    }

    #[test]
    fn test_pad_lines_right() -> anyhow::Result<()> {
        let mut test = vec![
            vec!["test", "line"].try_into()?, // 8 chars
            vec!["another one"].try_into()?,  // 11 chars
            Line::default(),                  // 0 chars
        ];
        let result = vec![
            vec!["test", "line", &" ".repeat(11 + 3)].try_into()?,
            vec!["another one", &" ".repeat(11)].try_into()?,
            vec![" ".repeat(11 + 11)].try_into()?,
        ];
        test.pad_lines_right(11);
        assert_eq!(test, result);

        Ok(())
    }

    #[test]
    fn test_pad_lines_left() -> anyhow::Result<()> {
        let mut test = vec![
            vec!["test", "line"].try_into()?, // 8 chars
            vec!["another one"].try_into()?,  // 11 chars
            Line::default(),                  // 0 chars
        ];
        let result = vec![
            vec![" ".repeat(11).as_ref(), "test", "line"].try_into()?,
            vec![" ".repeat(11).as_ref(), "another one"].try_into()?,
            vec![" ".repeat(11)].try_into()?,
        ];
        test.pad_lines_left(11);
        assert_eq!(test, result);

        Ok(())
    }

    #[test]
    fn test_pad_lines_bottom() -> anyhow::Result<()> {
        let mut test = vec![vec!["test"].try_into()?, vec!["another"].try_into()?];
        test.pad_lines_bottom(3);
        let result = vec![
            vec!["test"].try_into()?,
            vec!["another"].try_into()?,
            Line::default(),
            Line::default(),
            Line::default(),
        ];

        assert_eq!(test, result);

        Ok(())
    }

    #[test]
    fn test_pad_lines_top() -> anyhow::Result<()> {
        let mut test = vec![vec!["test"].try_into()?, vec!["another"].try_into()?];
        test.pad_lines_top(3);
        let result = vec![
            Line::default(),
            Line::default(),
            Line::default(),
            vec!["test"].try_into()?,
            vec!["another"].try_into()?,
        ];

        assert_eq!(test, result);

        Ok(())
    }

    #[test]
    fn test_truncate_lines_bottom() -> anyhow::Result<()> {
        let mut test = vec![
            vec!["test"].try_into()?,
            vec!["another"].try_into()?,
            vec!["one more"].try_into()?,
        ];
        test.truncate_lines_bottom(1);
        let output = vec![vec!["test"].try_into()?];
        assert_eq!(test, output);

        Ok(())
    }

    #[test]
    fn test_justify() -> anyhow::Result<()> {
        let mut test = vec![
            vec!["test"].try_into()?,
            Line::default(),
            vec!["ok"].try_into()?,
        ];

        test.justify();
        let expected = vec![
            vec!["test"].try_into()?,
            vec![" ".repeat(4)].try_into()?,
            vec!["ok", "  "].try_into()?,
        ];

        assert_eq!(test, expected);

        Ok(())
    }

    #[test]
    fn test_from_multiline_string() {
        let content = "foo bar\n\nbaz\nsome other line";
        let style = ContentStyle {
            foreground_color: Some(Color::Red),
            background_color: None,
            attributes: Default::default(),
        };
        let test = lines_from_multiline_string(content, style);
        let expected = vec![
            crate::line![Span::new_styled_lossy(StyledContent::new(
                style,
                "foo bar".to_owned()
            ))],
            crate::line![Span::new_styled_lossy(StyledContent::new(
                style,
                "".to_owned()
            ))],
            crate::line![Span::new_styled_lossy(StyledContent::new(
                style,
                "baz".to_owned()
            ))],
            crate::line![Span::new_styled_lossy(StyledContent::new(
                style,
                "some other line".to_owned()
            ))],
        ];

        assert_eq!(test, expected);
    }

    #[test]
    fn test_colored_from_multiline_string() {
        // Lots of little things we check in here, including that we persist state
        // across lines, that we only change the state that is requested to be changed,
        // that we control fg/bg properly, that we parse different ansi formats, etc.
        let test_string = format!(
            "This is a string
That has both {blue}8 bit blue {blue2}(in both formats)
in it,{reset} as well as {high_blue}256 color blue,
{reset}and {rgb_blue}RGB blue as well.{reset} It resets to the
console default at the end.
It can do {rgb_blue}{bg_blue}background colors, {blue}foreground colors,
{bold}colored, and {reset}normal {bold}bold,{remove_bold} and it
strips out {bs}invalid control sequences",
            blue = "\x1b[34m",
            blue2 = "\x1b[38;5;4m",
            high_blue = "\x1b[38;5;20m",
            rgb_blue = "\x1b[38;2;0;0;238m",
            bg_blue = "\x1b[44m",
            reset = "\x1b[0m",
            bold = "\x1b[1m",
            remove_bold = "\x1b[22m",
            bs = "\x1b[D"
        );

        let default = ContentStyle::default();
        // Both 34m and 38;5;4m parse to the same thing, so no need for blue2
        let blue = ContentStyle {
            foreground_color: Some(Color::AnsiValue(4)),
            ..Default::default()
        };
        let high_blue = ContentStyle {
            foreground_color: Some(Color::AnsiValue(20)),
            ..Default::default()
        };
        let rgb_blue = ContentStyle {
            foreground_color: Some(Color::from((0, 0, 238))),
            ..Default::default()
        };
        let bg_blue = ContentStyle {
            background_color: Some(Color::AnsiValue(4)),
            ..Default::default()
        };
        let rgb_blue_and_bg_blue = ContentStyle {
            foreground_color: rgb_blue.foreground_color,
            background_color: bg_blue.background_color,
            ..Default::default()
        };
        let blue_and_bg_blue = ContentStyle {
            foreground_color: blue.foreground_color,
            background_color: bg_blue.background_color,
            ..Default::default()
        };
        let blue_and_bg_blue_bold = ContentStyle {
            foreground_color: blue.foreground_color,
            background_color: bg_blue.background_color,
            attributes: Attributes::from(Attribute::Bold),
        };
        let bold = ContentStyle {
            attributes: Attributes::from(Attribute::Bold),
            ..Default::default()
        };

        let expected = vec![
            vec![StyledContent::new(default, "This is a string")],
            vec![
                StyledContent::new(default, "That has both "),
                StyledContent::new(blue, "8 bit blue "),
                StyledContent::new(blue, "(in both formats)"),
            ],
            vec![
                StyledContent::new(blue, "in it,"),
                StyledContent::new(default, " as well as "),
                StyledContent::new(high_blue, "256 color blue,"),
            ],
            vec![
                StyledContent::new(default, "and "),
                StyledContent::new(rgb_blue, "RGB blue as well."),
                StyledContent::new(default, " It resets to the"),
            ],
            vec![StyledContent::new(default, "console default at the end.")],
            vec![
                StyledContent::new(default, "It can do "),
                StyledContent::new(rgb_blue_and_bg_blue, "background colors, "),
                StyledContent::new(blue_and_bg_blue, "foreground colors,"),
            ],
            vec![
                StyledContent::new(blue_and_bg_blue_bold, "colored, and "),
                StyledContent::new(default, "normal "),
                StyledContent::new(bold, "bold,"),
                StyledContent::new(default, " and it"),
            ],
            vec![StyledContent::new(
                default,
                "strips out invalid control sequences",
            )],
        ];
        let expected: Lines = expected.into_map(|spans| {
            Line(spans.map(|sc| {
                Span::new_styled_lossy(StyledContent::new(*sc.style(), (*sc.content()).to_owned()))
            }))
        });

        let lines = colored_lines_from_multiline_string(&test_string);

        assert_eq!(expected, lines);
    }
}

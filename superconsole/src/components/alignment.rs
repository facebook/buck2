/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Oftentimes it is useful to align text inside the bounding box.
//! This component offers a composiion based approach to do so.
//! One may align text left, center, or right, and top, middle, or bottom.
//! Additionally, horizontally left aligned text may be optionally justified.

use crate::components::Blank;
use crate::Component;
use crate::Dimensions;
use crate::DrawMode;
use crate::Lines;
use crate::State;

/// Select the alignment of the vertical content
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum VerticalAlignmentKind {
    /// Content appears at the top.
    Top,
    /// Content appears approximately equidistant between top and bottom
    Center,
    /// Content appears at the bottom.
    Bottom,
}

/// Select the alignment of the horizontal content
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum HorizontalAlignmentKind {
    /// Content appears at the left.
    /// The argument determines whether the text is justified.
    Left(bool),
    /// Content appears in the middle.
    Center,
    /// Content appears to the right.
    Right,
}

/// The [`Aligned`](Aligned) [`Component`](Component) can be used to specify in which part of the view the content should live.
/// The [`HorizontalAlignmentKind`](HorizontalAlignmentKind) enum specifies the location relative to the x-axis.
/// The [`VerticalAlignmentKind`](VerticalAlignmentKind) enum specified the location relative to the y-axis.
#[derive(Debug)]
pub struct Aligned<C: Component = Box<dyn Component>> {
    pub child: C,
    pub horizontal: HorizontalAlignmentKind,
    pub vertical: VerticalAlignmentKind,
}

impl<C: Component> Aligned<C> {
    /// Creates a new `Alignment` component with the given alignments.
    pub fn new(
        child: C,
        horizontal: HorizontalAlignmentKind,
        vertical: VerticalAlignmentKind,
    ) -> Self {
        Self {
            child,
            horizontal,
            vertical,
        }
    }
}

impl Default for Aligned {
    fn default() -> Self {
        Self {
            child: Box::new(Blank),
            horizontal: HorizontalAlignmentKind::Left(false),
            vertical: VerticalAlignmentKind::Top,
        }
    }
}

impl<C: Component> Component for Aligned<C> {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let Dimensions { width, height } = dimensions;
        let mut output = self.child.draw(state, dimensions, mode)?;

        let number_of_lines = output.len();
        let padding_needed = height.saturating_sub(number_of_lines);
        match self.vertical {
            VerticalAlignmentKind::Top => {}
            VerticalAlignmentKind::Center => {
                let top_pad = padding_needed / 2;
                output.pad_lines_top(top_pad);
                output.pad_lines_bottom(padding_needed - top_pad);
            }
            VerticalAlignmentKind::Bottom => {
                output.pad_lines_top(padding_needed);
            }
        }

        match self.horizontal {
            HorizontalAlignmentKind::Left(justified) => {
                if justified {
                    output.justify();
                }
            }
            HorizontalAlignmentKind::Center => {
                for line in output.iter_mut() {
                    let output_len = line.len();
                    let padding_needed = width.saturating_sub(output_len);
                    let left_pad = padding_needed / 2;
                    line.pad_left(left_pad);
                    // handles any rounding issues
                    line.pad_right(padding_needed - left_pad);
                }
            }
            HorizontalAlignmentKind::Right => {
                for line in output.iter_mut() {
                    line.pad_left(width.saturating_sub(line.len()));
                }
            }
        }
        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use derive_more::AsRef;

    use crate::components::alignment::HorizontalAlignmentKind;
    use crate::components::alignment::VerticalAlignmentKind;
    use crate::components::echo::Echo;
    use crate::components::Aligned;
    use crate::components::DrawMode;
    use crate::Component;
    use crate::Dimensions;
    use crate::Line;
    use crate::Lines;
    use crate::State;

    #[derive(AsRef, Debug)]
    struct Msg(Lines);

    #[test]
    fn test_align_left_unjustified() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["pretty normal test"].try_into().unwrap(),
        ]);
        let component = Aligned::new(
            Echo(original.clone()),
            HorizontalAlignmentKind::Left(false),
            VerticalAlignmentKind::Top,
        );
        let dimensions = Dimensions::new(20, 20);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();

        assert_eq!(actual, original);
    }

    #[test]
    fn test_align_left_justified() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),        // 11 chars
            vec!["pretty normal test"].try_into().unwrap(), // 18 chars
            vec!["short"].try_into().unwrap(),              // 5 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Left(true),
            VerticalAlignmentKind::Top,
        );
        let dimensions = Dimensions::new(20, 20);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec!["hello world", &" ".repeat(18 - 11)]
                .try_into()
                .unwrap(),
            vec!["pretty normal test"].try_into().unwrap(),
            vec!["short", &" ".repeat(18 - 5)].try_into().unwrap(),
        ]);

        assert_eq!(actual, expected,);
    }

    #[test]
    fn test_align_col_center() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),          // 11 chars
            vec!["pretty normal testss"].try_into().unwrap(), // 20 chars
            vec!["shorts"].try_into().unwrap(),               // 6 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Center,
            VerticalAlignmentKind::Top,
        );
        let dimensions = Dimensions::new(20, 20);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec![" ".repeat(4).as_ref(), "hello world", &" ".repeat(5)]
                .try_into()
                .unwrap(),
            vec!["pretty normal testss"].try_into().unwrap(),
            vec![" ".repeat(7).as_ref(), "shorts", &" ".repeat(7)]
                .try_into()
                .unwrap(),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_align_right() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),           // 11 chars
            vec!["pretty normal testsss"].try_into().unwrap(), // 21 chars
            vec!["shorts"].try_into().unwrap(),                // 6 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Right,
            VerticalAlignmentKind::Top,
        );
        let dimensions = Dimensions::new(20, 20);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec![" ".repeat(9).as_ref(), "hello world"]
                .try_into()
                .unwrap(),
            vec!["pretty normal testss"].try_into().unwrap(),
            vec![" ".repeat(14).as_ref(), "shorts"].try_into().unwrap(),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_align_top() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),           // 11 chars
            vec!["pretty normal testsss"].try_into().unwrap(), // 21 chars
            vec!["shorts"].try_into().unwrap(),                // 6 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Left(false),
            VerticalAlignmentKind::Top,
        );
        let dimensions = Dimensions::new(20, 20);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["pretty normal testss"].try_into().unwrap(),
            vec!["shorts"].try_into().unwrap(),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_align_row_center() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),           // 11 chars
            vec!["pretty normal testsss"].try_into().unwrap(), // 21 chars
            vec!["shorts"].try_into().unwrap(),                // 6 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Left(false),
            VerticalAlignmentKind::Center,
        );
        let dimensions = Dimensions::new(20, 10);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            Line::default(),
            Line::default(),
            Line::default(),
            vec!["hello world"].try_into().unwrap(),
            vec!["pretty normal testss"].try_into().unwrap(),
            vec!["shorts"].try_into().unwrap(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
        ]);

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_align_bottom() {
        let original = Lines(vec![
            vec!["hello world"].try_into().unwrap(),           // 11 chars
            vec!["pretty normal testsss"].try_into().unwrap(), // 21 chars
            vec!["shorts"].try_into().unwrap(),                // 6 chars
        ]);
        let component = Aligned::new(
            Echo(original),
            HorizontalAlignmentKind::Left(false),
            VerticalAlignmentKind::Bottom,
        );
        let dimensions = Dimensions::new(20, 10);
        let actual = component
            .draw(&State::new(), dimensions, DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            vec!["hello world"].try_into().unwrap(),
            vec!["pretty normal testss"].try_into().unwrap(),
            vec!["shorts"].try_into().unwrap(),
        ]);

        assert_eq!(actual, expected);
    }
}

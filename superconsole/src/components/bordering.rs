/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::components::alignment::HorizontalAlignmentKind;
use crate::components::Aligned;
use crate::content::LinesExt;
use crate::Component;
use crate::Dimensions;
use crate::DrawMode;
use crate::Line;
use crate::Span;
use crate::State;

/// The `Bordered` component can be used to put borders on all sides of the output of its child.
/// This is useful for delimiting the boundaries of a component for reading and aesthetic purposes.
///
/// # About borders
/// Borders may be any `Span`, and can thus be more than a single character long.
///
/// They may consist of any valid sequence of utf-8 characters.
/// However, `superconsole` has a `unicode-segmentation` dependency, which is tied to a specific version of unicode.
/// Differing unicode versions have different `unicode-segmentation` dependencies, so if a newer (or older) version of unicode is used,
/// then some graphemes may cause superconsole to panic.  This is only relevant in the top and bottom borders, which iterate over the graphemes passed.
///
/// Horizontal borders (i.e. top and bottom) are transposed.  For example, if `top = Word::new_unstyled("@@")`,
/// then the resulting output would look something like this:
///
/// @@@@@@@@@@@@@@@@@@@@@
/// @@@@@@@@@@@@@@@@@@@@@
/// // rest of the output
#[derive(Debug)]
pub struct Bordered {
    child: Aligned,
    pub left: Option<Span>,
    pub right: Option<Span>,
    pub top: Option<Span>,
    pub bottom: Option<Span>,
}

/// The `BorderedSpec` allows the callee to specify the borders (or lack thereof) of each side.
/// The implementation of [`Default`] allows the user to leave some boundaries unspecified.
/// Unspecified boundaries default to:
/// * '|' if `left` or `right`
/// * '-' if `top` or `bottom`
#[derive(Debug)]
pub struct BorderedSpec {
    pub left: Option<Span>,
    pub right: Option<Span>,
    pub top: Option<Span>,
    pub bottom: Option<Span>,
}

impl Default for BorderedSpec {
    fn default() -> Self {
        let vertical = Some(Span::new_unstyled("|").unwrap());
        let horizontal = Some(Span::new_unstyled("-").unwrap());
        Self {
            left: vertical.clone(),
            right: vertical,
            top: horizontal.clone(),
            bottom: horizontal,
        }
    }
}

impl Bordered {
    pub fn new(
        child: Box<dyn Component>,
        BorderedSpec {
            left,
            right,
            top,
            bottom,
        }: BorderedSpec,
    ) -> Self {
        Self {
            child: Aligned {
                child,
                horizontal: HorizontalAlignmentKind::Left(true),
                ..Default::default()
            },
            left,
            right,
            top,
            bottom,
        }
    }
}

/// helper method to transpose horizontal padding.
fn construct_vertical_padding(padding: Span, width: usize) -> Vec<Line> {
    padding
        // iterating over the padding here allows us to retain the styling on each duplicate.
        .iter()
        .map(|mut span| {
            // iterator is a single character here, so fill to width.
            // it's possible that a word could be more than a single column, so the number of repetitions must reflect that.
            span.content = span.content.repeat(width / span.len());
            Line(vec![span])
        })
        .collect()
}

impl Component for Bordered {
    fn draw_unchecked(
        &self,
        state: &State,
        Dimensions { x, y }: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        // Reserve enough draw space for the walls.
        let opt_len = |opt_word: &Option<Span>| match opt_word {
            Some(word) => word.len(),
            None => 0,
        };
        let new_dims = Dimensions {
            x: x.saturating_sub(opt_len(&self.left) + opt_len(&self.right)),
            y: y.saturating_sub(opt_len(&self.top) + opt_len(&self.bottom)),
        };

        // The [`Aligned`] box ensures that the child is justified and bounded.
        let mut output = self.child.draw(state, new_dims, mode)?;

        for line in output.iter_mut() {
            if let Some(left) = &self.left {
                line.0.insert(0, left.clone());
            }
            if let Some(right) = &self.right {
                line.0.push(right.clone());
            }
        }
        if let Some(top) = &self.top {
            let lines = construct_vertical_padding(top.clone(), output.max_line_length());
            output.splice(0..0, lines.into_iter());
        }
        if let Some(bottom) = &self.bottom {
            let lines = construct_vertical_padding(bottom.clone(), output.max_line_length());
            output.extend(lines.into_iter());
        }

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use derive_more::AsRef;

    use super::*;
    use crate::components::Echo;

    #[derive(AsRef, Debug)]
    struct Msg(Vec<Line>);

    #[test]
    fn test_basic() -> anyhow::Result<()> {
        let component = Bordered::new(Box::new(Echo::<Msg>::new(true)), BorderedSpec::default());

        let msg = Msg(vec![
            vec!["Test"].try_into()?,              // 4 chars
            vec!["Longer"].try_into()?,            // 6 chars
            vec!["Even Longer", "ok"].try_into()?, // 13 chars
            Line::default(),
        ]);
        let output = component.draw(
            &crate::state![&msg],
            Dimensions::new(14, 5),
            DrawMode::Normal,
        )?;

        // A single character on the right side of the message gets truncated to make way for side padding
        let expected = vec![
            vec!["-".repeat(14)].try_into()?,
            vec!["|", "Test", &" ".repeat(12 - 4), "|"].try_into()?,
            vec!["|", "Longer", &" ".repeat(12 - 6), "|"].try_into()?,
            vec!["|", "Even Longer", "o", "|"].try_into()?,
            vec!["-".repeat(14)].try_into()?,
        ];

        assert_eq!(output, expected);

        Ok(())
    }

    #[test]
    fn test_complex() -> anyhow::Result<()> {
        let component = Bordered::new(
            Box::new(Echo::<Msg>::new(true)),
            BorderedSpec {
                top: Some("@@@".try_into()?),
                left: None,
                bottom: Some("@".try_into()?),
                ..Default::default()
            },
        );

        let msg = Msg(vec![
            vec!["Test"].try_into()?,              // 4 chars
            vec!["Longer"].try_into()?,            // 6 chars
            vec!["Even Longer", "ok"].try_into()?, // 13 chars
            Line::default(),
        ]);
        let output = component.draw(
            &crate::state![&msg],
            Dimensions::new(13, 7),
            DrawMode::Normal,
        )?;

        // A single character on the right side of the message gets truncated to make way for side padding
        let expected = vec![
            vec!["@".repeat(13)].try_into()?,
            vec!["@".repeat(13)].try_into()?,
            vec!["@".repeat(13)].try_into()?,
            vec!["Test", &" ".repeat(12 - 4), "|"].try_into()?,
            vec!["Longer", &" ".repeat(12 - 6), "|"].try_into()?,
            vec!["Even Longer", "o", "|"].try_into()?,
            vec!["@".repeat(13)].try_into()?,
        ];

        assert_eq!(output, expected);

        Ok(())
    }

    #[test]
    fn test_multi_width_unicode() -> anyhow::Result<()> {
        let multi_width = "🦶";

        let component = Bordered::new(
            Box::new(Echo::<Msg>::new(true)),
            BorderedSpec {
                top: Some(multi_width.try_into()?),
                left: None,
                right: None,
                bottom: None,
            },
        );

        let msg = Msg(vec![vec!["Tested"].try_into()?]);

        let output = component.draw(
            &crate::state![&msg],
            Dimensions::new(13, 7),
            DrawMode::Normal,
        )?;
        let expected = vec![vec!["🦶🦶🦶"].try_into()?, vec!["Tested"].try_into()?];

        assert_eq!(output, expected);
        Ok(())
    }
}

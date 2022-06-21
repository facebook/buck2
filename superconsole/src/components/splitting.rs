/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Splitting is one of the most primitive building blocks of a fully featured UI.
//! This module contains components and enums that allow for splits along either dimension.

use gazebo::prelude::*;
use itertools::Itertools;

use crate::content::LinesExt;
use crate::Component;
use crate::Dimensions;
use crate::Direction;
use crate::DrawMode;
use crate::Line;
use crate::State;

/// Controls the way the splitter displays its children.
#[derive(Clone, Debug)]
pub enum SplitKind {
    // Each child component takes up area proportional to its size relative to the other sizes.
    Sized(Vec<f64>),
    // Each child component takes up an equal % of the area long the given dimension.
    Equal,
    // Each child may take as much space as it would like along the given dimension.
    // No extra padding is given between children.
    Adaptive,
}

/// Internally, we want to alias away the Equal case
#[derive(Clone, Debug)]
enum InternalSplitKind {
    SizedNormalized(Vec<f64>),
    Adaptive,
}

impl SplitKind {
    fn to_internal_split_kind(self, children_len: usize) -> InternalSplitKind {
        match self {
            SplitKind::Sized(sizes) => {
                assert_eq!(
                    sizes.len(),
                    children_len,
                    "There must be an equal number of ratios and children."
                );

                let total: f64 = sizes.iter().sum();
                let normalized_sizes = sizes.into_map(|size| size / total);

                InternalSplitKind::SizedNormalized(normalized_sizes)
            }
            SplitKind::Equal => {
                InternalSplitKind::SizedNormalized(vec![1.0 / children_len as f64; children_len])
            }
            SplitKind::Adaptive => InternalSplitKind::Adaptive,
        }
    }
}

impl InternalSplitKind {
    pub fn draw(
        &self,
        children: &[Box<dyn Component>],
        direction: Direction,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Vec<Line>>> {
        match self {
            InternalSplitKind::SizedNormalized(sizes) => children
                .iter()
                .zip(sizes.iter())
                .map(|(child, size)| -> anyhow::Result<_> {
                    // allocate alloted size
                    let child_dimension = dimensions.multiply(*size, direction);
                    let mut output = child.draw(state, child_dimension, mode)?;

                    // bound non-splitting direction, pad splitting direction
                    match direction {
                        Direction::Horizontal => {
                            output.truncate_lines_bottom(child_dimension.y as usize);
                            output.set_lines_to_exact_width(child_dimension.x as usize);
                        }
                        Direction::Vertical => {
                            output.truncate_lines(child_dimension.x as usize);
                            output.set_lines_to_exact_length(child_dimension.y as usize);
                        }
                    }

                    Ok(output)
                })
                .collect(),
            InternalSplitKind::Adaptive => {
                let mut available = dimensions;
                children.try_map(|child| {
                    let mut output = child.draw(state, available, mode)?;
                    output.shrink_lines_to_dimensions(dimensions);

                    // decrease size by however much was just used
                    let used = Dimensions::dimension_from_output_truncated(&output, direction);
                    available = available.saturating_sub(used, direction);

                    Ok(output)
                })
            }
        }
    }
}

/// [`Splits`](SplitKind) along a given [`direction`](crate::Direction) for its child [`components`](Component).
/// Child components are truncated to the bounds passed to them.
#[derive(Debug)]
pub struct Split {
    children: Vec<Box<dyn Component>>,
    direction: Direction,
    split: InternalSplitKind,
}

impl Split {
    /// Preconditions:
    /// * At least one child.
    /// * If Sized, then ratios must sum to approximately 1.
    /// * If Sized, then there must be as many ratios as components
    pub fn new(children: Vec<Box<dyn Component>>, direction: Direction, split: SplitKind) -> Self {
        assert!(!children.is_empty(), "Must have at least one child.");

        let split = split.to_internal_split_kind(children.len());

        Self {
            children,
            direction,
            split,
        }
    }
}

impl Component for Split {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        let outputs = self
            .split
            .draw(&self.children, self.direction, state, dimensions, mode)?;

        Ok(match self.direction {
            Direction::Horizontal => {
                // unwrap ok because guaranteed > 0 children from constructor
                let longest = outputs.iter().map(|output| output.len()).max().unwrap();

                // pad all other outputs to be the same length.  Then, justify them to be uniform blocks.
                let padded = outputs.into_iter().update(|output| {
                    output.set_lines_to_exact_length(longest);
                    output.justify();
                });

                // can't do arbitrary zip, so this'll have to do
                padded
                    .reduce(|mut all, output| {
                        for (all_line, mut output_line) in all.iter_mut().zip(output.into_iter()) {
                            all_line.0.append(&mut output_line.0);
                        }

                        all
                    })
                    // safe to unwrap because at least one child component required
                    .unwrap()
            }
            Direction::Vertical => outputs.into_iter().flatten().collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::iter;

    use derive_more::AsRef;

    use super::Split;
    use super::SplitKind;
    use crate::components::Echo;
    use crate::Component;
    use crate::Direction;
    use crate::DrawMode;
    use crate::Line;
    use crate::State;

    #[derive(AsRef, Debug)]
    struct Echo1(Vec<Line>);
    #[derive(AsRef, Debug)]
    struct Echo2(Vec<Line>);

    #[derive(AsRef, Debug)]
    struct Echo3(Vec<Line>);

    fn make_splitter(kind: SplitKind, dimension: Direction) -> Split {
        Split::new(
            vec![
                Box::new(Echo::<Echo1>::new(false)),
                Box::new(Echo::<Echo2>::new(false)),
            ],
            dimension,
            kind,
        )
    }

    mod horizontal {
        use std::convert::TryInto;

        use super::*;
        use crate::Dimensions;
        #[test]
        fn test_adaptive() {
            let splitter = make_splitter(SplitKind::Adaptive, Direction::Horizontal);
            let mut state = State::new();

            // left rows all the same length
            let mut left_msg = vec![
                vec!["test"].try_into().unwrap(),
                vec!["ok", "so"].try_into().unwrap(),
                vec!["sync"].try_into().unwrap(),
            ];
            let right_msg = vec![
                vec!["xx"].try_into().unwrap(),
                vec!["yyy"].try_into().unwrap(),
                vec!["z"].try_into().unwrap(),
            ];
            let horizontal = vec![
                vec!["test", "xx", " "].try_into().unwrap(),
                vec!["ok", "so", "yyy"].try_into().unwrap(),
                vec!["sync", "z", "  "].try_into().unwrap(),
            ];

            let msg1 = Echo1(left_msg.clone());
            let msg2 = Echo2(right_msg);
            state.insert(&msg1);
            state.insert(&msg2);

            let output = splitter
                .draw(&state, Dimensions::new(10, 10), DrawMode::Normal)
                .unwrap();
            assert_eq!(output, horizontal);

            // uneven left row
            left_msg[0] = vec!["hello world"].try_into().unwrap();
            let horizontal = vec![
                vec!["hello world", "xx", " "].try_into().unwrap(),
                vec!["ok", "so", &" ".repeat(7), "yyy"].try_into().unwrap(),
                vec!["sync", &" ".repeat(7), "z", "  "].try_into().unwrap(),
            ];
            let msg1 = Echo1(left_msg);
            state.insert(&msg1);

            let output = splitter
                .draw(&state, Dimensions::new(15, 15), DrawMode::Normal)
                .unwrap();
            assert_eq!(output, horizontal);
        }

        #[test]
        fn test_equal() {
            let splitter = make_splitter(SplitKind::Equal, Direction::Horizontal);
            let mut state = State::new();

            // left rows all the same length
            let mut left_msg = vec![
                vec!["test"].try_into().unwrap(),
                vec!["ok", "so"].try_into().unwrap(),
                vec!["sync"].try_into().unwrap(),
            ];
            let right_msg = vec![
                vec!["xx"].try_into().unwrap(),
                vec!["yyy"].try_into().unwrap(),
                vec!["z"].try_into().unwrap(),
            ];
            let horizontal = vec![
                vec!["test", &" ".repeat(6), "xx", &" ".repeat(8)]
                    .try_into()
                    .unwrap(),
                vec!["ok", "so", &" ".repeat(6), "yyy", &" ".repeat(7)]
                    .try_into()
                    .unwrap(),
                vec!["sync", &" ".repeat(6), "z", &" ".repeat(9)]
                    .try_into()
                    .unwrap(),
            ];

            let msg1 = Echo1(left_msg.clone());
            let msg2 = Echo2(right_msg);
            state.insert(&msg1);
            state.insert(&msg2);

            let output = splitter
                .draw(&state, Dimensions::new(20, 20), DrawMode::Normal)
                .unwrap();
            assert_eq!(output, horizontal);

            // uneven left row
            left_msg[0] = vec!["hello worl"].try_into().unwrap();
            let horizontal = vec![
                vec!["hello worl", "xx", &" ".repeat(8)].try_into().unwrap(),
                vec!["ok", "so", &" ".repeat(6), "yyy", &" ".repeat(7)]
                    .try_into()
                    .unwrap(),
                vec!["sync", &" ".repeat(6), "z", &" ".repeat(9)]
                    .try_into()
                    .unwrap(),
            ];
            let msg1 = Echo1(left_msg);
            state.insert(&msg1);

            let output = splitter
                .draw(&state, Dimensions::new(20, 20), DrawMode::Normal)
                .unwrap();
            assert_eq!(output, horizontal);
        }

        #[test]
        fn test_many_sized() {
            let splitter = Split::new(
                vec![
                    Box::new(Echo::<Echo1>::new(false)),
                    Box::new(Echo::<Echo2>::new(false)),
                    Box::new(Echo::<Echo3>::new(false)),
                ],
                Direction::Horizontal,
                SplitKind::Sized(vec![0.25, 0.5, 0.25]),
            );
            let msg1 = Echo1(vec![
                vec!["test", "ok"].try_into().unwrap(),
                vec!["also"].try_into().unwrap(),
            ]);
            let msg2 = Echo2(vec![vec!["hola"].try_into().unwrap()]);
            let msg3 = Echo3(vec![vec!["way way way way too long"].try_into().unwrap()]);
            let state = crate::state!(&msg1, &msg2, &msg3);

            let output = splitter
                .draw(&state, Dimensions::new(20, 20), DrawMode::Normal)
                .unwrap();

            let expected = vec![
                vec!["test", "o", "hola", &" ".repeat(6), "way w"]
                    .try_into()
                    .unwrap(),
                vec!["also", " ", &" ".repeat(10), &" ".repeat(5)]
                    .try_into()
                    .unwrap(),
            ];

            assert_eq!(output, expected);
        }
    }

    mod vertical {
        use std::convert::TryInto;

        use super::*;
        use crate::Dimensions;
        #[test]
        fn test_equal() {
            let splitter = make_splitter(SplitKind::Equal, Direction::Vertical);
            let mut state = State::new();

            let top = vec![
                vec!["Line 1"].try_into().unwrap(),
                vec!["Line 2222"].try_into().unwrap(),
            ];
            let mut bottom = vec![
                vec!["Line 11"].try_into().unwrap(),
                vec!["Line 12"].try_into().unwrap(),
                vec!["Last line just kiddi"].try_into().unwrap(),
            ];
            let msg1 = Echo1(top.clone());
            let msg2 = Echo2(bottom.clone());
            state.insert(&msg1);
            state.insert(&msg2);

            let mut output = top;
            output.extend(iter::repeat(Line::default()).take(8));
            output.append(&mut bottom);
            output.extend(iter::repeat(Line::default()).take(7));

            let drawn = splitter
                .draw(&state, Dimensions::new(20, 20), DrawMode::Normal)
                .unwrap();

            assert_eq!(drawn, output);
        }

        #[test]
        fn test_adaptive() {
            let splitter = make_splitter(SplitKind::Adaptive, Direction::Vertical);

            let top = vec![
                vec!["Line 1"].try_into().unwrap(),
                vec!["Line 2222"].try_into().unwrap(),
            ];
            let mut bottom = vec![
                vec!["Line 11"].try_into().unwrap(),
                vec!["Line 12"].try_into().unwrap(),
                vec!["Last line just kiddi"].try_into().unwrap(),
            ];
            let msg1 = Echo1(top.clone());
            let msg2 = Echo2(bottom.clone());

            let mut output = top;
            output.append(&mut bottom);

            let drawn = splitter
                .draw(
                    &crate::state!(&msg1, &msg2),
                    Dimensions::new(20, 20),
                    DrawMode::Normal,
                )
                .unwrap();
            assert_eq!(drawn, output);
        }

        #[test]
        fn test_many_sized() {
            let splitter = Split::new(
                vec![
                    Box::new(Echo::<Echo1>::new(false)),
                    Box::new(Echo::<Echo2>::new(false)),
                    Box::new(Echo::<Echo3>::new(false)),
                ],
                Direction::Vertical,
                SplitKind::Sized(vec![0.25, 0.5, 0.25]),
            );
            let msg1 = Echo1(vec![
                vec!["line1"].try_into().unwrap(),
                vec!["line2"].try_into().unwrap(),
                vec!["line3"].try_into().unwrap(),
                vec!["line4"].try_into().unwrap(),
                vec!["line5"].try_into().unwrap(),
                vec!["line6"].try_into().unwrap(),
            ]);
            let msg2 = Echo2(vec![vec!["line7"].try_into().unwrap()]);
            let msg3 = Echo3(vec![vec!["line8"].try_into().unwrap()]);
            let state = crate::state!(&msg1, &msg2, &msg3);

            let output = splitter
                .draw(&state, Dimensions::new(20, 20), DrawMode::Normal)
                .unwrap();

            let expected = vec![
                vec!["line1"].try_into().unwrap(),
                vec!["line2"].try_into().unwrap(),
                vec!["line3"].try_into().unwrap(),
                vec!["line4"].try_into().unwrap(),
                vec!["line5"].try_into().unwrap(),
                vec!["line7"].try_into().unwrap(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
                vec!["line8"].try_into().unwrap(),
                Line::default(),
                Line::default(),
                Line::default(),
                Line::default(),
            ];

            assert_eq!(output, expected);
        }
    }

    mod panics {
        use super::*;
        use crate::components::Blank;

        #[test]
        #[should_panic(expected = "Must have at least one child.")]
        fn test_no_children() {
            Split::new(vec![], Direction::Horizontal, SplitKind::Equal);
        }

        #[test]
        #[should_panic(expected = "There must be an equal number of ratios and children.")]
        fn test_different_ratio_count() {
            Split::new(
                vec![Box::new(Blank), Box::new(Blank), Box::new(Blank)],
                Direction::Vertical,
                SplitKind::Sized(vec![0.4, 0.4]),
            );
        }
    }
}

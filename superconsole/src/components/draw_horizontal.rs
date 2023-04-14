/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::Component;
use crate::Dimensions;
use crate::DrawMode;
use crate::Lines;

/// Draw components horizontally, each next component is stacked on the right.
pub struct DrawHorizontal {
    /// Original dimensions.
    dim: Dimensions,
    /// Remaining dimensions.
    rem_width: usize,
    /// What we have drawn so far.
    blocks: Vec<Lines>,
}

impl DrawHorizontal {
    /// Construct rendered with given dimensions.
    pub fn new(dimensions: Dimensions) -> DrawHorizontal {
        DrawHorizontal {
            dim: dimensions,
            rem_width: dimensions.width,
            blocks: Vec::new(),
        }
    }

    /// Add another component.
    /// New component `draw` is called with remaining dimensions.
    pub fn draw(&mut self, component: &dyn Component, mode: DrawMode) -> anyhow::Result<()> {
        // We call `draw` even if no space is left, but maybe we should not.
        let output = component.draw(
            Dimensions {
                width: self.rem_width,
                height: self.dim.height,
            },
            mode,
        )?;
        self.rem_width = self.rem_width.saturating_sub(output.max_line_length());
        self.blocks.push(output);
        Ok(())
    }

    /// Finish rendering, return the result.
    pub fn finish(self) -> Lines {
        let mut lines = Lines::join_horizontally(self.blocks);
        // This should be no-op, but just in case.
        lines.shrink_lines_to_dimensions(self.dim);
        lines
    }
}

#[cfg(test)]
mod tests {

    use crate::components::draw_horizontal::DrawHorizontal;
    use crate::Component;
    use crate::Dimensions;
    use crate::DrawMode;
    use crate::Line;
    use crate::Lines;

    #[test]
    fn test_draw_horizontal() {
        #[derive(Debug)]
        struct C0;
        #[derive(Debug)]
        struct C1;

        impl Component for C0 {
            fn draw_unchecked(
                &self,

                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> anyhow::Result<Lines> {
                assert_eq!(
                    Dimensions {
                        width: 50,
                        height: 10,
                    },
                    dimensions
                );
                Ok(Lines(vec![
                    Line::sanitized("quick"),
                    Line::sanitized("fox"),
                    Line::sanitized("over"),
                ]))
            }
        }

        impl Component for C1 {
            fn draw_unchecked(
                &self,

                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> anyhow::Result<Lines> {
                assert_eq!(
                    Dimensions {
                        width: 45,
                        height: 10,
                    },
                    dimensions
                );
                Ok(Lines(vec![
                    Line::sanitized("brown"),
                    Line::sanitized("jumped"),
                ]))
            }
        }

        let mut draw = DrawHorizontal::new(Dimensions {
            width: 50,
            height: 10,
        });
        draw.draw(&C0, DrawMode::Normal).unwrap();
        draw.draw(&C1, DrawMode::Normal).unwrap();
        let output = draw.finish().normalize_spans();
        assert_eq!(
            Lines(vec![
                Line::sanitized("quickbrown "),
                Line::sanitized("fox  jumped"),
                Line::sanitized("over       "),
            ]),
            output
        );
    }
}

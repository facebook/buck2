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
use crate::State;

/// Draw components vertically, one after the other.
pub struct DrawVertical {
    /// Original dimensions.
    dim: Dimensions,
    /// What we have drawn so far.
    lines: Lines,
}

impl DrawVertical {
    /// Construct rendered with given dimensions.
    pub fn new(dimensions: Dimensions) -> DrawVertical {
        DrawVertical {
            dim: dimensions,
            lines: Lines::new(),
        }
    }

    /// Add another component.
    /// New component `draw` is called with remaining dimensions.
    pub fn draw(
        &mut self,
        component: &dyn Component,
        state: &State,
        mode: DrawMode,
    ) -> anyhow::Result<()> {
        // We call `draw` even if no space is left, but maybe we should not.
        let mut output = component.draw(
            state,
            Dimensions {
                width: self.dim.width,
                height: self.dim.height.saturating_sub(self.lines.0.len()),
            },
            mode,
        )?;
        self.lines.0.append(&mut output.0);
        Ok(())
    }

    /// Finish rendering, return the result.
    pub fn finish(mut self) -> Lines {
        // This should be no-op, but just in case.
        self.lines.shrink_lines_to_dimensions(self.dim);
        self.lines
    }
}

#[cfg(test)]
mod tests {
    use crate::components::DrawVertical;
    use crate::Component;
    use crate::Dimensions;
    use crate::DrawMode;
    use crate::Line;
    use crate::Lines;
    use crate::State;

    #[test]
    fn test_draw_vertical() {
        #[derive(Debug)]
        struct C0;
        #[derive(Debug)]
        struct C1;

        impl Component for C0 {
            fn draw_unchecked(
                &self,
                _state: &State,
                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> anyhow::Result<Lines> {
                assert_eq!(
                    Dimensions {
                        width: 10,
                        height: 20
                    },
                    dimensions
                );
                Ok(Lines(vec![Line::sanitized("foo"), Line::sanitized("bar")]))
            }
        }

        impl Component for C1 {
            fn draw_unchecked(
                &self,
                _state: &State,
                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> anyhow::Result<Lines> {
                assert_eq!(
                    Dimensions {
                        width: 10,
                        height: 18
                    },
                    dimensions
                );
                Ok(Lines(vec![
                    Line::sanitized("baz"),
                    Line::sanitized("qux"),
                    Line::sanitized("quux"),
                ]))
            }
        }

        let mut draw = DrawVertical::new(Dimensions {
            width: 10,
            height: 20,
        });
        draw.draw(&C0, &State::new(), DrawMode::Normal).unwrap();
        draw.draw(&C1, &State::new(), DrawMode::Normal).unwrap();
        let output = draw.finish();
        assert_eq!(
            Lines(vec![
                Line::sanitized("foo"),
                Line::sanitized("bar"),
                Line::sanitized("baz"),
                Line::sanitized("qux"),
                Line::sanitized("quux"),
            ]),
            output
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::Component;
use crate::Dimensions;
use crate::DrawMode;
use crate::Lines;

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
    pub fn draw<C: Component>(&mut self, component: &C, mode: DrawMode) -> Result<(), C::Error> {
        // We call `draw` even if no space is left, but maybe we should not.
        let output = component.draw(
            Dimensions {
                width: self.dim.width,
                height: self.dim.height.saturating_sub(self.lines.0.len()),
            },
            mode,
        )?;
        self.lines.extend(output);
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
    use std::convert::Infallible;

    use crate::Component;
    use crate::Dimensions;
    use crate::DrawMode;
    use crate::Line;
    use crate::Lines;
    use crate::components::DrawVertical;

    #[test]
    fn test_draw_vertical() {
        #[derive(Debug)]
        struct C0;
        #[derive(Debug)]
        struct C1;

        impl Component for C0 {
            type Error = Infallible;

            fn draw_unchecked(
                &self,
                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> Result<Lines, Infallible> {
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
            type Error = Infallible;

            fn draw_unchecked(
                &self,

                dimensions: Dimensions,
                _mode: DrawMode,
            ) -> Result<Lines, Infallible> {
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
        draw.draw(&C0, DrawMode::Normal).unwrap();
        draw.draw(&C1, DrawMode::Normal).unwrap();
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

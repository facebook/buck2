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

/// Component that ensures its child component has at most `max_size` render space.
#[derive(Debug)]
pub struct Bounded<C: Component = Box<dyn Component>> {
    child: C,
    max_size: Dimensions,
}

impl<C: Component> Bounded<C> {
    pub fn new(child: C, max_x: Option<usize>, max_y: Option<usize>) -> Self {
        Self {
            child,
            max_size: Dimensions {
                width: max_x.unwrap_or(usize::MAX),
                height: max_y.unwrap_or(usize::MAX),
            },
        }
    }
}

impl<C: Component> Component for Bounded<C> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let output = self.child.draw(dimensions.intersect(self.max_size), mode)?;
        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use derive_more::AsRef;

    use super::*;
    use crate::components::echo::Echo;
    use crate::Line;
    use crate::Span;

    #[derive(AsRef, Debug)]
    struct Msg(Lines);

    #[test]
    fn test_no_bounding() -> anyhow::Result<()> {
        let msg = Lines(vec![Line::from_iter([Span::new_unstyled("hello world")?])]);
        let test = Bounded::new(Echo(msg.clone()), Some(40), Some(40));
        let output = test.draw(
            Dimensions {
                width: 50,
                height: 50,
            },
            DrawMode::Normal,
        )?;

        assert_eq!(output, msg);

        Ok(())
    }

    #[test]
    fn test_bounding() -> anyhow::Result<()> {
        let msg = Lines(vec![
            Line::from_iter([Span::new_unstyled("hello world")?]),
            Line::from_iter([Span::new_unstyled("hello world")?]),
        ]);
        let test = Bounded::new(Echo(msg), Some(2), Some(1));
        let output = test.draw(
            Dimensions {
                width: 50,
                height: 50,
            },
            DrawMode::Normal,
        )?;
        let expected = Lines(vec![Line::from_iter([Span::new_unstyled("he")?])]);

        assert_eq!(output, expected);

        Ok(())
    }
}

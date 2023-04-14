/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::components::Blank;
use crate::components::Dimensions;
use crate::components::DrawMode;
use crate::Component;
use crate::Lines;

/// The `Padded` [`Component`](Component) wraps its child by padding left, right, above, and below its content.
/// This can be used to shift the content to a different location and ensure that following content comes after a certain distance.
/// It is worth noting that this component will also *truncate* any content that is too long to fit in the given window at draw time.
/// However, components are expected to constrain themselves to the given window, anyway.
///
/// Content is truncated preferentially over padding.
#[derive(Debug)]
pub struct Padded<C: Component = Box<dyn Component>> {
    pub child: C,
    pub left: usize,
    pub right: usize,
    pub top: usize,
    pub bottom: usize,
}

impl Default for Padded {
    fn default() -> Self {
        Self {
            child: Box::new(Blank),
            left: 0,
            right: 0,
            top: 0,
            bottom: 0,
        }
    }
}

impl<C: Component> Padded<C> {
    pub fn new(child: C, left: usize, right: usize, top: usize, bottom: usize) -> Self {
        Self {
            child,
            left,
            right,
            top,
            bottom,
        }
    }
}

impl<C: Component> Component for Padded<C> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        let mut output = self.child.draw(dimensions, mode)?;

        // ordering is important:
        // the top and bottom lines need to be padded horizontally too.
        output.pad_lines_top(self.top);
        // cut off enough space at the bottom for the bottom padding
        output.truncate_lines_bottom(dimensions.height.saturating_sub(self.bottom));
        output.pad_lines_bottom(self.bottom);

        output.pad_lines_left(self.left);
        // cut off enough space on the right for the right padding
        output.truncate_lines(dimensions.width.saturating_sub(self.right));
        output.pad_lines_right(self.right);

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use derive_more::AsRef;

    use crate::components::echo::Echo;
    use crate::components::Padded;
    use crate::Component;
    use crate::Dimensions;
    use crate::DrawMode;
    use crate::Line;
    use crate::Lines;

    #[derive(Debug, AsRef)]
    struct Msg(Lines);

    #[test]
    fn test_pad_left() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            left: 5,
            right: 0,
            top: 0,
            bottom: 0,
        };

        let drawing = padder
            .draw(Dimensions::new(20, 20), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec![" ".repeat(5).as_ref(), "hello world"]
                .try_into()
                .unwrap(),
            vec![" ".repeat(5).as_ref(), "ok"].try_into().unwrap(),
            vec![" ".repeat(5)].try_into().unwrap(),
        ]);
        assert_eq!(drawing, expected);
    }

    #[test]
    fn test_pad_right() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            right: 4,
            left: 0,
            top: 0,
            bottom: 0,
        };

        let drawing = padder
            .draw(Dimensions::new(20, 20), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec!["hello world", &" ".repeat(4)].try_into().unwrap(),
            vec!["ok", &" ".repeat(4 + 9)].try_into().unwrap(),
            vec![" ".repeat(4 + 11)].try_into().unwrap(),
        ]);
        assert_eq!(drawing, expected);
    }

    #[test]
    fn test_pad_top() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            top: 5,
            bottom: 0,
            left: 0,
            right: 0,
        };

        let drawing = padder
            .draw(Dimensions::new(15, 15), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);

        assert_eq!(drawing, expected);
    }

    #[test]
    fn test_pad_bottom() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            bottom: 5,
            top: 0,
            left: 0,
            right: 0,
        };

        let drawing = padder
            .draw(Dimensions::new(15, 15), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
            Line::default(),
        ]);

        assert_eq!(drawing, expected);
    }

    #[test]
    fn test_no_pad() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            top: 0,
            bottom: 0,
            left: 0,
            right: 0,
        };

        let drawing = padder
            .draw(Dimensions::new(15, 15), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);

        assert_eq!(drawing, expected);
    }

    #[test]
    fn test_truncated() {
        let msg = Lines(vec![
            vec!["hello world"].try_into().unwrap(),
            vec!["ok"].try_into().unwrap(),
            Line::default(),
        ]);
        let padder = Padded {
            child: Echo(msg),
            left: 5,
            right: 3,
            top: 3,
            bottom: 3,
        };
        let drawing = padder
            .draw(Dimensions::new(10, 8), DrawMode::Normal)
            .unwrap();
        let expected = Lines(vec![
            // 5 rows of padding at the top
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
            // 2 rows of content, padded on left and right
            vec![" ".repeat(5).as_ref(), "he", &" ".repeat(3)]
                .try_into()
                .unwrap(),
            vec![" ".repeat(5).as_ref(), "ok", &" ".repeat(3)]
                .try_into()
                .unwrap(),
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
            vec![" ".repeat(5), " ".repeat(5)].try_into().unwrap(),
        ]);

        assert_eq!(drawing, expected);
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::Component;
use crate::Lines;
use crate::components::Dimensions;
use crate::components::DrawMode;

/// The `Blank` component is a dead-end component that emits nothing.
/// It can be used for testing purposes or to make a portion of a dividing component empty.
/// By default, the [`SuperConsole`](crate::SuperConsole) is created with a [`Blank`](Blank) component.
#[derive(Debug)]
pub struct Blank;

impl Component for Blank {
    /// Returns the empty vector
    fn draw_unchecked(&self, _dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
        Ok(Lines::new())
    }
}

#[cfg(test)]
mod tests {
    use derive_more::AsRef;

    use crate::Component;
    use crate::Dimensions;
    use crate::Lines;
    use crate::components::DrawMode;
    use crate::components::echo::Echo;

    #[derive(AsRef, Debug)]
    #[allow(dead_code)]
    struct EchoMsg(Lines);

    #[test]
    fn test_echo_empty() {
        let output = Echo(Lines::new())
            .draw(Dimensions::new(10, 10), DrawMode::Normal)
            .unwrap();
        assert_eq!(output, Lines::new());
    }

    #[test]
    fn test_echo() {
        let output = Lines(vec![
            vec!["Line 1"].try_into().unwrap(),
            vec!["Line 2"].try_into().unwrap(),
        ]);

        let test_output = Echo(output.clone())
            .draw(Dimensions::new(10, 10), DrawMode::Final)
            .unwrap();
        assert_eq!(output, test_output);
    }
}

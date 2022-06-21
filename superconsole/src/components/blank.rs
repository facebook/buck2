/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::components::Dimensions;
use crate::components::DrawMode;
use crate::Component;
use crate::Line;
use crate::State;

/// The `Blank` component is a dead-end component that emits nothing.
/// It can be used for testing purposes or to make a portion of a dividing component empty.
/// By default, the [`SuperConsole`](crate::SuperConsole) is created with a [`Blank`](Blank) component.
#[derive(Debug)]
pub struct Blank;

impl Component for Blank {
    /// Returns the empty vector
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use derive_more::AsRef;

    use crate::components::DrawMode;
    use crate::components::Echo;
    use crate::Component;
    use crate::Dimensions;
    use crate::Line;

    #[derive(AsRef, Debug)]
    struct EchoMsg(Vec<Line>);

    #[test]
    fn test_echo_empty() {
        let echo: Echo<EchoMsg> = Echo::new(false);

        let test = EchoMsg(vec![]);

        let output = echo
            .draw(
                &crate::state!(&test),
                Dimensions::new(10, 10),
                DrawMode::Normal,
            )
            .unwrap();
        assert_eq!(output, []);
    }

    #[test]
    fn test_echo() {
        let echo: Echo<EchoMsg> = Echo::new(false);
        let output = vec![
            vec!["Line 1"].try_into().unwrap(),
            vec!["Line 2"].try_into().unwrap(),
        ];
        let state = EchoMsg(output.clone());

        let test_output = echo
            .draw(
                &crate::state!(&state),
                Dimensions::new(10, 10),
                DrawMode::Final,
            )
            .unwrap();
        assert_eq!(output, test_output);
    }
}

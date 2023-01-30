/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::io_state::IoState;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Lines;
use superconsole::State;

use crate::subscribers::superconsole::SuperConsoleConfig;

#[derive(Debug)]
pub(crate) struct IoHeader;

impl Component for IoHeader {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let config = state.get::<SuperConsoleConfig>()?;
        let io = state.get::<IoState>()?;
        io.render(mode, dimensions.width, config.enable_io)
    }
}

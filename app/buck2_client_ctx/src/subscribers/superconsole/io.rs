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

use crate::subscribers::superconsole::SuperConsoleConfig;

pub(crate) struct IoHeader<'s> {
    pub(crate) super_console_config: &'s SuperConsoleConfig,
    pub(crate) io_state: &'s IoState,
}

impl<'s> Component for IoHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        self.io_state
            .render(mode, dimensions.width, self.super_console_config.enable_io)
    }
}

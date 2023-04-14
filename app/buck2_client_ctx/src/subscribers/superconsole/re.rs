/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::re_state::ReState;
use superconsole::Component;

use crate::subscribers::superconsole::SuperConsoleConfig;

/// Draw the test summary line above the `timed_list`
pub(crate) struct ReHeader<'a> {
    pub(crate) super_console_config: &'a SuperConsoleConfig,
    pub(crate) re_state: &'a ReState,
}

impl<'a> Component for ReHeader<'a> {
    fn draw_unchecked(
        &self,

        _dimensions: superconsole::Dimensions,
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        self.re_state
            .render(self.super_console_config.enable_detailed_re, mode)
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::action_stats::ActionStats;
use superconsole::Component;
use superconsole::Line;
use superconsole::Lines;

use crate::subscribers::superconsole::SuperConsoleConfig;

pub(crate) struct CommandsComponent<'a> {
    pub(crate) super_console_config: &'a SuperConsoleConfig,
    pub(crate) action_stats: &'a ActionStats,
}

impl<'a> Component for CommandsComponent<'a> {
    fn draw_unchecked(
        &self,

        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        if !self.super_console_config.enable_commands {
            return Ok(Lines::new());
        }

        Ok(Lines(vec![Line::unstyled(&self.action_stats.to_string())?]))
    }
}

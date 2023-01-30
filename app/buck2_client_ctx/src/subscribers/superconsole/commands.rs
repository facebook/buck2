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

use crate::subscribers::superconsole::SuperConsoleConfig;

#[derive(Debug)]
pub(crate) struct CommandsComponent;

impl Component for CommandsComponent {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let config = state.get::<SuperConsoleConfig>()?;
        if !config.enable_commands {
            return Ok(vec![]);
        }

        let action_stats = state.get::<ActionStats>()?;
        Ok(vec![Line::unstyled(&action_stats.to_string())?])
    }
}

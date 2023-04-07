/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_event_observer::starlark_debug::StarlarkDebuggerState;
use gazebo::prelude::*;
use superconsole::Component;

#[derive(Debug)]
pub(crate) struct StarlarkDebuggerComponent;

impl Component for StarlarkDebuggerComponent {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let state = state.get::<StarlarkDebuggerState>()?;

        if !state.debugger_attached {
            return Ok(vec![]);
        }

        let mut lines: Vec<String> = Vec::new();
        lines.push("     ******** Buck2 Starlark Debugger is attached **********".to_owned());

        let total = state.this_stopped_evals.len() + state.other_stopped_evals.len();
        for stopped in &state.this_stopped_evals {
            if lines.len() < 7 {
                lines.push(format!(
                    "( stopped)   {:<60} @ {}",
                    &stopped.description, &stopped.stopped_at
                ));
            }
        }
        for stopped in &state.other_stopped_evals {
            if lines.len() < 7 {
                lines.push(format!(
                    "(*stopped) {:<60} @ {}",
                    &stopped.description, &stopped.stopped_at
                ));
            }
        }

        let shown = lines.len() - 1;
        if total > shown {
            lines.push(format!("{} more paused evaluations", total - shown));
        }

        lines.push("      *******************************************************".to_owned());

        lines.into_try_map(|v| vec![v].try_into())
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::time::SystemTime;

use buck2_data::DebugAdapterSnapshot;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct StoppedEval {
    pub description: String,
    pub stopped_at: String,
}

#[derive(Debug)]
pub struct StarlarkDebuggerState {
    pub debugger_attached: bool,
    pub this_stopped_evals: BTreeSet<StoppedEval>,
    pub other_stopped_evals: BTreeSet<StoppedEval>,
}

impl StarlarkDebuggerState {
    pub fn new() -> Self {
        Self {
            debugger_attached: false,
            this_stopped_evals: BTreeSet::new(),
            other_stopped_evals: BTreeSet::new(),
        }
    }

    pub fn update(
        &mut self,
        _event_time: SystemTime,
        event: &DebugAdapterSnapshot,
    ) -> anyhow::Result<()> {
        self.debugger_attached = true;
        self.this_stopped_evals.clear();
        self.other_stopped_evals.clear();

        for (handle, evals) in &event.current_handles {
            for stopped in &evals.stopped_evals {
                let v = StoppedEval {
                    description: stopped.description.clone(),
                    stopped_at: stopped.stopped_at.clone(),
                };
                if *handle == event.this_handle {
                    self.this_stopped_evals.insert(v);
                } else {
                    self.other_stopped_evals.insert(v);
                }
            }
        }
        Ok(())
    }
}

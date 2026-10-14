/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The selected command's action window: the clock bounds of its actions, or
//! none for a capture that recorded only the command.

use crate::types::F_NOT_RECIPE;
use crate::types::RunInfo;
use crate::types::Trace;

#[derive(Default)]
pub struct State {
    /// Absolute clock bounds, computed once rather than scanning millions of
    /// actions on every UI frame. Includes only the explicitly selected run.
    pub actions: Option<(i64, i64)>,
    pub unattributed_actions: usize,
}
impl State {
    pub fn new(tr: &Trace, run: Option<&str>) -> Self {
        if run.is_none() && (tr.runs.len() > 1 || tr.run_ids.len() > 1) {
            return Self::default();
        }
        let run = run.or_else(|| profile_run(&tr.runs, None).map(|r| r.run.as_str()));
        let mut actions: Option<(i64, i64)> = None;
        let mut unattributed_actions = 0;
        for a in &tr.acts {
            if a.flags & F_NOT_RECIPE == 0
                && a.run == crate::types::NO_STR
                && !tr.action_in_run(a, run)
            {
                unattributed_actions += 1;
            }
            if a.flags & F_NOT_RECIPE != 0 || !tr.action_in_run(a, run) {
                continue;
            }
            let start = tr.t0_abs.saturating_add(a.ts);
            let end = start.saturating_add(a.dur);
            actions = Some(match actions {
                Some((lo, hi)) => (lo.min(start), hi.max(end)),
                None => (start, end),
            });
        }
        Self {
            actions,
            unattributed_actions,
        }
    }
}

pub(super) fn profile_run<'a>(runs: &'a [RunInfo], run: Option<&str>) -> Option<&'a RunInfo> {
    match run {
        Some(id) => runs.iter().find(|r| r.run == id),
        None if runs.len() == 1 => runs.first(),
        None => None,
    }
}

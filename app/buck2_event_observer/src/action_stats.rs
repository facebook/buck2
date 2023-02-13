/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::last_command_execution_kind::get_last_command_execution_kind;
use crate::last_command_execution_kind::LastCommandExecutionKind;

/// Records the number of actions depending on how they executed.
/// There's no overlap between the actions - summing them all up
/// gives the total number of actions. `local_actions` + `remote_actions`
/// provides the total number of actually executed actions while
/// `cached_actions` provides number of actions which we found
/// in the action cache.  `fallback_actions` provides the number of actions
/// that had its command run more than once (hence, using fallback to run).
///
/// These stats only track executions/commands.
#[derive(Default)]
pub struct ActionStats {
    pub local_actions: u64,
    pub remote_actions: u64,
    pub cached_actions: u64,
    pub fallback_actions: u64,
}

impl ActionStats {
    pub fn action_cache_hit_percentage(&self) -> u8 {
        // We want special semantics for the return value: the terminal values (0% and 100%)
        // should _only_ be used when there are exactly no cache hits and full cache hits.
        // So, even if we have 99.6% cache hits, we want to display 99% and conversely,
        // if the value is 0.1% cache hits, we want to display 1%.
        //
        // This allows us to have special meaning for 0% and 100%: complete cache-divergence
        // and fully cacheable builds.
        let total_actions = self.total_executed_and_cached_actions();
        if total_actions == 0 || self.cached_actions == total_actions {
            100
        } else if self.cached_actions == 0 {
            0
        } else {
            let hit_percent = ((self.cached_actions as f64) / (total_actions as f64)) * 100f64;
            let integral_percent = (hit_percent.round()) as u8;
            integral_percent.clamp(1, 99)
        }
    }

    pub fn total_executed_actions(&self) -> u64 {
        self.local_actions + self.remote_actions
    }

    pub fn total_executed_and_cached_actions(&self) -> u64 {
        self.local_actions + self.remote_actions + self.cached_actions
    }

    pub fn update(&mut self, action: &buck2_data::ActionExecutionEnd) {
        if was_fallback_action(action) {
            self.fallback_actions += 1;
        }
        match get_last_command_execution_kind(action) {
            LastCommandExecutionKind::Local => {
                self.local_actions += 1;
            }
            LastCommandExecutionKind::Cached => {
                self.cached_actions += 1;
            }
            LastCommandExecutionKind::Remote => {
                self.remote_actions += 1;
            }
            LastCommandExecutionKind::NoCommand => {}
        }
    }

    pub fn log_stats(&self) -> bool {
        self.total_executed_and_cached_actions() > 0
    }
}

impl fmt::Display for ActionStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut action_stats_message = format!(
            "Cache hits: {}%. Commands: {} (cached: {}, remote: {}, local: {})",
            self.action_cache_hit_percentage(),
            self.total_executed_and_cached_actions(),
            self.cached_actions,
            self.remote_actions,
            self.local_actions
        );
        if self.fallback_actions > 0 {
            action_stats_message += format!(
                ". Fallback: {}/{}",
                self.fallback_actions,
                self.total_executed_actions()
            )
            .as_str();
        }
        write!(f, "{}", action_stats_message)
    }
}

pub fn was_fallback_action(action: &buck2_data::ActionExecutionEnd) -> bool {
    action.commands.len() > 1
}

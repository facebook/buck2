/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use dupe::Dupe;

use crate::cache_hit_rate::total_cache_hit_rate;
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
#[derive(Default, Clone, Dupe)]
pub struct ActionStats {
    pub local_actions: u64,
    pub remote_actions: u64,
    pub cached_actions: u64,
    pub fallback_actions: u64,
    pub remote_dep_file_cached_actions: u64,
    pub excess_cache_misses: u64,
}

impl ActionStats {
    pub fn total_cache_hit_percentage(&self) -> u8 {
        // We want special semantics for the return value: the terminal values (0% and 100%)
        // should _only_ be used when there are exactly no cache hits and full cache hits.
        // So, even if we have 99.6% cache hits, we want to display 99% and conversely,
        // if the value is 0.1% cache hits, we want to display 1%.
        //
        // This allows us to have special meaning for 0% and 100%: complete cache-divergence
        // and fully cacheable builds.
        let rate = total_cache_hit_rate(
            self.local_actions,
            self.remote_actions,
            self.cached_actions,
            self.remote_dep_file_cached_actions,
        ) * 100f64;
        let rate = if rate == 100.0 || rate == 0.0 {
            rate
        } else {
            let integral_percent = rate.round();
            integral_percent.clamp(1.0, 99.0)
        };
        rate as u8
    }

    pub fn total_cached_actions(&self) -> u64 {
        self.cached_actions + self.remote_dep_file_cached_actions
    }

    pub fn total_executed_actions(&self) -> u64 {
        self.local_actions + self.remote_actions
    }

    pub fn total_executed_and_cached_actions(&self) -> u64 {
        self.local_actions
            + self.remote_actions
            + self.cached_actions
            + self.remote_dep_file_cached_actions
    }

    pub fn update(&mut self, action: &buck2_data::ActionExecutionEnd) {
        // TODO(ezgi): consolidate with InvocationRecord creation at https://fburl.com/code/c8iitvvy
        if action.kind != buck2_data::ActionKind::Run as i32 {
            return;
        }
        if was_fallback_action(action) {
            self.fallback_actions += 1;
        }
        match get_last_command_execution_kind(action) {
            LastCommandExecutionKind::Local | LastCommandExecutionKind::LocalWorker => {
                self.local_actions += 1;
            }
            LastCommandExecutionKind::Cached => {
                self.cached_actions += 1;
            }
            LastCommandExecutionKind::Remote => {
                self.remote_actions += 1;
            }
            LastCommandExecutionKind::RemoteDepFileCached => {
                self.remote_dep_file_cached_actions += 1;
            }
            LastCommandExecutionKind::NoCommand => {}
        }
        if let Some(v) = &action.invalidation_info {
            if v.changed_file.is_none() {
                self.excess_cache_misses += 1;
            }
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
            self.total_cache_hit_percentage(),
            self.total_executed_and_cached_actions(),
            self.total_cached_actions(),
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

/// Identify whether an action was a fallback action. A fallback action is an action that executed
/// two commands, unless one of those was a Cancelled, which just means hybrid execution
/// cancelled the local run (and which is not a fallback).
pub fn was_fallback_action(action: &buck2_data::ActionExecutionEnd) -> bool {
    use buck2_data::command_execution::Status;

    action
        .commands
        .iter()
        .filter(|c| !matches!(c.status, Some(Status::Cancelled(..))))
        .count()
        > 1
}

#[cfg(test)]
mod tests {
    use buck2_data::ActionExecutionEnd;
    use buck2_data::ActionKind;

    use super::*;

    #[test]
    fn test_file_change_invalidation_source() {
        let mut action_stats = ActionStats::default();

        let action_execution_end = ActionExecutionEnd {
            kind: ActionKind::Run as i32,
            invalidation_info: Some(buck2_data::CommandInvalidationInfo {
                changed_file: Some(buck2_data::command_invalidation_info::InvalidationSource {}),
                ..Default::default()
            }),
            ..Default::default()
        };

        action_stats.update(&action_execution_end);

        assert_eq!(action_stats.excess_cache_misses, 0);
    }

    #[test]
    fn test_excess_cache_miss_with_no_invalidation_source() {
        let mut action_stats = ActionStats::default();

        let action_execution_end = ActionExecutionEnd {
            kind: ActionKind::Run as i32,
            invalidation_info: Some(buck2_data::CommandInvalidationInfo {
                changed_file: None,
                changed_any: None,
            }),
            ..Default::default()
        };

        action_stats.update(&action_execution_end);
        assert_eq!(action_stats.excess_cache_misses, 1);

        action_stats.update(&action_execution_end);
        assert_eq!(action_stats.excess_cache_misses, 2);
    }
}

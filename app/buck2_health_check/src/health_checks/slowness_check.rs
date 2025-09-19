/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)] // The code here will be used in future diffs.

use std::time::Duration;
use std::time::SystemTime;

use crate::health_checks::slowness::buildmate_slowness_detection_fetcher::BuildmateSlownessDetectionFetcher;
use crate::health_checks::slowness::buildmate_slowness_detection_fetcher::SlownessConfigurationFetcher;
use crate::interface::HealthCheck;
use crate::interface::HealthCheckContext;
use crate::interface::HealthCheckSnapshotData;
use crate::report::DisplayReport;
use crate::report::HealthIssue;
use crate::report::Message;
use crate::report::Report;
use crate::report::Severity;

pub const TAG: &str = "buildmate_slowness_diagnosis";

pub(crate) struct SlownessCheck {
    state: SlownessCheckState,
}

enum SlownessCheckState {
    Uninitialized,
    Initializing {
        command_start_timestamp: SystemTime,
    },
    Enabled {
        slowness_threshold: Duration,
        command_start_timestamp: SystemTime,
        buildmate_url: String,
    },
    Disabled,
}

impl SlownessCheck {
    pub(crate) fn new() -> Self {
        Self {
            state: SlownessCheckState::Uninitialized,
        }
    }

    pub(crate) fn run(&mut self, snapshot: HealthCheckSnapshotData) -> Option<Report> {
        if !matches!(self.state, SlownessCheckState::Enabled { .. }) {
            return None;
        }

        let is_slow_build = self.is_build_slow(snapshot);
        if !is_slow_build {
            return None;
        }
        Some(Report {
            display_report: self.generate_display_report(&(self.state)),
            tag: is_slow_build.then(|| TAG.to_owned()),
        })
    }

    pub(crate) async fn update_state(&mut self, context: &HealthCheckContext) {
        if !matches!(self.state, SlownessCheckState::Uninitialized) {
            return;
        }

        // Check if context.command_data is None, if so return early
        let (Some(command_data), Some(command_start_timestamp)) =
            (context.command_data.as_ref(), context.command_start_time)
        else {
            return;
        };

        if !matches!(
            command_data,
            buck2_data::command_start::Data::Build(..)
                | buck2_data::command_start::Data::Install(..)
        ) {
            self.state = SlownessCheckState::Disabled;
            return;
        }

        let build_uuid = self.extract_build_uuid(context);
        let target_patterns = self.extract_target_patterns(context);
        let is_incremental_build = self.determine_if_incremental_build(context);

        let (Some(uuid), Some(patterns)) = (build_uuid, target_patterns) else {
            return; // Data not received yet, will retry on next context update
        };
        self.state = SlownessCheckState::Initializing {
            command_start_timestamp,
        };

        let fetcher = BuildmateSlownessDetectionFetcher::new();
        match fetcher
            .get_slowness_configuration(uuid, patterns, is_incremental_build)
            .await
        {
            Ok(Some(config)) if config.enable_slowness_detection => {
                self.state = SlownessCheckState::Enabled {
                    slowness_threshold: Duration::from_secs(
                        config.slowness_threshold_minutes as u64 * 60,
                    ),
                    command_start_timestamp,
                    buildmate_url: config.buildmate_link,
                };
            }
            _ => {
                self.state = SlownessCheckState::Disabled;
            }
        }
    }

    fn extract_build_uuid(&self, context: &HealthCheckContext) -> Option<String> {
        context.trace_id.as_ref().map(|id| id.to_owned())
    }

    fn extract_target_patterns(&self, context: &HealthCheckContext) -> Option<Vec<String>> {
        context.parsed_target_patterns.as_ref().map(|patterns| {
            patterns
                .target_patterns
                .iter()
                .map(|p| p.value.clone())
                .collect()
        })
    }

    fn determine_if_incremental_build(&self, _context: &HealthCheckContext) -> bool {
        // TODO(junliqin) Determine if this is an incremental build based on context. Currently hardcoded to false. Will update in future diffs.
        false
    }

    fn is_build_slow(&mut self, snapshot: HealthCheckSnapshotData) -> bool {
        match &self.state {
            SlownessCheckState::Enabled {
                slowness_threshold,
                command_start_timestamp,
                ..
            } => {
                let current_timestamp = snapshot.timestamp;
                if let Ok(time_spent) = current_timestamp.duration_since(*command_start_timestamp) {
                    time_spent > *slowness_threshold
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn generate_display_report(&self, state: &SlownessCheckState) -> Option<DisplayReport> {
        if let SlownessCheckState::Enabled { buildmate_url, .. } = state {
            let health_issue = HealthIssue {
                severity: Severity::Warning,
                message: Message::Simple(format!(
                    "The build is detected to be a slow build. Consider clicking this link {} to diagnose with buildmate after it finishes.",
                    buildmate_url
                )),
                remediation: None,
            };
            Some(DisplayReport {
                health_check_type: crate::interface::HealthCheckType::SlowBuild,
                health_issue: Some(health_issue),
            })
        } else {
            None
        }
    }
}

#[async_trait::async_trait]
impl HealthCheck for SlownessCheck {
    fn run_check(
        &mut self,
        snapshot: HealthCheckSnapshotData,
    ) -> buck2_error::Result<Option<Report>> {
        Ok(self.run(snapshot))
    }

    async fn handle_context_update(&mut self, context: &HealthCheckContext) {
        self.update_state(context).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slow_build_detection_disabled() {
        let mut check = SlownessCheck::new();
        // New check should be uninitialized and not slow
        let snapshot = HealthCheckSnapshotData {
            timestamp: SystemTime::now(),
        };

        assert!(!check.is_build_slow(snapshot));
    }

    #[test]
    fn test_slow_build_detection_enabled() {
        let mut check = SlownessCheck::new();
        // Set up an enabled state with a short threshold
        let command_start = SystemTime::now() - Duration::from_secs(120); // 2 minutes ago
        check.state = SlownessCheckState::Enabled {
            slowness_threshold: Duration::from_secs(60), // 1 minute threshold
            command_start_timestamp: command_start,
            buildmate_url: "https://example.com".to_owned(),
        };

        let snapshot = HealthCheckSnapshotData {
            timestamp: SystemTime::now(),
        };

        // Should detect as slow since 2 minutes > 1 minute threshold
        assert!(check.is_build_slow(snapshot));
    }

    #[test]
    fn test_no_report_when_disabled() {
        let mut check = SlownessCheck::new();
        check.state = SlownessCheckState::Disabled;

        let snapshot = HealthCheckSnapshotData {
            timestamp: SystemTime::now(),
        };
        let report = check.run(snapshot);
        // Should return None since check is disabled
        assert!(report.is_none());
    }

    #[test]
    fn test_can_run_states() {
        let check = SlownessCheck::new();
        assert!(!matches!(check.state, SlownessCheckState::Enabled { .. })); // Uninitialized

        let mut check = SlownessCheck::new();
        check.state = SlownessCheckState::Disabled;
        assert!(!matches!(check.state, SlownessCheckState::Enabled { .. })); // Disabled

        let mut check = SlownessCheck::new();
        check.state = SlownessCheckState::Initializing {
            command_start_timestamp: SystemTime::now(),
        };
        assert!(!matches!(check.state, SlownessCheckState::Enabled { .. })); // Initializing

        let mut check = SlownessCheck::new();
        check.state = SlownessCheckState::Enabled {
            slowness_threshold: Duration::from_secs(60),
            command_start_timestamp: SystemTime::now(),
            buildmate_url: "https://example.com".to_owned(),
        };
        assert!(matches!(check.state, SlownessCheckState::Enabled { .. })); // Enabled
    }
}

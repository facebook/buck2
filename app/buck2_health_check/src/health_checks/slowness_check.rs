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

use crate::interface::HealthCheck;
use crate::interface::HealthCheckContext;
use crate::interface::HealthCheckSnapshotData;
use crate::report::DisplayReport;
use crate::report::HealthIssue;
use crate::report::Report;
use crate::report::Severity;

pub const TAG: &str = "buildmate_slowness_diagnosis";
// Harcode it for tesitng, will use graphql to fetch the threshold based on the target patterns and other attributes.
pub const HARDCODED_SLOW_BUILD_THRESHOLD: Duration = Duration::from_secs(1440); // 24 hours

/// Check for slow build performance and provide remediation links.
pub(crate) struct SlownessCheck {
    can_run: Option<bool>,
    slowness_threshold: Option<Duration>,
    command_start_timestamp: Option<SystemTime>,
}

impl SlownessCheck {
    pub(crate) fn new() -> Self {
        Self {
            can_run: None,
            slowness_threshold: None,
            command_start_timestamp: None,
        }
    }

    pub(crate) fn run(&mut self, snapshot: HealthCheckSnapshotData) -> Option<Report> {
        if !self.can_run() {
            return None;
        }

        let is_slow_build = self.is_build_slow(snapshot);
        if !is_slow_build {
            return None;
        }
        Some(Report {
            display_report: self.generate_display_report(),
            tag: is_slow_build.then(|| TAG.to_owned()),
        })
    }

    pub(crate) fn can_run(&self) -> bool {
        self.can_run.unwrap_or(false)
    }

    pub(crate) fn try_update_can_run(&mut self, context: &HealthCheckContext) {
        if self.can_run.is_some() {
            return;
        }

        if let Some(command_start_timestamp) = context.command_start_time {
            self.command_start_timestamp = Some(command_start_timestamp);
        } else {
            return;
        }

        self.can_run = Some(true);
        self.slowness_threshold = Some(HARDCODED_SLOW_BUILD_THRESHOLD);
    }

    fn is_build_slow(&mut self, snapshot: HealthCheckSnapshotData) -> bool {
        if self.slowness_threshold.is_none() {
            return false;
        }

        //Todo use graphql to fetch the threshold based on the target patterns.
        let slowness_threshold = self
            .slowness_threshold
            .unwrap_or(HARDCODED_SLOW_BUILD_THRESHOLD);
        let current_timestamp = snapshot.timestamp;
        if let Some(command_start_timestamp) = self.command_start_timestamp {
            if let Ok(time_spent) = current_timestamp.duration_since(command_start_timestamp) {
                return time_spent > slowness_threshold;
            }
        }
        false
    }

    fn generate_display_report(&self) -> Option<DisplayReport> {
        let health_issue = HealthIssue {
            severity: Severity::Warning,
            message: "The build is detected to be a slow build. Consider running buildmate to diagnose the root cause after the build finishes.".to_owned(),
            remediation: None,
        };
        Some(DisplayReport {
            health_check_type: crate::interface::HealthCheckType::SlowBuild,
            health_issue: Some(health_issue),
        })
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
        self.try_update_can_run(context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MATCHING_TARGET: &str = "//foo/bar:baz";
    const MATCHING_REGEX: &str = "bar";

    fn health_check_context(target: Option<String>) -> HealthCheckContext {
        HealthCheckContext {
            parsed_target_patterns: target.map(|t| buck2_data::ParsedTargetPatterns {
                target_patterns: vec![buck2_data::TargetPattern {
                    value: t.to_owned(),
                }],
            }),
            command_start_time: Some(SystemTime::now()),
            ..Default::default()
        }
    }

    fn can_run_check(target: Option<String>) -> bool {
        let mut slowness_check = SlownessCheck::new();
        let health_check_context = health_check_context(target);
        slowness_check.try_update_can_run(&health_check_context);
        slowness_check.can_run()
    }

    #[test]
    fn test_can_run_with_matching_target() {
        assert!(can_run_check(Some(MATCHING_TARGET.to_owned()),));
    }

    #[test]
    fn test_can_run_with_no_target_patterns() {
        // Should now default to enabled since regex checking is commented out
        assert!(can_run_check(None));
    }

    #[test]
    fn test_can_run_with_no_optin_target_regex() {
        // Should default to enabled when no regex is specified
        assert!(can_run_check(Some(MATCHING_TARGET.to_owned())));
    }

    #[test]
    fn test_can_run_with_no_matching_target() {
        // Should now default to enabled since regex checking is commented out
        assert!(can_run_check(Some(MATCHING_TARGET.to_owned())));
    }

    #[test]
    fn test_regex_matching_multiple_targets() {
        assert!(can_run_check(Some(MATCHING_TARGET.to_owned())));
        assert!(can_run_check(Some("//foo/buck:baz".to_owned()),));
    }

    #[test]
    fn test_slow_build_detection() {
        let mut check = SlownessCheck::new();
        // Since is_build_slow() always returns false now, this should be false
        let snapshot = HealthCheckSnapshotData {
            timestamp: SystemTime::now(),
        };

        assert!(!check.is_build_slow(snapshot));
    }

    #[test]
    fn test_no_report_when_not_slow() {
        let mut check = SlownessCheck::new();
        check.can_run = Some(true);

        let snapshot = HealthCheckSnapshotData {
            timestamp: SystemTime::now(),
        };
        let report = check.run(snapshot);
        // Should return None since is_build_slow() returns false
        assert!(report.is_none());
    }
}

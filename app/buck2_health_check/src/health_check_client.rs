/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // TODO(rajneeshl): Remove this when the health checks are moved to the server.

use buck2_core::soft_error;
use tokio::sync::mpsc::error::TrySendError;
use tokio::sync::mpsc::Sender;

use crate::health_check_context::HealthCheckContext;
#[cfg(fbcode_build)]
use crate::health_checks::facebook::warm_revision::warm_revision_check::WarmRevisionCheck;
use crate::health_checks::vpn_check::VpnCheck;
use crate::report::DisplayReport;

/// This client maintains the context and make requests to the health check server.
pub struct HealthCheckClient {
    health_check_context: HealthCheckContext,

    // TODO(rajneeshl): These are temporary hacks to call health checks directly from client.
    // Move these to the health check server when it is ready.
    #[cfg(fbcode_build)]
    warm_revision_check: WarmRevisionCheck,
    vpn_check: VpnCheck,
    // Writer to send tags to be logged to scuba.
    // TODO(rajneeshl): Make this required when the event_observer reference is removed.
    tags_sender: Option<Sender<Vec<String>>>,
    // Writer to send health check reports to be displayed to the user.
    display_reports_sender: Option<Sender<Vec<DisplayReport>>>,
}

impl HealthCheckClient {
    pub fn new(
        trace_id: String,
        tags_sender: Option<Sender<Vec<String>>>,
        display_reports_sender: Option<Sender<Vec<DisplayReport>>>,
    ) -> Self {
        Self {
            health_check_context: HealthCheckContext {
                trace_id,
                ..Default::default()
            },
            #[cfg(fbcode_build)]
            warm_revision_check: WarmRevisionCheck::new(),
            vpn_check: VpnCheck::new(),
            tags_sender,
            display_reports_sender,
        }
    }

    pub async fn update_command_data(
        &mut self,
        command_data: Option<buck2_data::command_start::Data>,
    ) {
        self.health_check_context.command_data = command_data;
        self.try_update_warm_revision_check().await;
    }

    pub async fn update_parsed_target_patterns(
        &mut self,
        parsed_target_patterns: &buck2_data::ParsedTargetPatterns,
    ) {
        self.health_check_context.parsed_target_patterns = Some(parsed_target_patterns.clone());
        self.vpn_check
            .try_update_can_run(&self.health_check_context);
        self.try_update_warm_revision_check().await;
    }

    pub async fn update_branched_from_revision(&mut self, branched_from_revision: &str) {
        self.health_check_context.branched_from_revision = Some(branched_from_revision.to_owned());
        self.try_update_warm_revision_check().await;
    }

    pub async fn update_excess_cache_miss(&mut self, action_end: &buck2_data::ActionExecutionEnd) {
        // Action stats are currently only used for cache misses. If we need more data, store an aggregated action stats.
        if self.health_check_context.has_excess_cache_misses {
            // If we already know that we have excess cache misses, we don't need updates again.
            return;
        }
        if let Some(v) = &action_end.invalidation_info {
            if v.changed_file.is_none() {
                self.health_check_context.has_excess_cache_misses = true;
                self.try_update_warm_revision_check().await;
            }
        }
    }

    // TODO(rajneeshl): Deprecate this behavior. Merge with `update_excess_cache_miss` instead.
    pub async fn update_excess_cache_misses(&mut self, has_excess_cache_misses: bool) {
        self.health_check_context.has_excess_cache_misses = has_excess_cache_misses;
        self.try_update_warm_revision_check().await;
    }

    pub async fn update_experiment_configurations(
        &mut self,
        experiment_configurations: &buck2_data::SystemInfo,
    ) {
        self.health_check_context.experiment_configurations =
            Some(experiment_configurations.clone());
        self.vpn_check
            .try_update_can_run(&self.health_check_context);
        self.try_update_warm_revision_check().await;
    }

    async fn try_update_warm_revision_check(&mut self) {
        #[cfg(fbcode_build)]
        {
            self.warm_revision_check
                .try_compute_targets_not_on_stable(&self.health_check_context)
                .await;
        }
    }

    pub async fn run_checks(
        &mut self,
        _snapshot: &buck2_data::Snapshot,
    ) -> buck2_error::Result<()> {
        #[cfg(fbcode_build)]
        {
            let mut tags: Vec<String> = Vec::new();
            let mut display_reports: Vec<DisplayReport> = Vec::new();

            if let Some(report) = self.warm_revision_check.run() {
                if let Some(tag) = report.tag {
                    tags.push(tag);
                }
                if let Some(display_report) = report.display_report {
                    display_reports.push(display_report);
                }
            }
            if let Some(report) = self.vpn_check.run() {
                if let Some(tag) = report.tag {
                    tags.push(tag);
                }
                if let Some(display_report) = report.display_report {
                    display_reports.push(display_report);
                }
            }
            self.send_tags(tags);
            self.send_display_reports(display_reports);
        }
        Ok(())
    }

    fn send_tags(&mut self, tags: Vec<String>) {
        if tags.is_empty() {
            // Since tags are aggregated and written to logs only once, we don't need to publish empty lists.
            return;
        }
        if let Some(tags_sender) = &mut self.tags_sender {
            match tags_sender.try_send(tags) {
                Err(TrySendError::Closed(_)) => {
                    // If the receiver is dropped, drop the sender to stop sending next time.
                    self.tags_sender = None;
                }
                _ => {
                    // No error or TrySendError::Full
                    // If the channel is full, skip sending these tags rather than OOMing due to huge buffers.
                }
            }
        }
    }

    fn send_display_reports(&mut self, display_reports: Vec<DisplayReport>) {
        // Even if there are no reports, publish an empty list to signify that we have run the health checks.

        // TODO(rajneeshl): Move this description of reporting to a HealthCheck trait when that is ready.
        // There are 3 ways a health check can report back to the console:
        // All OK: With a valid DisplayReport with `warning` field being None.
        // User warning: With a valid DisplayReport with `warning` field set to user message and remediation.
        // Cannot run: When a health check cannot run it will return an empty Report e.g. target-specific checks.

        if let Some(display_reports_sender) = &mut self.display_reports_sender {
            match display_reports_sender.try_send(display_reports) {
                Err(TrySendError::Closed(_)) => {
                    // If the receiver is dropped, drop the sender to stop sending next time.
                    self.display_reports_sender = None;
                }
                Err(TrySendError::Full(_)) => {
                    // If the channel is full, skip sending these reports rather than OOMing due to huge buffers.
                    let _ignored = soft_error!(
                        "health_check_report_publish_error",
                        buck2_error::buck2_error!(buck2_error::ErrorTag::ClientGrpc, "Buffer full while publishing health check reports"),
                        quiet: true
                    );
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use buck2_data::*;

    use crate::health_check_client::HealthCheckClient;

    #[tokio::test]
    async fn test_excess_cache_miss_with_no_invalidation_source() -> buck2_error::Result<()> {
        let action_execution_end = ActionExecutionEnd {
            invalidation_info: Some(buck2_data::CommandInvalidationInfo {
                changed_file: None,
                changed_any: None,
            }),
            ..Default::default()
        };
        let mut client = HealthCheckClient::new("test".to_owned(), None, None);
        client.update_excess_cache_miss(&action_execution_end).await;
        assert!(client.health_check_context.has_excess_cache_misses);

        // Second update should not change the value.
        client.update_excess_cache_miss(&action_execution_end).await;
        assert!(client.health_check_context.has_excess_cache_misses);

        Ok(())
    }

    #[tokio::test]
    async fn test_excess_cache_miss_with_invalidation_source() -> buck2_error::Result<()> {
        let action_execution_end = ActionExecutionEnd {
            invalidation_info: Some(buck2_data::CommandInvalidationInfo {
                changed_file: Some(command_invalidation_info::InvalidationSource {}),
                changed_any: None,
            }),
            ..Default::default()
        };
        let mut client = HealthCheckClient::new("test".to_owned(), None, None);
        client.update_excess_cache_miss(&action_execution_end).await;
        assert!(!client.health_check_context.has_excess_cache_misses);

        Ok(())
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // TODO(rajneeshl): Remove this when the health checks are moved to the server.

use buck2_core::is_open_source;
use buck2_core::soft_error;
use buck2_health_check_proto::health_check_context_event;
use buck2_health_check_proto::HealthCheckContextEvent;
use tokio::sync::mpsc::error::TrySendError;
use tokio::sync::mpsc::Sender;

use crate::health_check_context::HealthCheckContext;
#[cfg(fbcode_build)]
use crate::health_checks::facebook::warm_revision::warm_revision_check::WarmRevisionCheck;
use crate::health_checks::vpn_check::VpnCheck;
use crate::report::DisplayReport;
#[cfg(fbcode_build)]
use crate::rpc::health_check_rpc_client::HealthCheckRpcClient;

/// This client maintains the context and make requests to the health check server.
pub struct HealthCheckClient {
    health_check_context: HealthCheckContext,

    // TODO(rajneeshl): These are temporary hacks to call health checks directly from client.
    // Move these to the health check server when it is ready.
    #[cfg(fbcode_build)]
    warm_revision_check: Option<WarmRevisionCheck>,
    vpn_check: VpnCheck,
    // Writer to send tags to be logged to scuba.
    tags_sender: Option<Sender<Vec<String>>>,
    // Writer to send health check reports to be displayed to the user.
    display_reports_sender: Option<Sender<Vec<DisplayReport>>>,
    #[cfg(fbcode_build)]
    rpc_client: HealthCheckRpcClient,
    run_server_out_of_process: bool,
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
            warm_revision_check: WarmRevisionCheck::new().ok(),
            vpn_check: VpnCheck::new(),
            tags_sender,
            display_reports_sender,
            #[cfg(fbcode_build)]
            rpc_client: HealthCheckRpcClient::new(),
            run_server_out_of_process: !is_open_source(),
        }
    }

    pub async fn update_command_data(&mut self, command_start: buck2_data::CommandStart) {
        if self.run_server_out_of_process {
            let event = HealthCheckContextEvent {
                data: Some(health_check_context_event::Data::CommandStart(
                    command_start,
                )),
            };
            self.update_context(event).await;
        } else {
            self.health_check_context.command_data = command_start.data;
            self.try_update_warm_revision_check().await;
        }
    }

    pub async fn update_parsed_target_patterns(
        &mut self,
        parsed_target_patterns: &buck2_data::ParsedTargetPatterns,
    ) {
        if self.run_server_out_of_process {
            let event = HealthCheckContextEvent {
                data: Some(health_check_context_event::Data::ParsedTargetPatterns(
                    parsed_target_patterns.clone(),
                )),
            };
            self.update_context(event).await;
        } else {
            self.health_check_context.parsed_target_patterns = Some(parsed_target_patterns.clone());
            self.vpn_check
                .try_update_can_run(&self.health_check_context);
            self.try_update_warm_revision_check().await;
        }
    }

    pub async fn update_branched_from_revision(&mut self, branched_from_revision: &str) {
        if self.run_server_out_of_process {
            let event = HealthCheckContextEvent {
                data: Some(health_check_context_event::Data::BranchedFromRevision(
                    branched_from_revision.to_owned(),
                )),
            };
            self.update_context(event).await;
        } else {
            self.health_check_context.branched_from_revision =
                Some(branched_from_revision.to_owned());
            self.try_update_warm_revision_check().await;
        }
    }

    pub async fn update_excess_cache_misses(
        &mut self,
        action_end: &buck2_data::ActionExecutionEnd,
    ) {
        if self.run_server_out_of_process {
            if let Some(v) = &action_end.invalidation_info {
                if v.changed_file.is_none() {
                    let event = HealthCheckContextEvent {
                        data: Some(health_check_context_event::Data::HasExcessCacheMisses(true)),
                    };
                    self.update_context(event).await;
                }
            }
        } else {
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
    }

    pub async fn update_experiment_configurations(
        &mut self,
        experiment_configurations: &buck2_data::SystemInfo,
    ) {
        let process_isolation = experiment_configurations.enable_health_check_process_isolation();
        self.run_server_out_of_process = self.run_server_out_of_process && process_isolation;

        if self.run_server_out_of_process {
            let event = HealthCheckContextEvent {
                data: Some(health_check_context_event::Data::ExperimentConfigurations(
                    experiment_configurations.clone(),
                )),
            };
            self.update_context(event).await;
        } else {
            self.health_check_context.experiment_configurations =
                Some(experiment_configurations.clone());
            self.vpn_check
                .try_update_can_run(&self.health_check_context);
            self.try_update_warm_revision_check().await;
        }
    }

    async fn try_update_warm_revision_check(&mut self) {
        #[cfg(fbcode_build)]
        {
            if let Some(check) = &mut self.warm_revision_check {
                check
                    .try_compute_targets_not_on_stable(&self.health_check_context)
                    .await;
            }
        }
    }

    #[allow(unused_variables)] // event unused in OSS
    async fn update_context(&self, event: HealthCheckContextEvent) {
        #[cfg(fbcode_build)]
        {
            if let Err(e) = self.rpc_client.update_context(event).await {
                let _ignored =
                    soft_error!("health_check_context_update_error", e.into(), quiet: true);
            }
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
            if self.run_server_out_of_process {
                for report in self.rpc_client.run_checks().await? {
                    if let Some(tag) = report.tag {
                        tags.push(tag);
                    }

                    if let Some(display_report) = report.display_report {
                        display_reports.push(display_report);
                    }
                }
            } else {
                // TODO(rajneeshl): Call the health check executor directly here.
                if let Some(report) = self
                    .warm_revision_check
                    .as_ref()
                    .map(|check| check.run())
                    .flatten()
                {
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // TODO(rajneeshl): Remove this when the health checks are moved to the server.

use buck2_health_check_proto::health_check_context_event;
use buck2_health_check_proto::HealthCheckContextEvent;
use tokio::sync::mpsc::error::TrySendError;
use tokio::sync::mpsc::Sender;

use crate::health_check_service::HealthCheckService;
use crate::report::DisplayReport;
#[cfg(fbcode_build)]
use crate::rpc::health_check_rpc_client::HealthCheckRpcClient;

/// This client maintains the context and make requests to the health check server.
pub struct HealthCheckClient {
    health_check_service: Box<dyn HealthCheckService>,
    // Writer to send tags to be logged to scuba.
    tags_sender: Option<Sender<Vec<String>>>,
    // Writer to send health check reports to be displayed to the user.
    display_reports_sender: Option<Sender<Vec<DisplayReport>>>,
}

impl HealthCheckClient {
    pub fn new(
        tags_sender: Option<Sender<Vec<String>>>,
        display_reports_sender: Option<Sender<Vec<DisplayReport>>>,
    ) -> Self {
        Self {
            tags_sender,
            display_reports_sender,
            health_check_service: Self::create_service(),
        }
    }

    fn create_service() -> Box<dyn HealthCheckService> {
        #[cfg(fbcode_build)]
        {
            Box::new(HealthCheckRpcClient::new())
        }
        #[cfg(not(fbcode_build))]
        {
            // There is no easy binary distribution mechanism for OSS, hence default to in-process execution.
            Box::new(crate::health_check_executor::HealthCheckExecutor::new())
        }
    }

    pub async fn update_command_data(
        &mut self,
        command_start: buck2_data::CommandStart,
    ) -> buck2_error::Result<()> {
        let event = HealthCheckContextEvent {
            data: Some(health_check_context_event::Data::CommandStart(
                command_start,
            )),
        };
        self.health_check_service.update_context(&event).await
    }

    pub async fn update_parsed_target_patterns(
        &mut self,
        parsed_target_patterns: &buck2_data::ParsedTargetPatterns,
    ) -> buck2_error::Result<()> {
        let event = HealthCheckContextEvent {
            data: Some(health_check_context_event::Data::ParsedTargetPatterns(
                parsed_target_patterns.clone(),
            )),
        };
        self.health_check_service.update_context(&event).await
    }

    pub async fn update_branched_from_revision(
        &mut self,
        branched_from_revision: &str,
    ) -> buck2_error::Result<()> {
        let event = HealthCheckContextEvent {
            data: Some(health_check_context_event::Data::BranchedFromRevision(
                branched_from_revision.to_owned(),
            )),
        };
        self.health_check_service.update_context(&event).await
    }

    pub async fn update_excess_cache_misses(
        &mut self,
        action_end: &buck2_data::ActionExecutionEnd,
    ) -> buck2_error::Result<()> {
        let has_excess_cache_miss = action_end
            .invalidation_info
            .as_ref()
            .is_some_and(|v| v.changed_file.is_none());
        if has_excess_cache_miss {
            let event = HealthCheckContextEvent {
                data: Some(health_check_context_event::Data::HasExcessCacheMisses(true)),
            };
            self.health_check_service.update_context(&event).await?
        }
        Ok(())
    }

    pub async fn update_experiment_configurations(
        &mut self,
        experiment_configurations: &buck2_data::SystemInfo,
    ) -> buck2_error::Result<()> {
        let event = HealthCheckContextEvent {
            data: Some(health_check_context_event::Data::ExperimentConfigurations(
                experiment_configurations.clone(),
            )),
        };
        self.health_check_service.update_context(&event).await
    }

    pub async fn run_checks(
        &mut self,
        _snapshot: &buck2_data::Snapshot,
    ) -> buck2_error::Result<()> {
        let mut tags: Vec<String> = Vec::new();
        let mut display_reports: Vec<DisplayReport> = Vec::new();

        let reports = self.health_check_service.run_checks().await?;

        for report in reports {
            if let Some(tag) = report.tag {
                tags.push(tag);
            }

            if let Some(display_report) = report.display_report {
                display_reports.push(display_report);
            }
        }
        self.send_tags(tags)?;
        self.send_display_reports(display_reports)?;
        Ok(())
    }

    fn send_tags(&mut self, tags: Vec<String>) -> buck2_error::Result<()> {
        if tags.is_empty() {
            // Since tags are aggregated and written to logs only once, we don't need to publish empty lists.
            return Ok(());
        }
        if let Some(tags_sender) = &mut self.tags_sender {
            match tags_sender.try_send(tags) {
                Err(TrySendError::Closed(_)) => {
                    // If the receiver is dropped, drop the sender to stop sending next time.
                    self.tags_sender = None;
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::HealthCheck,
                        "Health check tags listener closed."
                    ));
                }
                Err(TrySendError::Full(_)) => {
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::HealthCheck,
                        "Health check tags channel full. Dropping tags."
                    ));
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn send_display_reports(
        &mut self,
        display_reports: Vec<DisplayReport>,
    ) -> buck2_error::Result<()> {
        // Even if there are no reports, publish an empty list to signify that we have run the health checks.
        if let Some(display_reports_sender) = &mut self.display_reports_sender {
            match display_reports_sender.try_send(display_reports) {
                Err(TrySendError::Closed(_)) => {
                    // If the receiver is dropped, drop the sender to stop sending next time.
                    self.display_reports_sender = None;
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::HealthCheck,
                        "Health check display reports listener closed."
                    ));
                }
                Err(TrySendError::Full(_)) => {
                    // If the channel is full, skip sending these reports rather than OOMing due to huge buffers.
                    return Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::HealthCheck,
                        "Health check diplay reports channel full. Dropping reports."
                    ));
                }
                _ => {}
            }
        }
        Ok(())
    }
}

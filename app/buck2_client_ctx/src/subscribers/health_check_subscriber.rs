/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // TODO(rajneeshl): Remove this when we start forwarding events to client
use std::sync::Arc;

use async_trait::async_trait;
use buck2_data::buck_event::Data::*;
use buck2_error::BuckErrorContext;
use buck2_events::BuckEvent;
use buck2_health_check::health_check_client::HealthCheckClient;
use buck2_health_check::report::DisplayReport;
use buck2_wrapper_common::invocation_id::TraceId;
use tokio::sync::mpsc::Sender;

use crate::subscribers::subscriber::EventSubscriber;

/// This subscriber is responsible for forwarding events to the health check client
pub struct HealthCheckSubscriber {
    health_check_client: HealthCheckClient,
}

#[async_trait]
impl EventSubscriber for HealthCheckSubscriber {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> buck2_error::Result<()> {
        for ev in events {
            self.handle_event(ev).await?;
        }
        Ok(())
    }
}

impl HealthCheckSubscriber {
    pub fn new(
        trace_id: TraceId,
        tags_sender: Sender<Vec<String>>,
        display_reports_sender: Sender<Vec<DisplayReport>>,
    ) -> Box<Self> {
        Box::new(Self {
            health_check_client: HealthCheckClient::new(
                trace_id.to_string(),
                Some(tags_sender),
                Some(display_reports_sender),
            ),
        })
    }

    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> buck2_error::Result<()> {
        match event.data() {
            SpanStart(start) => match &start.data {
                Some(buck2_data::span_start_event::Data::Command(command)) => {
                    self.health_check_client
                        .update_command_data(command.data.clone())
                        .await;
                }
                _ => {}
            },
            SpanEnd(end) => {
                use buck2_data::span_end_event::Data::*;
                match end
                    .data
                    .as_ref()
                    .buck_error_context("Missing `data` in SpanEnd")?
                {
                    FileWatcher(file_watcher) => {
                        if let Some(merge_base) = file_watcher
                            .stats
                            .as_ref()
                            .and_then(|stats| stats.branched_from_revision.as_ref())
                        {
                            self.health_check_client
                                .update_branched_from_revision(&merge_base)
                                .await;
                        }
                    }
                    ActionExecution(action_execution_end) => {
                        self.health_check_client
                            .update_excess_cache_miss(action_execution_end)
                            .await;
                    }
                    _ => {}
                }
            }
            Instant(instant) => {
                use buck2_data::instant_event::Data::*;
                match instant
                    .data
                    .as_ref()
                    .buck_error_context("Missing `data` in `Instant`")?
                {
                    SystemInfo(system_info) => {
                        self.health_check_client
                            .update_experiment_configurations(&system_info)
                            .await;
                    }
                    TargetPatterns(target_patterns) => {
                        self.health_check_client
                            .update_parsed_target_patterns(&target_patterns)
                            .await;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        Ok(())
    }
}

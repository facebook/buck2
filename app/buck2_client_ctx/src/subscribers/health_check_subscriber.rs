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
use buck2_events::BuckEvent;
use buck2_health_check::health_check_client::HealthCheckClient;
use buck2_wrapper_common::invocation_id::TraceId;

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
    pub fn new(trace_id: TraceId) -> Box<Self> {
        Box::new(Self {
            health_check_client: HealthCheckClient::new(trace_id.to_string()),
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
            _ => {}
        }
        Ok(())
    }
}

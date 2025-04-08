/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Unused in oss

use buck2_error::BuckErrorContext;
use buck2_health_check_proto::health_check_context_event::Data;
use buck2_health_check_proto::HealthCheckContextEvent;

use crate::health_check_context::HealthCheckContext;
#[cfg(fbcode_build)]
use crate::health_checks::facebook::warm_revision::warm_revision_check::WarmRevisionCheck;
use crate::health_checks::vpn_check::VpnCheck;
use crate::interface::HealthCheck;
use crate::report::Report;

/// This executor is responsible for maintaining the health check context and running the checks.
pub struct HealthCheckExecutor {
    health_check_context: HealthCheckContext,
    health_checks: Vec<Box<dyn HealthCheck>>,
}

impl HealthCheckExecutor {
    pub fn new() -> Self {
        Self {
            health_check_context: HealthCheckContext {
                ..Default::default()
            },
            health_checks: Self::register_health_checks(),
        }
    }

    #[allow(clippy::vec_init_then_push)] // Ignore warning in OSS build
    fn register_health_checks() -> Vec<Box<dyn HealthCheck>> {
        let mut health_checks: Vec<Box<dyn HealthCheck>> = Vec::new();
        #[cfg(fbcode_build)]
        {
            // Facebook-only health checks
            if let Ok(stable_revision_check) = WarmRevisionCheck::new() {
                health_checks.push(Box::new(stable_revision_check));
            }
        }
        health_checks.push(Box::new(VpnCheck::new()));

        health_checks
    }

    pub(crate) async fn update_context(
        &mut self,
        event: &HealthCheckContextEvent,
    ) -> buck2_error::Result<()> {
        let data = event
            .data
            .as_ref()
            .buck_error_context("Missing `data` in HealthCheckContextEvent")?;

        match data {
            Data::ParsedTargetPatterns(parsed_target_patterns) => {
                self.health_check_context.parsed_target_patterns =
                    Some(parsed_target_patterns.clone());
            }
            Data::CommandStart(command_start) => {
                self.health_check_context.command_data = command_start.data.clone();
            }
            Data::BranchedFromRevision(branched_from_revision) => {
                self.health_check_context.branched_from_revision =
                    Some(branched_from_revision.to_owned());
            }
            Data::HasExcessCacheMisses(_) => {
                self.health_check_context.has_excess_cache_misses = true;
            }
            Data::ExperimentConfigurations(system_info) => {
                self.health_check_context.experiment_configurations = Some(system_info.clone());
            }
        }
        for check in self.health_checks.iter_mut() {
            check
                .handle_context_update(&self.health_check_context)
                .await;
        }
        Ok(())
    }

    pub(crate) async fn run_checks(&self) -> Vec<Report> {
        let mut reports = Vec::new();
        for check in &self.health_checks {
            if let Some(report) = check.run_check().ok().flatten() {
                reports.push(report);
            }
        }
        reports
    }
}

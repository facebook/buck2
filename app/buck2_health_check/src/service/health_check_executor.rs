/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Unused in oss

#[cfg(fbcode_build)]
use crate::health_checks::facebook::stable_revision::stable_revision_check::StableRevisionCheck;
use crate::health_checks::vpn_check::VpnCheck;
use crate::interface::HealthCheck;
use crate::interface::HealthCheckContext;
use crate::interface::HealthCheckContextEvent;
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
            if let Ok(stable_revision_check) = StableRevisionCheck::new() {
                health_checks.push(Box::new(stable_revision_check));
            }
        }
        health_checks.push(Box::new(VpnCheck::new()));

        health_checks
    }

    pub async fn update_context(
        &mut self,
        event: HealthCheckContextEvent,
    ) -> buck2_error::Result<()> {
        match event {
            HealthCheckContextEvent::CommandStart(command_start) => {
                self.health_check_context.command_data = command_start.data;
            }
            HealthCheckContextEvent::ParsedTargetPatterns(parsed_target_patterns) => {
                self.health_check_context.parsed_target_patterns = Some(parsed_target_patterns);
            }
            HealthCheckContextEvent::BranchedFromRevision(rev) => {
                self.health_check_context.branched_from_revision = Some(rev);
            }
            HealthCheckContextEvent::HasExcessCacheMisses() => {
                self.health_check_context.has_excess_cache_misses = true;
            }
            HealthCheckContextEvent::ExperimentConfigurations(system_info) => {
                self.health_check_context.experiment_configurations = Some(system_info);
            }
        }
        for check in self.health_checks.iter_mut() {
            check
                .handle_context_update(&self.health_check_context)
                .await;
        }
        Ok(())
    }

    pub async fn run_checks(&mut self) -> buck2_error::Result<Vec<Report>> {
        let mut reports = Vec::new();
        for check in &self.health_checks {
            if let Some(report) = check.run_check().ok().flatten() {
                reports.push(report);
            }
        }
        Ok(reports)
    }
}

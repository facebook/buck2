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
use crate::report::Report;

/// This executor is responsible for maintaining the health check context and running the checks.
pub struct HealthCheckExecutor {
    health_check_context: HealthCheckContext,
    // TODO(rajneeshl): Create a trait for health checks and store these as a vec.
    #[cfg(fbcode_build)]
    warm_revision_check: Option<WarmRevisionCheck>,
    vpn_check: VpnCheck,
}

impl HealthCheckExecutor {
    pub fn new() -> Self {
        Self {
            health_check_context: HealthCheckContext {
                ..Default::default()
            },
            #[cfg(fbcode_build)]
            warm_revision_check: WarmRevisionCheck::new().ok(),
            vpn_check: VpnCheck::new(),
        }
    }

    pub(crate) async fn update_context(
        &mut self,
        event: &HealthCheckContextEvent,
    ) -> buck2_error::Result<()> {
        // TODO(rajneeshl): Convert this to the rust HealthCheckContext and pass that to the checks instead to simplify this code.
        let data = event
            .data
            .as_ref()
            .buck_error_context("Missing `data` in HealthCheckContextEvent")?;

        match data {
            Data::ParsedTargetPatterns(parsed_target_patterns) => {
                self.update_parsed_target_patterns(&parsed_target_patterns)
                    .await
            }
            Data::CommandStart(command_start) => {
                self.update_command_data(command_start.data.clone()).await
            }
            Data::BranchedFromRevision(branched_from_revision) => {
                self.update_branched_from_revision(&branched_from_revision)
                    .await
            }
            Data::HasExcessCacheMisses(_) => self.update_excess_cache_misses().await,
            Data::ExperimentConfigurations(system_info) => {
                self.update_experiment_configurations(&system_info).await
            }
        }
        Ok(())
    }

    pub(crate) async fn run_checks(&self) -> Vec<Report> {
        let mut reports = Vec::new();
        #[cfg(fbcode_build)]
        {
            if let Some(report) = self
                .warm_revision_check
                .as_ref()
                .map(|check| check.run())
                .flatten()
            {
                reports.push(report);
            }
        }
        if let Some(report) = self.vpn_check.run() {
            reports.push(report);
        }
        reports
    }

    async fn update_command_data(&mut self, command_data: Option<buck2_data::command_start::Data>) {
        self.health_check_context.command_data = command_data;
        self.try_update_warm_revision_check().await;
    }

    async fn update_parsed_target_patterns(
        &mut self,
        parsed_target_patterns: &buck2_data::ParsedTargetPatterns,
    ) {
        self.health_check_context.parsed_target_patterns = Some(parsed_target_patterns.clone());
        self.vpn_check
            .try_update_can_run(&self.health_check_context);
        self.try_update_warm_revision_check().await;
    }

    async fn update_branched_from_revision(&mut self, branched_from_revision: &str) {
        self.health_check_context.branched_from_revision = Some(branched_from_revision.to_owned());
        self.try_update_warm_revision_check().await;
    }

    async fn update_excess_cache_misses(&mut self) {
        if self.health_check_context.has_excess_cache_misses {
            // If we already know that we have excess cache misses, we don't need updates again.
            return;
        }
        self.health_check_context.has_excess_cache_misses = true;
        self.try_update_warm_revision_check().await;
    }

    async fn update_experiment_configurations(
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
            if let Some(check) = &mut self.warm_revision_check {
                check
                    .try_compute_targets_not_on_stable(&self.health_check_context)
                    .await;
            }
        }
    }
}

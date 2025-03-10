/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::health_check_context::HealthCheckContext;
#[cfg(fbcode_build)]
use crate::health_checks::facebook::warm_revision::warm_revision_check::WarmRevisionCheck;
use crate::health_checks::vpn_check::VpnCheck;

/// This client maintains the context and make requests to the health check server.
pub struct HealthCheckClient {
    health_check_context: HealthCheckContext,

    // TODO(rajneeshl): These are temporary hacks to call health checks directly from client.
    // Move these to the health check server when it is ready.
    #[cfg(fbcode_build)]
    warm_revision_check: WarmRevisionCheck,
    vpn_check: VpnCheck,
}

impl HealthCheckClient {
    pub fn new(trace_id: String) -> Self {
        Self {
            health_check_context: HealthCheckContext {
                trace_id,
                ..Default::default()
            },
            #[cfg(fbcode_build)]
            warm_revision_check: WarmRevisionCheck::new(),
            vpn_check: VpnCheck::new(),
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

    pub fn check_stable_revision(&self) -> Option<&[String]> {
        #[cfg(fbcode_build)]
        {
            self.warm_revision_check.run()
        }
        #[cfg(not(fbcode_build))]
        {
            None
        }
    }

    pub fn is_vpn_check_enabled(&self) -> bool {
        self.vpn_check.can_run()
    }

    async fn try_update_warm_revision_check(&mut self) {
        #[cfg(fbcode_build)]
        {
            self.warm_revision_check
                .try_compute_targets_not_on_stable(&self.health_check_context)
                .await;
        }
    }
}

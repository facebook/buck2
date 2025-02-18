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

/// This client maintains the context and make requests to the health check server.
pub struct HealthCheckClient {
    health_check_context: HealthCheckContext,

    // TODO(rajneeshl): This is a temporary hack to unblock the warm revision check. Remove this once we have a proper health check server.
    #[cfg(fbcode_build)]
    warm_revision_check: WarmRevisionCheck,
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
        }
    }

    pub fn update_command_data(&mut self, command_data: Option<buck2_data::command_start::Data>) {
        self.health_check_context.command_data = command_data;
    }

    pub fn update_parsed_target_patterns(
        &mut self,
        parsed_target_patterns: &buck2_data::ParsedTargetPatterns,
    ) {
        self.health_check_context.parsed_target_patterns = Some(parsed_target_patterns.clone());
    }

    pub fn update_branched_from_revision(&mut self, branched_from_revision: &str) {
        self.health_check_context.branched_from_revision = Some(branched_from_revision.to_owned());
    }

    pub fn update_excess_cache_misses(&mut self, has_excess_cache_misses: bool) {
        self.health_check_context.has_excess_cache_misses = Some(has_excess_cache_misses);
    }

    pub async fn check_stable_revision(&self) -> Option<Vec<String>> {
        #[cfg(fbcode_build)]
        {
            self.warm_revision_check
                .run(&self.health_check_context)
                .await
        }
        #[cfg(not(fbcode_build))]
        {
            None
        }
    }

    pub fn is_vpn_check_enabled(&self) -> bool {
        #[cfg(fbcode_build)]
        {
            crate::health_checks::vpn_check::can_run(&self.health_check_context)
        }
        #[cfg(not(fbcode_build))]
        {
            false
        }
    }
}

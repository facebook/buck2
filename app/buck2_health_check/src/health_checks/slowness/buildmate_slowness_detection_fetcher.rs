/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::health_checks::slowness::buildmate_slowness_detection_client::BuildmateSlownessConfiguration;
use crate::health_checks::slowness::buildmate_slowness_detection_client::get_buildmate_slowness_configuration;

pub struct BuildmateSlownessDetectionFetcher {
    fb: fbinit::FacebookInit,
}

#[async_trait::async_trait]
pub trait SlownessConfigurationFetcher {
    async fn get_slowness_configuration(
        &self,
        build_uuid: String,
        target_patterns: Vec<String>,
        is_incremental_build: bool,
    ) -> buck2_error::Result<Option<BuildmateSlownessConfiguration>>;
}

#[async_trait::async_trait]
impl SlownessConfigurationFetcher for BuildmateSlownessDetectionFetcher {
    async fn get_slowness_configuration(
        &self,
        build_uuid: String,
        target_patterns: Vec<String>,
        is_incremental_build: bool,
    ) -> buck2_error::Result<Option<BuildmateSlownessConfiguration>> {
        get_buildmate_slowness_configuration(
            self.fb,
            build_uuid,
            target_patterns,
            is_incremental_build,
        )
        .await
    }
}

impl BuildmateSlownessDetectionFetcher {
    pub fn new() -> Self {
        Self {
            // This should have already been initialized by the buck2 client.
            fb: buck2_common::fbinit::get_or_init_fbcode_globals(),
        }
    }
}

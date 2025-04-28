/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::init::ResourceControlConfig;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_forkserver::client::ForkserverClient;

#[cfg(unix)]
pub async fn maybe_launch_forkserver(
    root_config: &LegacyBuckConfig,
    forkserver_state_dir: &AbsNormPath,
    resource_control: &ResourceControlConfig,
) -> buck2_error::Result<Option<ForkserverClient>> {
    use buck2_common::legacy_configs::key::BuckconfigKeyRef;
    use buck2_core::rollout_percentage::RolloutPercentage;
    use buck2_error::BuckErrorContext;

    let config = root_config
        .parse::<RolloutPercentage>(BuckconfigKeyRef {
            section: "buck2",
            property: "forkserver",
        })?
        .unwrap_or_else(RolloutPercentage::always);

    if !config.roll() {
        return Ok(None);
    }

    let exe = std::env::current_exe().buck_error_context("Cannot access current_exe")?;
    Some(
        buck2_forkserver::unix::launch_forkserver(
            exe,
            &["forkserver"],
            forkserver_state_dir,
            resource_control.serialize()?,
        )
        .await,
    )
    .transpose()
}

#[cfg(not(unix))]
pub async fn maybe_launch_forkserver(
    _root_config: &LegacyBuckConfig,
    _forkserver_state_dir: &AbsNormPath,
    _resource_control: &ResourceControlConfig,
) -> buck2_error::Result<Option<ForkserverClient>> {
    Ok(None)
}

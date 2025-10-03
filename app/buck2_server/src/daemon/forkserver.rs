/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::init::ResourceControlConfig;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_execute_impl::executors::local::ForkserverAccess;
use buck2_resource_control::memory_tracker::MemoryTrackerHandle;

#[cfg(unix)]
pub async fn maybe_launch_forkserver(
    root_config: &LegacyBuckConfig,
    forkserver_state_dir: &AbsNormPath,
    resource_control: &ResourceControlConfig,
    memory_tracker: Option<MemoryTrackerHandle>,
) -> buck2_error::Result<ForkserverAccess> {
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
        return Ok(ForkserverAccess::None);
    }

    let exe = std::env::current_exe().buck_error_context("Cannot access current_exe")?;
    Ok(ForkserverAccess::Client(
        buck2_forkserver::unix::launch_forkserver(
            exe,
            &["forkserver"],
            forkserver_state_dir,
            resource_control,
            memory_tracker,
        )
        .await?,
    ))
}

#[cfg(not(unix))]
pub async fn maybe_launch_forkserver(
    _root_config: &LegacyBuckConfig,
    _forkserver_state_dir: &AbsNormPath,
    _resource_control: &ResourceControlConfig,
    _memory_tracker: Option<MemoryTrackerHandle>,
) -> buck2_error::Result<ForkserverAccess> {
    Ok(ForkserverAccess::None)
}

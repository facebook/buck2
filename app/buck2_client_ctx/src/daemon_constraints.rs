/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::legacy_configs::init::DaemonStartupConfig;
use buck2_core::ci::sandcastle_id;

use crate::version::BuckVersion;

/// Checks an environment variable to see if we were spawned by a buck daemon and if so, returns the
/// UUID of that daemon.
///
/// This is used to detect nested invocations, but returning `Some` does not guarantee that this is
/// a nested invocation.
pub fn get_possibly_nested_invocation_daemon_uuid() -> Option<String> {
    // Intentionally don't use `buck2_env!` because we don't want this showing up in help output
    std::env::var("BUCK2_DAEMON_UUID").ok()
}

pub fn gen_daemon_constraints(
    daemon_startup_config: &DaemonStartupConfig,
) -> anyhow::Result<buck2_cli_proto::DaemonConstraints> {
    Ok(buck2_cli_proto::DaemonConstraints {
        version: version(),
        user_version: user_version()?,
        daemon_id: buck2_events::daemon_id::DAEMON_UUID.to_string(),
        daemon_startup_config: Some(daemon_startup_config.serialize()?),
        extra: None,
    })
}

pub fn version() -> String {
    BuckVersion::get_unique_id().to_owned()
}

pub fn user_version() -> anyhow::Result<Option<String>> {
    Ok(sandcastle_id()?.map(|s| s.to_owned()))
}

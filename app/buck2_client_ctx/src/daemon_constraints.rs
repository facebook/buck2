/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::env_helper::EnvHelper;

use crate::version::BuckVersion;

pub fn gen_daemon_constraints(
    extra: Option<buck2_cli_proto::ExtraDaemonConstraints>,
    daemon_buster: Option<u64>,
) -> anyhow::Result<buck2_cli_proto::DaemonConstraints> {
    Ok(buck2_cli_proto::DaemonConstraints {
        version: version(),
        user_version: user_version()?,
        daemon_id: buck2_events::daemon_id::DAEMON_UUID.to_string(),
        daemon_buster,
        extra,
    })
}

pub fn version() -> String {
    BuckVersion::get_unique_id().to_owned()
}

pub fn user_version() -> anyhow::Result<Option<String>> {
    static SANDCASTLE_ID: EnvHelper<String> = EnvHelper::new("SANDCASTLE_ID");
    Ok(SANDCASTLE_ID.get()?.cloned())
}

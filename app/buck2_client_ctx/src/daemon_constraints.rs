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

pub fn gen_daemon_constraints() -> anyhow::Result<buck2_cli_proto::DaemonConstraints> {
    static SANDCASTLE_ID: EnvHelper<String> = EnvHelper::new("SANDCASTLE_ID");

    Ok(buck2_cli_proto::DaemonConstraints {
        version: BuckVersion::get_unique_id().to_owned(),
        user_version: SANDCASTLE_ID.get()?.cloned(),
    })
}

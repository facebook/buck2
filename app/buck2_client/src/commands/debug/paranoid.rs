/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::SystemTime;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_core::fs::fs_util;
use prost::Message;

/// Configure paranoid mode.
///
/// In paranoid mode, Buck2 makes minimal assumptions about the reliability of the RE Backend it
/// interacts with. In particular, Buck2 will always race local and remote execution (including
/// cache queries), eagerly download outputs, and not cancel local executions until it successfully
/// downloads outputs from RE.
#[derive(Debug, clap::Parser)]
pub enum ParanoidCommand {
    Enable(EnableParanoidCommand),
    Disable(DisableParanoidCommand),
}

/// Enable paranoid mode for a period of time.
#[derive(Debug, clap::Parser)]
pub struct EnableParanoidCommand {
    /// Duration, in seconds, to enable paranoid mode for.
    #[clap(long, default_value = "3h")]
    ttl: humantime::Duration,
}

/// Disable paranoid mode
#[derive(Debug, clap::Parser)]
pub struct DisableParanoidCommand {}

impl ParanoidCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let paranoid_info_path = ctx.paths()?.roots.paranoid_info_path()?;

        if let Some(parent) = paranoid_info_path.parent() {
            fs_util::create_dir_all(parent)?;
        }

        match self {
            Self::Enable(enable) => {
                let ttl: Duration = enable.ttl.into();
                let expires = SystemTime::now() + ttl;

                let data = buck2_cli_proto::ParanoidInfo {
                    expires_at: Some(expires.into()),
                }
                .encode_to_vec();

                fs_util::write(&paranoid_info_path, data)?;
            }
            Self::Disable(_) => {
                fs_util::remove_all(&paranoid_info_path)?;
            }
        }

        ExitResult::success()
    }
}

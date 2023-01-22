/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;

#[derive(Debug, thiserror::Error)]
#[error(
    "`buck2 killall` command should have been handled by buck2 launcher, \
    this code is practically unreachable"
)]
struct KillallCommandError;

#[derive(Debug, clap::Parser)]
#[clap(about = "Kill all buck2 processes on the machine")]
pub struct KillallCommand {}

impl KillallCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        Err(KillallCommandError.into())
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::FutureExt;

use crate::{daemon::client::BuckdConnectOptions, CommandContext};

#[derive(Debug, clap::Parser)]
#[clap(about = "Kill the buck daemon")]
pub(crate) struct KillCommand {}

impl KillCommand {
    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> anyhow::Result<()> {
        ctx.with_runtime(async move |ctx| {
            match ctx
                .connect_buckd(BuckdConnectOptions::existing_only())
                .await
            {
                Err(_) => {
                    crate::eprintln!("no buckd server running")?;
                }
                Ok(mut client) => {
                    crate::eprintln!("killing buckd server")?;
                    client
                        .with_flushing(|client| client.kill("`buck kill` invoked").boxed())
                        .await??;
                }
            }
            Ok(())
        })
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use chrono::NaiveDateTime;
use clap::ArgMatches;
use futures::FutureExt;
use humantime::format_duration;

use crate::client_command_context::ClientCommandContext;
use crate::daemon::client::BuckdConnectOptions;

#[derive(Debug, clap::Parser)]
#[clap(about = "Buckd status")]
pub(crate) struct StatusCommand {
    #[clap(long, help = "Whether to include a state snapshot in the output.")]
    snapshot: bool,
}

impl StatusCommand {
    pub(crate) fn exec(
        self,
        _matches: &ArgMatches,
        ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        ctx.with_runtime(async move |ctx| {
            match ctx
                .connect_buckd(BuckdConnectOptions::existing_only())
                .await
            {
                Err(_) => {
                    buck2_client::eprintln!("no buckd running")?;
                    // Should this be an error?
                    Ok(())
                }
                Ok(mut client) => {
                    let status = client
                        .with_flushing(|client| client.status(self.snapshot).boxed())
                        .await??;
                    let timestamp = match status.start_time {
                        None => "unknown".to_owned(),
                        Some(timestamp) => {
                            NaiveDateTime::from_timestamp(timestamp.seconds, timestamp.nanos as u32)
                                .format("%Y-%m-%d %H:%M:%S")
                                .to_string()
                        }
                    };
                    let uptime = match status.uptime {
                        None => "unknown".to_owned(),
                        Some(uptime) => {
                            let uptime = Duration::new(uptime.seconds as u64, uptime.nanos as u32);
                            format_duration(uptime).to_string()
                        }
                    };
                    let json_status = serde_json::json!({
                        "start_time": timestamp,
                        "uptime": uptime,
                        "process_info": serde_json::to_value(status.process_info)?,
                        "bytes_allocated" : status.bytes_allocated,
                        "bytes_resident" : status.bytes_resident,
                        "bytes_retained" : status.bytes_retained,
                        "snapshot": serde_json::to_value(status.snapshot)?,
                    });
                    buck2_client::println!("{}", serde_json::to_string_pretty(&json_status)?)?;
                    Ok(())
                }
            }
        })
    }
}

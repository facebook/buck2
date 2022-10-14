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
use humantime::format_duration;

use crate::client_ctx::ClientCommandContext;
use crate::daemon::client::connect::BuckdConnectOptions;

#[derive(Debug, clap::Parser)]
#[clap(about = "Buckd status")]
pub struct StatusCommand {
    #[clap(long, help = "Whether to include a state snapshot in the output.")]
    snapshot: bool,
}

impl StatusCommand {
    fn timestamp_to_string(seconds: u64, nanos: u32) -> String {
        NaiveDateTime::from_timestamp(seconds as i64, nanos)
            .format("%Y-%m-%dT%H:%M:%SZ")
            .to_string()
    }

    fn duration_to_string(duration: Duration) -> String {
        let duration = Duration::from_secs(duration.as_secs());
        format_duration(duration).to_string()
    }

    pub fn exec(self, _matches: &ArgMatches, ctx: ClientCommandContext) -> anyhow::Result<()> {
        ctx.with_runtime(async move |ctx| {
            match ctx
                .connect_buckd(BuckdConnectOptions::existing_only_no_console())
                .await
            {
                Err(_) => {
                    crate::eprintln!("no buckd running")?;
                    // Should this be an error?
                    Ok(())
                }
                Ok(mut client) => {
                    let status = client.with_flushing().status(self.snapshot).await?;
                    let timestamp = match status.start_time {
                        None => "unknown".to_owned(),
                        Some(timestamp) => Self::timestamp_to_string(
                            timestamp.seconds as u64,
                            timestamp.nanos as u32,
                        ),
                    };
                    let uptime = match status.uptime {
                        None => "unknown".to_owned(),
                        Some(uptime) => {
                            let uptime = Duration::new(uptime.seconds as u64, uptime.nanos as u32);
                            Self::duration_to_string(uptime)
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
                    crate::println!("{}", serde_json::to_string_pretty(&json_status)?)?;
                    Ok(())
                }
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::commands::status::StatusCommand;

    #[test]
    fn test_timestamp_to_string() {
        // Check with `TZ=UTC date -r 1662516832 -Iseconds`.
        assert_eq!(
            "2022-09-07T02:13:52Z",
            StatusCommand::timestamp_to_string(1662516832, 123)
        );
    }

    #[test]
    fn test_duration_to_string() {
        assert_eq!(
            "1h 2m 3s",
            StatusCommand::duration_to_string(Duration::new(3600 + 120 + 3, 123456789))
        );
    }
}

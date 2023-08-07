/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context;
use buck2_cli_proto::StatusResponse;
use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::argv::SanitizedArgv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::establish_connection_existing;
use buck2_client_ctx::daemon::client::connect::BuckdConnectOptions;
use buck2_client_ctx::subscribers::stdout_stderr_forwarder::StdoutStderrForwarder;
use buck2_common::daemon_dir::DaemonDir;
use chrono::NaiveDateTime;
use humantime::format_duration;
use walkdir::WalkDir;

#[derive(Debug, thiserror::Error)]
enum StatusError {
    #[error("Incorrect seconds/nanos argument")]
    NativeDateTime,
}

#[derive(Debug, clap::Parser)]
#[clap(about = "Buckd status")]
pub struct StatusCommand {
    #[clap(long, help = "Whether to include a state snapshot in the output.")]
    snapshot: bool,
    #[clap(long, help = "Enable printing status for all running buckd")]
    all: bool,
}

impl StatusCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext<'_>,
    ) -> anyhow::Result<()> {
        ctx.with_runtime(async move |ctx| {
            if self.all {
                let mut daemon_dirs = Vec::new();
                let root = ctx.paths()?.roots.common_buckd_dir()?;
                let walker = WalkDir::new(&root).follow_links(false).into_iter();
                for entry in walker {
                    let entry = entry?;
                    if entry.file_type().is_dir() {
                        let dir = DaemonDir {
                            path: entry.into_path().try_into()?,
                        };

                        if dir.buckd_info().exists() {
                            daemon_dirs.push(dir);
                        }
                    }
                }

                let mut statuses = Vec::new();
                for dir in daemon_dirs {
                    if let Ok(bootstrap_client) = establish_connection_existing(&dir).await {
                        statuses.push(process_status(
                            bootstrap_client
                                .with_subscribers(vec![Box::new(StdoutStderrForwarder)])
                                .with_flushing()
                                .status(self.snapshot)
                                .await?,
                        )?);
                    }
                }

                buck2_client_ctx::println!("{}", serde_json::to_string_pretty(&statuses)?)?;
            } else {
                match ctx
                    .connect_buckd(BuckdConnectOptions::existing_only_no_console())
                    .await
                {
                    Err(_) => {
                        buck2_client_ctx::eprintln!("no buckd running")?;
                        // Should this be an error?
                    }
                    Ok(mut client) => {
                        let json_status =
                            process_status(client.with_flushing().status(self.snapshot).await?)?;
                        buck2_client_ctx::println!(
                            "{}",
                            serde_json::to_string_pretty(&json_status)?
                        )?;
                    }
                }
            }

            Ok(())
        })
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}

fn timestamp_to_string(seconds: u64, nanos: u32) -> anyhow::Result<String> {
    Ok(NaiveDateTime::from_timestamp_opt(seconds as i64, nanos)
        .context(StatusError::NativeDateTime)?
        .format("%Y-%m-%dT%H:%M:%SZ")
        .to_string())
}

fn duration_to_string(duration: Duration) -> String {
    let duration = Duration::from_secs(duration.as_secs());
    format_duration(duration).to_string()
}

fn process_status(status: StatusResponse) -> anyhow::Result<serde_json::Value> {
    let timestamp = match status.start_time {
        None => "unknown".to_owned(),
        Some(timestamp) => timestamp_to_string(timestamp.seconds as u64, timestamp.nanos as u32)?,
    };
    let uptime = match status.uptime {
        None => "unknown".to_owned(),
        Some(uptime) => {
            let uptime = Duration::new(uptime.seconds as u64, uptime.nanos as u32);
            duration_to_string(uptime)
        }
    };

    Ok(serde_json::json!({
        "start_time": timestamp,
        "uptime": uptime,
        "process_info": serde_json::to_value(status.process_info)?,
        "daemon_constraints": serde_json::to_value(status.daemon_constraints)?,
        "snapshot": serde_json::to_value(status.snapshot)?,
        "project_root": status.project_root,
        "isolation_dir": status.isolation_dir,
        "forkserver_pid": serde_json::to_value(status.forkserver_pid)?,
        "supports_vpnless": status.supports_vpnless.unwrap_or_default(),
    }))
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::commands::status::duration_to_string;
    use crate::commands::status::timestamp_to_string;

    #[test]
    fn test_timestamp_to_string() {
        // Check with `TZ=UTC date -r 1662516832 -Iseconds`.
        assert_eq!(
            "2022-09-07T02:13:52Z",
            timestamp_to_string(1662516832, 123).unwrap(),
        );
    }

    #[test]
    fn test_duration_to_string() {
        assert_eq!(
            "1h 2m 3s",
            duration_to_string(Duration::new(3600 + 120 + 3, 123456789))
        );
    }
}

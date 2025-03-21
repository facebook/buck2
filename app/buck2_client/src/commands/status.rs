/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_cli_proto::StatusResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::daemon::client::connect::connect_buckd;
use buck2_client_ctx::daemon::client::connect::establish_connection_existing;
use buck2_client_ctx::daemon::client::connect::BuckdConnectConstraints;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::subscribers::stdout_stderr_forwarder::StdoutStderrForwarder;
use buck2_client_ctx::subscribers::subscribers::EventSubscribers;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use buck2_common::daemon_dir::DaemonDir;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use chrono::DateTime;
use humantime::format_duration;
use walkdir::WalkDir;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
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
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
    ) -> buck2_error::Result<()> {
        ctx.with_runtime(|ctx| async move {
            let mut events_ctx =
                EventsCtx::new(EventSubscribers::new(vec![Box::new(StdoutStderrForwarder)]));
            if self.all {
                let mut daemon_dirs = Vec::new();
                let root = ctx.paths()?.roots.common_buckd_dir()?;
                let walker = WalkDir::new(&root).follow_links(false).into_iter();
                for entry in walker {
                    let entry =
                        entry.map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
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
                                .to_connector()
                                .with_flushing()
                                .status(&mut events_ctx, self.snapshot)
                                .await?,
                        )?);
                    }
                }

                buck2_client_ctx::println!("{}", serde_json::to_string_pretty(&statuses)?)?;
            } else {
                match connect_buckd(
                    BuckdConnectConstraints::ExistingOnly,
                    &mut events_ctx,
                    ctx.paths()?,
                )
                .await
                {
                    Err(_) => {
                        buck2_client_ctx::eprintln!("no buckd running")?;
                        // Should this be an error?
                    }
                    Ok(mut client) => {
                        let json_status = process_status(
                            client
                                .with_flushing()
                                .status(&mut events_ctx, self.snapshot)
                                .await?,
                        )?;
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

fn timestamp_to_string(seconds: u64, nanos: u32) -> buck2_error::Result<String> {
    Ok(DateTime::from_timestamp(seconds as i64, nanos)
        .buck_error_context(StatusError::NativeDateTime)?
        .format("%Y-%m-%dT%H:%M:%SZ")
        .to_string())
}

fn duration_to_string(duration: Duration) -> String {
    let duration = Duration::from_secs(duration.as_secs());
    format_duration(duration).to_string()
}

fn process_status(status: StatusResponse) -> buck2_error::Result<serde_json::Value> {
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

    let mut value = serde_json::json!({
        "start_time": timestamp,
        "uptime": uptime,
        "process_info": serde_json::to_value(status.process_info)?,
        "daemon_constraints": serde_json::to_value(status.daemon_constraints)?,
        "snapshot": serde_json::to_value(status.snapshot)?,
        "project_root": status.project_root,
        "isolation_dir": status.isolation_dir,
        "forkserver_pid": serde_json::to_value(status.forkserver_pid)?,
        "supports_vpnless": status.supports_vpnless.unwrap_or_default(),
        "http2": status.http2,
        "io_provider": status.io_provider,
    });

    if let Some(valid_working_directory) = status.valid_working_directory {
        value["valid_working_directory"] = serde_json::to_value(valid_working_directory)?;
    }

    if let Some(valid_buck_out_mount) = status.valid_buck_out_mount {
        value["valid_buck_out_mount"] = serde_json::to_value(valid_buck_out_mount)?;
    }

    Ok(value)
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

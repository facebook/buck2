/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::time::Duration;
use std::time::SystemTime;

use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::EventLogPathBuf;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use chrono::DateTime;
use chrono::Local;
use futures::TryStreamExt;
use humantime::format_duration;
use thiserror::Error;

#[derive(Debug, Error)]
enum BuildInfoError {
    #[error("Failed to read event log")]
    EventLogReadFail,
}

struct LogInfo {
    revision: Option<String>,
    daemon_uptime_s: Option<u64>,
    timestamp_end: Option<SystemTime>,
}

pub(crate) struct BuildInfo {
    uuid: TraceId,
    pub timestamp: DateTime<Local>,
    pub command: String,
    working_dir: String,
    pub buck2_revision: String,
    pub command_duration: Option<Duration>,
    pub daemon_uptime_s: Option<u64>,
}

impl fmt::Display for BuildInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "buck2 UI: https://www.internalfb.com/buck2/{}
timestamp: {}
command: {}
working dir: {}
buck2_revision: {}
command duration: {}
daemon uptime: {}
        ",
            self.uuid,
            self.timestamp.format("%c %Z"),
            self.command,
            self.working_dir,
            self.buck2_revision,
            seconds_to_string(self.command_duration.map(|d| d.as_secs())),
            seconds_to_string(self.daemon_uptime_s),
        )
    }
}

pub(crate) async fn get(log: &EventLogPathBuf) -> anyhow::Result<BuildInfo> {
    let (invocation, events) = log.unpack_stream().await?;
    let mut filtered_events = events.try_filter_map(|log| {
        let maybe_buck_event = match log {
            StreamValue::Result(_) => None,
            StreamValue::Event(buck_event) => Some(buck_event),
        };
        futures::future::ready(Ok(maybe_buck_event))
    });

    let first_event: BuckEvent = filtered_events
        .try_next()
        .await?
        .ok_or(BuildInfoError::EventLogReadFail)?
        .try_into()?;

    let mut info = LogInfo {
        revision: None,
        daemon_uptime_s: None,
        timestamp_end: None,
    };
    loop {
        let res = match filtered_events.try_next().await {
            Ok(Some(event)) => extract_info(&mut info, event),
            Ok(None) => break,
            Err(e) => Err(e),
        };
        if let Err(e) = res {
            buck2_client_ctx::eprintln!("Error found when iterating through logs: {:#}", e)?;
            break;
        }
    }

    let timestamp_start = first_event.timestamp();
    let duration = {
        if let Some(end) = info.timestamp_end {
            Some(end.duration_since(timestamp_start)?)
        } else {
            None
        }
    };

    let t_start: DateTime<Local> = timestamp_start.into();

    let output = BuildInfo {
        uuid: first_event.trace_id()?,
        timestamp: t_start,
        command: format_cmd(&invocation.command_line_args),
        working_dir: invocation.working_dir,
        buck2_revision: info.revision.unwrap_or_else(|| "".to_owned()),
        command_duration: duration,
        daemon_uptime_s: info.daemon_uptime_s,
    };

    Ok(output)
}

fn extract_info(info: &mut LogInfo, event: Box<buck2_data::BuckEvent>) -> anyhow::Result<()> {
    match event.data {
        Some(buck2_data::buck_event::Data::SpanStart(span)) => match &span.data {
            Some(buck2_data::span_start_event::Data::Command(action)) => {
                if info.revision.is_none() && action.metadata.contains_key("buck2_revision") {
                    if let Some(buck2_revision) = action.metadata.get("buck2_revision") {
                        info.revision.get_or_insert(buck2_revision.clone());
                    }
                }
            }
            _ => (),
        },
        Some(buck2_data::buck_event::Data::Instant(span)) => match &span.data {
            Some(buck2_data::instant_event::Data::Snapshot(snapshot)) => {
                info.daemon_uptime_s.get_or_insert(snapshot.daemon_uptime_s);
            }
            _ => (),
        },

        _ => (),
    }
    if let Some(timestamp) = event.timestamp {
        info.timestamp_end = Some(SystemTime::try_from(timestamp)?)
    };
    Ok(())
}

fn seconds_to_string(seconds: Option<u64>) -> String {
    if let Some(seconds) = seconds {
        let duration = Duration::from_secs(seconds);
        format_duration(duration).to_string()
    } else {
        "".to_owned()
    }
}

pub fn format_cmd(cmd_args: &[String]) -> String {
    if cmd_args.is_empty() {
        "???".to_owned()
    } else {
        let mut program_name: &str = &cmd_args[0];
        let program_args = &cmd_args[1..];
        if program_name.ends_with("fbcode/buck2/.buck2") {
            program_name = "buck2";
        }
        format!("{} {}", program_name, program_args.join(" "))
    }
}

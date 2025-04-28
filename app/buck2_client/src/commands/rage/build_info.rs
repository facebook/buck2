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

use buck2_event_log::read::EventLogPathBuf;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_log::utils::Invocation;
use buck2_events::BuckEvent;
use buck2_util::truncate::truncate;
use buck2_wrapper_common::invocation_id::TraceId;
use chrono::DateTime;
use chrono::Local;
use futures::TryStreamExt;
use humantime::format_duration;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum BuildInfoError {
    #[error("Failed to read event log")]
    EventLogReadFail,
}

struct LogInfo {
    revision: Option<String>,
    daemon_uptime_s: Option<u64>,
    timestamp_end: Option<SystemTime>,
    re_session_id: Option<String>,
}

pub(crate) struct BuildInfo {
    uuid: TraceId,
    pub timestamp: DateTime<Local>,
    pub command: String,
    working_dir: String,
    pub buck2_revision: String,
    pub command_duration: Option<Duration>,
    pub daemon_uptime_s: Option<u64>,
    pub re_session_id: Option<String>,
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
RE session id: {}
        ",
            self.uuid,
            self.timestamp.format("%c %Z"),
            self.command,
            self.working_dir,
            self.buck2_revision,
            seconds_to_string(self.command_duration.map(|d| d.as_secs())),
            seconds_to_string(self.daemon_uptime_s),
            self.re_session_id
                .as_ref()
                .map_or_else(|| "", |s| s.as_str()),
        )
    }
}

pub(crate) async fn get(log: &EventLogPathBuf) -> buck2_error::Result<BuildInfo> {
    let (invocation, events) = log.unpack_stream().await?;
    let mut filtered_events = events.try_filter_map(|log| {
        let maybe_buck_event = match log {
            StreamValue::Result(_) | StreamValue::PartialResult(_) => None,
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
        re_session_id: None,
    };
    loop {
        let res = match filtered_events.try_next().await {
            Ok(Some(event)) => extract_info(&mut info, event),
            Ok(None) => break,
            Err(e) => Err(e.into()),
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
        command: format_cmd(&invocation),
        working_dir: invocation.working_dir,
        buck2_revision: info.revision.unwrap_or_else(|| "".to_owned()),
        command_duration: duration,
        daemon_uptime_s: info.daemon_uptime_s,
        re_session_id: info.re_session_id,
    };

    Ok(output)
}

fn extract_info(info: &mut LogInfo, event: Box<buck2_data::BuckEvent>) -> buck2_error::Result<()> {
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
            Some(buck2_data::instant_event::Data::ReSession(session)) => {
                info.re_session_id.get_or_insert(session.session_id.clone());
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

pub fn format_cmd(cmd: &Invocation) -> String {
    truncate(&cmd.display_expanded_command_line(), 256)
}

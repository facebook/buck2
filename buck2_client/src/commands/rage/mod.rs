/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod rage_dumps;

use std::fmt;
use std::future::Future;
use std::process::Stdio;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::file_names::get_local_logs;
use buck2_client_ctx::subscribers::event_log::EventLogPathBuf;
use buck2_client_ctx::subscribers::event_log::EventLogSummary;
use buck2_data::RageInvoked;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use chrono::offset::Local;
use chrono::DateTime;
use futures::future::FutureExt;
use futures::future::LocalBoxFuture;
use futures::TryStreamExt;
use gazebo::prelude::*;
use humantime::format_duration;
use serde::Serialize;
use thiserror::Error;
use tokio::io::AsyncBufRead;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio::process::Command;

#[derive(Debug, Error)]
enum RageError {
    #[error("Failed to get a valid user selection")]
    InvalidSelectionError,
    #[error("Failed to find the logs for command")]
    LogNotFoundError,
    #[error("Pastry command timeout, make sure you are on Lighthouse/VPN")]
    PastryTimeout,
    #[error("Failed to spawn pastry")]
    PastrySpawnError,
    #[error("Error writing to pastry")]
    PastryWriteError,
    #[error("Error reading pastry output")]
    PastryOutputError,
    #[error("Pastry command failed with code '{0}' and error '{1}' ")]
    PastryCommandError(i32, String),
    #[error("HG snapshot command failed with code '{0}' and error '{1}' ")]
    SnapshotCommandError(i32, String),
    #[error("Failed to read event log")]
    EventLogReadError,
}

#[derive(Debug, PartialEq, Serialize)]
struct RageSection {
    title: String,
    status: CommandStatus,
}

#[derive(Debug, PartialEq, Serialize)]
enum CommandStatus {
    Success { output: String },
    Failure { error: String },
    Timeout,
}

impl RageSection {
    fn get<'a, Fut>(
        title: String,
        timeout: Duration,
        command: impl FnOnce() -> Fut,
    ) -> LocalBoxFuture<'a, Self>
    where
        Fut: Future<Output = anyhow::Result<String>> + 'a,
    {
        let fut = command();

        async move {
            match tokio::time::timeout(timeout, fut).await {
                Err(_) => RageSection {
                    title,
                    status: CommandStatus::Timeout,
                },
                Ok(Ok(output)) => RageSection {
                    title,
                    status: CommandStatus::Success { output },
                },
                Ok(Err(e)) => RageSection {
                    title,
                    status: CommandStatus::Failure {
                        error: format!("Error: {:?}", e),
                    },
                },
            }
        }
        .boxed_local()
    }

    fn pretty_print_section(
        &self,
        f: &mut fmt::Formatter,
        content: &str,
    ) -> Result<(), std::fmt::Error> {
        let content_divider = "-".repeat(30);
        write!(
            f,
            "{title}\n{content_divider}\n{content}\n\n\n",
            title = self.title
        )
    }
}

impl fmt::Display for RageSection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.status {
            CommandStatus::Success { output } => self.pretty_print_section(f, output),
            CommandStatus::Failure { error } => self.pretty_print_section(f, error),
            CommandStatus::Timeout {} => self.pretty_print_section(f, "Timeout"),
        }
    }
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "rage",
    about = "Record information about the previous failed buck2 command"
)]
pub struct RageCommand {
    /// Stop collecting information after <timeout> seconds
    #[clap(long, default_value = "60")]
    timeout: u64,
    /// Use value 0 to select last invocation, 1 to select second to last and so on
    #[clap(long)]
    invocation: Option<usize>,
    /// We may want to omit paste if this is not a user
    /// or is called in a machine with no pastry command
    #[clap(long)]
    no_paste: bool,
}

impl RageCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        buck2_core::facebook_only();

        ctx.with_runtime(async move |mut ctx| {
            let timeout = Duration::from_secs(self.timeout);
            let log_dir = ctx.paths.as_ref().map_err(|e| e.dupe())?.log_dir();
            let logs = get_local_logs(&log_dir)?
                .into_iter()
                .rev() // newest first
                .map(|log_path| EventLogPathBuf::infer(log_path.into_abs_path_buf()))
                .collect::<anyhow::Result<Vec<_>>>()?;

            if logs.is_empty() {
                buck2_client_ctx::eprintln!("No recent buck invocation to report")?;
                return ExitResult::failure();
            }

            // TODO iguridi: this fails when run right after `buck2 clean`
            let selected_log = match self.invocation {
                Some(i) => logs.get(i).ok_or_else(|| RageError::LogNotFoundError.into()),
                None => {
                    let mut stdin = BufReader::new(ctx.stdin());
                    user_prompt_select_log(&mut stdin, &logs).await
                },
            }?;

            let log_summary = selected_log.get_summary().await?;
            let old_trace_id = log_summary.trace_id;
            let new_trace_id = TraceId::new();

            dispatch_event_to_scribe(&ctx, &new_trace_id, &old_trace_id)?;

            buck2_client_ctx::eprintln!("Collection will terminate after {} seconds (override with --timeout param)", self.timeout)?;
            buck2_client_ctx::eprintln!("Collecting debug info...\n\n")?;

            let sections = vec![
                RageSection::get("System info".to_owned(), timeout, get_system_info),
                RageSection::get("Hg snapshot ID".to_owned(), timeout, get_hg_snapshot),
                RageSection::get("Build info".to_owned(), timeout, || {
                    get_build_info(selected_log)
                }),
                RageSection::get("Dice Dump".to_owned(), timeout, || {
                    rage_dumps::upload_dice_dump(&ctx, &new_trace_id, &old_trace_id)
                }),
            ];

            let sections = futures::future::join_all(sections).await;
            let output: Vec<String> = sections.iter().map(|i| i.to_string()).collect();
            let output = output.join("");

            if self.no_paste {
                buck2_client_ctx::println!("{}", output)?;
            } else {
                let paste = generate_paste("Buck2 Rage", &output).await?;
                buck2_client_ctx::eprintln!(
                    "\nPlease post in https://fb.workplace.com/groups/buck2users with the following link:\n\n{}\n",
                    paste
                )?;
            };

            ExitResult::success()
        })
    }
}

async fn get_system_info() -> anyhow::Result<String> {
    let info = metadata::system_info();
    let output = format!(
        "username: {}
hostname: {}
os: {}
os_version: {}
",
        info.username.unwrap_or_else(|| "".to_owned()),
        info.hostname.unwrap_or_else(|| "".to_owned()),
        info.os,
        info.os_version.unwrap_or_else(|| "".to_owned()),
    );
    Ok(output)
}

async fn get_hg_snapshot() -> anyhow::Result<String> {
    let result = Command::new("hg")
        .args(["snapshot", "create"])
        .env("HGPLAIN", "1")
        .output()
        .await?;

    if !result.status.success() {
        let error = String::from_utf8(result.stderr).context("hg snapshot stderr was not UTF-8")?;
        let code = result
            .status
            .code()
            .ok_or_else(|| RageError::SnapshotCommandError(1, error.clone()))?;
        return Err(RageError::SnapshotCommandError(code, error).into());
    }

    let output = String::from_utf8(result.stdout).context("hg snapshot stdout was not UTF-8")?;
    Ok(output)
}

async fn get_build_info(log: &EventLogPathBuf) -> anyhow::Result<String> {
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
        .ok_or(RageError::EventLogReadError)?
        .try_into()?;
    let mut revision = None;
    let mut daemon_uptime_s = None;
    let mut timestamp_end: Option<SystemTime> = None;
    while let Some(event) = filtered_events.try_next().await? {
        match event.data {
            Some(buck2_data::buck_event::Data::SpanStart(span)) => match &span.data {
                Some(buck2_data::span_start_event::Data::Command(action)) => {
                    if revision.is_none() && action.metadata.contains_key("buck2_revision") {
                        if let Some(buck2_revision) = action.metadata.get("buck2_revision") {
                            revision.get_or_insert(buck2_revision.clone());
                        }
                    }
                }
                _ => (),
            },
            Some(buck2_data::buck_event::Data::Instant(span)) => match &span.data {
                Some(buck2_data::instant_event::Data::Snapshot(snapshot)) => {
                    daemon_uptime_s.get_or_insert(snapshot.daemon_uptime_s);
                }
                _ => (),
            },

            _ => (),
        }
        if let Some(timestamp) = event.timestamp {
            timestamp_end = Some(SystemTime::try_from(timestamp.clone())?)
        };
    }

    let timestamp_start = first_event.timestamp();
    let duration = {
        if let Some(end) = timestamp_end {
            Some(end.duration_since(timestamp_start)?)
        } else {
            None
        }
    };

    let t_start: DateTime<Local> = timestamp_start.into();

    let output = format!(
        "buck2 UI: https://www.internalfb.com/buck2/{}
timestamp: {}
command: {}
working dir: {}
buck2_revision: {}
command duration: {}
daemon uptime: {}
",
        first_event.trace_id()?,
        t_start.format("%c %Z"),
        format_cmd(&invocation.command_line_args),
        invocation.working_dir,
        revision.unwrap_or_else(|| "".to_owned()),
        seconds_to_string(duration.map(|d| d.as_secs())),
        seconds_to_string(daemon_uptime_s),
    );

    Ok(output)
}

fn dispatch_event_to_scribe(
    ctx: &ClientCommandContext,
    new_trace_id: &TraceId,
    old_trace_id: &TraceId,
) -> anyhow::Result<()> {
    // dispatch event to scribe if possible
    match create_scribe_event_dispatcher(ctx, new_trace_id.to_owned())? {
        Some(dispatcher) => {
            let recent_command_trace_id = old_trace_id.to_string();
            let metadata = metadata::collect();
            let rage_invoked = RageInvoked {
                metadata,
                recent_command_trace_id,
            };
            dispatcher.instant_event(rage_invoked);
        }
        None => {}
    };
    Ok(())
}

#[allow(unused_variables)] // Conditional compilation
fn create_scribe_event_dispatcher(
    ctx: &ClientCommandContext,
    trace_id: TraceId,
) -> anyhow::Result<Option<EventDispatcher>> {
    // TODO(swgiillespie) scribe_logging is likely the right feature for this, but we should be able to inject a sink
    // without using configurations at the call site
    let sink = new_thrift_scribe_sink_if_enabled(
        ctx.fbinit(),
        /* buffer size */ 100,
        /* retry_backoff */ Duration::from_millis(500),
        /* retry_attempts */ 5,
    )?;
    Ok(sink.map(|sink| EventDispatcher::new(trace_id, sink)))
}

async fn user_prompt_select_log<'a>(
    stdin: impl AsyncBufRead + Unpin,
    logs: &'a [EventLogPathBuf],
) -> anyhow::Result<&EventLogPathBuf> {
    buck2_client_ctx::eprintln!("Which buck invocation would you like to report?\n")?;
    let logs_summary =
        futures::future::try_join_all(logs.iter().map(|log_path| log_path.get_summary())).await?;
    for (index, log_summary) in logs_summary.iter().enumerate() {
        print_log_summary(index, log_summary)?;
    }
    buck2_client_ctx::eprintln!()?;
    let prompt = format!(
        "Invocation: (type a number between 0 and {}) ",
        logs_summary.len() - 1
    );
    let selection = get_user_selection(stdin, &prompt, |i| i < logs_summary.len()).await?;

    let chosen_log = logs_summary.get(selection).expect("Selection out of range");

    let timestamp: DateTime<Local> = chosen_log.timestamp.into();
    buck2_client_ctx::eprintln!("Selected invocation at {}\n", timestamp.format("%c %Z"))?;
    logs.get(selection)
        .ok_or_else(|| RageError::LogNotFoundError.into())
}

async fn get_user_selection<P>(
    mut stdin: impl AsyncBufRead + Unpin,
    prompt: &str,
    predicate: P,
) -> anyhow::Result<usize>
where
    P: Fn(usize) -> bool,
{
    buck2_client_ctx::eprint!("{}", prompt)?;

    let mut input = String::new();
    stdin.read_line(&mut input).await?;

    match input.trim().parse() {
        Ok(selection) if predicate(selection) => Ok(selection),
        _ => Err(RageError::InvalidSelectionError.into()),
    }
}

fn print_log_summary(index: usize, log_summary: &EventLogSummary) -> anyhow::Result<()> {
    let cmd = format_cmd(&log_summary.invocation.command_line_args);

    let timestamp: DateTime<Local> = log_summary.timestamp.into();
    buck2_client_ctx::eprintln!(
        "{:<7} {}    {}",
        format!("[{}].", index),
        timestamp.format("%c %Z"),
        cmd
    )
}

fn format_cmd(cmd_args: &[String]) -> String {
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

async fn generate_paste(title: &str, content: &str) -> anyhow::Result<String> {
    let mut pastry = Command::new("pastry")
        .args(["--title", title])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .context(RageError::PastrySpawnError)?;
    let mut stdin = pastry.stdin.take().expect("Stdin should open");

    let writer = async move {
        stdin
            .write_all(content.as_bytes())
            .await
            .context(RageError::PastryWriteError)
    };

    let reader = async move {
        let output = tokio::time::timeout(Duration::from_secs(10), pastry.wait_with_output())
            .await
            .context(RageError::PastryTimeout)?
            .context(RageError::PastryOutputError)?;
        if !output.status.success() {
            let error = String::from_utf8_lossy(&output.stderr).to_string();
            let code = output
                .status
                .code()
                .ok_or_else(|| RageError::PastryCommandError(1, error.clone()))?;
            return Err(RageError::PastryCommandError(code, error).into());
        }
        let output = String::from_utf8(output.stdout).context(RageError::PastryOutputError)?;
        Ok(output)
    };

    let ((), paste) = futures::future::try_join(writer, reader).await?;

    Ok(paste)
}

fn seconds_to_string(seconds: Option<u64>) -> String {
    if let Some(seconds) = seconds {
        let duration = Duration::from_secs(seconds);
        format_duration(duration).to_string()
    } else {
        "".to_owned()
    }
}

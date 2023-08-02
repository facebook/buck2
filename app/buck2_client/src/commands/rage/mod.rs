/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod build_info;
mod dice;
mod manifold;
mod materializer;
mod source_control;
mod system_info;
mod thread_dump;

use std::collections::HashMap;
use std::fmt;
use std::future::Future;
use std::process::Stdio;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use buck2_client_ctx::argv::Argv;
use buck2_client_ctx::argv::SanitizedArgv;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::manifold::Bucket;
use buck2_client_ctx::manifold::ManifoldClient;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::subscribers::event_log::file_names::do_find_log_by_trace_id;
use buck2_client_ctx::subscribers::event_log::file_names::get_local_logs;
use buck2_client_ctx::subscribers::event_log::read::EventLogPathBuf;
use buck2_client_ctx::subscribers::event_log::read::EventLogSummary;
use buck2_common::result::ToSharedResultExt;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_data::instant_event::Data;
use buck2_data::InstantEvent;
use buck2_data::RageResult;
use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use buck2_events::sink::scribe::ThriftScribeSink;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use chrono::offset::Local;
use chrono::DateTime;
use derive_more::Display;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::future::LocalBoxFuture;
use manifold::file_to_manifold;
use manifold::manifold_url;
use maplit::convert_args;
use maplit::hashmap;
use serde::Serialize;
use thiserror::Error;
use tokio::io::AsyncBufRead;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio::process::Command;

use crate::commands::debug::upload_re_logs;

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
}

#[derive(Debug, PartialEq, Serialize)]
struct RageSection<T> {
    title: String,
    status: CommandStatus<T>,
}

#[derive(Display)]
pub enum MaterializerRageUploadData {
    #[display(fmt = "state")]
    State,
    #[display(fmt = "fsck")]
    Fsck,
}

#[derive(Debug, PartialEq, Serialize)]
enum CommandStatus<T> {
    Success { output: T },
    Failure { error: String },
    Timeout,
    Skipped,
}

impl<'a, T> RageSection<T>
where
    T: std::fmt::Display + 'a,
{
    fn get<Fut>(
        title: String,
        timeout: Duration,
        command: impl FnOnce() -> Fut,
    ) -> LocalBoxFuture<'a, Self>
    where
        Fut: Future<Output = anyhow::Result<T>> + 'a,
    {
        let fut = command();
        async move {
            let status = match tokio::time::timeout(timeout, fut).await {
                Err(_) => CommandStatus::Timeout,
                Ok(Ok(output)) => CommandStatus::Success { output },
                Ok(Err(e)) => CommandStatus::Failure {
                    error: format!("Error: {:?}", e),
                },
            };
            RageSection { title, status }
        }
        .boxed_local()
    }

    fn get_skipped(title: String) -> LocalBoxFuture<'a, Self> {
        let status = CommandStatus::Skipped;
        async { RageSection { title, status } }.boxed_local()
    }

    fn output(&self) -> String {
        match &self.status {
            CommandStatus::Success { output } => output.to_string(),
            CommandStatus::Failure { error } => error.to_owned(),
            CommandStatus::Timeout {} => "Timeout".to_owned(),
            CommandStatus::Skipped {} => "Skipped".to_owned(),
        }
    }

    fn get_field<D>(&self, extract_field: impl FnOnce(&T) -> Option<D>) -> Option<D> {
        match &self.status {
            CommandStatus::Success { output } => extract_field(output),
            _ => None,
        }
    }

    fn pretty_print_section(
        &self,
        f: &mut fmt::Formatter,
        content: String,
    ) -> Result<(), std::fmt::Error> {
        let content_divider = "-".repeat(30);
        write!(
            f,
            "{title}\n{content_divider}\n{content}\n\n\n",
            title = self.title
        )
    }
}

impl<T> fmt::Display for RageSection<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pretty_print_section(f, self.output())
    }
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "rage",
    about = "Record information about the previous failed buck2 command",
    group = clap::ArgGroup::new("invocation").multiple(false)
)]
pub struct RageCommand {
    /// Stop collecting information after `<timeout>` seconds
    #[clap(long, default_value = "60")]
    timeout: u64,
    /// Use value 0 to select last invocation, 1 to select second to last and so on
    #[clap(long, group = "invocation")]
    invocation_offset: Option<usize>,
    /// Select invocation directly using the invocation's UUID
    #[clap(long, group = "invocation")]
    invocation_id: Option<TraceId>,
    /// Collect rage report about buck2 in general, not about specific invocation
    #[clap(long, group = "invocation")]
    no_invocation: bool,
    /// We may want to omit paste if this is not a user
    /// or is called in a machine with no pastry command
    #[clap(long)]
    no_paste: bool,
}

impl RageCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        buck2_core::facebook_only();

        ctx.with_runtime(async move |mut ctx| {
            let timeout = Duration::from_secs(self.timeout);
            let paths = ctx.paths.as_ref().map_err(|e| e.dupe())?;
            let daemon_dir = paths.daemon_dir()?;
            let stderr_path = daemon_dir.buckd_stderr();
            let re_logs_dir = ctx.paths()?.re_logs_dir();
            let logdir = paths.log_dir();
            let dice_dump_dir = paths.dice_dump_dir();

            let client_ctx = ctx.empty_client_context("rage")?;

            // Don't fail the rage if you can't figure out whether to do vpnless.
            let manifold =
                ManifoldClient::new(ctx.allow_vpnless_for_logging().unwrap_or_default())?;

            let rage_id = TraceId::new();
            let mut manifold_id = format!("{}", rage_id);
            let sink = create_scribe_sink(&ctx)?;

            buck2_client_ctx::eprintln!(
                "Data collection will terminate after {} seconds (override with --timeout param)",
                self.timeout
            )?;

            // If there is a daemon, start connecting.
            let info = BuckdProcessInfo::load(&daemon_dir).shared_error();

            let buckd = match &info {
                Ok(info) => async {
                    info.create_channel()
                        .await
                        .shared_error()?
                        .upgrade()
                        .await
                        .shared_error()
                }
                .boxed(),
                Err(e) => futures::future::ready(Err(e.dupe())).boxed(),
            }
            .shared();

            let selected_invocation = maybe_select_invocation(ctx.stdin(), &logdir, &self).await?;
            let invocation_id = get_trace_id(&selected_invocation).await?;
            if let Some(ref invocation_id) = invocation_id {
                manifold_id = format!("{}_{}", invocation_id, manifold_id);
            }

            buck2_client_ctx::eprintln!("Collecting debug info...")?;

            let thread_dump = {
                let title = "Thread dump".to_owned();
                RageSection::get(title, timeout, || {
                    thread_dump::upload_thread_dump(&info, &manifold, &manifold_id)
                })
            };
            let build_info_command = {
                let title = "Associated invocation info".to_owned();
                match selected_invocation.as_ref() {
                    None => RageSection::get_skipped(title),
                    Some(invocation) => {
                        RageSection::get(title, timeout, || build_info::get(invocation))
                    }
                }
            };

            let (thread_dump, build_info) = tokio::join!(
                // Get thread dump before making any new connections to daemon (T159606309)
                thread_dump,
                // We need the RE session ID from here to upload RE logs
                build_info_command
            );

            let system_info_command =
                RageSection::get("System info".to_owned(), timeout, system_info::get);
            let daemon_stderr_command =
                RageSection::get("Daemon stderr".to_owned(), timeout, || {
                    upload_daemon_stderr(stderr_path, &manifold, &manifold_id)
                });
            let hg_snapshot_id_command = RageSection::get(
                "Source control".to_owned(),
                timeout,
                source_control::get_info,
            );
            let dice_dump_command = RageSection::get("Dice dump".to_owned(), timeout, || async {
                dice::upload_dice_dump(buckd.clone().await?, dice_dump_dir, &manifold, &manifold_id)
                    .await
            });
            let materializer_state =
                RageSection::get("Materializer state".to_owned(), timeout, || {
                    materializer::upload_materializer_data(
                        buckd.clone(),
                        &client_ctx,
                        &manifold,
                        &manifold_id,
                        MaterializerRageUploadData::State,
                    )
                });
            let materializer_fsck =
                RageSection::get("Materializer fsck".to_owned(), timeout, || {
                    materializer::upload_materializer_data(
                        buckd.clone(),
                        &client_ctx,
                        &manifold,
                        &manifold_id,
                        MaterializerRageUploadData::Fsck,
                    )
                });
            let event_log_command = {
                let title = "Event log upload".to_owned();
                match selected_invocation.as_ref() {
                    None => RageSection::get_skipped(title),
                    Some(path) => RageSection::get(title, timeout, || {
                        upload_event_logs(path, &manifold, &manifold_id)
                    }),
                }
            };
            let re_logs_command = {
                let title = "RE logs upload".to_owned();
                let re_session_id = build_info.get_field(|o| o.re_session_id.clone());
                match re_session_id {
                    None => RageSection::get_skipped(title),
                    Some(re_session_id) => RageSection::get(title, timeout, || {
                        upload_re_logs_impl(&manifold, &re_logs_dir, re_session_id)
                    }),
                }
            };

            let (
                system_info,
                daemon_stderr_dump,
                hg_snapshot_id,
                dice_dump,
                materializer_state,
                materializer_fsck,
                event_log_dump,
                re_logs,
            ) = tokio::join!(
                system_info_command,
                daemon_stderr_command,
                hg_snapshot_id_command,
                dice_dump_command,
                materializer_state,
                materializer_fsck,
                event_log_command,
                re_logs_command
            );
            let sections = vec![
                system_info.to_string(),
                daemon_stderr_dump.to_string(),
                hg_snapshot_id.to_string(),
                dice_dump.to_string(),
                materializer_state.to_string(),
                materializer_fsck.to_string(),
                thread_dump.to_string(),
                build_info.to_string(),
                event_log_dump.to_string(),
                re_logs.to_string(),
            ];
            output_rage(self.no_paste, &sections.join("")).await?;

            self.send_to_scuba(
                &rage_id,
                sink,
                invocation_id,
                system_info,
                daemon_stderr_dump,
                hg_snapshot_id,
                dice_dump,
                materializer_state,
                materializer_fsck,
                thread_dump,
                event_log_dump,
                build_info,
                re_logs,
            )
            .await?;
            ExitResult::success()
        })
    }

    async fn send_to_scuba(
        &self,
        rage_id: &TraceId,
        sink: Option<ThriftScribeSink>,
        invocation_id: Option<TraceId>,
        system_info: RageSection<system_info::SystemInfo>,
        daemon_stderr_dump: RageSection<String>,
        hg_snapshot_id: RageSection<String>,
        dice_dump: RageSection<String>,
        materializer_state: RageSection<String>,
        materializer_fsck: RageSection<String>,
        thread_dump: RageSection<String>,
        event_log_dump: RageSection<String>,
        build_info: RageSection<build_info::BuildInfo>,
        re_logs: RageSection<String>,
    ) -> anyhow::Result<()> {
        let mut string_data = convert_args!(
            keys = String::from,
            hashmap! (
                "dice_dump" => dice_dump.output(),
                "materializer_state" => materializer_state.output(),
                "materializer_fsck" => materializer_fsck.output(),
                "thread_dump" => thread_dump.output(),
                "daemon_stderr_dump" => daemon_stderr_dump.output(),
                "hg_snapshot_id" => hg_snapshot_id.output(),
                "invocation_id" => invocation_id.map(|inv| inv.to_string()).unwrap_or_default(),
                "event_log_dump" => event_log_dump.output(),
                "re_logs" => re_logs.output(),
            )
        );

        let command = build_info.get_field(|o| Some(o.command.to_owned()));
        let buck2_revision = build_info.get_field(|o| Some(o.buck2_revision.to_owned()));
        let username = system_info.get_field(|o| o.username.to_owned());
        let hostname = system_info.get_field(|o| o.hostname.to_owned());
        let os = system_info.get_field(|o| Some(o.os.to_owned()));
        let os_version = system_info.get_field(|o| o.os_version.to_owned());

        insert_if_some(&mut string_data, "command", command);
        insert_if_some(&mut string_data, "buck2_revision", buck2_revision);
        insert_if_some(&mut string_data, "username", username);
        insert_if_some(&mut string_data, "hostname", hostname);
        insert_if_some(&mut string_data, "os", os);
        insert_if_some(&mut string_data, "os_version", os_version);

        let mut int_data = HashMap::new();
        let daemon_uptime_s = build_info.get_field(|o| o.daemon_uptime_s);
        insert_if_some(&mut int_data, "daemon_uptime_s", daemon_uptime_s);

        let timestamp = build_info.get_field(|o| Some(SystemTime::from(o.timestamp).into()));
        let command_duration = build_info.get_field(|o| {
            Some(prost_types::Duration {
                seconds: o.command_duration?.as_secs() as i64,
                nanos: o.command_duration?.subsec_nanos() as i32,
            })
        });

        dispatch_result_event(
            sink.as_ref(),
            rage_id,
            RageResult {
                string_data,
                int_data,
                timestamp,
                command_duration,
            },
        )
        .await
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}

fn insert_if_some<D>(data: &mut HashMap<String, D>, key: &str, value: Option<D>) {
    if let Some(value) = value {
        data.insert(key.to_owned(), value);
    }
}

async fn upload_daemon_stderr(
    path: AbsNormPathBuf,
    manifold: &ManifoldClient,
    manifold_id: &str,
) -> anyhow::Result<String> {
    file_to_manifold(manifold, &path, format!("flat/{}.stderr", manifold_id)).await
}

async fn upload_event_logs(
    path: &EventLogPathBuf,
    manifold: &ManifoldClient,
    manifold_id: &str,
) -> anyhow::Result<String> {
    let filename = format!("flat/{}-event_log{}", manifold_id, path.extension());
    file_to_manifold(manifold, path.path(), filename).await
}

async fn upload_re_logs_impl(
    manifold: &ManifoldClient,
    re_logs_dir: &AbsNormPath,
    re_session_id: String,
) -> anyhow::Result<String> {
    let bucket = Bucket::RAGE_DUMPS;
    let filename = format!("flat/{}-re_logs.zst", &re_session_id);
    upload_re_logs::upload_re_logs(manifold, bucket, re_logs_dir, &re_session_id, &filename)
        .await?;

    Ok(manifold_url(&bucket, filename))
}

async fn dispatch_result_event(
    sink: Option<&ThriftScribeSink>,
    rage_id: &TraceId,
    result: RageResult,
) -> anyhow::Result<()> {
    let data = Some(Data::RageResult(result));
    dispatch_event_to_scribe(sink, rage_id, InstantEvent { data }).await?;
    Ok(())
}

async fn dispatch_event_to_scribe(
    sink: Option<&ThriftScribeSink>,
    trace_id: &TraceId,
    event: InstantEvent,
) -> anyhow::Result<()> {
    if let Some(sink) = sink {
        sink.send_now(BuckEvent::new(
            SystemTime::now(),
            trace_id.to_owned(),
            None,
            None,
            event.into(),
        ))
        .await;
    } else {
        tracing::warn!(
            "Couldn't send rage results to scribe, rage ID `{}`",
            trace_id
        )
    };
    Ok(())
}

#[allow(unused_variables)] // Conditional compilation
fn create_scribe_sink(ctx: &ClientCommandContext) -> anyhow::Result<Option<ThriftScribeSink>> {
    // TODO(swgiillespie) scribe_logging is likely the right feature for this, but we should be able to inject a sink
    // without using configurations at the call site
    new_thrift_scribe_sink_if_enabled(
        ctx.fbinit(),
        /* buffer size */ 100,
        /* retry_backoff */ Duration::from_millis(500),
        /* retry_attempts */ 5,
        /* message_batch_size */ None,
    )
}

async fn maybe_select_invocation(
    stdin: &mut Stdin,
    logdir: &AbsNormPathBuf,
    command: &RageCommand,
) -> anyhow::Result<Option<EventLogPathBuf>> {
    if command.no_invocation {
        return Ok(None);
    };

    if let Some(trace_id) = &command.invocation_id {
        return Ok(Some(do_find_log_by_trace_id(logdir, trace_id)?));
    }

    let logs = get_local_logs(logdir)?;
    let mut logs = logs
        .into_iter()
        .rev() // newest first
        .collect::<Vec<_>>();
    if logs.is_empty() {
        return Ok(None);
    }
    let index = log_index(stdin, &logs, command.invocation_offset).await?;
    if index >= logs.len() {
        return Err(RageError::LogNotFoundError.into());
    }
    Ok(Some(logs.swap_remove(index)))
}

async fn log_index(
    stdin: &mut Stdin,
    logs: &[EventLogPathBuf],
    invocation_offset: Option<usize>,
) -> Result<usize, anyhow::Error> {
    let index = match invocation_offset {
        Some(i) => i,
        None => {
            let mut stdin = BufReader::new(stdin);
            user_prompt_select_log(&mut stdin, logs).await?
        }
    };
    Ok(index)
}

async fn user_prompt_select_log<'a>(
    stdin: impl AsyncBufRead + Unpin,
    logs: &'a [EventLogPathBuf],
) -> anyhow::Result<usize> {
    buck2_client_ctx::eprintln!("Which buck invocation would you like to report?\n")?;
    let logs_summary = futures::future::join_all(
        logs.iter()
            .map(async move |log_path| log_path.get_summary().await.ok()),
    )
    .await;
    for (index, log_summary) in logs_summary.iter().enumerate() {
        print_log_summary(index, log_summary)?;
    }
    buck2_client_ctx::eprintln!()?;
    let prompt = format!(
        "Invocation: (type a number between 0 and {}) ",
        logs_summary.len() - 1
    );
    let selection = get_user_selection(stdin, &prompt, |i| i < logs_summary.len()).await?;

    buck2_client_ctx::eprintln!("Selected invocation {}\n", selection)?;
    Ok(selection)
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

fn print_log_summary(index: usize, log_summary: &Option<EventLogSummary>) -> anyhow::Result<()> {
    if let Some(log_summary) = log_summary {
        let cmd = build_info::format_cmd(&log_summary.invocation);

        let timestamp: DateTime<Local> = log_summary.timestamp.into();
        buck2_client_ctx::eprintln!(
            "{:<7} {}    {}",
            format!("[{}].", index),
            timestamp.format("%c %Z"),
            cmd
        )
    } else {
        buck2_client_ctx::eprintln!(
            "{:<7} <<Unable to display information>>",
            format!("[{}].", index),
        )
    }
}

async fn output_rage(no_paste: bool, output: &str) -> anyhow::Result<()> {
    if no_paste {
        buck2_client_ctx::println!("{}", output)?;
    } else {
        let paste = generate_paste("Buck2 Rage", output).await?;
        buck2_client_ctx::eprintln!(
            "\nPlease post in https://fb.workplace.com/groups/buck2users with the following link:\n\n{}\n",
            paste
        )?;
    };
    Ok(())
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

async fn get_trace_id(invocation: &Option<EventLogPathBuf>) -> anyhow::Result<Option<TraceId>> {
    let invocation_id = match invocation {
        None => None,
        Some(invocation) => Some(invocation.uuid_from_filename()?),
    };
    Ok(invocation_id)
}

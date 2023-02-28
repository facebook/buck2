/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod build_info;
mod rage_dumps;
mod source_control;

use std::fmt;
use std::future::Future;
use std::process::Stdio;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::manifold;
use buck2_client_ctx::manifold::UploadError;
use buck2_client_ctx::stdin::Stdin;
use buck2_client_ctx::subscribers::event_log::file_names::get_local_logs_if_exist;
use buck2_client_ctx::subscribers::event_log::EventLogPathBuf;
use buck2_client_ctx::subscribers::event_log::EventLogSummary;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_data::instant_event::Data;
use buck2_data::InstantEvent;
use buck2_data::RageInvoked;
use buck2_data::RageResult;
use buck2_events::metadata;
use buck2_events::sink::scribe::new_thrift_scribe_sink_if_enabled;
use buck2_events::sink::scribe::ThriftScribeSink;
use buck2_events::trace::TraceId;
use buck2_events::BuckEvent;
use chrono::offset::Local;
use chrono::DateTime;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::future::LocalBoxFuture;
use maplit::convert_args;
use maplit::hashmap;
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
    #[error("Failed to open file `{0}`")]
    OpenFileError(String),
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
    Skipped,
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

    fn get_skipped<'a>(title: String) -> LocalBoxFuture<'a, Self> {
        let status = CommandStatus::Skipped;
        async { RageSection { title, status } }.boxed_local()
    }

    fn output(&self) -> &str {
        match &self.status {
            CommandStatus::Success { output } => output,
            CommandStatus::Failure { error } => error,
            CommandStatus::Timeout {} => "Timeout",
            CommandStatus::Skipped {} => "Skipped",
        }
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
        self.pretty_print_section(f, self.output())
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[derive(clap::ArgEnum)]
enum Origin {
    HangDetector,
    Unspecified,
}

impl fmt::Display for Origin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "rage",
    about = "Record information about the previous failed buck2 command"
)]
pub struct RageCommand {
    /// Stop collecting information after `<timeout>` seconds
    #[clap(long, default_value = "60")]
    timeout: u64,
    /// Use value 0 to select last invocation, 1 to select second to last and so on
    #[clap(long)]
    invocation: Option<usize>,
    /// We may want to omit paste if this is not a user
    /// or is called in a machine with no pastry command
    #[clap(long)]
    no_paste: bool,
    /// Where buck2 rage is being called from
    #[clap(long, arg_enum, default_value_t = Origin::Unspecified)]
    origin: Origin,
}

impl RageCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        buck2_core::facebook_only();

        ctx.with_runtime(async move |mut ctx| {
            let timeout = Duration::from_secs(self.timeout);
            let paths = ctx.paths.as_ref().map_err(|e| e.dupe())?;
            let stderr_path = paths.daemon_dir()?.buckd_stderr();
            let logdir = paths.log_dir();

            let rage_id = TraceId::new();
            let mut manifold_id = format!("{}", rage_id);
            let sink = create_scribe_sink(&ctx)?;

            buck2_client_ctx::eprintln!(
                "Collection will terminate after {} seconds (override with --timeout param)",
                self.timeout
            )?;
            buck2_client_ctx::eprintln!("Collecting debug info...\n\n")?;

            let selected_invocation =
                maybe_select_invocation(ctx.stdin(), &logdir, self.invocation).await?;
            let invocation_id = get_trace_id(&selected_invocation).await?;
            dispatch_invoked_event(sink.as_ref(), &rage_id, invocation_id.as_ref()).await?;
            if let Some(ref invocation_id) = invocation_id {
                manifold_id = format!("{}_{}", invocation_id, manifold_id);
            }

            let system_info_command =
                RageSection::get("System info".to_owned(), timeout, get_system_info);
            let daemon_stderr_command =
                RageSection::get("Daemon stderr".to_owned(), timeout, || {
                    upload_daemon_stderr(stderr_path, &manifold_id)
                });
            let hg_snapshot_id_command = RageSection::get(
                "Source control".to_owned(),
                timeout,
                source_control::get_info,
            );
            let dice_dump_command = RageSection::get("Dice Dump".to_owned(), timeout, || {
                rage_dumps::upload_dice_dump(&ctx, &manifold_id)
            });
            let build_info_command = {
                let title = "Associated invocation info".to_owned();
                match selected_invocation.as_ref() {
                    None => RageSection::get_skipped(title),
                    Some(invocation) => {
                        RageSection::get(title, timeout, || build_info::get(invocation))
                    }
                }
            };

            let event_log_command = {
                let title = "Event log upload".to_owned();
                match selected_invocation.as_ref() {
                    None => RageSection::get_skipped(title),
                    Some(path) => {
                        RageSection::get(title, timeout, || upload_event_logs(path, &manifold_id))
                    }
                }
            };

            let (
                system_info,
                daemon_stderr_dump,
                hg_snapshot_id,
                dice_dump,
                build_info,
                event_log_dump,
            ) = tokio::join!(
                system_info_command,
                daemon_stderr_command,
                hg_snapshot_id_command,
                dice_dump_command,
                build_info_command,
                event_log_command,
            );
            let sections = vec![
                &system_info,
                &daemon_stderr_dump,
                &hg_snapshot_id,
                &dice_dump,
                &build_info,
                &event_log_dump,
            ];
            let sections: Vec<String> = sections.iter().map(|i| i.to_string()).collect();
            output_rage(self.no_paste, &sections.join("")).await?;

            let string_data = convert_args!(
                keys = String::from,
                hashmap! (
                    "dice_dump" => dice_dump.output().to_owned(),
                    "daemon_stderr_dump" => daemon_stderr_dump.output().to_owned(),
                    "system_info" => system_info.output().to_owned(),
                    "hg_snapshot_id" => hg_snapshot_id.output().to_owned(),
                    "invocation_id" => invocation_id.map(|inv| inv.to_string()).unwrap_or_default(),
                    "origin" => self.origin.to_string(),
                    "event_log_dump" => event_log_dump.output().to_owned(),
                )
            );
            dispatch_result_event(sink.as_ref(), &rage_id, RageResult { string_data }).await?;
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

async fn upload_daemon_stderr(
    path: AbsNormPathBuf,
    manifold_id: &String,
) -> anyhow::Result<String> {
    // can't use async_fs_util
    // the trait to convert from tokio::fs::File is not implemented for Stdio
    let upload_log_file: Stdio = std::fs::File::open(&path)
        .context(RageError::OpenFileError(path.display().to_string()))?
        .into();
    let filename = format!("{}.stderr", manifold_id);
    let mut upload = manifold::upload_command(manifold::Bucket::RageDumps, &filename)?
        .context(UploadError::CommandNotFound)?;
    upload.stdin(upload_log_file);
    match upload
        .spawn()?
        .wait()
        .await?
        .code()
        .context(UploadError::NoResultCodeError(path.display().to_string()))?
    {
        0 => Ok::<(), anyhow::Error>(()),
        e => Err(UploadError::ExitCodeError(path.display().to_string(), e).into()),
    }?;
    Ok(format!("buck2_rage_dumps/flat/{}", filename))
}

async fn upload_event_logs(path: &EventLogPathBuf, manifold_id: &str) -> anyhow::Result<String> {
    let filename = format!("{}-event_log{}", manifold_id, path.extension());
    let bucket = manifold::Bucket::RageDumps;
    manifold::Upload::new(bucket, path.path(), &filename)?
        .spawn(None)
        .await?;
    Ok(format!("{}/flat/{}", bucket.info().name, filename))
}

async fn dispatch_invoked_event(
    sink: Option<&ThriftScribeSink>,
    rage_id: &TraceId,
    trace_id: Option<&TraceId>,
) -> anyhow::Result<()> {
    let recent_command_trace_id = trace_id.map(|x| x.to_string());
    let metadata = metadata::collect();
    let data = Some(Data::RageInvoked(RageInvoked {
        metadata,
        recent_command_trace_id,
    }));
    dispatch_event_to_scribe(sink, rage_id, InstantEvent { data }).await?;
    Ok(())
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
    invocation: Option<usize>,
) -> anyhow::Result<Option<EventLogPathBuf>> {
    let mut logs = match get_local_logs_if_exist(logdir)? {
        None => return Ok(None),
        Some(logs) => logs
            .into_iter()
            .rev() // newest first
            .map(|log_path| EventLogPathBuf::infer(log_path.into_abs_path_buf()))
            .collect::<anyhow::Result<Vec<_>>>()?,
    };
    if logs.is_empty() {
        return Ok(None);
    }
    let index = match invocation {
        Some(i) => i,
        None => {
            let mut stdin = BufReader::new(stdin);
            user_prompt_select_log(&mut stdin, &logs).await?
        }
    };
    if index >= logs.len() {
        return Err(RageError::LogNotFoundError.into());
    }
    Ok(Some(logs.swap_remove(index)))
}

async fn user_prompt_select_log<'a>(
    stdin: impl AsyncBufRead + Unpin,
    logs: &'a [EventLogPathBuf],
) -> anyhow::Result<usize> {
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

fn print_log_summary(index: usize, log_summary: &EventLogSummary) -> anyhow::Result<()> {
    let cmd = build_info::format_cmd(&log_summary.invocation.command_line_args);

    let timestamp: DateTime<Local> = log_summary.timestamp.into();
    buck2_client_ctx::eprintln!(
        "{:<7} {}    {}",
        format!("[{}].", index),
        timestamp.format("%c %Z"),
        cmd
    )
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
        Some(invocation) => Some(invocation.get_summary().await?.trace_id),
    };
    Ok(invocation_id)
}

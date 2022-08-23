/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::needless_return)] // FIXME?

use anyhow::Context;
use async_trait::async_trait;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::daemon::client::BuckdConnectOptions;
use buck2_client::exit_result::ExitResult;
use buck2_client::find_certs::find_tls_cert;
use buck2_client::subscribers::event_log::get_local_logs;
use buck2_client::subscribers::event_log::log_upload_url;
use buck2_client::subscribers::event_log::EventLogPathBuf;
use buck2_client::subscribers::event_log::EventLogSummary;
use buck2_core::fs::anyhow::create_dir_all;
use buck2_core::fs::anyhow::remove_dir_all;
use buck2_core::process::async_background_command;
use buck2_core::process::background_command;
use buck2_data::RageInvoked;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_events::TraceId;
use chrono::offset::Local;
use chrono::DateTime;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use cli_proto::UnstableDiceDumpRequest;
use futures::stream::FuturesOrdered;
use futures::FutureExt;
use futures::TryStreamExt;
use thiserror::Error;

use crate::client_command_context::ClientCommandContext;
use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::common::ConsoleType;
use crate::Path;
use crate::StreamingCommand;

#[derive(Debug, Error)]
enum ManifoldUploadError {
    #[error("Failed to upload dice dump folder `{0}` to manifold with exit code {1}")]
    ManifoldUploadWithExitCodeError(String, i32),
    #[error("Failed to upload dice dump folder `{0}` to manifold due to signal interrupt")]
    ManifoldUploadSignalInterruptError(String),
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "rage",
    about = "Record information about the previous failed buck2 command"
)]
pub(crate) struct RageCommand {}

#[async_trait]
impl StreamingCommand for RageCommand {
    const COMMAND_NAME: &'static str = "rage";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b ClientCommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> ExitResult {
        let log_dir = ctx.paths()?.log_dir();
        let logs_summary = get_local_logs(&log_dir)?
            .into_iter()
            .rev() // newest first
            .map(|log| log_dir.as_path().join(log.path()))
            .map(|log_path| async move {
                let log_path = EventLogPathBuf::infer(log_path.to_path_buf())?;
                log_path.get_summary().await
            })
            .collect::<FuturesOrdered<_>>()
            .try_collect::<Vec<EventLogSummary>>()
            .await?;

        if logs_summary.is_empty() {
            buck2_client::eprintln!("No recent buck invocation to report")?;
            return ExitResult::failure();
        }
        let chosen_log = user_prompt_select_log(&logs_summary).await?;
        let old_trace_id = &chosen_log.trace_id;
        let new_trace_id = TraceId::new();

        // dispatch event to scribe if possible
        match create_scribe_event_dispatcher(&ctx, new_trace_id.to_owned()) {
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
        }

        let dice_dump_folder_name = format!("{:?}", chrono::Utc::now());
        let dice_dump_folder = ctx.paths()?.dice_dump_dir();

        create_dir_all(&dice_dump_folder).with_context(|| {
            format!(
                "Failed to create directory {:?}, no dice dump will be created",
                &dice_dump_folder
            )
        })?;

        let this_dice_dump_folder = dice_dump_folder
            .as_path()
            .join(Path::new(&dice_dump_folder_name));

        buck2_client::eprintln!("Dumping Buck2 internal state...")?;

        buckd
            .with_flushing(|client| {
                client
                    .unstable_dice_dump(UnstableDiceDumpRequest {
                        destination_path: this_dice_dump_folder.to_str().unwrap().to_owned(),
                        format: DiceDumpFormat::Tsv.into(),
                    })
                    .boxed()
            })
            .await?
            .with_context(|| {
                format!(
                    "Dice Dump at {:?} failed to complete",
                    this_dice_dump_folder,
                )
            })?;

        // create dice dump name using the old command being rage on and the trace id of this rage command.
        let filename = format!("{}_{}_dice-dump.gz", old_trace_id, new_trace_id,);
        buck2_client::eprintln!(
            "Compressed internal state file being uploaded to manifold as {}...",
            &filename
        )?;
        dice_dump_upload(&this_dice_dump_folder, &filename)
            .await
            .with_context(|| "Failed during manifold upload!")?;

        remove_dir_all(&this_dice_dump_folder).with_context(|| {
            format!(
                "Failed to remove Buck2 internal state folder at {:?}. Please remove this manually as it could be quite large.",
                this_dice_dump_folder
            )
        })?;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}

#[allow(unused_variables)] // Conditional compilation
fn create_scribe_event_dispatcher(
    ctx: &ClientCommandContext,
    trace_id: TraceId,
) -> Option<EventDispatcher> {
    // TODO(swgiillespie) scribe_logging is likely the right feature for this, but we should be able to inject a sink
    // without using configurations at the call site
    #[cfg(fbcode_build)]
    {
        use buck2_events::sink::scribe;
        if scribe::is_enabled() {
            Some(EventDispatcher::new(
                trace_id,
                scribe::ThriftScribeSink::new(
                    ctx.fbinit(),
                    "buck2_events".to_owned(),
                    /* buffer size */ 100,
                )
                .ok()?,
            ))
        } else {
            None
        }
    }

    #[cfg(not(fbcode_build))]
    None
}

async fn dice_dump_upload(dice_dump_folder_to_upload: &Path, filename: &str) -> anyhow::Result<()> {
    if !cfg!(target_os = "windows") {
        buck2_core::facebook_only();
        let manifold_url = match log_upload_url() {
            None => return Ok(()),
            Some(x) => x,
        };

        let cert = find_tls_cert()?;

        let tar_gzip = background_command("tar")
            .arg("-c")
            .arg(dice_dump_folder_to_upload)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()?;

        let mut upload = async_background_command("curl");
        upload.args([
                "--fail",
                "-X",
                "PUT",
                "--data-binary",
                "@-",
                &format!("{}/v0/write/flat/{}?bucketName=buck2_logs&apiKey=buck2_logs-key&timeoutMsec=300000", manifold_url, filename),
                "-E",
        ]);
        upload.arg(cert);
        upload.stdin(tar_gzip.stdout.unwrap());
        let exit_code_result = upload.spawn()?.wait().await?.code();

        match exit_code_result {
            Some(code) => match code {
                0 => {}
                e => {
                    return Err(anyhow::anyhow!(
                        ManifoldUploadError::ManifoldUploadWithExitCodeError(
                            dice_dump_folder_to_upload.display().to_string(),
                            e
                        )
                    ));
                }
            },
            None => {
                return Err(anyhow::anyhow!(
                    ManifoldUploadError::ManifoldUploadSignalInterruptError(
                        dice_dump_folder_to_upload.display().to_string()
                    )
                ));
            }
        }
    }
    Ok(())
}

async fn user_prompt_select_log(
    logs_summary: &[EventLogSummary],
) -> anyhow::Result<&EventLogSummary> {
    buck2_client::eprintln!("Which buck invocation would you like to report?\n")?;
    for (index, log_summary) in logs_summary.iter().enumerate() {
        print_log_summary(index, log_summary)?;
    }
    buck2_client::eprintln!()?;
    let prompt = format!(
        "Invocation: (type a number between 0 and {}) ",
        logs_summary.len() - 1
    );
    let selection = get_user_selection(&prompt, |i| i < logs_summary.len()).await?;

    let chosen_log = logs_summary.get(selection).expect("Selection out of range");

    let timestamp: DateTime<Local> = chosen_log.timestamp.into();
    buck2_client::eprintln!("Selected invocation at {}\n", timestamp.format("%c %Z"))?;

    Ok(chosen_log)
}

async fn get_user_selection<P>(prompt: &str, predicate: P) -> anyhow::Result<usize>
where
    P: Fn(usize) -> bool,
{
    buck2_client::eprint!("{}", prompt)?;
    // For interactive uses, it is recommended to spawn a thread dedicated to user input and
    // use blocking IO directly in that thread, instead of using tokio::io::Stdin directly
    // https://docs.rs/tokio/latest/tokio/io/struct.Stdin.html
    let input = tokio::task::spawn_blocking(|| {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(bytes_read) if bytes_read > 0 => Ok(input),
            _ => return Err(anyhow::anyhow!("Fail to get a valid selection")),
        }
    })
    .await??;
    match input.trim().parse() {
        Ok(selection) if predicate(selection) => Ok(selection),
        _ => return Err(anyhow::anyhow!("Fail to get a valid selection")),
    }
}

fn print_log_summary(index: usize, log_summary: &EventLogSummary) -> anyhow::Result<()> {
    let cmd: String;
    if log_summary.invocation.command_line_args.is_empty() {
        cmd = "???".to_owned();
    } else {
        let mut program_name: &str = &log_summary.invocation.command_line_args[0];
        let program_args = &log_summary.invocation.command_line_args[1..];
        if program_name.ends_with("fbcode/buck2/.buck2") {
            program_name = "buck2";
        }
        cmd = format!("{} {}", program_name, program_args.join(" "));
    }

    let timestamp: DateTime<Local> = log_summary.timestamp.into();
    buck2_client::eprintln!(
        "{:<7} {}    {}",
        format!("[{}].", index),
        timestamp.format("%c %Z"),
        cmd
    )
}

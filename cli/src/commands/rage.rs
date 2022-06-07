/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::{
    exit_result::ExitResult,
    fs::anyhow::{create_dir_all, remove_dir_all},
};
use buck2_data::RageInvoked;
use cli_proto::{unstable_dice_dump_request::DiceDumpFormat, UnstableDiceDumpRequest};
use events::{dispatch::EventDispatcher, TraceId};
use futures::{FutureExt, TryStreamExt};
use structopt::{clap, StructOpt};
use thiserror::Error;
use tokio::process::Command;

use crate::{
    commands::{
        common::{
            subscribers::event_log::{log_upload_url, EventLogPathBuf},
            CommonConsoleOptions, CommonEventLogOptions, ConsoleType,
        },
        debug::replay::retrieve_nth_recent_log,
    },
    daemon::client::{BuckdClientConnector, StreamValue},
    metadata, BuckdConnectOptions, CommandContext, Path, StreamingCommand,
};

#[derive(Debug, Error)]
enum RageError {
    #[error("Reached End of File before reading BuckEvent in log `{0}`")]
    EndOfFile(String),
}

#[derive(Debug, Error)]
enum ManifoldUploadError {
    #[error("Failed to upload dice dump folder `{0}` to manifold with exit code {1}")]
    ManifoldUploadWithExitCodeError(String, i32),
    #[error("Failed to upload dice dump folder `{0}` to manifold due to signal interrupt")]
    ManifoldUploadSignalInterruptError(String),
}

#[derive(Debug, StructOpt)]
#[structopt(
    name = "rage",
    about = "Record information about the previous failed buck2 command"
)]
pub struct RageCommand {}

#[async_trait]
impl StreamingCommand for RageCommand {
    const COMMAND_NAME: &'static str = "rage";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let log_path = retrieve_nth_recent_log(&ctx, 0)?;
        crate::eprintln!("most recent log: {:?}", log_path)?;
        let trace_id_str = get_trace_id_from_log(&log_path).await?;
        let old_trace_id = TraceId::from_str(trace_id_str.as_str())?;
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

        let this_dice_dump_folder = dice_dump_folder.join(Path::new(&dice_dump_folder_name));

        crate::eprintln!("Dumping Buck2 internal state...")?;

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
        crate::eprintln!(
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

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }
}

#[allow(unused_variables)] // Conditional compilation
fn create_scribe_event_dispatcher(
    ctx: &CommandContext,
    trace_id: TraceId,
) -> Option<EventDispatcher> {
    // TODO(swgiillespie) scribe_logging is likely the right feature for this, but we should be able to inject a sink
    // without using configurations at the call site
    #[cfg(fbcode_build)]
    {
        use events::sink::scribe;
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

        let tar_gzip = std::process::Command::new("tar")
            .arg("-c")
            .arg(dice_dump_folder_to_upload)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null())
            .spawn()?;
        let exit_code_result = Command::new("curl")
            .args([
                "--fail",
                "-X",
                "PUT",
                "--data-binary",
                "@-",
                &format!("{}/v0/write/flat/{}?bucketName=buck2_logs&apiKey=buck2_logs-key&timeoutMsec=300000", manifold_url, filename),
                "-E",
                &format!("/var/facebook/credentials/{0}/x509/{0}.pem", std::env::var("USER")?),
            ])
            .stdin(tar_gzip.stdout.unwrap())
            .spawn()?.wait().await?.code();
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

async fn get_trace_id_from_log(log: &Path) -> anyhow::Result<String> {
    let log_path = EventLogPathBuf::infer(log.to_path_buf())?;
    let (_, mut events) = log_path.unpack_stream().await?;
    while let Some(log) = events.try_next().await? {
        match log {
            StreamValue::Result(_) => {}
            StreamValue::Event(buck_event) => return Ok(buck_event.trace_id),
        }
    }
    Err(anyhow::anyhow!(RageError::EndOfFile(
        log.to_str().unwrap().to_owned()
    )))
}

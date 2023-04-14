/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::Stdio;

use anyhow::Context;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::subscribers::event_log::file_names::find_log_by_trace_id;
use buck2_client_ctx::subscribers::event_log::file_names::retrieve_nth_recent_log;
use buck2_client_ctx::subscribers::event_log::read::EventLogPathBuf;
use buck2_client_ctx::subscribers::event_log::utils::Encoding;
use buck2_common::temp_path::TempPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_util::process::async_background_command;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use rand::Rng;

#[derive(Debug, thiserror::Error)]
enum EventLogOptionsError {
    #[error("Manifold failed; stderr:\n{}", "indent(&_0)")]
    ManifoldFailed(String),
    #[error(
        "Log not found locally by trace id `{0}`; try `--allow-remote` to download from manifold"
    )]
    LogNotFoundLocally(TraceId),
}

#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::with_name("event_log"))]
pub(crate) struct EventLogOptions {
    /// Open the event-log file from a recent command.
    #[clap(long, group = "event_log", value_name = "NUMBER")]
    recent: Option<usize>,

    /// Show log by trace id.
    #[clap(long, group = "event_log", value_name = "ID")]
    trace_id: Option<TraceId>,

    /// Allow downloading the log from manifold if it's not found locally.
    #[clap(long, requires = "trace-id")]
    allow_remote: bool,

    /// A path to an event-log file to read from.
    #[clap(group = "event_log", value_name = "PATH")]
    path: Option<PathArg>,
}

impl EventLogOptions {
    pub(crate) async fn get(&self, ctx: &ClientCommandContext) -> anyhow::Result<EventLogPathBuf> {
        let path = if let Some(path) = &self.path {
            path.resolve(&ctx.working_dir)
        } else if let Some(id) = &self.trace_id {
            if let Some(log_path) = find_log_by_trace_id(&ctx.paths()?.log_dir(), id)? {
                log_path.into_abs_path_buf()
            } else if self.allow_remote {
                self.download_remote_id(id, ctx).await?
            } else {
                return Err(EventLogOptionsError::LogNotFoundLocally(id.dupe()).into());
            }
        } else {
            retrieve_nth_recent_log(ctx, self.recent.unwrap_or(0))?.into_abs_path_buf()
        };
        EventLogPathBuf::infer(path)
    }

    fn random_string() -> String {
        let mut s = String::with_capacity(10);
        for _ in 0..10 {
            s.push(rand::thread_rng().gen_range('a'..='z'));
        }
        s
    }

    async fn download_remote_id(
        &self,
        trace_id: &TraceId,
        ctx: &ClientCommandContext,
    ) -> anyhow::Result<AbsPathBuf> {
        let manifold_file_name = FileNameBuf::try_from(format!(
            "{}{}",
            trace_id,
            // TODO(nga): hardcoded default, should at least use the same default buck2 uses,
            //   or better enumerate all the possible suffixes.
            Encoding::PROTO_ZSTD.extensions[0]
        ))?;

        let log_path = ctx
            .paths()?
            .log_dir()
            .join(FileName::new(&format!("dl-{}", manifold_file_name))?);

        if fs_util::try_exists(&log_path)? {
            return Ok(log_path.into_abs_path_buf());
        }

        let temp_path = ctx.paths()?.tmp_dir().join(FileName::new(&format!(
            "dl.{}.{}.tmp",
            manifold_file_name,
            Self::random_string()
        ))?);

        // Delete the file on failure.
        let temp_path = TempPath::new_path(temp_path);

        let args = [
            "get",
            &format!("buck2_logs/flat/{}", manifold_file_name),
            temp_path
                .path()
                .as_os_str()
                .to_str()
                .context("temp_path is not valid UTF-8")?,
        ];
        buck2_client_ctx::eprintln!("Spawning: manifold {}", args.join(" "))?;
        let command = async_background_command("manifold")
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()?;

        // No timeout here, just press Ctrl-C if you want it to cancel.
        let result = command.wait_with_output().await?;
        if !result.status.success() {
            return Err(EventLogOptionsError::ManifoldFailed(
                String::from_utf8_lossy(&result.stderr).into_owned(),
            )
            .into());
        }

        fs_util::rename(temp_path.path(), &log_path)?;
        buck2_client_ctx::eprintln!("Downloaded event-log to `{}`", log_path.display())?;

        temp_path.close()?;

        Ok(log_path.into_abs_path_buf())
    }
}

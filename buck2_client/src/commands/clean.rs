/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use anyhow::Context;
use buck2_common::daemon_dir::DaemonDir;
use buck2_core::fs::fs_util;
use buck2_core::fs::fs_util::remove_dir_all;
use buck2_core::fs::paths::AbsPathBuf;
use gazebo::prelude::SliceExt;
use gazebo::prelude::*;
use threadpool::ThreadPool;
use walkdir::WalkDir;

use crate::client_ctx::ClientCommandContext;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::connect::BuckdConnectOptions;
use crate::daemon::client::BuckdLifecycleLock;
use crate::final_console::FinalConsole;
#[derive(Debug, clap::Parser)]
#[clap(about = "Delete generated files and caches")]
pub struct CleanCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(
        long = "dry-run",
        help = "Performs a dry-run and prints the paths that would be removed."
    )]
    dry_run: bool,
}

impl CleanCommand {
    pub fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> anyhow::Result<()> {
        ctx.with_runtime(async move |ctx| {
            let buck_out_dir = ctx.paths()?.buck_out_path();
            let daemon_dir = ctx.paths()?.daemon_dir()?;
            let console = &self.console_opts.final_console();

            if self.dry_run {
                return clean(buck_out_dir, daemon_dir, self.dry_run, console).await;
            }

            // Kill the daemon and make sure a new daemon does not spin up while we're performing clean up operations
            // This will ensure we have exclusive access to the directories in question
            let _lifecycle_lock = BuckdLifecycleLock::lock_with_timeout(
                ctx.paths()?.daemon_dir()?,
                Duration::from_secs(10),
            )
            .await
            .with_context(|| "when locking buckd lifecycle.lock")?;
            if let Ok(mut buckd) = ctx
                .connect_buckd(BuckdConnectOptions::existing_only())
                .await
            {
                buckd
                    .with_flushing()
                    .kill("Killing daemon to safely clean")
                    .await?;
            }
            clean(buck_out_dir, daemon_dir, self.dry_run, console).await
        })
    }
}

async fn clean(
    buck_out_dir: AbsPathBuf,
    daemon_dir: DaemonDir,
    dry_run: bool,
    console: &FinalConsole,
) -> anyhow::Result<()> {
    let mut paths_to_clean = Vec::new();
    // Try to clean EdenFS based buck-out first. For EdenFS based buck-out, "eden rm"
    // is effecient. Notice eden rm will remove the buck-out root direcotry,
    // but for the native fs, the buck-out root directory is kept.
    if let Some(paths) = try_clean_eden_buck_out(&buck_out_dir, dry_run).await? {
        paths_to_clean = paths;
    } else if buck_out_dir.exists() {
        paths_to_clean =
            collect_paths_to_clean(&buck_out_dir)?.map(|path| path.display().to_string());
        if !dry_run {
            tokio::task::spawn_blocking(move || clean_buck_out(&buck_out_dir))
                .await?
                .context("Failed to spawn clean")?;
        }
    }

    if daemon_dir.path.exists() {
        paths_to_clean.push(daemon_dir.to_string());
        if !dry_run {
            remove_dir_all(&daemon_dir.path)?;
        }
    }

    for path in paths_to_clean {
        console.print_stderr(&path)?;
    }
    Ok(())
}

fn collect_paths_to_clean(buck_out_path: &AbsPathBuf) -> anyhow::Result<Vec<AbsPathBuf>> {
    let mut paths_to_clean = vec![];
    let dir = fs_util::read_dir(buck_out_path)?;
    for entry in dir {
        let entry = entry?;
        let path = entry.path();
        paths_to_clean.push(path);
    }

    Ok(paths_to_clean)
}

fn clean_buck_out(path: &AbsPathBuf) -> anyhow::Result<()> {
    let walk = WalkDir::new(path);
    let thread_pool = ThreadPool::new(num_cpus::get());
    let error = Arc::new(Mutex::new(None));
    // collect dir paths to delete them after deleting files in them
    // we need reverse order to make sure the dir is already empty when
    // we delete it, otherwise remove would fail with DirNotEmpty exception
    let mut reverse_dir_paths = Vec::new();
    for dir_entry in walk.into_iter().flatten() {
        if dir_entry.file_type().is_dir() {
            reverse_dir_paths.push(dir_entry.into_path());
        } else {
            let error = error.dupe();
            thread_pool.execute(move || match fs_util::remove_file(dir_entry.path()) {
                Ok(_) => {}
                Err(e) => {
                    let mut error = error.lock().unwrap();
                    if error.is_none() {
                        *error = Some(e);
                    }
                }
            })
        }
    }

    thread_pool.join();
    if let Some(e) = error.lock().unwrap().take() {
        return Err(e);
    }

    // first entry is buck-out root dir and we don't want to remove it
    for path in reverse_dir_paths.iter().skip(1).rev() {
        fs_util::remove_dir(path)?;
    }

    Ok(())
}

#[cfg(off)] // @oss-enable
async fn try_clean_eden_buck_out(
    buck_out: &AbsPathBuf,
    dryrun: bool,
) -> anyhow::Result<Option<Vec<String>>> {
    use std::process::Stdio;

    use buck2_core::process::async_background_command;
    use buck2_execute::materialize::eden_api::is_recas_eden_mount;

    if !cfg!(unix) {
        return Ok(None);
    }

    if !is_recas_eden_mount(buck_out)? {
        return Ok(None);
    }

    // Run eden rm <buck-out> to rm a mount
    let mut eden_rm_cmd = async_background_command("eden");
    eden_rm_cmd
        .arg("rm")
        .arg("-y") // No promot
        .arg(buck_out.as_os_str())
        .current_dir("/")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    crate::eprintln!(
        "The following command will be executed: `eden rm -y {}`",
        buck_out
    )?;

    if !dryrun {
        eden_rm_cmd
            .spawn()
            .context("Failed to start to remove EdenFS buck-out mount.")?
            .wait()
            .await
            .context("Failed to remove EdenFS buck-out mount.")?;

        // eden rm might not delete the buck-out completed.
        if buck_out.exists() {
            fs_util::remove_dir(buck_out)?;
        }
    }

    Ok(Some(vec![buck_out.display().to_string()]))
}

// @oss-disable: #[cfg(off)]
async fn try_clean_eden_buck_out(
    _buck_out: &AbsPathBuf,
    _dryrun: bool,
) -> anyhow::Result<Option<Vec<String>>> {
    Ok(None)
}

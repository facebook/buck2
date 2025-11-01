/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgUnusedOptions;
use buck2_client_ctx::daemon::client::BuckdLifecycleLock;
use buck2_client_ctx::daemon::client::kill::kill_command_impl;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::final_console::FinalConsole;
use buck2_client_ctx::startup_deadline::StartupDeadline;
use buck2_common::daemon_dir::DaemonDir;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use threadpool::ThreadPool;
use uuid::Uuid;
use walkdir::WalkDir;

use crate::commands::clean_stale::CleanStaleCommand;
use crate::commands::clean_stale::parse_clean_stale_args;

/// Delete generated files and caches.
///
/// The command also kills the buck2 daemon.
#[derive(Debug, clap::Parser)]
pub struct CleanCommand {
    #[clap(
        long = "dry-run",
        help = "Performs a dry-run and prints the paths that would be removed."
    )]
    dry_run: bool,

    #[clap(
        long = "stale",
        help = "Delete artifacts from buck-out older than 1 week or older than
the specified duration, without killing the daemon",
        value_name = "DURATION"
    )]
    stale: Option<Option<humantime::Duration>>,

    // Like stale but since a specific timestamp, for testing
    #[clap(long = "keep-since-time", conflicts_with = "stale", hide = true)]
    keep_since_time: Option<i64>,

    /// Only considers tracked artifacts for cleanup.
    ///
    /// `buck-out` can contain untracked artifacts for different reasons:
    ///  - Outputs from aborted actions
    ///  - State getting deleted (e.g., new buckversion that changes the on-disk state format)
    ///  - Writing to `buck-out` without being expected by Buck
    #[clap(long = "tracked-only", requires = "stale")]
    tracked_only: bool,

    #[clap(
        long = "background",
        help = "Run the clean operation in the background"
    )]
    background: bool,

    /// Command doesn't need these flags, but they are used in mode files, so we need to keep them.
    #[clap(flatten)]
    _target_cfg: TargetCfgUnusedOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

impl CleanCommand {
    pub fn exec(
        self,
        matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        if let Some(keep_since_arg) = parse_clean_stale_args(self.stale, self.keep_since_time)? {
            let cmd = CleanStaleCommand {
                common_opts: self.common_opts,
                keep_since_arg,
                dry_run: self.dry_run,
                tracked_only: self.tracked_only,
            };
            ctx.exec(cmd, matches, events_ctx)
        } else {
            ctx.exec(
                InnerCleanCommand {
                    dry_run: self.dry_run,
                    background: self.background,
                    common_opts: self.common_opts,
                },
                matches,
                events_ctx,
            )
        }
    }

    pub fn command_name(&self) -> &'static str {
        if parse_clean_stale_args(self.stale, self.keep_since_time)
            .ok()
            .is_some()
        {
            "clean-stale"
        } else {
            "clean"
        }
    }
}

struct InnerCleanCommand {
    dry_run: bool,
    background: bool,
    common_opts: CommonCommandOptions,
}

impl BuckSubcommand for InnerCleanCommand {
    const COMMAND_NAME: &'static str = "clean";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut buck2_client_ctx::events_ctx::EventsCtx,
    ) -> ExitResult {
        let paths = ctx.paths()?;
        let buck_out_dir = paths.buck_out_path();
        let daemon_dir = paths.daemon_dir()?;
        let trash_dir = paths.trash_dir();
        let console = &self.common_opts.console_opts.final_console();

        if self.dry_run {
            return clean(
                buck_out_dir,
                daemon_dir,
                trash_dir,
                console,
                None,
                self.background,
            )
            .await
            .into();
        }

        // Kill the daemon and make sure a new daemon does not spin up while we're performing clean up operations
        // This will ensure we have exclusive access to the directories in question
        let lifecycle_lock = BuckdLifecycleLock::lock_with_timeout(
            daemon_dir.clone(),
            StartupDeadline::duration_from_now(Duration::from_secs(10))?,
        )
        .await
        .with_buck_error_context(|| "Error locking buckd lifecycle.lock")?;

        kill_command_impl(&lifecycle_lock, "`buck2 clean` was invoked").await?;

        clean(
            buck_out_dir,
            daemon_dir,
            trash_dir,
            console,
            Some(&lifecycle_lock),
            self.background,
        )
        .await
        .into()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.common_opts.event_log_opts
    }
}

async fn clean(
    buck_out_dir: AbsNormPathBuf,
    daemon_dir: DaemonDir,
    trash_dir: AbsNormPathBuf,
    console: &FinalConsole,
    // None means "dry run".
    lifecycle_lock: Option<&BuckdLifecycleLock>,
    background: bool,
) -> buck2_error::Result<()> {
    if background {
        let trash_uuid = Uuid::new_v4();
        let trash_target = trash_dir.as_abs_path().join(trash_uuid.to_string());

        // Create trash directory if it doesn't exist
        if !trash_dir.exists() {
            fs_util::create_dir_all(&trash_dir)?;
        }

        // Move buck-out to trash folder
        if buck_out_dir.exists() {
            console.print_stderr(&format!(
                "Moving {} to {}",
                buck_out_dir.display(),
                trash_target.display()
            ))?;
            fs_util::rename(&buck_out_dir, &trash_target)?;
        }

        // Clean the daemon_dir first
        let mut paths_to_clean = Vec::new();
        if daemon_dir.path.exists() {
            paths_to_clean.push(daemon_dir.to_string());
            if let Some(lifecycle_lock) = lifecycle_lock {
                lifecycle_lock.clean_daemon_dir(false)?;
            }
        }

        console.print_stderr("Buck-out moved to trash. Now cleaning up...")?;
        console.print_stderr(
            "Tip: Use Ctrl-Z to put this in the background, or run in a new terminal.",
        )?;
        console.print_stderr("You can run other buck2 commands while this completes.")?;

        // Delete the moved directory
        let trash_target_normalized = AbsNormPathBuf::new(trash_target.to_path_buf())?;
        if trash_target_normalized.exists() {
            paths_to_clean.extend(
                collect_paths_to_clean(&trash_target_normalized)?
                    .map(|path| path.display().to_string()),
            );
            tokio::task::spawn_blocking(move || {
                clean_buck_out_with_retry(&trash_target_normalized)
            })
            .await?
            .buck_error_context("Failed to spawn clean")?;
        }
        for path in paths_to_clean {
            console.print_stderr(&path)?;
        }
    } else {
        let mut paths_to_clean = Vec::new();

        if buck_out_dir.exists() {
            paths_to_clean =
                collect_paths_to_clean(&buck_out_dir)?.map(|path| path.display().to_string());
            if lifecycle_lock.is_some() {
                tokio::task::spawn_blocking(move || clean_buck_out_with_retry(&buck_out_dir))
                    .await?
                    .buck_error_context("Failed to spawn clean")?;
            }
        }

        if daemon_dir.path.exists() {
            paths_to_clean.push(daemon_dir.to_string());
            if let Some(lifecycle_lock) = lifecycle_lock {
                lifecycle_lock.clean_daemon_dir(false)?;
            }
        }

        for path in paths_to_clean {
            console.print_stderr(&path)?;
        }
    }

    Ok(())
}

fn collect_paths_to_clean(
    buck_out_path: &AbsNormPathBuf,
) -> buck2_error::Result<Vec<AbsNormPathBuf>> {
    let mut paths_to_clean = vec![];
    let dir = fs_util::read_dir(buck_out_path)?;
    for entry in dir {
        let entry = entry?;
        let path = entry.path();
        paths_to_clean.push(path);
    }

    Ok(paths_to_clean)
}

/// In Windows, we've observed the buck-out clean immediately after killing
/// the daemon can fail with this error: `The process cannot access the
/// file because it is being used by another process.`. To get around this,
/// add a single retry.
fn clean_buck_out_with_retry(path: &AbsNormPathBuf) -> buck2_error::Result<()> {
    let mut result = clean_buck_out(path);
    match result {
        Ok(_) => {
            return result;
        }
        Err(e) => {
            tracing::info!(
                "Retrying buck-out clean, first attempted failed with: {:#}",
                e
            );
            result = clean_buck_out(path);
        }
    }
    result
}

fn clean_buck_out(path: &AbsNormPathBuf) -> buck2_error::Result<()> {
    let walk = WalkDir::new(path);
    let thread_pool = ThreadPool::new(buck2_util::threads::available_parallelism());
    let error = Arc::new(Mutex::new(None));
    for dir_entry in walk.into_iter().flatten() {
        let file_type = dir_entry.file_type();
        // As in the daemon, heavily parallel writes to directories in btrfs perform really poorly,
        // so we only parallelize file deletions and do the rest synchronously.
        //
        // FIXME(JakobDegen): The parallelism cap for file deletions in the daemon is much smaller
        // than it is here. Change that or write a comment justifying it.
        if !file_type.is_dir() && !file_type.is_symlink() {
            let error = error.dupe();
            thread_pool.execute(move || {
                // The wlak gives us back absolute paths since we give it absolute paths.
                let res = AbsPath::new(dir_entry.path())
                    .and_then(|p| fs_util::remove_file(p).map_err(Into::into));

                match res {
                    Ok(_) => {}
                    Err(e) => {
                        let mut error = error.lock().unwrap();
                        if error.is_none() {
                            *error = Some(e);
                        }
                    }
                }
            })
        }
    }

    thread_pool.join();
    if let Some(e) = error.lock().unwrap().take() {
        return Err(e.into());
    }

    // Buck's cwd is typically the directory that is passed in here, which means that on Windows we
    // often fail to delete this if we don't clean up all our child processes. Leaving zombies
    // around isn't great though...
    let dir = fs_util::read_dir(path)?;
    for entry in dir {
        let entry = entry?;
        let path = entry.path();
        fs_util::remove_dir_all(path)?;
    }
    Ok(())
}

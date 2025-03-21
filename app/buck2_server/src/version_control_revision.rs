/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::process::Output;
use std::process::Stdio;

use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::EventDispatcher;
use buck2_util::process::async_background_command;
use tokio::io::AsyncReadExt;
use tokio::process::Child;
use tokio::sync::OnceCell;

/// Spawn tasks to collect version control information
/// and return a droppable handle that will cancel them on drop.
pub(crate) fn spawn_version_control_collector(dispatch: EventDispatcher) -> AbortOnDropHandle {
    AbortOnDropHandle {
        handle: tokio::spawn(async move {
            let event = create_revision_data().await;
            dispatch.instant_event(event);
        }),
    }
}

/// Abort the underlying task on drop.
pub(crate) struct AbortOnDropHandle {
    pub handle: tokio::task::JoinHandle<()>,
}

impl Drop for AbortOnDropHandle {
    fn drop(&mut self) {
        self.handle.abort();
    }
}

#[derive(Clone, Copy, Debug)]
enum RepoVcs {
    Hg,
    Git,
    Unknown,
}

/// A wrapper over a child process that will reap the child process on drop.
/// On Unix platforms, a child process becomes a zombie until it is reaped by its parent.
struct ProperlyReapedChild {
    child: Option<Child>,
}

impl ProperlyReapedChild {
    async fn output(mut self) -> buck2_error::Result<Output> {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();
        let mut child = mem::take(&mut self.child).internal_error("child field must be set")?;
        let mut stdout_pipe = child
            .stdout
            .take()
            .buck_error_context("stdout is not piped")?;
        let mut stderr_pipe = child
            .stderr
            .take()
            .buck_error_context("stderr is not piped")?;
        let (stdout_error, stderr_error, status) = tokio::join!(
            stdout_pipe.read_to_end(&mut stdout),
            stderr_pipe.read_to_end(&mut stderr),
            child.wait(),
        );

        let result = match stdout_error.is_ok() || stderr_error.is_ok() {
            true => Ok(Output {
                status: status?,
                stdout,
                stderr,
            }),
            false => Err(internal_error!("Failed to read stdout and stderr")),
        };
        reap_child(child);
        result
    }
}

impl Drop for ProperlyReapedChild {
    fn drop(&mut self) {
        if let Some(child) = mem::take(&mut self.child) {
            reap_child(child);
        }
    }
}

fn reap_on_drop_command(command: &str, args: &[&str]) -> buck2_error::Result<ProperlyReapedChild> {
    async_background_command(command)
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .map(|child| ProperlyReapedChild { child: Some(child) })
        .map_err(|e| e.into())
}

fn reap_child(mut child: Child) {
    tokio::spawn(async move {
        if let Some(child_id) = child.id() {
            // If a child process has already exited, the child.id() is None.
            tracing::warn!("Killed child process: {:?}", child_id);
        }
        drop(child.kill().await);
    });
}

async fn create_revision_data() -> buck2_data::VersionControlRevision {
    let mut revision = buck2_data::VersionControlRevision::default();
    match repo_type().await {
        Ok(repo_vcs) => {
            match repo_vcs {
                RepoVcs::Hg => {
                    if let Err(e) = add_hg_data(&mut revision).await {
                        revision.command_error = Some(e.to_string());
                    }
                }
                RepoVcs::Git => {
                    // TODO(rajneeshl): Implement the git data
                }
                RepoVcs::Unknown => {
                    revision.command_error = Some("Unknown repository type".to_owned());
                }
            }
        }
        Err(e) => {
            revision.command_error = Some(e.to_string());
        }
    }
    revision
}

async fn add_hg_data(revision: &mut buck2_data::VersionControlRevision) -> buck2_error::Result<()> {
    // We fire 2 hg command in parallel:
    //  The `hg whereami` returns the full hash of the revision
    //  The `hg status` returns if there are any local changes
    let whereami_command = reap_on_drop_command("hg", &["whereami"])?;
    let status_command = reap_on_drop_command("hg", &["status"])?;

    let (whereami_output, status_output) =
        tokio::join!(whereami_command.output(), status_command.output());

    match whereami_output {
        Ok(result) => {
            if !result.status.success() {
                revision.command_error = Some(format!(
                    "Command `hg whereami` failed with error code {}; stderr:\n{}",
                    result.status,
                    std::str::from_utf8(&result.stderr)?
                ));
                return Ok(());
            }
            let stdout = std::str::from_utf8(&result.stdout)?.trim();
            if stdout.len() == 40 {
                revision.hg_revision = Some(stdout.to_owned());
            } else {
                revision.command_error = Some(format!("Unexpected revision : {}", stdout));
            }
        }
        Err(e) => {
            revision.command_error =
                Some(format!("Command `hg whereami` failed with error: {:?}", e));
        }
    }

    match status_output {
        Ok(result) => {
            if !result.status.success() {
                revision.command_error = Some(format!(
                    "Command `hg status` failed with error code {}; stderr:\n{}",
                    result.status,
                    std::str::from_utf8(&result.stderr)?
                ));
                return Ok(());
            }
            revision.has_local_changes =
                Some(!std::str::from_utf8(&result.stdout)?.trim().is_empty());
            return Ok(());
        }
        Err(e) => {
            revision.command_error =
                Some(format!("Command `hg status` failed with error: {:?}", e));
        }
    };
    Ok(())
}

async fn repo_type() -> buck2_error::Result<&'static RepoVcs> {
    static REPO_TYPE: OnceCell<buck2_error::Result<RepoVcs>> = OnceCell::const_new();
    async fn repo_type_impl() -> buck2_error::Result<RepoVcs> {
        let (hg_output, git_output) = tokio::join!(
            reap_on_drop_command("hg", &["root"])?.output(),
            reap_on_drop_command("git", &["rev-parse", "--is-inside-work-tree"])?.output()
        );

        let is_hg = hg_output.map_or(false, |output| {
            std::str::from_utf8(&output.stdout).map_or(false, |s| !s.trim().is_empty())
        });
        let is_git = git_output.map_or(false, |output| {
            std::str::from_utf8(&output.stdout).map_or(false, |s| s.trim() == "true")
        });

        if is_hg {
            Ok(RepoVcs::Hg)
        } else if is_git {
            Ok(RepoVcs::Git)
        } else {
            Ok(RepoVcs::Unknown)
        }
    }
    REPO_TYPE
        .get_or_init(repo_type_impl)
        .await
        .as_ref()
        .map_err(|e| e.clone())
}

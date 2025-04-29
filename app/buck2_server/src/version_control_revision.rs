/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::async_fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::soft_error;
use buck2_data::VersionControlRevision;
use buck2_events::dispatch::EventDispatcher;
use buck2_util::properly_reaped_child::reap_on_drop_command;
use futures::future::BoxFuture;
use futures::stream::FuturesUnordered;
use tokio::sync::OnceCell;
use tokio_stream::StreamExt;

/// Spawn tasks to collect version control information
/// and return a droppable handle that will cancel them on drop.
pub(crate) fn spawn_version_control_collector(
    dispatch: EventDispatcher,
    repo_root: AbsNormPathBuf,
) -> AbortOnDropHandle {
    let handle = tokio::spawn(async move {
        let mut tasks = FuturesUnordered::<BoxFuture<VersionControlRevision>>::new();

        tasks.push(Box::pin(create_revision_data(
            &repo_root,
            RevisionDataType::CurrentRevision,
        )));
        tasks.push(Box::pin(create_revision_data(
            &repo_root,
            RevisionDataType::Status,
        )));

        while let Some(event) = tasks.next().await {
            if let Some(error) = &event.command_error {
                soft_error!(
                    "spawn_version_control_collector_failed",
                    buck2_error::buck2_error!(buck2_error::ErrorTag::Input, "{}", error),
                    quiet: true
                )
                .ok();
            }

            dispatch.instant_event(event);
        }
    });

    AbortOnDropHandle { handle }
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

#[derive(Clone, Copy, Debug)]
enum RevisionDataType {
    CurrentRevision,
    Status,
}

async fn create_revision_data(
    repo_root: &AbsNormPathBuf,
    revision_type: RevisionDataType,
) -> buck2_data::VersionControlRevision {
    let mut revision = buck2_data::VersionControlRevision::default();
    match repo_type(repo_root).await {
        Ok(repo_vcs) => {
            match repo_vcs {
                RepoVcs::Hg => create_hg_data(&mut revision, revision_type).await,
                RepoVcs::Git => {
                    // TODO(rajneeshl): Implement the git data
                    // Add a message for now so we can actually tell if revision is null due to git
                    revision.command_error = Some("Git revision data not implemented".to_owned());
                }
                RepoVcs::Unknown => {
                    revision.command_error = Some("Unknown repository type".to_owned());
                }
            }
        }
        Err(e) => {
            revision.command_error = Some(format!("Failed to get repository type: {:#}", e));
        }
    }
    revision
}

async fn create_hg_data(
    revision: &mut buck2_data::VersionControlRevision,
    revision_type: RevisionDataType,
) {
    match revision_type {
        RevisionDataType::CurrentRevision => add_hg_whereami(revision).await,
        RevisionDataType::Status => add_hg_status(revision).await,
    }
}

async fn add_hg_whereami(revision: &mut buck2_data::VersionControlRevision) {
    // `hg whereami` returns the full hash of the revision
    let whereami_output = match reap_on_drop_command("hg", &["whereami"], Some(&[("HGPLAIN", "1")]))
    {
        Ok(command) => command.output().await,
        Err(e) => {
            revision.command_error = Some(format!(
                "reap_on_drop_command for `hg whereami` failed\n {:#}",
                e
            ));
            return;
        }
    };

    match whereami_output {
        Ok(result) => {
            if !result.status.success() {
                let stderr = match std::str::from_utf8(&result.stderr) {
                    Ok(s) => s,
                    Err(e) => {
                        revision.command_error =
                            Some(format!("hg whereami stderr is not utf8: {}", e));
                        return;
                    }
                };

                revision.command_error = Some(format!(
                    "Command `hg whereami` failed with error code {}; stderr:\n{}",
                    result.status, stderr
                ));
                return;
            }

            let stdout = match std::str::from_utf8(&result.stdout) {
                Ok(s) => s.trim(),
                Err(e) => {
                    revision.command_error = Some(format!("hg whereami stdout is not utf8: {}", e));
                    return;
                }
            };
            // whereami will sometimes return multiple revisions (Possibly due to merge state not handled well)
            // This is not a common pattern (less than 1%) and the last revision should be accurate enough
            // `hg log -r . -T '{node}'`` handles this properly but it's ~40% slower, we should switch if that becomes more performant
            let last_line = stdout.split('\n').next_back().unwrap_or(stdout);
            if last_line.len() == 40 {
                revision.hg_revision = Some(last_line.to_owned());
            } else {
                revision.command_error = Some(format!("Unexpected revision: {}", stdout));
            }
        }
        Err(e) => {
            revision.command_error =
                Some(format!("Command `hg whereami` failed with error: {:?}", e));
        }
    };
}

async fn add_hg_status(revision: &mut buck2_data::VersionControlRevision) {
    // `hg status` returns if there are any local changes
    let status_output = match reap_on_drop_command("hg", &["status"], Some(&[("HGPLAIN", "1")])) {
        Ok(command) => command.output().await,
        Err(e) => {
            revision.command_error = Some(format!(
                "reap_on_drop_command for `hg status` failed\n {}",
                e
            ));
            return;
        }
    };

    match status_output {
        Ok(result) => {
            if !result.status.success() {
                let stderr = match std::str::from_utf8(&result.stderr) {
                    Ok(s) => s,
                    Err(e) => {
                        revision.command_error =
                            Some(format!("hg status stderr is not utf8: {}", e));
                        return;
                    }
                };
                revision.command_error = Some(format!(
                    "Command `hg status` failed with error code {}; stderr:\n{}",
                    result.status, stderr
                ));
                return;
            }

            let stdout = match std::str::from_utf8(&result.stdout) {
                Ok(s) => s.trim(),
                Err(e) => {
                    revision.command_error = Some(format!("hg status stdout is not utf8: {}", e));
                    return;
                }
            };
            revision.has_local_changes = Some(!stdout.is_empty());
        }
        Err(e) => {
            revision.command_error =
                Some(format!("Command `hg status` failed with error: {:?}", e));
        }
    };
}

async fn repo_type(repo_root: &AbsNormPathBuf) -> buck2_error::Result<&'static RepoVcs> {
    static REPO_TYPE: OnceCell<buck2_error::Result<RepoVcs>> = OnceCell::const_new();
    async fn repo_type_impl(repo_root: &AbsNormPathBuf) -> buck2_error::Result<RepoVcs> {
        let (hg_metadata, git_metadata) = tokio::join!(
            async_fs_util::metadata(repo_root.join(ForwardRelativePath::new(".hg").unwrap())),
            async_fs_util::metadata(repo_root.join(ForwardRelativePath::new(".git").unwrap()))
        );

        let is_hg = hg_metadata.is_ok_and(|output| output.is_dir());
        let is_git = git_metadata.is_ok_and(|output| output.is_dir());

        if is_hg {
            Ok(RepoVcs::Hg)
        } else if is_git {
            Ok(RepoVcs::Git)
        } else {
            Ok(RepoVcs::Unknown)
        }
    }

    REPO_TYPE
        .get_or_init(|| repo_type_impl(repo_root))
        .await
        .as_ref()
        .map_err(|e| e.clone())
}

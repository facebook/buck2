/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::soft_error;
use buck2_data::VersionControlRevision;
use buck2_events::dispatch::EventDispatcher;
use buck2_fs::async_fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
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
    buck2_core::execution_types::revision::clear_revision();
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
        Ok(repo_vcs) => match repo_vcs {
            RepoVcs::Hg => create_hg_data(&mut revision, revision_type, repo_root).await,
            RepoVcs::Git => create_git_data(&mut revision, revision_type, repo_root).await,
            RepoVcs::Unknown => {
                revision.command_error = Some("Unknown repository type".to_owned());
            }
        },
        Err(e) => {
            revision.command_error = Some(format!("Failed to get repository type: {e:#}"));
        }
    }
    revision
}

async fn create_hg_data(
    revision: &mut buck2_data::VersionControlRevision,
    revision_type: RevisionDataType,
    repo_root: &AbsNormPathBuf,
) {
    match revision_type {
        RevisionDataType::CurrentRevision => get_hg_revision(revision, repo_root).await,
        RevisionDataType::Status => get_hg_status(revision).await,
    }
}

async fn get_hg_revision(
    revision: &mut buck2_data::VersionControlRevision,
    repo_root: &AbsNormPathBuf,
) {
    // The contents of dirstate may be arbitrarily large, but the id is always
    // in the first 20 bytes, so we only need to read the first 20 bytes
    let mut buffer = [0; 20];
    let dirstate = repo_root
        .join(ForwardRelativePath::new(".hg").unwrap())
        .join(ForwardRelativePath::new("dirstate").unwrap());

    if let Err(e) = async_fs_util::read(&dirstate, &mut buffer).await {
        revision.command_error = Some(format!(
            "Failed to read the first 20 bytes of {}: {:#}",
            dirstate.display(),
            e.categorize_internal(),
        ));
        return;
    }

    let curr_revision = buffer.iter().map(|b| format!("{b:02x}")).collect();
    revision.hg_revision = Some(curr_revision);
}

async fn get_hg_status(revision: &mut buck2_data::VersionControlRevision) {
    // `hg status` returns if there are any local changes
    let status_output = match reap_on_drop_command("hg", &["status"], Some(&[("HGPLAIN", "1")])) {
        Ok(command) => command.output().await,
        Err(e) => {
            revision.command_error =
                Some(format!("reap_on_drop_command for `hg status` failed: {e}"));
            return;
        }
    };

    match status_output {
        Ok(result) => {
            if !result.status.success() {
                let stderr = match std::str::from_utf8(&result.stderr) {
                    Ok(s) => s,
                    Err(e) => {
                        revision.command_error = Some(format!("hg status stderr is not utf8: {e}"));
                        return;
                    }
                };
                revision.command_error = Some(format!(
                    "Command `hg status` failed with error code {}; stderr: {}",
                    result.status, stderr
                ));
                return;
            }

            let stdout = match std::str::from_utf8(&result.stdout) {
                Ok(s) => s.trim(),
                Err(e) => {
                    revision.command_error = Some(format!("hg status stdout is not utf8: {e}"));
                    return;
                }
            };
            revision.has_local_changes = Some(!stdout.is_empty());
        }
        Err(e) => {
            revision.command_error = Some(format!("Command `hg status` failed with error: {e:?}"));
        }
    };
}

async fn create_git_data(
    revision: &mut buck2_data::VersionControlRevision,
    revision_type: RevisionDataType,
    repo_root: &AbsNormPathBuf,
) {
    match revision_type {
        RevisionDataType::CurrentRevision => get_git_revision(revision, repo_root).await,
        RevisionDataType::Status => get_git_status(revision, repo_root).await,
    }
}

async fn get_git_revision(
    revision: &mut buck2_data::VersionControlRevision,
    repo_root: &AbsNormPathBuf,
) {
    // `git rev-parse HEAD` resolves the current commit hash. This handles
    // packed refs, detached HEADs, etc. without us having to parse `.git`.
    match run_git(repo_root, &["rev-parse", "HEAD"]).await {
        Ok(stdout) => revision.git_revision = Some(stdout.trim().to_owned()),
        Err(e) => revision.command_error = Some(e),
    }
}

async fn get_git_status(
    revision: &mut buck2_data::VersionControlRevision,
    repo_root: &AbsNormPathBuf,
) {
    // `git status --porcelain` prints one line per change; empty output means
    // there are no local changes. Note that this counts untracked files as local
    // changes (they show up as `??` lines), matching the behavior of `hg status`.
    match run_git(repo_root, &["status", "--porcelain"]).await {
        Ok(stdout) => revision.has_local_changes = Some(!stdout.trim().is_empty()),
        Err(e) => revision.command_error = Some(e),
    }
}

/// Run a `git` command rooted at `repo_root`, returning its stdout on success or
/// an error message describing the failure.
async fn run_git(repo_root: &AbsNormPathBuf, args: &[&str]) -> Result<String, String> {
    let Some(repo_root) = repo_root.as_path().to_str() else {
        return Err(format!(
            "Repository root is not valid utf8: {}",
            repo_root.as_path().display()
        ));
    };

    // `-C <repo_root>` makes git operate on the repository regardless of the
    // daemon's current working directory.
    let mut full_args = vec!["-C", repo_root];
    full_args.extend_from_slice(args);

    // `GIT_OPTIONAL_LOCKS=0` prevents git from taking the index lock or
    // refreshing the index on disk, so we don't contend with a concurrent user
    // `git` invocation (e.g. for `git status`).
    let output = match reap_on_drop_command("git", &full_args, Some(&[("GIT_OPTIONAL_LOCKS", "0")]))
    {
        Ok(command) => command.output().await,
        Err(e) => {
            return Err(format!(
                "reap_on_drop_command for `git {}` failed: {e}",
                args.join(" ")
            ));
        }
    };

    let result = match output {
        Ok(result) => result,
        Err(e) => {
            return Err(format!(
                "Command `git {}` failed with error: {e:?}",
                args.join(" ")
            ));
        }
    };

    if !result.status.success() {
        let stderr = match std::str::from_utf8(&result.stderr) {
            Ok(s) => s,
            Err(e) => return Err(format!("git {} stderr is not utf8: {e}", args.join(" "))),
        };
        return Err(format!(
            "Command `git {}` failed with error code {}; stderr: {}",
            args.join(" "),
            result.status,
            stderr
        ));
    }

    match std::str::from_utf8(&result.stdout) {
        Ok(s) => Ok(s.to_owned()),
        Err(e) => Err(format!("git {} stdout is not utf8: {e}", args.join(" "))),
    }
}

async fn repo_type(repo_root: &AbsNormPathBuf) -> buck2_error::Result<&'static RepoVcs> {
    static REPO_TYPE: OnceCell<buck2_error::Result<RepoVcs>> = OnceCell::const_new();
    async fn repo_type_impl(repo_root: &AbsNormPathBuf) -> buck2_error::Result<RepoVcs> {
        let (hg_metadata, git_metadata) = tokio::join!(
            async_fs_util::metadata(repo_root.join(ForwardRelativePath::new(".hg").unwrap())),
            async_fs_util::metadata(repo_root.join(ForwardRelativePath::new(".git").unwrap()))
        );

        let is_hg = hg_metadata.is_ok_and(|output| output.is_dir());
        // `.git` can be a symlink or a file with contents like:
        //
        //     gitdir: /home/dog/buck2/.git/worktrees/buck3
        let is_git = git_metadata.is_ok();

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

#[cfg(test)]
mod tests {
    use super::*;

    /// Run a `git` command in `repo_root`, panicking on failure. Used to set up
    /// and inspect test repositories.
    async fn git(repo_root: &AbsNormPathBuf, args: &[&str]) -> String {
        run_git(repo_root, args)
            .await
            .unwrap_or_else(|e| panic!("`git {}` failed: {e}", args.join(" ")))
    }

    /// Initialize an empty git repo with a deterministic identity so that commits
    /// succeed even in environments without a global git config.
    async fn init_git_repo(repo_root: &AbsNormPathBuf) {
        git(repo_root, &["init", "-q"]).await;
        git(repo_root, &["config", "user.name", "Buck Test"]).await;
        git(
            repo_root,
            &["config", "user.email", "buck-test@example.com"],
        )
        .await;
        git(repo_root, &["config", "commit.gpgsign", "false"]).await;
    }

    fn temp_repo_root(temp_dir: &tempfile::TempDir) -> buck2_error::Result<AbsNormPathBuf> {
        AbsNormPathBuf::try_from(temp_dir.path().to_path_buf())
    }

    fn write_file(repo_root: &AbsNormPathBuf, name: &str, contents: &str) {
        let repo_path = std::path::Path::new(repo_root.as_path().to_str().unwrap());
        std::fs::write(repo_path.join(name), contents).unwrap();
    }

    #[tokio::test]
    async fn test_get_git_revision_matches_head() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let repo_root = temp_repo_root(&temp_dir)?;
        init_git_repo(&repo_root).await;
        write_file(&repo_root, "file.txt", "hello");
        git(&repo_root, &["add", "file.txt"]).await;
        git(&repo_root, &["commit", "-q", "-m", "initial"]).await;

        let head = git(&repo_root, &["rev-parse", "HEAD"]).await;
        let head = head.trim();

        let mut revision = VersionControlRevision::default();
        get_git_revision(&mut revision, &repo_root).await;

        assert_eq!(revision.command_error, None);
        assert_eq!(revision.git_revision.as_deref(), Some(head));
        let git_revision = revision.git_revision.unwrap();
        assert_eq!(git_revision.len(), 40);
        assert!(git_revision.chars().all(|c| c.is_ascii_hexdigit()));
        Ok(())
    }

    #[tokio::test]
    async fn test_get_git_status_clean() -> buck2_error::Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let repo_root = temp_repo_root(&temp_dir)?;
        init_git_repo(&repo_root).await;
        write_file(&repo_root, "file.txt", "hello");
        git(&repo_root, &["add", "file.txt"]).await;
        git(&repo_root, &["commit", "-q", "-m", "initial"]).await;

        let mut revision = VersionControlRevision::default();
        get_git_status(&mut revision, &repo_root).await;

        assert_eq!(revision.command_error, None);
        assert_eq!(revision.has_local_changes, Some(false));
        Ok(())
    }
}

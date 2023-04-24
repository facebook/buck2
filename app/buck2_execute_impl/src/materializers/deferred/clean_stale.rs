/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_common::file_ops::FileType;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::quiet_soft_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use chrono::DateTime;
use chrono::Utc;
use derivative::Derivative;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use more_futures::cancellation::CancellationContext;
use thiserror::Error;
use tokio::sync::oneshot::Sender;
use tracing::error;

use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::join_all_existing_futs;
use crate::materializers::deferred::ArtifactMaterializationData;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::DefaultIoHandler;
use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::sqlite::MaterializerStateSqliteDb;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct CleanStaleArtifacts {
    pub keep_since_time: DateTime<Utc>,
    pub dry_run: bool,
    pub tracked_only: bool,
    #[derivative(Debug = "ignore")]
    pub sender: Sender<BoxFuture<'static, anyhow::Result<buck2_cli_proto::CleanStaleResponse>>>,
    pub dispatcher: EventDispatcher,
}

fn skip_clean_response_with_message(
    message: &str,
) -> anyhow::Result<(
    BoxFuture<'static, anyhow::Result<()>>,
    buck2_cli_proto::CleanStaleResponse,
)> {
    Ok((
        futures::future::ready(Ok(())).boxed(),
        buck2_cli_proto::CleanStaleResponse {
            message: Some(message.to_owned()),
            stats: None,
        },
    ))
}

impl ExtensionCommand<DefaultIoHandler> for CleanStaleArtifacts {
    fn execute(
        self: Box<Self>,
        processor: &mut DeferredMaterializerCommandProcessor<DefaultIoHandler>,
    ) {
        let res = if let Some(sqlite_db) = processor.sqlite_db.as_mut() {
            if !processor.defer_write_actions {
                skip_clean_response_with_message(
                    "Skipping clean, set buck2.defer_write_actions to use clean --stale",
                )
            } else {
                gather_clean_futures_for_stale_artifacts(
                    &mut processor.tree,
                    self.keep_since_time,
                    self.dry_run,
                    self.tracked_only,
                    sqlite_db,
                    &processor.io,
                    processor.digest_config,
                    processor.cancellations,
                    &self.dispatcher,
                )
            }
        } else {
            skip_clean_response_with_message(
                "Skipping clean, set buck2.sqlite_materializer_state to use clean --stale",
            )
        };
        let fut = async move {
            let (fut, response) = res?;
            fut.await?;
            tracing::trace!("finished cleaning stale artifacts");
            Ok(response)
        }
        .boxed();
        let _ignored = self.sender.send(fut);
    }
}

#[derive(Debug, Clone, Error)]
#[error("Internal error: materializer state exists (num db entries: {}) but no artifacts were found by clean ({:?}). Not cleaning untracked artifacts.", .db_size, .stats)]
pub(crate) struct CleanStaleError {
    db_size: usize,
    stats: buck2_data::CleanStaleStats,
}

fn gather_clean_futures_for_stale_artifacts(
    tree: &mut ArtifactTree,
    keep_since_time: DateTime<Utc>,
    dry_run: bool,
    tracked_only: bool,
    sqlite_db: &mut MaterializerStateSqliteDb,
    io: &Arc<DefaultIoHandler>,
    digest: DigestConfig,
    cancellations: &'static CancellationContext,
    dispatcher: &EventDispatcher,
) -> anyhow::Result<(
    BoxFuture<'static, anyhow::Result<()>>,
    buck2_cli_proto::CleanStaleResponse,
)> {
    let gen_path = io
        .buck_out_path
        .join(ProjectRelativePathBuf::unchecked_new("gen".to_owned()));
    let gen_dir = io.fs.resolve(&gen_path);
    if !fs_util::try_exists(&gen_dir)? {
        return skip_clean_response_with_message("Nothing to clean");
    }
    tracing::trace!(gen_dir = %gen_dir, "Scanning");

    let mut stats = buck2_data::CleanStaleStats::default();
    let mut paths_to_remove = Vec::new();
    let mut paths_to_invalidate = Vec::new();

    if tracked_only {
        find_stale_tracked_only(tree, keep_since_time, &mut stats, &mut paths_to_invalidate)?
    } else {
        let gen_subtree = tree
            .get_subtree(&mut gen_path.iter())
            .context("Found a file where gen dir expected")?;

        let empty;

        let gen_subtree = match gen_subtree {
            Some(t) => t,
            None => {
                empty = HashMap::new();
                &empty
            }
        };

        StaleFinder {
            fs: &io.fs,
            dispatcher,
            keep_since_time,
            stats: &mut stats,
            paths_to_remove: &mut paths_to_remove,
            paths_to_invalidate: &mut paths_to_invalidate,
        }
        .visit_recursively(gen_path, gen_subtree)?;
    };

    // If no stale or retained artifact founds, the db should be empty.
    if stats.stale_artifact_count + stats.retained_artifact_count == 0 {
        // Just need to know if any entries exist, could be a simpler query.
        // Checking the db directly in case tree is somehow not in sync.
        let materializer_state = sqlite_db.materializer_state_table().read_all(digest)?;

        // Entries in the db should have been found in buck-out, return error and skip cleaning untracked artifacts.
        if !materializer_state.is_empty() {
            let error = CleanStaleError {
                db_size: materializer_state.len(),
                stats,
            };
            // quiet just because it's also returned, soft_error to log to scribe
            quiet_soft_error!("clean_stale_error", error.clone().into()).unwrap();
            return Err(anyhow::anyhow!(error));
        }
    }

    let fut = if dry_run {
        futures::future::ready(Ok(())).boxed()
    } else {
        let io = io.dupe();

        stats.cleaned_path_count = paths_to_remove.len() as u64;
        stats.cleaned_artifact_count = stats.stale_artifact_count + stats.untracked_artifact_count;
        stats.cleaned_bytes = stats.untracked_bytes + stats.stale_bytes;

        let existing_futs =
            tree.invalidate_paths_and_collect_futures(paths_to_invalidate, Some(sqlite_db))?;

        async move {
            // Wait for all in-progress operations to finish on the paths we are about to
            // remove from disk.
            join_all_existing_futs(existing_futs).await?;

            // Then actually delete them. Note that we kick off one CleanOutputPaths per path. We
            // do this to get parallelism.
            futures::future::try_join_all(paths_to_remove.into_iter().map(|path| {
                io.io_executor.execute_io(
                    Box::new(CleanOutputPaths { paths: vec![path] }),
                    cancellations,
                )
            }))
            .await?;

            anyhow::Ok(())
        }
        .boxed()
    };

    Ok((
        fut,
        buck2_cli_proto::CleanStaleResponse {
            message: None,
            stats: Some(stats),
        },
    ))
}

/// Get file size or directory size, without following symlinks
pub fn get_size(path: &AbsNormPath) -> anyhow::Result<u64> {
    let mut result = 0;
    if path.is_dir() {
        for entry in fs_util::read_dir(path)? {
            result += get_size(&entry?.path())?;
        }
    } else {
        result = path.symlink_metadata()?.len();
    }
    Ok(result)
}

struct StaleFinder<'a> {
    fs: &'a ProjectRoot,
    dispatcher: &'a EventDispatcher,
    keep_since_time: DateTime<Utc>,
    stats: &'a mut buck2_data::CleanStaleStats,
    /// Those paths will be deleted on disk.
    paths_to_remove: &'a mut Vec<ProjectRelativePathBuf>,
    /// Those paths will be invalidated in the materiaizer.
    paths_to_invalidate: &'a mut Vec<ProjectRelativePathBuf>,
}

impl<'a> StaleFinder<'a> {
    /// Start from `path` and `subtree` and visit everything below.
    fn visit_recursively<'t>(
        &mut self,
        path: ProjectRelativePathBuf,
        subtree: &'t HashMap<FileNameBuf, ArtifactTree>,
    ) -> anyhow::Result<()> {
        let mut queue = vec![(path, subtree)];

        while let Some((path, tree)) = queue.pop() {
            self.visit(&path, tree, &mut queue)?;
        }

        Ok(())
    }

    /// Visit one directory.
    fn visit<'t>(
        &mut self,
        path: &ProjectRelativePath,
        subtree: &'t HashMap<FileNameBuf, ArtifactTree>,
        queue: &mut Vec<(
            ProjectRelativePathBuf,
            &'t HashMap<FileNameBuf, ArtifactTree>,
        )>,
    ) -> anyhow::Result<()> {
        let abs_path = self.fs.resolve(path);

        for child in fs_util::read_dir(&abs_path)? {
            let child = child?;

            let file_name = child.file_name();
            let file_name = file_name.to_str().and_then(|f| FileName::new(f).ok());

            let file_name = match file_name {
                Some(file_name) => file_name,
                None => {
                    // If the file name is invalid, then it can't be tracked by the materializer.
                    // We should ideally delete this, but currently we don't support doing that.
                    continue;
                }
            };

            let path = path.join(file_name);

            let file_type = FileType::from(child.file_type()?);

            let subtree = match subtree.get(file_name) {
                Some(subtree) => subtree,
                None => {
                    // This path is not tracked by the materializer, we can delete it.
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as untracked");
                    self.stats.untracked_artifact_count += 1;
                    self.stats.untracked_bytes += get_size(&child.path())?;
                    if self.stats.untracked_artifact_count <= 2000 {
                        self.dispatcher.instant_event(buck2_data::UntrackedFile {
                            path: path.to_string(),
                            file_type: format!("{:?}", file_type),
                        });
                    }
                    self.paths_to_remove.push(path);
                    continue;
                }
            };

            match subtree {
                ArtifactTree::Tree(subtree) if file_type.is_dir() => {
                    queue.push((path, subtree));
                }
                ArtifactTree::Data(box ArtifactMaterializationData {
                    stage:
                        ArtifactMaterializationStage::Materialized {
                            active: false,
                            last_access_time,
                            metadata,
                        },
                    ..
                }) if *last_access_time < self.keep_since_time => {
                    // This is something we can invalidate.
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as stale");
                    self.stats.stale_artifact_count += 1;
                    self.stats.stale_bytes += metadata.size();
                    self.paths_to_invalidate.push(path.clone());
                    self.paths_to_remove.push(path);
                }
                ArtifactTree::Data(box ArtifactMaterializationData {
                    stage: ArtifactMaterializationStage::Materialized { metadata, .. },
                    ..
                }) => {
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as retained");
                    self.stats.retained_artifact_count += 1;
                    self.stats.retained_bytes += metadata.size();
                }
                _ => {
                    // What we have on disk does not match what we have in the materializer (which is
                    // not clean stale's problem to fix).
                    tracing::trace!(path = %path, file_type = ?file_type, "skipping");
                }
            }
        }

        Ok(())
    }
}

fn find_stale_tracked_only(
    tree: &ArtifactTree,
    keep_since_time: DateTime<Utc>,
    stats: &mut buck2_data::CleanStaleStats,
    paths_to_invalidate: &mut Vec<ProjectRelativePathBuf>,
) -> anyhow::Result<()> {
    for (f_path, v) in tree.iter_with_paths() {
        if let ArtifactMaterializationStage::Materialized {
            last_access_time,
            active,
            ..
        } = &v.stage
        {
            let path = ProjectRelativePathBuf::from(f_path);
            if *last_access_time < keep_since_time && !active {
                tracing::trace!(path = %path, "stale artifact");
                stats.stale_artifact_count += 1;
                paths_to_invalidate.push(path);
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                stats.retained_artifact_count += 1;
            }
        }
    }
    Ok(())
}

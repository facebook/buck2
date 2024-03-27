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
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_futures::cancellation::CancellationContext;
use chrono::DateTime;
use chrono::Utc;
use derivative::Derivative;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::sync::oneshot::Sender;
use tracing::error;

use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::io_handler::IoHandler;
use crate::materializers::deferred::join_all_existing_futs;
use crate::materializers::deferred::ArtifactMaterializationData;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
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

fn ready_clean_response(
    message: Option<&str>,
    stats: Option<buck2_data::CleanStaleStats>,
) -> anyhow::Result<BoxFuture<'static, anyhow::Result<buck2_cli_proto::CleanStaleResponse>>> {
    Ok(
        futures::future::ready(Ok(buck2_cli_proto::CleanStaleResponse {
            message: message.map(|m| m.to_owned()),
            stats,
        }))
        .boxed(),
    )
}

impl<T: IoHandler> ExtensionCommand<T> for CleanStaleArtifacts {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        let fut = if let Some(sqlite_db) = processor.sqlite_db.as_mut() {
            if !processor.defer_write_actions {
                ready_clean_response(
                    Some("Skipping clean, set buck2.defer_write_actions to use clean --stale"),
                    None,
                )
            } else {
                gather_clean_futures_for_stale_artifacts(
                    &mut processor.tree,
                    self.keep_since_time,
                    self.dry_run,
                    self.tracked_only,
                    sqlite_db,
                    &processor.io,
                    processor.cancellations,
                    &self.dispatcher,
                )
            }
        } else {
            ready_clean_response(
                Some("Skipping clean, set buck2.sqlite_materializer_state to use clean --stale"),
                None,
            )
        };
        let fut = async move {
            let response = fut?.await?;
            tracing::trace!("finished cleaning stale artifacts");
            Ok(response)
        }
        .boxed();
        let _ignored = self.sender.send(fut);
    }
}

#[derive(Debug, Clone, buck2_error::Error)]
#[error("Internal error: materializer state exists (num db entries: {}) but no artifacts were found by clean ({:?}). Not cleaning untracked artifacts.", .db_size, .stats)]
pub(crate) struct CleanStaleError {
    db_size: usize,
    stats: buck2_data::CleanStaleStats,
}

fn stats_for_paths(paths: &Vec<FoundPath>) -> buck2_data::CleanStaleStats {
    let mut stats = buck2_data::CleanStaleStats::default();
    for path in paths {
        match path {
            FoundPath::Untracked(_, _, size) => {
                stats.untracked_artifact_count += 1;
                stats.untracked_bytes += *size;
            }
            FoundPath::Stale(_, size) => {
                stats.stale_artifact_count += 1;
                stats.stale_bytes += *size;
            }
            FoundPath::Retained(size) => {
                stats.retained_artifact_count += 1;
                stats.retained_bytes += *size;
            }
        }
    }
    stats
}

fn gather_clean_futures_for_stale_artifacts<T: IoHandler>(
    tree: &mut ArtifactTree,
    keep_since_time: DateTime<Utc>,
    dry_run: bool,
    tracked_only: bool,
    sqlite_db: &mut MaterializerStateSqliteDb,
    io: &Arc<T>,
    cancellations: &'static CancellationContext,
    dispatcher: &EventDispatcher,
) -> anyhow::Result<BoxFuture<'static, anyhow::Result<buck2_cli_proto::CleanStaleResponse>>> {
    let gen_path = io
        .buck_out_path()
        .join(ProjectRelativePathBuf::unchecked_new("gen".to_owned()));
    let gen_dir = io.fs().resolve(&gen_path);
    if !fs_util::try_exists(&gen_dir)? {
        return ready_clean_response(Some("Nothing to clean"), None);
    }
    tracing::trace!(gen_dir = %gen_dir, "Scanning");

    let mut found_paths = Vec::new();
    if tracked_only {
        find_stale_tracked_only(tree, keep_since_time, &mut found_paths)?
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
            io: io.dupe(),
            keep_since_time,
            found_paths: &mut found_paths,
        }
        .visit_recursively(gen_path, gen_subtree)?;
    };

    let mut stats = stats_for_paths(&found_paths);
    // Log limited number of untracked artifacts to avoid logging spikes if schema changes.
    for (path, file_type) in found_paths
        .iter()
        .filter_map(|x| match x {
            FoundPath::Untracked(path, file_type, _) => Some((path, file_type)),
            _ => None,
        })
        .take(2000)
    {
        dispatcher.instant_event(buck2_data::UntrackedFile {
            path: path.to_string(),
            file_type: format!("{:?}", file_type),
        });
    }

    // If no stale or retained artifact founds, the db should be empty.
    if stats.stale_artifact_count + stats.retained_artifact_count == 0 {
        // Just need to know if any entries exist, could be a simpler query.
        // Checking the db directly in case tree is somehow not in sync.
        let materializer_state = sqlite_db
            .materializer_state_table()
            .read_all(io.digest_config())?;

        // Entries in the db should have been found in buck-out, return error and skip cleaning untracked artifacts.
        if !materializer_state.is_empty() {
            let error = CleanStaleError {
                db_size: materializer_state.len(),
                stats,
            };
            // quiet just because it's also returned, soft_error to log to scribe
            return Err(soft_error!("clean_stale_error", error.into(), quiet: true)?);
        }
    }

    let fut = if dry_run {
        return ready_clean_response(None, Some(stats));
    } else {
        let io = io.dupe();

        let paths_to_invalidate: Vec<ProjectRelativePathBuf> = found_paths
            .iter()
            .filter_map(|x| match x {
                FoundPath::Stale(p, ..) => Some(p.clone()),
                _ => None,
            })
            .collect();

        let existing_futs =
            tree.invalidate_paths_and_collect_futures(paths_to_invalidate, Some(sqlite_db))?;

        async move {
            // Wait for all in-progress operations to finish on the paths we are about to
            // remove from disk.
            join_all_existing_futs(existing_futs).await?;

            // Then actually delete them. Note that we kick off one CleanOutputPaths per path. We
            // do this to get parallelism.
            let res = futures::future::try_join_all(
                found_paths
                    .into_iter()
                    .filter_map(|x| match x {
                        FoundPath::Untracked(p, _, size) => Some((p, size)),
                        FoundPath::Stale(p, size) => Some((p, size)),
                        _ => None,
                    })
                    .map(|(path, size)| clean_artifact(path, size, cancellations, &io)),
            )
            .await?;

            stats.cleaned_artifact_count += res.len() as u64;
            stats.cleaned_bytes = res.iter().sum();
            anyhow::Ok(buck2_cli_proto::CleanStaleResponse {
                message: None,
                stats: Some(stats),
            })
        }
        .boxed()
    };
    Ok(fut)
}

async fn clean_artifact<T: IoHandler>(
    path: ProjectRelativePathBuf,
    size: u64,
    cancellations: &'static CancellationContext<'_>,
    io: &Arc<T>,
) -> anyhow::Result<u64> {
    io.clean_invalidated_path(path, cancellations).await?;
    Ok(size)
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

struct StaleFinder<'a, T: IoHandler> {
    io: Arc<T>,
    keep_since_time: DateTime<Utc>,
    found_paths: &'a mut Vec<FoundPath>,
}

#[derive(Clone)]
enum FoundPath {
    /// These will be deleted on disk.
    Untracked(ProjectRelativePathBuf, FileType, u64),
    /// These will be invalidated in the materiaizer.
    Stale(ProjectRelativePathBuf, u64),
    Retained(u64),
}

impl<'a, T: IoHandler> StaleFinder<'a, T> {
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
        let abs_path = self.io.fs().resolve(path);

        for child in self.io.read_dir(&abs_path)? {
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
                    self.found_paths.push(FoundPath::Untracked(
                        path,
                        file_type,
                        get_size(&child.path())?,
                    ));
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
                    self.found_paths
                        .push(FoundPath::Stale(path, metadata.size()));
                }
                ArtifactTree::Data(box ArtifactMaterializationData {
                    stage: ArtifactMaterializationStage::Materialized { metadata, .. },
                    ..
                }) => {
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as retained");
                    self.found_paths.push(FoundPath::Retained(metadata.size()));
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
    found_paths: &mut Vec<FoundPath>,
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
                found_paths.push(FoundPath::Stale(path, 0));
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                found_paths.push(FoundPath::Retained(0));
            }
        }
    }
    Ok(())
}

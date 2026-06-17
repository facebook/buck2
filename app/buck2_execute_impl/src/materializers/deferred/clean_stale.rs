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
use std::time::Duration;
use std::time::Instant;

use buck2_common::file_ops::metadata::FileType;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::liveliness_observer::LivelinessObserverSync;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_data::CleanStaleResultKind;
use buck2_data::CleanStaleStats;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::buck2_error;
use buck2_events::daemon_id::DaemonId;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_execute::execute::blocking::IoRequest;
use buck2_execute::execute::clean_output_paths::cleanup_path;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::fs_util::disk_space_stats;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_hash::StdBuckHashMap;
use buck2_wrapper_common::invocation_id::TraceId;
use chrono::DateTime;
use chrono::Utc;
use derivative::Derivative;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use tokio::sync::oneshot::Sender;

use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::DeferredMaterializerCommandProcessor;
use crate::materializers::deferred::artifact_tree::ArtifactMaterializationData;
use crate::materializers::deferred::artifact_tree::ArtifactTree;
use crate::materializers::deferred::artifact_tree::artifact_metadata_size;
use crate::materializers::deferred::extension::ExtensionCommand;
use crate::materializers::deferred::io_handler::IoHandler;
use crate::materializers::deferred::join_all_existing_futs;
use crate::sqlite::materializer_db::MaterializerStateSqliteDb;

#[derive(Debug, Clone)]
pub struct CleanStaleArtifactsCommand {
    pub keep_since_time: DateTime<Utc>,
    pub dry_run: bool,
    pub tracked_only: bool,
    pub dispatcher: EventDispatcher,
    /// When set, after the normal stale scan also promote the oldest
    /// non-active retained artifacts to stale until projected free disk %
    /// rises above the threshold.
    pub adaptive_low_disk: Option<AdaptiveLowDiskParams>,
    /// Root path for `disk_space_stats` during the adaptive promotion pass.
    /// Constructed once by the caller and shared via `Arc::dupe`. `None` when no
    /// valid filesystem root could be constructed (e.g. on Windows, where `/` is
    /// not an absolute path); the adaptive pass is then skipped.
    pub root_abs_path: Option<Arc<AbsPathBuf>>,
}

#[derive(Debug, Clone)]
pub struct AdaptiveLowDiskParams {
    pub threshold_percent: f64,
    /// Retained artifacts last accessed at or after this instant are protected
    /// from adaptive promotion, regardless of free disk pressure.
    pub min_access_time: DateTime<Utc>,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct CleanStaleArtifactsExtensionCommand {
    pub cmd: CleanStaleArtifactsCommand,
    #[derivative(Debug = "ignore")]
    pub sender: Sender<BoxFuture<'static, buck2_error::Result<CleanResult>>>,
}

#[derive(Clone)]
pub struct CleanResult {
    kind: CleanStaleResultKind,
    stats: CleanStaleStats,
}

enum PendingCleanResult {
    Finished(CleanResult),
    Pending(BoxFuture<'static, buck2_error::Result<CleanResult>>),
}

impl From<CleanStaleResultKind> for PendingCleanResult {
    fn from(val: CleanStaleResultKind) -> Self {
        PendingCleanResult::Finished(CleanResult {
            kind: val,
            stats: CleanStaleStats::default(),
        })
    }
}

impl From<CleanResult> for PendingCleanResult {
    fn from(val: CleanResult) -> Self {
        PendingCleanResult::Finished(val)
    }
}

impl From<CleanResult> for buck2_cli_proto::CleanStaleResponse {
    fn from(result: CleanResult) -> Self {
        let message = match result.kind {
            CleanStaleResultKind::SkippedNoGenDir => Some("Nothing to clean"),
            CleanStaleResultKind::SkippedDeferWriteDisabled => {
                Some("Skipping clean, set buck2.defer_write_actions to use clean --stale")
            }
            CleanStaleResultKind::SkippedSqliteDisabled => {
                Some("Skipping clean, set buck2.sqlite_materializer_state to use clean --stale")
            }
            CleanStaleResultKind::SkippedDryRun => None,
            CleanStaleResultKind::Interrupted => Some("Interrupted"),
            CleanStaleResultKind::Finished => None,
            CleanStaleResultKind::Failed => None,
        };
        Self {
            message: message.map(|m| m.to_owned()),
            stats: Some(result.stats),
        }
    }
}

fn create_result(
    result: Result<CleanResult, buck2_error::Error>,
    trace_id: Option<TraceId>,
    daemon_id: &DaemonId,
    total_duration_s: u64,
) -> buck2_data::CleanStaleResult {
    let (kind, mut stats, error) = match result {
        Ok(result) => (result.kind, result.stats, None),
        Err(e) => (
            CleanStaleResultKind::Failed,
            CleanStaleStats::default(),
            Some((&e).into()),
        ),
    };
    stats.total_duration_s = total_duration_s;
    buck2_data::CleanStaleResult {
        kind: kind.into(),
        stats: Some(stats),
        metadata: metadata::collect(daemon_id),
        error,
        command_uuid: trace_id.map(|id| id.to_string()),
    }
}

impl<T: IoHandler> ExtensionCommand<T> for CleanStaleArtifactsExtensionCommand {
    fn execute(self: Box<Self>, processor: &mut DeferredMaterializerCommandProcessor<T>) {
        let trace_id = self.cmd.dispatcher.trace_id().clone();
        let daemon_id = self.cmd.dispatcher.daemon_id().clone();
        let fut = self
            .cmd
            .create_clean_fut(processor, Some(trace_id), daemon_id);
        let _ignored = self.sender.send(fut);
    }
}

impl CleanStaleArtifactsCommand {
    pub(super) fn create_clean_fut<T: IoHandler>(
        &self,
        processor: &mut DeferredMaterializerCommandProcessor<T>,
        trace_id: Option<TraceId>,
        daemon_id: DaemonId,
    ) -> BoxFuture<'static, buck2_error::Result<CleanResult>> {
        let start_time = Instant::now();
        let pending_result = self.create_pending_clean_result(processor);
        let dispatcher_dup = self.dispatcher.dupe();
        async move {
            let result = match pending_result {
                Ok(res) => match res {
                    PendingCleanResult::Finished(result) => Ok(result),
                    PendingCleanResult::Pending(fut) => fut.await,
                },
                Err(e) => Err(e),
            };
            let result: Result<CleanResult, buck2_error::Error> = result;
            let result_event: buck2_data::CleanStaleResult = create_result(
                result.clone(),
                trace_id,
                &daemon_id,
                (Instant::now() - start_time).as_secs(),
            );
            dispatcher_dup.instant_event(result_event);
            result
        }
        .boxed()
    }

    fn create_pending_clean_result<T: IoHandler>(
        &self,
        processor: &mut DeferredMaterializerCommandProcessor<T>,
    ) -> buck2_error::Result<PendingCleanResult> {
        let (liveliness_observer, liveliness_guard) = LivelinessGuard::create_sync();
        *processor.command_sender.clean_guard.write() = Some(liveliness_guard);

        if let Some(sqlite_db) = processor.sqlite_db.as_mut() {
            if !processor.defer_write_actions {
                Ok(CleanStaleResultKind::SkippedDeferWriteDisabled.into())
            } else {
                self.scan_and_create_clean_fut(
                    &mut processor.tree,
                    sqlite_db,
                    &processor.io,
                    processor.cancellations,
                    liveliness_observer.clone(),
                )
            }
        } else {
            Ok(CleanStaleResultKind::SkippedSqliteDisabled.into())
        }
    }

    fn scan_and_create_clean_fut<T: IoHandler>(
        &self,
        tree: &mut ArtifactTree,
        sqlite_db: &mut MaterializerStateSqliteDb,
        io: &Arc<T>,
        cancellations: &'static CancellationContext,
        liveliness_observer: Arc<dyn LivelinessObserverSync>,
    ) -> buck2_error::Result<PendingCleanResult> {
        let start_time = Instant::now();

        let mut artifact_dirs = Vec::new();
        for dir_name in &["gen", "art"] {
            let dir_path = io
                .buck_out_path()
                .join(ProjectRelativePathBuf::unchecked_new(dir_name.to_string()));
            let dir_abs = io.fs().resolve(&dir_path);
            if fs_util::try_exists(&dir_abs)? {
                artifact_dirs.push(dir_path);
            }
        }
        if artifact_dirs.is_empty() {
            return Ok(CleanStaleResultKind::SkippedNoGenDir.into());
        }

        let mut found_paths = Vec::new();
        if self.tracked_only {
            find_stale_tracked_only(tree, self.keep_since_time, &mut found_paths)?
        } else {
            for dir_path in &artifact_dirs {
                tracing::trace!(dir = %io.fs().resolve(dir_path), "Scanning");

                let dir_subtree = tree
                    .get_subtree(&mut dir_path.iter())
                    .with_buck_error_context(|| {
                        format!("Found a file where directory was expected: {}", dir_path)
                    })?;

                let empty;

                let dir_subtree = match dir_subtree {
                    Some(t) => t,
                    None => {
                        empty = StdBuckHashMap::default();
                        &empty
                    }
                };

                StaleFinder {
                    io: io.dupe(),
                    keep_since_time: self.keep_since_time,
                    found_paths: &mut found_paths,
                    liveliness_observer: liveliness_observer.clone(),
                }
                .visit_recursively(dir_path.clone(), dir_subtree)?;
            }
        };

        if let Some(params) = self.adaptive_low_disk.as_ref()
            && let Some(root_abs_path) = self.root_abs_path.as_ref()
        {
            match disk_space_stats(&**root_abs_path) {
                Ok(disk_stats) => apply_adaptive_low_disk(
                    &mut found_paths,
                    disk_stats.free_space,
                    disk_stats.total_space,
                    params.threshold_percent,
                    params.min_access_time,
                ),
                Err(e) => {
                    let _unused = soft_error!("disk_space_stats", e);
                }
            }
        }

        let mut stats = stats_for_paths(&found_paths);
        stats.scan_duration_s = (Instant::now() - start_time).as_secs();

        // Log limited number of untracked artifacts to avoid logging spikes if schema changes.
        for (path, file_type) in found_paths
            .iter()
            .filter_map(|x| match x {
                FoundPath::Untracked(path, file_type, _) => Some((path, file_type)),
                _ => None,
            })
            .take(2000)
        {
            self.dispatcher.instant_event(buck2_data::UntrackedFile {
                path: path.to_string(),
                file_type: format!("{file_type:?}"),
            });
        }

        if !liveliness_observer.is_alive_sync() {
            return Ok(PendingCleanResult::Finished(CleanResult {
                kind: CleanStaleResultKind::Interrupted,
                stats,
            }));
        }

        // If no stale or retained artifact founds, the db should be empty.
        if stats.stale_artifact_count + stats.retained_artifact_count == 0 {
            // Just need to know if any entries exist, could be a simpler query.
            // Checking the db directly in case tree is somehow not in sync.
            let materializer_state = sqlite_db
                .materializer_state_table()
                .read_materializer_state(io.digest_config())?;

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

        if self.dry_run {
            Ok(PendingCleanResult::Finished(CleanResult {
                kind: CleanStaleResultKind::SkippedDryRun,
                stats,
            }))
        } else {
            Ok(PendingCleanResult::Pending(create_clean_fut(
                found_paths,
                stats,
                tree,
                sqlite_db,
                io,
                cancellations,
                liveliness_observer,
            )?))
        }
    }
}

#[derive(Debug, Clone, buck2_error::Error)]
#[error("Internal error: materializer state exists (num db entries: {}) but no artifacts were found by clean ({:?}). Not cleaning untracked artifacts.", .db_size, .stats)]
#[buck2(tag = CleanStale)]
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
            FoundPath::Tracked {
                size,
                state: TrackedState::Stale,
                ..
            } => {
                stats.stale_artifact_count += 1;
                stats.stale_bytes += *size;
            }
            FoundPath::Tracked {
                size,
                state: TrackedState::Retained { .. } | TrackedState::ActiveRetained,
                ..
            } => {
                stats.retained_artifact_count += 1;
                stats.retained_bytes += *size;
            }
        }
    }
    stats
}

fn create_clean_fut<T: IoHandler>(
    found_paths: Vec<FoundPath>,
    mut stats: CleanStaleStats,
    tree: &mut ArtifactTree,
    sqlite_db: &mut MaterializerStateSqliteDb,
    io: &Arc<T>,
    cancellations: &'static CancellationContext,
    liveliness_observer: Arc<dyn LivelinessObserverSync>,
) -> buck2_error::Result<BoxFuture<'static, buck2_error::Result<CleanResult>>> {
    let io = io.dupe();

    let paths_to_invalidate: Vec<ProjectRelativePathBuf> = found_paths
        .iter()
        .filter_map(|x| match x {
            FoundPath::Tracked {
                path,
                state: TrackedState::Stale,
                ..
            } => Some(path.clone()),
            _ => None,
        })
        .collect();

    let existing_clean_futs =
        tree.invalidate_paths_and_collect_futures(paths_to_invalidate, Some(sqlite_db))?;
    let mut existing_materialization_futs = vec![];
    for data in tree.iter_without_paths() {
        if let Some(active) = data.processing.active_ref()
            && let super::ProcessingFuture::Materializing(future) = &active.future
        {
            existing_materialization_futs.push(future.clone());
        }
    }

    let fut = async move {
        let start_time = Instant::now();
        // Wait for all in-progress operations to finish on the paths we are about to
        // remove from disk.
        join_all_existing_futs(existing_clean_futs).await?;
        // Untracked artifacts can be produced during materialization that should not be cleaned while materialization is in progress.
        // Wait for all materializations since the path for the future may not be associated with the untracked path.
        for fut in existing_materialization_futs.into_iter() {
            fut.await.ok();
        }

        // Then actually delete them. Note that we kick off one CleanOutputPaths per path. We
        // do this to get parallelism.
        let res = buck2_util::future::try_join_all(
            found_paths
                .into_iter()
                .filter_map(|x| match x {
                    FoundPath::Untracked(p, _, size) => Some((p, size)),
                    FoundPath::Tracked {
                        path,
                        size,
                        state: TrackedState::Stale,
                    } => Some((path, size)),
                    _ => None,
                })
                .map(|(path, size)| {
                    clean_artifact(path, size, cancellations, &io, liveliness_observer.dupe())
                }),
        )
        .await?;

        let cleaned_sizes: Vec<u64> = res.iter().filter_map(|x| *x).collect();
        stats.cleaned_artifact_count += cleaned_sizes.len() as u64;
        stats.cleaned_bytes = cleaned_sizes.iter().sum();
        stats.clean_duration_s = (Instant::now() - start_time).as_secs();
        let kind = if !liveliness_observer.is_alive().await {
            CleanStaleResultKind::Interrupted
        } else {
            CleanStaleResultKind::Finished
        };
        Ok(CleanResult { kind, stats })
    };
    Ok(fut.boxed())
}

async fn clean_artifact<T: IoHandler>(
    path: ProjectRelativePathBuf,
    size: u64,
    cancellations: &'static CancellationContext,
    io: &Arc<T>,
    liveliness_observer: Arc<dyn LivelinessObserverSync>,
) -> buck2_error::Result<Option<u64>> {
    match io
        .clean_invalidated_path(
            CleanInvalidatedPathRequest {
                path,
                liveliness_observer: liveliness_observer.dupe(),
            },
            cancellations,
        )
        .await
    {
        Ok(()) => Ok(Some(size)),
        Err(e) => {
            if e.has_tag(ErrorTag::CleanInterrupt) {
                Ok(None)
            } else {
                Err(e)
            }
        }
    }
}

pub struct CleanInvalidatedPathRequest {
    path: ProjectRelativePathBuf,
    pub(crate) liveliness_observer: Arc<dyn LivelinessObserverSync>,
}

impl IoRequest for CleanInvalidatedPathRequest {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
        if !self.liveliness_observer.is_alive_sync() {
            return Err(buck2_error!(ErrorTag::CleanInterrupt, "Interrupt"));
        }
        cleanup_path(project_fs, &self.path)?;
        Ok(())
    }
}

/// Get file size or directory size, without following symlinks
pub fn get_size(path: &AbsNormPath) -> buck2_error::Result<u64> {
    let mut result = 0;
    if path.is_dir() {
        for entry in fs_util::read_dir(path).categorize_internal()? {
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
    liveliness_observer: Arc<dyn LivelinessObserverSync>,
}

#[derive(Clone)]
enum FoundPath {
    /// Will be deleted on disk.
    Untracked(ProjectRelativePathBuf, FileType, u64),
    /// Tracked by the materializer. `state` decides what (if anything) we do with it.
    Tracked {
        path: ProjectRelativePathBuf,
        size: u64,
        state: TrackedState,
    },
}

#[derive(Clone, Copy)]
enum TrackedState {
    /// Will be invalidated in the materializer.
    Stale,
    /// Materialized, not-stale, and not active. Eligible to be promoted
    /// to `Stale` by `apply_adaptive_low_disk`.
    Retained { last_access_time: DateTime<Utc> },
    /// Materialized and is active. Must never be promoted/deleted.
    ActiveRetained,
}

impl<T: IoHandler> StaleFinder<'_, T> {
    /// Start from `path` and `subtree` and visit everything below.
    fn visit_recursively(
        &mut self,
        path: ProjectRelativePathBuf,
        subtree: &StdBuckHashMap<FileNameBuf, ArtifactTree>,
    ) -> buck2_error::Result<()> {
        let mut queue = vec![(path, subtree)];

        while let Some((path, tree)) = queue.pop() {
            if !self.liveliness_observer.is_alive_sync() {
                break;
            }
            self.visit(&path, tree, &mut queue)?;
        }

        Ok(())
    }

    /// Visit one directory.
    fn visit<'t>(
        &mut self,
        path: &ProjectRelativePath,
        subtree: &'t StdBuckHashMap<FileNameBuf, ArtifactTree>,
        queue: &mut Vec<(
            ProjectRelativePathBuf,
            &'t StdBuckHashMap<FileNameBuf, ArtifactTree>,
        )>,
    ) -> buck2_error::Result<()> {
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
                    self.found_paths.push(FoundPath::Tracked {
                        path,
                        size: artifact_metadata_size(metadata),
                        state: TrackedState::Stale,
                    });
                }
                ArtifactTree::Data(box ArtifactMaterializationData {
                    stage:
                        ArtifactMaterializationStage::Materialized {
                            active: false,
                            last_access_time,
                            metadata,
                        },
                    ..
                }) => {
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as retained");
                    self.found_paths.push(FoundPath::Tracked {
                        path,
                        size: artifact_metadata_size(metadata),
                        state: TrackedState::Retained {
                            last_access_time: *last_access_time,
                        },
                    });
                }
                ArtifactTree::Data(box ArtifactMaterializationData {
                    stage: ArtifactMaterializationStage::Materialized { metadata, .. },
                    ..
                }) => {
                    tracing::trace!(path = %path, file_type = ?file_type, "marking as active retained");
                    self.found_paths.push(FoundPath::Tracked {
                        path,
                        size: artifact_metadata_size(metadata),
                        state: TrackedState::ActiveRetained,
                    });
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
) -> buck2_error::Result<()> {
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
                found_paths.push(FoundPath::Tracked {
                    path,
                    size: 0,
                    state: TrackedState::Stale,
                });
            } else if *active {
                tracing::trace!(path = %path, "retaining artifact (active)");
                found_paths.push(FoundPath::Tracked {
                    path,
                    size: 0,
                    state: TrackedState::ActiveRetained,
                });
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                found_paths.push(FoundPath::Tracked {
                    path,
                    size: 0,
                    state: TrackedState::Retained {
                        last_access_time: *last_access_time,
                    },
                });
            }
        }
    }
    Ok(())
}

/// Promote retained, non-active artifacts to stale, oldest-access-first, until
/// projected free disk space rises above `threshold_percent` of `total_space`,
/// or every promotable artifact has been promoted. Artifacts last accessed at
/// or after `min_access_time` are excluded — i.e. they are protected by the
/// adaptive minimum TTL even when disk pressure remains.
fn apply_adaptive_low_disk(
    found_paths: &mut [FoundPath],
    free_space: u64,
    total_space: u64,
    threshold_percent: f64,
    min_access_time: DateTime<Utc>,
) {
    if total_space == 0 {
        return;
    }
    let free_pct = free_space as f64 / total_space as f64 * 100.0;
    if free_pct > threshold_percent {
        return;
    }

    let target_free = (threshold_percent / 100.0 * total_space as f64).ceil() as u64;
    let bytes_needed = target_free.saturating_sub(free_space);
    if bytes_needed == 0 {
        return;
    }

    let mut promotable: Vec<(usize, DateTime<Utc>, u64)> = found_paths
        .iter()
        .enumerate()
        .filter_map(|(i, p)| match p {
            FoundPath::Tracked {
                state: TrackedState::Retained { last_access_time },
                size,
                ..
            } if *last_access_time < min_access_time => Some((i, *last_access_time, *size)),
            _ => None,
        })
        .collect();
    promotable.sort_by_key(|(_, last_access_time, _)| *last_access_time);

    let mut accumulated: u64 = 0;
    for (i, _, size) in promotable {
        if accumulated >= bytes_needed {
            break;
        }
        if let FoundPath::Tracked { state, .. } = &mut found_paths[i] {
            *state = TrackedState::Stale;
        }
        accumulated = accumulated.saturating_add(size);
    }
}

pub struct CleanStaleConfig {
    // Time before running first clean, after daemon start
    pub start_offset: Duration,
    pub clean_period: Duration,
    pub artifact_ttl: Duration,
    pub dry_run: bool,
    pub low_disk: Option<LowDiskCleanConfig>,
}

/// Configures how the scheduled clean reacts to low free disk space.
#[derive(Debug, Clone)]
pub struct LowDiskCleanConfig {
    /// Free disk space (as a percentage of total) at or below which the
    /// `mode` engages.
    pub threshold_percent: f64,
    pub mode: LowDiskCleanMode,
}

#[derive(Debug, Clone)]
pub enum LowDiskCleanMode {
    /// Use this smaller TTL when free disk % is at/below the threshold.
    Fixed(Duration),
    /// Run the normal-TTL clean, then keep marking retained, non-active
    /// artifacts (oldest access first) as stale until projected free disk %
    /// rises back above the threshold or every eligible artifact has been
    /// marked. Retained artifacts younger than `min_ttl` are protected — they
    /// are never promoted, even when disk pressure persists.
    Adaptive { min_ttl: Duration },
}

impl CleanStaleConfig {
    pub fn from_buck_config(root_config: &LegacyBuckConfig) -> buck2_error::Result<Option<Self>> {
        let clean_stale_enabled = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_enabled",
            })?
            .unwrap_or(false);
        let clean_stale_artifact_ttl_hours = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_artifact_ttl_hours",
            })?
            .unwrap_or(24.0 * 7.0);
        let clean_stale_period_hours = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_period_hours",
            })?
            .unwrap_or(24.0);
        let clean_stale_start_offset_hours = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_start_offset_hours",
            })?
            .unwrap_or(12.0);
        let clean_stale_dry_run = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_dry_run",
            })?
            .unwrap_or(false);
        let adaptive_enabled = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_low_disk_adaptive_enabled",
            })?
            .unwrap_or(false);
        let adaptive_min_ttl_hours: f64 = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "clean_stale_low_disk_adaptive_min_ttl_hours",
            })?
            .unwrap_or(12.0);
        let adaptive_min_ttl = Duration::from_hours(1).mul_f64(adaptive_min_ttl_hours);
        let low_disk_artifact_ttl_hours: Option<f64> = root_config.parse(BuckconfigKeyRef {
            section: "buck2",
            property: "clean_stale_low_disk_artifact_ttl_hours",
        })?;
        let low_disk_mode = if adaptive_enabled {
            LowDiskCleanMode::Adaptive {
                min_ttl: adaptive_min_ttl,
            }
        } else {
            let hours = low_disk_artifact_ttl_hours.unwrap_or(48.0);
            LowDiskCleanMode::Fixed(Duration::from_hours(1).mul_f64(hours))
        };
        let low_disk_threshold_percent: Option<f64> = root_config.parse(BuckconfigKeyRef {
            section: "buck2",
            property: "clean_stale_low_disk_threshold",
        })?;
        let low_disk = low_disk_threshold_percent.map(|threshold_percent| LowDiskCleanConfig {
            threshold_percent,
            mode: low_disk_mode,
        });
        let clean_stale_config = if clean_stale_enabled {
            Some(Self {
                clean_period: Duration::from_hours(1).mul_f64(clean_stale_period_hours),
                artifact_ttl: Duration::from_hours(1).mul_f64(clean_stale_artifact_ttl_hours),
                start_offset: Duration::from_hours(1).mul_f64(clean_stale_start_offset_hours),
                low_disk,
                dry_run: clean_stale_dry_run,
            })
        } else {
            None
        };
        Ok(clean_stale_config)
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use chrono::DateTime;
    use chrono::TimeZone;
    use chrono::Utc;

    use crate::materializers::deferred::clean_stale::FoundPath;
    use crate::materializers::deferred::clean_stale::TrackedState;
    use crate::materializers::deferred::clean_stale::apply_adaptive_low_disk;

    fn t(secs: i64) -> DateTime<Utc> {
        Utc.timestamp_opt(secs, 0).unwrap()
    }

    fn retained(name: &str, last_access_secs: i64, size: u64) -> FoundPath {
        FoundPath::Tracked {
            path: ProjectRelativePathBuf::unchecked_new(name.to_owned()),
            size,
            state: TrackedState::Retained {
                last_access_time: t(last_access_secs),
            },
        }
    }

    fn active_retained(size: u64) -> FoundPath {
        FoundPath::Tracked {
            path: ProjectRelativePathBuf::unchecked_new("active".to_owned()),
            size,
            state: TrackedState::ActiveRetained,
        }
    }

    fn is_stale(p: &FoundPath, expected_size: u64) -> bool {
        matches!(
            p,
            FoundPath::Tracked {
                size,
                state: TrackedState::Stale,
                ..
            } if *size == expected_size
        )
    }

    fn is_retained(p: &FoundPath) -> bool {
        matches!(
            p,
            FoundPath::Tracked {
                state: TrackedState::Retained { .. },
                ..
            }
        )
    }

    fn is_active_retained(p: &FoundPath) -> bool {
        matches!(
            p,
            FoundPath::Tracked {
                state: TrackedState::ActiveRetained,
                ..
            }
        )
    }

    /// Sentinel `min_access_time` that protects no artifacts — every retained
    /// artifact has `last_access_time` strictly less than this far-future
    /// instant, so the full promotion logic is exercised end-to-end.
    fn no_min_ttl() -> DateTime<Utc> {
        DateTime::<Utc>::MAX_UTC
    }

    #[test]
    fn threshold_already_met_is_noop() {
        let mut paths = vec![retained("a", 100, 50)];
        // 60% free, threshold 50% -> already above, do nothing.
        apply_adaptive_low_disk(&mut paths, 60, 100, 50.0, no_min_ttl());
        assert!(
            is_retained(&paths[0]),
            "free disk (60%) already exceeds threshold (50%); nothing should be promoted",
        );
    }

    #[test]
    fn promotes_oldest_first_until_threshold_crossed() {
        // total=1000, free=100 (10%), threshold=50% -> need 400 bytes promoted.
        let mut paths = vec![
            retained("new", 300, 200),
            retained("oldest", 100, 100),
            retained("old", 200, 350),
        ];
        apply_adaptive_low_disk(&mut paths, 100, 1000, 50.0, no_min_ttl());
        // Oldest (t=100, size=100) + next (t=200, size=350) = 450 >= 400 bytes_needed.
        // Newest (t=300) is left alone — promotion stops as soon as the running total crosses bytes_needed.
        assert!(
            is_stale(&paths[1], 100),
            "oldest retained (size 100) should be promoted to stale first",
        );
        assert!(
            is_stale(&paths[2], 350),
            "second-oldest retained (size 350) should be promoted; running total 100+350=450 >= 400",
        );
        assert!(
            is_retained(&paths[0]),
            "newest retained (t=300) should remain retained — threshold already met after promoting the two older entries",
        );
    }

    #[test]
    fn all_retained_insufficient_promotes_all_non_active() {
        // total=1000, free=0, threshold=100% -> need everything we can spare.
        let mut paths = vec![
            retained("a", 100, 10),
            retained("b", 200, 20),
            active_retained(9999),
        ];
        apply_adaptive_low_disk(&mut paths, 0, 1000, 100.0, no_min_ttl());
        assert!(
            is_stale(&paths[0], 10),
            "non-active retained `a` should be promoted"
        );
        assert!(
            is_stale(&paths[1], 20),
            "non-active retained `b` should be promoted"
        );
        // Active retained MUST stay put — invariant: adaptive never deletes
        // more than `clean --keep-since-time=<future>` would.
        assert!(
            is_active_retained(&paths[2]),
            "active retained must never be promoted, even when the threshold cannot be met",
        );
    }

    #[test]
    fn mixed_inputs_only_touch_retained() {
        let mut paths = vec![
            FoundPath::Untracked(
                ProjectRelativePathBuf::unchecked_new("u".to_owned()),
                buck2_common::file_ops::metadata::FileType::File,
                42,
            ),
            FoundPath::Tracked {
                path: ProjectRelativePathBuf::unchecked_new("already_stale".to_owned()),
                size: 7,
                state: TrackedState::Stale,
            },
            retained("r", 100, 100),
            active_retained(50),
        ];
        apply_adaptive_low_disk(&mut paths, 0, 1000, 100.0, no_min_ttl());
        assert!(
            matches!(&paths[0], FoundPath::Untracked(_, _, 42)),
            "untracked entries should never be touched",
        );
        assert!(
            is_stale(&paths[1], 7),
            "already-stale entries should remain stale (and unchanged)",
        );
        assert!(
            is_stale(&paths[2], 100),
            "the lone retained entry should be promoted"
        );
        assert!(
            is_active_retained(&paths[3]),
            "active retained must never be promoted",
        );
    }

    #[test]
    fn zero_total_is_noop() {
        let mut paths = vec![retained("a", 100, 50)];
        apply_adaptive_low_disk(&mut paths, 0, 0, 100.0, no_min_ttl());
        assert!(
            is_retained(&paths[0]),
            "total_space=0 must short-circuit to a no-op (avoids divide-by-zero)",
        );
    }

    #[test]
    fn min_ttl_protects_recent_retained() {
        // Disk pressure is maximal (free=0, threshold=100%) so adaptive would
        // normally promote every retained, non-active artifact.
        // `min_access_time = t(150)` protects everything accessed at or after t=150.
        let mut paths = vec![
            retained("old", 100, 50),
            retained("on_boundary", 150, 50),
            retained("recent", 200, 50),
        ];
        apply_adaptive_low_disk(&mut paths, 0, 1000, 100.0, t(150));
        assert!(
            is_stale(&paths[0], 50),
            "retained accessed before min_access_time is eligible for promotion",
        );
        assert!(
            is_retained(&paths[1]),
            "retained whose last_access_time equals min_access_time is protected (cutoff is strict <)",
        );
        assert!(
            is_retained(&paths[2]),
            "retained accessed after min_access_time must be protected by the adaptive minimum TTL",
        );
    }

    #[test]
    fn min_ttl_can_block_all_promotions() {
        // Even with maximal disk pressure, a min_access_time at or below every
        // retained access time means every artifact is within the min-TTL
        // window — adaptive cleaning never violates the minimum TTL floor.
        let mut paths = vec![retained("a", 100, 100), retained("b", 200, 100)];
        apply_adaptive_low_disk(&mut paths, 0, 1000, 100.0, t(50));
        assert!(
            is_retained(&paths[0]) && is_retained(&paths[1]),
            "no retained artifact may be promoted when all are within the adaptive min-TTL window",
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::sync::Arc;
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
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
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
            let result: Result<CleanResult, buck2_error::Error> = result.map_err(|e| e.into());
            let result_event: buck2_data::CleanStaleResult = create_result(
                result.clone(),
                trace_id,
                &daemon_id,
                (Instant::now() - start_time).as_secs(),
            );
            dispatcher_dup.instant_event(result_event);
            Ok(result?.into())
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
        let gen_path = io
            .buck_out_path()
            .join(ProjectRelativePathBuf::unchecked_new("gen".to_owned()));
        let gen_dir = io.fs().resolve(&gen_path);
        if !fs_util::try_exists(&gen_dir)? {
            return Ok(CleanStaleResultKind::SkippedNoGenDir.into());
        }
        tracing::trace!(gen_dir = %gen_dir, "Scanning");

        let mut found_paths = Vec::new();
        if self.tracked_only {
            find_stale_tracked_only(tree, self.keep_since_time, &mut found_paths)?
        } else {
            let gen_subtree = tree
                .get_subtree(&mut gen_path.iter())
                .buck_error_context("Found a file where gen dir expected")?;

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
                keep_since_time: self.keep_since_time,
                found_paths: &mut found_paths,
                liveliness_observer: liveliness_observer.clone(),
            }
            .visit_recursively(gen_path, gen_subtree)?;
        };

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
                return Err(soft_error!("clean_stale_error", error.into(), quiet: true)
                    .map(|e| e.into())?);
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
            FoundPath::Stale(p, ..) => Some(p.clone()),
            _ => None,
        })
        .collect();

    let existing_clean_futs =
        tree.invalidate_paths_and_collect_futures(paths_to_invalidate, Some(sqlite_db))?;
    let mut existing_materialization_futs = vec![];
    for data in tree.iter_without_paths() {
        match &data.processing {
            super::Processing::Active {
                future: super::ProcessingFuture::Materializing(future),
                ..
            } => existing_materialization_futs.push(future.clone()),
            _ => (),
        };
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
                    FoundPath::Stale(p, size) => Some((p, size)),
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
            let e: buck2_error::Error = e.into();
            if e.has_tag(ErrorTag::CleanInterrupt) {
                Ok(None)
            } else {
                Err(e.into())
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
            return Err(buck2_error!(ErrorTag::CleanInterrupt, "Interrupt").into());
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
    /// These will be deleted on disk.
    Untracked(ProjectRelativePathBuf, FileType, u64),
    /// These will be invalidated in the materiaizer.
    Stale(ProjectRelativePathBuf, u64),
    Retained(u64),
}

impl<T: IoHandler> StaleFinder<'_, T> {
    /// Start from `path` and `subtree` and visit everything below.
    fn visit_recursively(
        &mut self,
        path: ProjectRelativePathBuf,
        subtree: &HashMap<FileNameBuf, ArtifactTree>,
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
        subtree: &'t HashMap<FileNameBuf, ArtifactTree>,
        queue: &mut Vec<(
            ProjectRelativePathBuf,
            &'t HashMap<FileNameBuf, ArtifactTree>,
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
                found_paths.push(FoundPath::Stale(path, 0));
            } else {
                tracing::trace!(path = %path, "retaining artifact");
                found_paths.push(FoundPath::Retained(0));
            }
        }
    }
    Ok(())
}

pub struct CleanStaleConfig {
    // Time before running first clean, after daemon start
    pub start_offset: std::time::Duration,
    pub clean_period: std::time::Duration,
    pub artifact_ttl: std::time::Duration,
    pub dry_run: bool,
    pub decreased_ttl_hours: Option<std::time::Duration>,
    pub decreased_ttl_hours_disk_threshold: Option<f64>,
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
        let decreased_ttl_hours: Option<f64> = root_config.parse(BuckconfigKeyRef {
            section: "buck2",
            property: "clean_stale_low_disk_artifact_ttl_hours",
        })?;
        let decreased_ttl_hours_disk_threshold = root_config.parse(BuckconfigKeyRef {
            section: "buck2",
            property: "clean_stale_low_disk_threshold",
        })?;

        let secs_in_hour = 60.0 * 60.0;
        let clean_stale_config = if clean_stale_enabled {
            Some(Self {
                clean_period: std::time::Duration::from_secs_f64(
                    secs_in_hour * clean_stale_period_hours,
                ),
                artifact_ttl: std::time::Duration::from_secs_f64(
                    secs_in_hour * clean_stale_artifact_ttl_hours,
                ),
                start_offset: std::time::Duration::from_secs_f64(
                    secs_in_hour * clean_stale_start_offset_hours,
                ),
                decreased_ttl_hours: decreased_ttl_hours
                    .map(|hours| std::time::Duration::from_secs_f64(secs_in_hour * hours)),
                decreased_ttl_hours_disk_threshold,
                dry_run: clean_stale_dry_run,
            })
        } else {
            None
        };
        Ok(clean_stale_config)
    }
}

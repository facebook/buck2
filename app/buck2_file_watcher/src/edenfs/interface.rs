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
use std::collections::HashSet;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::dice::FileChangeTracker;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::cells::CellResolver;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_data::FileWatcherEventType as Type;
use buck2_data::FileWatcherKind as Kind;
use buck2_eden::connection::EdenConnectionManager;
use buck2_eden::semaphore;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::span_async;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use dice::DiceTransactionUpdater;
use edenfs::ChangeNotification;
use edenfs::ChangesSinceV2Params;
use edenfs::Dtype;
use edenfs::JournalPosition;
use edenfs::LargeChangeNotification;
use edenfs::SmallChangeNotification;
use fbinit::FacebookInit;
use tokio::sync::RwLock;
use tracing::debug;
use tracing::info;

use crate::edenfs::sapling::MergebaseDetails;
use crate::edenfs::sapling::SaplingGetStatusResult;
use crate::edenfs::sapling::SaplingStatus;
use crate::edenfs::sapling::get_dir_diff;
use crate::edenfs::sapling::get_mergebase;
use crate::edenfs::sapling::get_status;
use crate::edenfs::utils::bytes_to_string_or_unknown;
use crate::edenfs::utils::dtype_into_file_watcher_kind;
use crate::edenfs::utils::find_first_valid_parent;
use crate::file_watcher::FileWatcher;
use crate::mergebase::Mergebase;
use crate::stats::FileWatcherStats;

const MAX_SAPLING_STATUS_CHANGES: usize = 10_000;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum EdenFsWatcherError {
    #[buck2(tag = IoNotConnected)]
    #[error("Failed to connect to EdenFS {0}")]
    EdenConnectionError(buck2_error::Error),
    #[buck2(tag = Input)]
    #[error("Eden mount point is not absolute normalized path")]
    NotAbsNormPath,
}

#[derive(PartialEq)]
enum ProcessChangeStatus {
    Processed,
    LargeOrUnknown,
}

#[derive(Eq, PartialEq, Hash)]
struct EdenFsEvent {
    event_type: Type,
    event_kind: Kind,
    path: String,
}

#[derive(Allocative)]
pub(crate) struct EdenFsFileWatcher {
    manager: EdenConnectionManager,
    mount_point: Vec<u8>,
    eden_root: AbsNormPathBuf,
    #[allocative(skip)]
    position: RwLock<JournalPosition>,
    cells: CellResolver,
    // The project root, relative to the eden mount point
    project_root: ForwardRelativePathBuf,
    ignore_specs: HashMap<CellName, IgnoreSet>,
    mergebase: RwLock<Option<MergebaseDetails>>,
    last_mergebase: RwLock<Option<MergebaseDetails>>,
    mergebase_with: Option<String>,
    eden_version: RwLock<Option<String>>,
}

impl EdenFsFileWatcher {
    pub(crate) fn new(
        fb: FacebookInit,
        project_root: &ProjectRoot,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> Result<Self, EdenFsWatcherError> {
        let manager = EdenConnectionManager::new(
            fb,
            project_root,
            Some(semaphore::buck2_default()),
        )
        .map_err(EdenFsWatcherError::EdenConnectionError)?
        .ok_or(EdenFsWatcherError::EdenConnectionError(
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Environment,
                "Couldn't initiate connection to Eden. This is usually due to .eden dir missing"
            ),
        ))?;

        let mount_point = manager.get_mount_point();
        let eden_root = AbsNormPath::new(manager.get_mount_point_path())
            .map_err(|_| EdenFsWatcherError::NotAbsNormPath)?
            .to_owned();
        let project_root = manager.get_proj_relative_path().to_owned();

        let mergebase_with = root_config
            .get(BuckconfigKeyRef {
                section: "project",
                property: "watchman_merge_base",
            })
            .map(|s| s.to_owned());

        Ok(Self {
            manager,
            mount_point,
            eden_root,
            position: RwLock::new(JournalPosition::default()),
            cells,
            project_root,
            ignore_specs,
            mergebase: RwLock::new(None),
            last_mergebase: RwLock::new(None),
            mergebase_with,
            eden_version: RwLock::new(None),
        })
    }

    async fn update(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, DiceTransactionUpdater)> {
        let position = self.position.read().await.clone();
        let changes_since_v2_params = ChangesSinceV2Params {
            mountPoint: self.mount_point.clone(),
            fromPosition: position,
            includeVCSRoots: Some(false),
            includedRoots: None,
            excludedRoots: None,
            ..Default::default()
        };
        let result = self
            .manager
            .with_eden(|eden| eden.changesSinceV2(&changes_since_v2_params))
            .await
            .buck_error_context("Failed to query EdenFS for changes since last position.")?;
        let mut position = self.position.write().await;
        *position = result.toPosition;

        let mut file_change_tracker = FileChangeTracker::new();
        let base_stats = self.base_file_watcher_stats().await?;
        let mut stats = FileWatcherStats::new(base_stats, result.changes.len());
        let mut large_or_unknown_change = false;
        // EdenFS Sometimes emits duplicated notifications
        // This can happen as we receive file changes from a commit transitions and from an explicit notification.
        // Also eden will report duplicates if there were another changes between changes on the same file.
        // We want to ignore duplicates, so we store unique changes in the set
        let mut processed_changes: HashSet<EdenFsEvent> = HashSet::new();
        for change in result.changes {
            // Once a large or unknown change is detected, we need to invalidate DICE. Therefore,
            // skip processing the rest of the changes and continue to propagate true.
            if large_or_unknown_change {
                self.skip_change(&change).await?;
            } else {
                large_or_unknown_change = self
                    .process_change(
                        &change,
                        &mut file_change_tracker,
                        &mut stats,
                        &mut processed_changes,
                    )
                    .await?
                    == ProcessChangeStatus::LargeOrUnknown;
            }
        }

        let mut dice = dice;
        if large_or_unknown_change {
            (stats, file_change_tracker, dice) = self
                .on_large_or_unknown_change(dice)
                .await
                .buck_error_context("Failed to handle large or unknown change.")?;
        }

        file_change_tracker.write_to_dice(&mut dice)?;
        Ok((stats.finish(), dice))
    }

    async fn process_change(
        &self,
        change: &ChangeNotification,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<ProcessChangeStatus> {
        let large_or_unknown_change = match change {
            ChangeNotification::smallChange(small_change) => match small_change {
                SmallChangeNotification::added(added) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(added.fileType),
                        Type::Create,
                        &added.path,
                        processed_changes,
                    )?;
                    ProcessChangeStatus::Processed
                }
                SmallChangeNotification::modified(modified) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(modified.fileType),
                        Type::Modify,
                        &modified.path,
                        processed_changes,
                    )?;
                    ProcessChangeStatus::Processed
                }
                SmallChangeNotification::renamed(renamed) => {
                    if renamed.fileType == Dtype::DIR {
                        soft_error!(
                            "edenfs_small_change_dir_rename",
                            buck2_error::buck2_error!(
                                buck2_error::ErrorTag::Environment,
                                "EdenFS reported SmallChangeNotification::renamed directory: '{}' -> '{}'. \
                                 Directory renames are handled as LargeChangeNotification changes. \
                                 EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                                 bytes_to_string_or_unknown(&renamed.from),
                                 bytes_to_string_or_unknown(&renamed.to)
                            )
                            .into()
                        )?;
                    } else {
                        let kind = dtype_into_file_watcher_kind(renamed.fileType);
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Create,
                            &renamed.to,
                            processed_changes,
                        )?;
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Delete,
                            &renamed.from,
                            processed_changes,
                        )?;
                    }
                    ProcessChangeStatus::Processed
                }
                SmallChangeNotification::replaced(replaced) => {
                    if replaced.fileType == Dtype::DIR {
                        // The only case when it could happen is if newname exists and
                        // is an empty directory, it is removed, and oldname is renamed to newname.
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::Directory,
                            Type::Create,
                            &replaced.to,
                            processed_changes,
                        )?;
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::Directory,
                            Type::Delete,
                            &replaced.from,
                            processed_changes,
                        )?;
                    } else {
                        let kind = dtype_into_file_watcher_kind(replaced.fileType);
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Create,
                            &replaced.to,
                            processed_changes,
                        )?;
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Delete,
                            &replaced.from,
                            processed_changes,
                        )?;
                    }
                    ProcessChangeStatus::Processed
                }
                SmallChangeNotification::removed(removed) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(removed.fileType),
                        Type::Delete,
                        &removed.path,
                        processed_changes,
                    )?;
                    ProcessChangeStatus::Processed
                }
                SmallChangeNotification::UnknownField(_) => {
                    soft_error!(
                        "edenfs_small_change_unknown",
                        buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Environment,
                            "EdenFS reported an unknown SmallChangeNotification: '{:?}'. \
                             EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                            small_change
                        )
                        .into()
                    )?;
                    ProcessChangeStatus::LargeOrUnknown
                }
            },
            ChangeNotification::largeChange(large_change) => match large_change {
                LargeChangeNotification::directoryRenamed(directory_renamed) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        Kind::Directory,
                        Type::Create,
                        &directory_renamed.to,
                        processed_changes,
                    )?;
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        Kind::Directory,
                        Type::Delete,
                        &directory_renamed.from,
                        processed_changes,
                    )?;
                    // NOTE: even though a directory rename is a large change,
                    // we handle by reporting two small changes to DICE.
                    // Return Processed here indicating no large change.
                    ProcessChangeStatus::Processed
                }
                LargeChangeNotification::commitTransition(commit_transition) => {
                    let from = hex::encode(&commit_transition.from);
                    let to = hex::encode(&commit_transition.to);
                    self.process_commit_transition(tracker, stats, &from, &to, processed_changes)
                        .await
                        .buck_error_context("Failed to process commit transition.")?
                }
                LargeChangeNotification::lostChanges(_lost_changes) => {
                    let current_rev = ".";
                    self.update_mergebase(current_rev)
                        .await
                        .buck_error_context("Failed to update mergebase.")?;
                    // Return LargeOrUnknown indicating a large change (i.e. invalidate DICE).
                    ProcessChangeStatus::LargeOrUnknown
                }
                LargeChangeNotification::UnknownField(_) => {
                    soft_error!(
                        "edenfs_large_change_unknown",
                        buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Environment,
                            "EdenFS reported an unknown LargeChangeNotification: '{:?}'. \
                             EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                            large_change
                        )
                        .into()
                    )?;
                    ProcessChangeStatus::LargeOrUnknown
                }
            },
            ChangeNotification::stateChange(_) => {
                // Ignored
                ProcessChangeStatus::Processed
            }
            ChangeNotification::UnknownField(_) => {
                soft_error!(
                    "edenfs_change_unknown",
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Environment,
                        "EdenFS reported an unknown ChangeNotification: '{:?}'. \
                         EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                        change
                    )
                    .into()
                )?;
                ProcessChangeStatus::LargeOrUnknown
            }
        };

        Ok(large_or_unknown_change)
    }

    // Skip processing changes after a large or unknown change. Commit
    // transitions still need to be "processed" to update the mergebase.
    async fn skip_change(&self, change: &ChangeNotification) -> buck2_error::Result<()> {
        if let ChangeNotification::largeChange(LargeChangeNotification::commitTransition(
            commit_transition,
        )) = change
        {
            let to = hex::encode(&commit_transition.to);
            self.update_mergebase(&to)
                .await
                .buck_error_context("Failed to update mergebase.")?;
        }

        Ok(())
    }

    fn process_file_watcher_event(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        kind: Kind,
        event: Type,
        path: &[u8],
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<()> {
        let eden_rel_path = PathBuf::from(str::from_utf8(path)?);

        // If the path is invalid, then walk up all the way until you find a valid dir to
        // invalidate listings. We don't need to invalidate the file itself, as we can't
        // read invalid files.

        let (eden_rel_path, kind, event) = match ForwardRelativePath::new(&eden_rel_path) {
            Ok(path) => (path, kind, event),
            Err(_) => {
                // If we error out here then we might miss other changes. This seems like
                // it shouldn't happen, since the empty path should always be a valid path.
                let path = find_first_valid_parent(&eden_rel_path)
                    .ok_or_else(|| {
                        internal_error!(
                            "Invalid path had no valid parent: `{}`",
                            eden_rel_path.display()
                        )
                    })
                    .unwrap();

                // Create a synthetic directory change event.
                (path, Kind::Directory, Type::Modify)
            }
        };

        let project_rel_path = match eden_rel_path.strip_prefix(&self.project_root) {
            Ok(path) => <&ProjectRelativePath>::from(path),
            // we ignore any changes that are not relative to the project root
            Err(_) => return Ok(()),
        };
        let cell_path = self.cells.get_cell_path(project_rel_path);

        let ignore = self
            .ignore_specs
            .get(&cell_path.cell())
            // This shouldn't ever really happen. However, because of the bugs caused by just
            // storing the `CellResolver` in the watcher permanently, sometimes it can, so we just
            // default to not ignoring the file in that case
            .is_some_and(|ignore| ignore.is_match(cell_path.path()));

        info!("EdenFS: {:?} (ignore = {})", cell_path, ignore);

        if ignore {
            stats.add_ignored(1);
        } else if processed_changes.insert(EdenFsEvent {
            event_type: event,
            event_kind: kind,
            path: cell_path.to_string(),
        }) {
            stats.add(cell_path.to_string(), event, kind);

            match (event, kind) {
                (Type::Create, Kind::Directory) => tracker.dir_added_or_removed(cell_path),
                (Type::Create, _) => {
                    if kind == Kind::Symlink {
                        debug!(
                            "New symlink detected (source symlinks are not supported): {}",
                            cell_path
                        );
                    }
                    tracker.file_added_or_removed(cell_path)
                }
                (Type::Modify, Kind::Directory) => {
                    // FIXME(JakobDegen): This should not be needed
                    tracker.dir_entries_changed_force_invalidate(cell_path)
                }
                (Type::Modify, _) => tracker.file_contents_changed(cell_path),
                (Type::Delete, Kind::Directory) => tracker.dir_added_or_removed(cell_path),
                (Type::Delete, _) => tracker.file_added_or_removed(cell_path),
            };
        }

        Ok(())
    }

    async fn process_source_control_changes(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        from: &str,
        to: Option<&str>,
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<ProcessChangeStatus> {
        // `sl status` only reports added/removed/modified files, not directories.
        // we use `sl debugdiffdirs` to get changes for directories
        if self
            .process_sapling_status(tracker, stats, from, to, processed_changes)
            .await?
            == ProcessChangeStatus::LargeOrUnknown
        {
            return Ok(ProcessChangeStatus::LargeOrUnknown);
        }

        if self
            .process_sapling_diffdirs(tracker, stats, from, to, processed_changes)
            .await?
            == ProcessChangeStatus::LargeOrUnknown
        {
            return Ok(ProcessChangeStatus::LargeOrUnknown);
        }

        Ok(ProcessChangeStatus::Processed)
    }

    async fn process_sapling_status(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        from: &str,
        to: Option<&str>,
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<ProcessChangeStatus> {
        // limit results to MAX_SAPLING_STATUS_CHANGES
        match get_status(&self.eden_root, &from, to, MAX_SAPLING_STATUS_CHANGES)
            .await
            .buck_error_context("Failed to get Sapling status.")?
        {
            SaplingGetStatusResult::TooManyChanges => Ok(ProcessChangeStatus::LargeOrUnknown),
            SaplingGetStatusResult::Normal(status) => {
                // Process statuses - if any fail, will terminate early.
                let results: buck2_error::Result<Vec<_>> = status
                    .into_iter()
                    .map(|(change, path)| match change {
                        SaplingStatus::Added
                        | SaplingStatus::NotTracked
                        | SaplingStatus::Copied
                        | SaplingStatus::Ignored => self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::File,
                            Type::Create,
                            path.as_bytes(),
                            processed_changes,
                        ),
                        SaplingStatus::Modified => self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::File,
                            Type::Modify,
                            path.as_bytes(),
                            processed_changes,
                        ),
                        SaplingStatus::Removed | SaplingStatus::Missing | SaplingStatus::Clean => {
                            self.process_file_watcher_event(
                                tracker,
                                stats,
                                Kind::File,
                                Type::Delete,
                                path.as_bytes(),
                                processed_changes,
                            )
                        }
                    })
                    .collect();

                // Return false indicating no large change.
                results
                    .map(|_| ProcessChangeStatus::Processed)
                    .buck_error_context("Failed to process Sapling statuses.")
            }
        }
    }

    async fn process_sapling_diffdirs(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        from: &str,
        to: Option<&str>,
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<ProcessChangeStatus> {
        // limit results to MAX_SAPLING_STATUS_CHANGES
        match get_dir_diff(&self.eden_root, &from, to, MAX_SAPLING_STATUS_CHANGES)
            .await
            .buck_error_context("Failed to get Sapling debugdiffdirs.")?
        {
            SaplingGetStatusResult::TooManyChanges => Ok(ProcessChangeStatus::LargeOrUnknown),
            SaplingGetStatusResult::Normal(status) => {
                // Process statuses - if any fail, will terminate early.
                let results: buck2_error::Result<Vec<_>> = status
                    .into_iter()
                    .map(|(change, path)| match change {
                        SaplingStatus::Added
                        | SaplingStatus::NotTracked
                        | SaplingStatus::Copied
                        | SaplingStatus::Ignored => self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::Directory,
                            Type::Create,
                            path.as_bytes(),
                            processed_changes,
                        ),
                        SaplingStatus::Modified => self.process_file_watcher_event(
                            tracker,
                            stats,
                            Kind::Directory,
                            Type::Modify,
                            path.as_bytes(),
                            processed_changes,
                        ),
                        SaplingStatus::Removed | SaplingStatus::Missing | SaplingStatus::Clean => {
                            self.process_file_watcher_event(
                                tracker,
                                stats,
                                Kind::Directory,
                                Type::Delete,
                                path.as_bytes(),
                                processed_changes,
                            )
                        }
                    })
                    .collect();

                // Return false indicating no large change.
                results
                    .map(|_| ProcessChangeStatus::Processed)
                    .buck_error_context("Failed to process Sapling dir diffs.")
            }
        }
    }

    // Process commit transitions. Compute the actual changes using the
    // mergebase and mergebase-with using Sapling. In cases where mergebase
    // has changed, we need to invalidate DICE. In cases where it has not
    // changed, update the tracker and stats.
    async fn process_commit_transition(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        from: &str,
        to: &str,
        processed_changes: &mut HashSet<EdenFsEvent>,
    ) -> buck2_error::Result<ProcessChangeStatus> {
        if self
            .update_mergebase(&to)
            .await
            .buck_error_context("Failed to update mergebase.")?
        {
            // Mergebase has changed - invalidate DICE.
            Ok(ProcessChangeStatus::LargeOrUnknown)
        } else {
            // Mergebase has not changed - compute changes form source control
            self.process_source_control_changes(tracker, stats, &from, Some(to), processed_changes)
                .await
        }
    }

    // Handle large or unknown changes other than commit transitions.
    async fn on_large_or_unknown_change(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(FileWatcherStats, FileChangeTracker, DiceTransactionUpdater)> {
        // A large change is one that affects numerous files or is otherwise unbounded in nature.
        // For example:
        // - A commit transition (e.g. a rebase, checkout, etc.).
        // - A directory rename - a directory was renamed. Depending on the directory,
        //   could be a large number of files.
        // - Lost changes - EdenFS was unable to provide the list of changes due to a
        //   remount, restart, memory pressure, or too many files were changed.
        //
        // In the case of lost changes, we need to treat the DICE map and dep files as
        // invalid because we have no way of knowing which files were changed.
        //
        // In the case of a directory rename, we handle this earlier by reporting two
        // small changes to DICE - delete and add. It may be needed to enumerate all
        // of the related files, in which case we will need to use Sapling to obtain.
        //

        // Invalidate everything - including the dep files - and recompute everything.
        crate::dep_files::flush_non_local_dep_files();

        // Dropping the entire DICE map can be somewhat computationally expensive as there
        // are a lot of destructors to run. On the other hand, we don't have to wait for
        // it. So, we just send it off to its own thread.
        let dice = dice.unstable_take();

        // Get mergebase state
        let last_mergebase_info = self.last_mergebase.read().await.clone();
        let mergebase_info = self.mergebase.read().await.clone();
        let base_stats = self.base_file_watcher_stats().await?;
        let mut base_stats = buck2_data::FileWatcherStats {
            fresh_instance: true,
            watchman_version: None,
            fresh_instance_data: Some(buck2_data::FreshInstance {
                new_mergebase: last_mergebase_info.is_none()
                    || last_mergebase_info != mergebase_info,
                cleared_dice: true,
                cleared_dep_files: true,
            }),
            ..base_stats
        };

        if let Some(mergebase) = mergebase_info.map(|m| m.mergebase) {
            let mut tracker = FileChangeTracker::new();
            let mut stats = FileWatcherStats::new(base_stats, 0);
            let mut processed_changes: HashSet<EdenFsEvent> = HashSet::new();
            self.process_source_control_changes(
                &mut tracker,
                &mut stats,
                &mergebase,
                None,
                &mut processed_changes,
            )
            .await?;
            Ok((stats, tracker, dice))
        } else {
            base_stats.incomplete_events_reason = Some("Large or Unknown change".to_owned());
            Ok((
                FileWatcherStats::new(base_stats, 0),
                FileChangeTracker::new(),
                dice,
            ))
        }
    }

    // Compute and update the mergebase.
    async fn update_mergebase(&self, to: &str) -> buck2_error::Result<bool> {
        if let Some(mergebase_with) = &self.mergebase_with {
            // Compute new mergebase.
            let mergebase = get_mergebase(&self.eden_root, &to, mergebase_with)
                .await
                .buck_error_context("Failed to get mergebase")?;
            let last_mergebase = self.mergebase.read().await.clone();

            // Update mergebases
            {
                let mut last_mergebase_write_lock = self.last_mergebase.write().await;
                let mut mergebase_write_lock = self.mergebase.write().await;
                *last_mergebase_write_lock = last_mergebase.clone();
                *mergebase_write_lock = mergebase.clone();
            }

            // Return true indicating a large change (i.e. invalidate DICE).
            Ok(last_mergebase.is_none() || last_mergebase != mergebase)
        } else {
            // If we have no mergebase_with, return true
            // indicating a large change (i.e. invalidate DICE).
            Ok(true)
        }
    }

    async fn base_file_watcher_stats(&self) -> buck2_error::Result<buck2_data::FileWatcherStats> {
        let eden_version = { self.eden_version.read().await.clone() };
        let eden_version = if eden_version.is_some() {
            eden_version.clone()
        } else {
            let eden_version = self.manager.get_eden_version().await?;
            let mut eden_version_lock = self.eden_version.write().await;
            *eden_version_lock = eden_version.clone();
            eden_version
        };
        let mergebase_info = self.mergebase.read().await.clone();
        let mergebase = mergebase_info.as_ref().map(|m| m.mergebase.clone());
        let branched_from_revision_timestamp = mergebase_info.as_ref().and_then(|m| m.timestamp);
        let branched_from_global_rev = mergebase_info.as_ref().and_then(|m| m.global_rev);
        Ok(buck2_data::FileWatcherStats {
            branched_from_revision: mergebase,
            branched_from_global_rev,
            branched_from_revision_timestamp,
            eden_version,
            ..Default::default()
        })
    }
}

#[async_trait]
impl FileWatcher for EdenFsFileWatcher {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(DiceTransactionUpdater, Mergebase)> {
        span_async(
            buck2_data::FileWatcherStart {
                provider: buck2_data::FileWatcherProvider::EdenFs as i32,
            },
            async {
                let (stats, res) = match self.update(dice).await {
                    Ok((stats, dice)) => {
                        let mergebase = Mergebase(Arc::new(stats.branched_from_revision.clone()));
                        ((Some(stats)), Ok((dice, mergebase)))
                    }
                    Err(e) => (None, Err(e)),
                };
                (res, buck2_data::FileWatcherEnd { stats })
            },
        )
        .await
    }
}

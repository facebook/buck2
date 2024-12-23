/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;
use std::str;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::file_ops::FileChangeTracker;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::buck2_env;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_data::FileWatcherEventType as Type;
use buck2_data::FileWatcherKind as Kind;
use buck2_eden::connection::EdenConnectionManager;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async;
use dice::DiceTransactionUpdater;
use edenfs::ChangeNotification;
use edenfs::ChangesSinceV2Params;
use edenfs::Dtype;
use edenfs::JournalPosition;
use edenfs::LargeChangeNotification;
use edenfs::SmallChangeNotification;
use fbinit::FacebookInit;
use tokio::sync::RwLock;
use tokio::sync::Semaphore;
use tracing::info;
use tracing::warn;

use crate::edenfs::sapling::get_mergebase;
use crate::edenfs::sapling::get_status;
use crate::edenfs::sapling::SaplingGetStatusResult;
use crate::edenfs::sapling::SaplingStatus;
use crate::edenfs::utils::bytes_to_string_or_unknown;
use crate::edenfs::utils::dtype_into_file_watcher_kind;
use crate::file_watcher::FileWatcher;
use crate::mergebase::Mergebase;
use crate::stats::FileWatcherStats;
use crate::utils::find_first_valid_parent;

#[derive(Allocative)]
pub(crate) struct EdenFsFileWatcher {
    manager: EdenConnectionManager,
    mount_point: Vec<u8>,
    root: String,
    #[allocative(skip)]
    position: RwLock<JournalPosition>,
    cells: CellResolver,
    ignore_specs: HashMap<CellName, IgnoreSet>,
    mergebase: RwLock<Option<String>>,
    last_mergebase: RwLock<Option<String>>,
    mergebase_with: Option<String>,
}

impl EdenFsFileWatcher {
    pub(crate) fn new(
        fb: FacebookInit,
        project_root: &ProjectRoot,
        root_config: &LegacyBuckConfig,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
        let eden_semaphore =
            buck2_env!("BUCK2_EDEN_SEMAPHORE", type=usize, default=2048, applicability=internal)?;

        let manager = EdenConnectionManager::new(fb, project_root, Semaphore::new(eden_semaphore))?
            .expect("Failed to connect to EdenFS");
        let mount_point = manager.get_mount_point();
        let root = String::from_utf8(mount_point.clone())
            .buck_error_context("Failed to convert mount point to string.")?;

        let mergebase_with = root_config
            .get(BuckconfigKeyRef {
                section: "project",
                property: "watchman_merge_base",
            })
            .map(|s| s.to_owned());

        Ok(Self {
            manager,
            mount_point,
            root,
            position: RwLock::new(JournalPosition::default()),
            cells,
            ignore_specs,
            mergebase: RwLock::new(None),
            last_mergebase: RwLock::new(None),
            mergebase_with,
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
        let mut stats = FileWatcherStats::new(Default::default(), result.changes.len());
        let mut large_or_unknown_change = false;
        for change in result.changes {
            // Once a large or unknown change is detected, we need to invalidate DICE. Therefore,
            // skip processing the rest of the changes and continue to propagate true.
            if large_or_unknown_change {
                self.skip_change(&change).await?;
            } else {
                large_or_unknown_change = self
                    .process_change(&change, &mut file_change_tracker, &mut stats)
                    .await?;
            }
        }

        let mut dice = dice;
        if large_or_unknown_change {
            (stats, dice) = self
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
    ) -> buck2_error::Result<bool> {
        let large_or_unknown_change = match change {
            ChangeNotification::smallChange(small_change) => match small_change {
                SmallChangeNotification::added(added) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(added.fileType),
                        Type::Create,
                        &added.path,
                    )?;
                    false
                }
                SmallChangeNotification::modified(modified) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(modified.fileType),
                        Type::Modify,
                        &modified.path,
                    )?;
                    false
                }
                SmallChangeNotification::renamed(renamed) => {
                    if renamed.fileType == Dtype::DIR {
                        soft_error!(
                            "edenfs_small_change_dir_rename",
                            buck2_error::buck2_error!(
                                [],
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
                        )?;
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Delete,
                            &renamed.from,
                        )?;
                    }
                    false
                }
                SmallChangeNotification::replaced(replaced) => {
                    if replaced.fileType == Dtype::DIR {
                        soft_error!(
                            "edenfs_small_change_dir_replace",
                            buck2_error::buck2_error!(
                                [],
                                "EdenFS reported SmallChangeNotification::replaced directory: '{}' -> '{}'. \
                                 Directory cannot be replaced (e.g. moving an a dir over an existing dir). \
                                 EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                                 bytes_to_string_or_unknown(&replaced.from),
                                 bytes_to_string_or_unknown(&replaced.to)
                            )
                            .into()
                        )?;
                    } else {
                        let kind = dtype_into_file_watcher_kind(replaced.fileType);
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Create,
                            &replaced.to,
                        )?;
                        self.process_file_watcher_event(
                            tracker,
                            stats,
                            kind,
                            Type::Delete,
                            &replaced.from,
                        )?;
                    }
                    false
                }
                SmallChangeNotification::removed(removed) => {
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        dtype_into_file_watcher_kind(removed.fileType),
                        Type::Delete,
                        &removed.path,
                    )?;
                    false
                }
                SmallChangeNotification::UnknownField(_) => {
                    soft_error!(
                        "edenfs_small_change_unknown",
                        buck2_error::buck2_error!(
                            [],
                            "EdenFS reported an unknown SmallChangeNotification: '{:?}'. \
                             EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                            small_change
                        )
                        .into()
                    )?;
                    true
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
                    )?;
                    self.process_file_watcher_event(
                        tracker,
                        stats,
                        Kind::Directory,
                        Type::Delete,
                        &directory_renamed.from,
                    )?;
                    // NOTE: even though a directory rename is a large change,
                    // we handle by reporting two small changes to DICE.
                    // Return false here indicating no large change.
                    false
                }
                LargeChangeNotification::commitTransition(commit_transition) => {
                    // TODO: temporary. There is a bug in EdenFS that causes it to report
                    //      the to as from and the from as to. This is a temporary
                    //      and will be removed once the bug is fixed and fully propogated.
                    // NOTE: builds should still work, but the mergebase will be wrong.
                    let from = hex::encode(&commit_transition.to);
                    let to = hex::encode(&commit_transition.from);
                    self.process_commit_transition(tracker, stats, from, to)
                        .await
                        .buck_error_context("Failed to process commit transition.")?
                }
                LargeChangeNotification::lostChanges(_lost_changes) => {
                    // Return true indicating a large change (i.e. invalidate DICE).
                    true
                }
                LargeChangeNotification::UnknownField(_) => {
                    soft_error!(
                        "edenfs_large_change_unknown",
                        buck2_error::buck2_error!(
                            [],
                            "EdenFS reported an unknown LargeChangeNotification: '{:?}'. \
                             EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                            large_change
                        )
                        .into()
                    )?;
                    true
                }
            },
            ChangeNotification::UnknownField(_) => {
                soft_error!(
                    "edenfs_change_unknown",
                    buck2_error::buck2_error!(
                        [],
                        "EdenFS reported an unknown ChangeNotification: '{:?}'. \
                         EdenFS Thrift API has changed and the buck2 code needs to be updated.",
                        change
                    )
                    .into()
                )?;
                true
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
            // TODO: temporary. There is a bug in EdenFS that causes it to report
            //      the to as from and the from as to. This is a temporary
            //      and will be removed once the bug is fixed and fully propagated.
            // NOTE: builds should still work, but the mergebase will be wrong.
            let to = hex::encode(&commit_transition.from);
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
    ) -> buck2_error::Result<()> {
        let path_buf = PathBuf::from(str::from_utf8(path)?);

        // If the path is invalid, then walk up all the way until you find a valid dir to
        // invalidate listings. We don't need to invalidate the file itself, as we can't
        // read invalid files.

        let (relative_path, kind, event) = match ProjectRelativePath::new(&path_buf) {
            Ok(path) => (path, kind, event),
            Err(_) => {
                // If we error out here then we might miss other changes. This seems like
                // it shouldn't happen, since the empty path should always be a valid path.
                let path = find_first_valid_parent(&path_buf)
                    .with_buck_error_context(|| {
                        format!("Invalid path had no valid parent: `{}`", path_buf.display())
                    })
                    .unwrap();

                // Create a synthetic directory change event.
                (path, Kind::Directory, Type::Modify)
            }
        };

        let cell_path = self
            .cells
            .get_cell_path(relative_path)
            .buck_error_context("Failed to convert path to cell.")?;

        let ignore = self
            .ignore_specs
            .get(&cell_path.cell())
            // This shouldn't ever really happen. However, because of the bugs caused by just
            // storing the `CellResolver` in the watcher permanantly, sometimes it can, so we just
            // default to not ignoring the file in that case
            .map_or(false, |ignore| ignore.is_match(cell_path.path()));

        info!("EdenFS: {:?} (ignore = {})", cell_path, ignore);

        if ignore {
            stats.add_ignored(1);
        } else {
            stats.add(cell_path.to_string(), event, kind);

            match (event, kind) {
                (Type::Create, Kind::Directory) => tracker.dir_added(cell_path),
                (Type::Create, _) => {
                    if kind == Kind::Symlink {
                        warn!(
                            "New symlink detected (source symlinks are not supported): {}",
                            cell_path
                        );
                    }
                    tracker.file_added(cell_path)
                }
                (Type::Modify, Kind::Directory) => tracker.dir_changed(cell_path),
                (Type::Modify, _) => tracker.file_changed(cell_path),
                (Type::Delete, Kind::Directory) => tracker.dir_removed(cell_path),
                (Type::Delete, _) => tracker.file_removed(cell_path),
            };
        }

        Ok(())
    }

    // Process commit transitions. Compute the actual changes using the
    // mergebase and mergebase-with using Sapling. In cases where mergebase
    // has changed, we need to invalidate DICE. In cases where it has not
    // changed, update the tracker and stats.
    async fn process_commit_transition(
        &self,
        tracker: &mut FileChangeTracker,
        stats: &mut FileWatcherStats,
        from: String,
        to: String,
    ) -> buck2_error::Result<bool> {
        if self
            .update_mergebase(&to)
            .await
            .buck_error_context("Failed to update mergebase.")?
        {
            // Mergebase has changed - invalidate DICE.
            Ok(true)
        } else {
            // Mergebase has not changed - compute status - limit results to 10,000.
            match get_status(&self.root, &from, Some(&to), 10_000)
                .await
                .buck_error_context("Failed to get Sapling status.")?
            {
                SaplingGetStatusResult::TooManyChanges => Ok(true),
                SaplingGetStatusResult::Normal(status) => {
                    // Process statuses - if any fail, will terminate early.
                    let results: buck2_error::Result<Vec<_>> = status
                        .into_iter()
                        .map(|(change, path)| match change {
                            SaplingStatus::Added | SaplingStatus::NotTracked => self
                                .process_file_watcher_event(
                                    tracker,
                                    stats,
                                    Kind::File,
                                    Type::Create,
                                    path.as_bytes(),
                                ),
                            SaplingStatus::Modified => self.process_file_watcher_event(
                                tracker,
                                stats,
                                Kind::File,
                                Type::Modify,
                                path.as_bytes(),
                            ),
                            SaplingStatus::Removed | SaplingStatus::Missing => self
                                .process_file_watcher_event(
                                    tracker,
                                    stats,
                                    Kind::File,
                                    Type::Delete,
                                    path.as_bytes(),
                                ),
                            _ => Err(buck2_error!(
                                [],
                                "Unexpected SaplingStatus returned from sapling::get_status."
                            )),
                        })
                        .collect();

                    // Return false indicating no large change.
                    results
                        .map(|_| false)
                        .buck_error_context("Failed to process Sapling statuses.")
                }
            }
        }
    }

    // Handle large or unknown changes other than commit transitions.
    async fn on_large_or_unknown_change(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(FileWatcherStats, DiceTransactionUpdater)> {
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
        let last_mergebase = self.last_mergebase.read().await.clone();
        let mergebase = self.mergebase.read().await.clone();

        // TODO: refactor or reuse this struct - utilizing fields when data is available.
        let mut base_stats = buck2_data::FileWatcherStats {
            fresh_instance: true,
            branched_from_revision: None,
            branched_from_global_rev: None,
            branched_from_revision_timestamp: None,
            watchman_version: None,
            fresh_instance_data: Some(buck2_data::FreshInstance {
                new_mergebase: last_mergebase.is_none() || last_mergebase != mergebase,
                cleared_dice: true,
                cleared_dep_files: true,
            }),
            ..Default::default()
        };

        base_stats.incomplete_events_reason = Some("Large or Unknown change".to_owned());

        Ok((FileWatcherStats::new(base_stats, 0), dice))
    }

    // Compute and update the mergebase.
    async fn update_mergebase(&self, to: &str) -> buck2_error::Result<bool> {
        if let Some(mergebase_with) = &self.mergebase_with {
            // Compute new mergebase.
            let mergebase = get_mergebase(&self.root, &to, mergebase_with)
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

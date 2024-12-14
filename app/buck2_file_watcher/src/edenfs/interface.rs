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
use buck2_core::buck2_env;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_data::FileWatcherEventType as Type;
use buck2_data::FileWatcherKind as Kind;
use buck2_eden::connection::EdenConnectionManager;
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
    #[allocative(skip)]
    position: RwLock<JournalPosition>,
    cells: CellResolver,
    ignore_specs: HashMap<CellName, IgnoreSet>,
}

impl EdenFsFileWatcher {
    pub(crate) fn new(
        fb: FacebookInit,
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
        let eden_semaphore =
            buck2_env!("BUCK2_EDEN_SEMAPHORE", type=usize, default=2048, applicability=internal)?;

        let manager = EdenConnectionManager::new(fb, root, Semaphore::new(eden_semaphore))?
            .expect("Failed to connect to EdenFS");
        let mount_point = manager.get_mount_point();

        Ok(Self {
            manager,
            mount_point,
            position: RwLock::new(JournalPosition::default()),
            cells,
            ignore_specs,
        })
    }

    async fn update(
        &self,
        mut dice: DiceTransactionUpdater,
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
            .await?;
        let mut position = self.position.write().await;
        *position = result.toPosition;

        let mut file_change_tracker = FileChangeTracker::new();
        let mut stats = FileWatcherStats::new(Default::default(), result.changes.len());
        let _large_or_unknown_change =
            result
                .changes
                .iter()
                .try_fold(false, |acc, change| -> buck2_error::Result<bool> {
                    self.process_change(change, &mut file_change_tracker, &mut stats)
                        .map_or_else(|_e| Ok(true), |reset| Ok(acc || reset))
                });

        file_change_tracker.write_to_dice(&mut dice)?;
        Ok((stats.finish(), dice))
    }

    fn process_change(
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
                    // TODO: use Sapling to generate the list of file changes -or- treat as if everything changed
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
                    true
                }
                LargeChangeNotification::commitTransition(_commit_transition) => {
                    // TODO: use Sapling to generate the list of file changes -or- treat as if everything changed
                    true
                }
                LargeChangeNotification::lostChanges(_lost_changes) => {
                    // TODO: treat as if everything changed
                    true
                }
                LargeChangeNotification::UnknownField(_) => {
                    // TODO: print warning
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

        let cell_path = self.cells.get_cell_path(relative_path)?;

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

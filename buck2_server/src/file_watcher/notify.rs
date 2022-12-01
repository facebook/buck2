/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::mem;
use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::file_ops::FileChangeTracker;
use buck2_common::file_ops::IgnoreSet;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::span_async;
use dice::DiceTransaction;
use gazebo::prelude::*;
use notify::event::CreateKind;
use notify::event::MetadataKind;
use notify::event::ModifyKind;
use notify::event::RemoveKind;
use notify::EventKind;
use notify::RecommendedWatcher;
use notify::Watcher;
use tracing::info;

use crate::file_watcher::stats::FileWatcherStats;
use crate::file_watcher::FileWatcher;

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
enum ChangeType {
    None,
    FileContents,
    FileExistence,
    DirExistence,
    SomeExistence,
    Unknown,
}

impl ChangeType {
    fn new(x: EventKind) -> Self {
        match x {
            EventKind::Access(_) => Self::None,
            EventKind::Create(x) => match x {
                CreateKind::File => Self::FileExistence,
                CreateKind::Folder => Self::DirExistence,
                CreateKind::Any | CreateKind::Other => Self::SomeExistence,
            },
            EventKind::Modify(x) => match x {
                ModifyKind::Data(_) => Self::FileContents,
                ModifyKind::Metadata(x) => match x {
                    MetadataKind::Ownership | MetadataKind::Permissions => Self::FileContents,
                    _ => Self::None,
                },
                ModifyKind::Name(_) => Self::SomeExistence,
                ModifyKind::Any | ModifyKind::Other => Self::Unknown,
            },
            EventKind::Remove(x) => match x {
                RemoveKind::File => Self::FileExistence,
                RemoveKind::Folder => Self::DirExistence,
                RemoveKind::Any | RemoveKind::Other => Self::SomeExistence,
            },
            EventKind::Any | EventKind::Other => Self::Unknown,
        }
    }
}

#[derive(Allocative)]
struct NotifyFileData {
    changed: FileChangeTracker,
    stats: FileWatcherStats,
    #[allocative(skip)]
    error: Option<anyhow::Error>,
}

impl NotifyFileData {
    fn new() -> Self {
        Self {
            changed: FileChangeTracker::new(),
            stats: FileWatcherStats::new(0, None),
            error: None,
        }
    }

    fn process(
        &mut self,
        event: notify::Result<notify::Event>,
        root: &ProjectRoot,
        cells: &CellResolver,
        ignore_specs: &HashMap<CellName, IgnoreSet>,
    ) {
        if self.error.is_some() {
            return;
        }
        if let Err(e) = self.process_err(event, root, cells, ignore_specs) {
            self.error = Some(e);
            // Might as well clear out the memory we aren't going to use
            self.changed = FileChangeTracker::new();
            self.stats = FileWatcherStats::new(0, None);
        }
    }

    fn process_err(
        &mut self,
        event: notify::Result<notify::Event>,
        root: &ProjectRoot,
        cells: &CellResolver,
        ignore_specs: &HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<()> {
        let event = event?;
        let change_type = ChangeType::new(event.kind);
        for path in event.paths {
            // Testing shows that we get absolute paths back from the `notify` library.
            // It's not documented though.
            let path = root.relativize(AbsNormPath::new(&path)?)?;
            let cell_path = cells.get_cell_path(&path)?;
            let cell_path_str = cell_path.to_string();

            let ignore = ignore_specs
                .get(cell_path.cell())
                .expect("unexpected cell name mismatch")
                .is_match(cell_path.path());

            info!(
                "FileWatcher: {:?} {:?} (ignore = {})",
                path, change_type, ignore
            );

            if ignore || change_type == ChangeType::None {
                self.stats.add_ignored();
            } else {
                match change_type {
                    ChangeType::None => {}
                    ChangeType::FileContents => self.changed.file_changed(cell_path),
                    ChangeType::FileExistence => self.changed.file_added_or_removed(cell_path),
                    ChangeType::DirExistence => self.changed.dir_added_or_removed(cell_path),
                    ChangeType::SomeExistence | ChangeType::Unknown => {
                        self.changed.dir_added_or_removed(cell_path.clone());
                        self.changed.file_added_or_removed(cell_path)
                    }
                }
                // The event type and watcher kind are just made up, but that's not a big deal
                // since we only use this path open source, where we don't log the information to Scuba anyway.
                // The path is right, which is probably what matters most
                self.stats.add(
                    cell_path_str,
                    buck2_data::FileWatcherEventType::Modify,
                    buck2_data::FileWatcherKind::File,
                );
            }
        }
        Ok(())
    }

    fn sync(&mut self) -> anyhow::Result<(buck2_data::FileWatcherStats, FileChangeTracker)> {
        let changed = mem::replace(&mut self.changed, FileChangeTracker::new());
        let stats = mem::replace(&mut self.stats, FileWatcherStats::new(0, None));

        match self.error.take() {
            Some(err) => Err(err),
            None => Ok((stats.finish(), changed)),
        }
    }
}

#[derive(Allocative)]
pub struct NotifyFileWatcher {
    #[allocative(skip)]
    watcher: RecommendedWatcher,
    data: Arc<Mutex<NotifyFileData>>,
}

impl NotifyFileWatcher {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Self> {
        let data = Arc::new(Mutex::new(NotifyFileData::new()));
        let data2 = data.dupe();
        let root2 = root.dupe();
        let mut watcher = notify::recommended_watcher(move |event| {
            data2
                .lock()
                .unwrap()
                .process(event, &root2, &cells, &ignore_specs)
        })?;
        watcher.watch(root.root().as_path(), notify::RecursiveMode::Recursive)?;
        Ok(Self { watcher, data })
    }

    fn sync2(
        &self,
        dice: DiceTransaction,
    ) -> anyhow::Result<(buck2_data::FileWatcherStats, DiceTransaction)> {
        let (stats, changes) = self.data.lock().unwrap().sync()?;
        changes.write_to_dice(&dice)?;
        Ok((stats, dice))
    }
}

#[async_trait]
impl FileWatcher for NotifyFileWatcher {
    async fn sync(&self, dice: DiceTransaction) -> anyhow::Result<DiceTransaction> {
        span_async(
            buck2_data::FileWatcherStart {
                provider: buck2_data::FileWatcherProvider::RustNotify as i32,
            },
            async {
                let (stats, res) = match self.sync2(dice) {
                    Ok((stats, dice)) => ((Some(stats)), Ok(dice)),
                    Err(e) => (None, Err(e)),
                };
                (res, buck2_data::FileWatcherEnd { stats })
            },
        )
        .await
    }
}

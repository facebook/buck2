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
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::collections::ordered_set::OrderedSet;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::span_async;
use dice::DiceTransaction;
use dupe::Dupe;
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

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, Hash, Allocative)]
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

/// Buffer containing the events that have happened since we last got a message.
/// Used to dedupe events, since notify sends a notification on every change.
#[derive(Allocative)]
struct NotifyFileData {
    ignored: u64,
    events: OrderedSet<(CellPath, ChangeType)>,
}

impl NotifyFileData {
    fn new() -> Self {
        Self {
            ignored: 0,
            events: OrderedSet::new(),
        }
    }

    fn process(
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

            // We ignore the buck-out prefix, as those are uninteresting events caused by us.
            // We also ignore other buck-out directories, as if you have two isolation dirs running at once, they are not interesting.
            // We do this in the notify-watcher, rather than a generic layer, as watchman users should configure
            // to ignore buck-out, to reduce the number of events, rather than hiding them later.
            if path.starts_with(InvocationPaths::buck_out_dir_prefix()) {
                // We don't want to event add them as ignored events, since they are super common
                // and very boring
                continue;
            }

            let cell_path = cells.get_cell_path(&path)?;
            let ignore = ignore_specs
                .get(&cell_path.cell())
                .expect("unexpected cell name mismatch")
                .is_match(cell_path.path());

            info!(
                "FileWatcher: {:?} {:?} (ignore = {})",
                path, change_type, ignore
            );

            if ignore || change_type == ChangeType::None {
                self.ignored += 1;
            } else {
                self.events.insert((cell_path, change_type));
            }
        }
        Ok(())
    }

    fn sync(self) -> (buck2_data::FileWatcherStats, FileChangeTracker) {
        // The changes that go into the DICE transaction
        let mut changed = FileChangeTracker::new();
        // The files that were changed for accumulating the stats
        let mut changed_paths = OrderedSet::new();

        for (cell_path, change_type) in self.events {
            let cell_path_str = cell_path.to_string();
            match change_type {
                ChangeType::None => {}
                ChangeType::FileContents => changed.file_changed(cell_path),
                ChangeType::FileExistence => changed.file_added_or_removed(cell_path),
                ChangeType::DirExistence => changed.dir_added_or_removed(cell_path),
                ChangeType::SomeExistence | ChangeType::Unknown => {
                    changed.dir_added_or_removed(cell_path.clone());
                    changed.file_added_or_removed(cell_path)
                }
            }
            // We use changed_paths to deduplicate
            changed_paths.insert(cell_path_str);
        }

        let mut stats = FileWatcherStats::new(changed_paths.len(), None);
        stats.add_ignored(self.ignored);
        for path in changed_paths {
            // The event type and watcher kind are just made up, but that's not a big deal
            // since we only use this path open source, where we don't log the information to Scuba anyway.
            // The path is right, which is probably what matters most
            stats.add(
                path,
                buck2_data::FileWatcherEventType::Modify,
                buck2_data::FileWatcherKind::File,
            );
        }

        (stats.finish(), changed)
    }
}

#[derive(Allocative)]
pub struct NotifyFileWatcher {
    #[allocative(skip)]
    watcher: RecommendedWatcher,
    data: Arc<Mutex<anyhow::Result<NotifyFileData>>>,
}

impl NotifyFileWatcher {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> anyhow::Result<Self> {
        let data = Arc::new(Mutex::new(Ok(NotifyFileData::new())));
        let data2 = data.dupe();
        let root2 = root.dupe();
        let mut watcher = notify::recommended_watcher(move |event| {
            let mut guard = data2.lock().unwrap();
            if let Ok(state) = &mut *guard {
                if let Err(e) = state.process(event, &root2, &cells, &ignore_specs) {
                    *guard = Err(e);
                }
            }
        })?;
        watcher.watch(root.root().as_path(), notify::RecursiveMode::Recursive)?;
        Ok(Self { watcher, data })
    }

    fn sync2(
        &self,
        dice: DiceTransaction,
    ) -> anyhow::Result<(buck2_data::FileWatcherStats, DiceTransaction)> {
        let mut guard = self.data.lock().unwrap();
        let old = mem::replace(&mut *guard, Ok(NotifyFileData::new()));
        let (stats, changes) = old?.sync();
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

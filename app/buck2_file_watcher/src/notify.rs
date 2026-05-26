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
use std::mem;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::SystemTime;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::dice::FileChangeTracker;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_data::FileWatcherEventType;
use buck2_data::FileWatcherKind;
use buck2_error::conversion::from_any_with_tag;
use buck2_events::dispatch::span_async;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_hash::StdBuckHashMap;
use dice::DiceTransactionUpdater;
use dupe::Dupe;
use notify::EventKind;
use notify::RecommendedWatcher;
use notify::Watcher;
use notify::event::CreateKind;
use notify::event::DataChange;
use notify::event::MetadataKind;
use notify::event::ModifyKind;
use notify::event::RemoveKind;
use starlark_map::ordered_set::OrderedSet;
use tracing::debug;
use tracing::info;

use crate::file_watcher::FileWatcher;
use crate::mergebase::Mergebase;
use crate::stats::FileWatcherStats;

fn ignore_event_kind(event_kind: EventKind) -> bool {
    match event_kind {
        EventKind::Access(_) => true,
        EventKind::Modify(ModifyKind::Metadata(MetadataKind::Ownership))
        | EventKind::Modify(ModifyKind::Metadata(MetadataKind::Permissions)) => false,
        EventKind::Modify(ModifyKind::Metadata(_)) => true,
        _ => false,
    }
}

/// Buffer containing the events that have happened since we last got a message.
/// Used to dedupe events, since notify sends a notification on every change.
#[derive(Allocative)]
struct NotifyFileData {
    ignored: u64,
    #[allocative(skip)]
    events: OrderedSet<(CellPath, EventKind)>,
    /// Whether file system changes were missed
    missed_events: bool,
}

impl NotifyFileData {
    fn new() -> Self {
        Self {
            ignored: 0,
            events: OrderedSet::new(),
            missed_events: false,
        }
    }

    fn process(
        &mut self,
        event: notify::Result<notify::Event>,
        root: &ProjectRoot,
        cells: &CellResolver,
        ignore_specs: &StdBuckHashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<()> {
        let event =
            event.map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;

        for path in &event.paths {
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

            let cell_path = cells.get_cell_path(&path);
            let ignore = ignore_specs
                .get(&cell_path.cell())
                // See the comment on the analogous code in `watchman/interface.rs`
                .is_some_and(|ignore| ignore.is_match(cell_path.path()));

            info!(
                "FileWatcher: {:?} {:?} (ignore = {})",
                path, &event.kind, ignore
            );

            if event.need_rescan() {
                self.missed_events = true;
                debug!("FileWatcher: File change events were missed");
            }

            if ignore || ignore_event_kind(event.kind) {
                self.ignored += 1;
            } else {
                self.events.insert((cell_path, event.kind));
            }
        }
        Ok(())
    }

    fn sync(self) -> (buck2_data::FileWatcherStats, Option<FileChangeTracker>) {
        // The changes that go into the DICE transaction
        let mut changed = FileChangeTracker::new();
        // If we missed events, sync2() will drop the entire DICE graph. Surface that to
        // telemetry/UI by reusing the fresh-instance fields the watchman path uses for
        // the equivalent wipe.
        let base = if self.missed_events {
            buck2_data::FileWatcherStats {
                fresh_instance: true,
                fresh_instance_data: Some(buck2_data::FreshInstance {
                    new_mergebase: false,
                    cleared_dice: true,
                    cleared_dep_files: false,
                }),
                incomplete_events_reason: Some(
                    "notify dropped events (kernel queue overflow)".to_owned(),
                ),
                ..Default::default()
            }
        } else {
            Default::default()
        };
        let mut stats = FileWatcherStats::new(base, self.events.len());
        stats.add_ignored(self.ignored);

        for (cell_path, event_kind) in self.events {
            let cell_path_str = cell_path.to_string();
            match event_kind {
                EventKind::Create(create_kind) => match create_kind {
                    CreateKind::File => {
                        changed.file_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Create,
                            FileWatcherKind::File,
                        );
                    }
                    CreateKind::Folder => {
                        changed.dir_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Create,
                            FileWatcherKind::Directory,
                        );
                    }
                    CreateKind::Any | CreateKind::Other => {
                        changed.file_added_or_removed(cell_path.clone());
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Create,
                            FileWatcherKind::File,
                        );
                        changed.dir_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Create,
                            FileWatcherKind::Directory,
                        );
                    }
                },
                EventKind::Modify(modify_kind) => match modify_kind {
                    ModifyKind::Data(_) | ModifyKind::Metadata(_) => {
                        changed.file_contents_changed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Modify,
                            FileWatcherKind::File,
                        );
                    }
                    ModifyKind::Name(_) | ModifyKind::Any | ModifyKind::Other => {
                        changed.file_added_or_removed(cell_path.clone());
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Create,
                            FileWatcherKind::File,
                        );
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Delete,
                            FileWatcherKind::File,
                        );
                        changed.dir_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Create,
                            FileWatcherKind::Directory,
                        );
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Delete,
                            FileWatcherKind::Directory,
                        );
                    }
                },
                EventKind::Remove(remove_kind) => match remove_kind {
                    RemoveKind::File => {
                        changed.file_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Delete,
                            FileWatcherKind::File,
                        );
                    }
                    RemoveKind::Folder => {
                        changed.dir_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Delete,
                            FileWatcherKind::Directory,
                        );
                    }
                    RemoveKind::Any | RemoveKind::Other => {
                        changed.file_added_or_removed(cell_path.clone());
                        stats.add(
                            cell_path_str.clone(),
                            FileWatcherEventType::Delete,
                            FileWatcherKind::File,
                        );
                        changed.dir_added_or_removed(cell_path);
                        stats.add(
                            cell_path_str,
                            FileWatcherEventType::Delete,
                            FileWatcherKind::Directory,
                        );
                    }
                },
                _ => {}
            }
        }

        let stats = stats.finish();
        let changed = if self.missed_events {
            None
        } else {
            Some(changed)
        };

        (stats, changed)
    }
}

/// Last-known state for a non-ignored top-level entry under the project root,
/// used by [`rescan_root`] to detect modifications to root-level files
/// (e.g. `BUCK`, `.buckconfig`) and to discover new top-level subdirectories
/// that need a recursive watch added.
#[derive(Debug, Clone, PartialEq)]
enum RootEntryState {
    File {
        size: u64,
        mtime: Option<SystemTime>,
    },
    Dir,
}

#[derive(Allocative)]
pub struct NotifyFileWatcher {
    #[allocative(skip)]
    watcher: Mutex<RecommendedWatcher>,
    data: Arc<Mutex<buck2_error::Result<NotifyFileData>>>,
    root: ProjectRoot,
    cells: CellResolver,
    ignore_specs: StdBuckHashMap<CellName, IgnoreSet>,
    /// Last-known state of non-ignored top-level entries. `Some` when the
    /// per-subdirectory watching strategy is active; `None` when the project
    /// root is watched recursively.
    #[allocative(skip)]
    root_state: Option<Mutex<HashMap<PathBuf, RootEntryState>>>,
}

impl NotifyFileWatcher {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: StdBuckHashMap<CellName, IgnoreSet>,
        watch_included_root_dirs_only: bool,
    ) -> buck2_error::Result<Self> {
        let data = Arc::new(Mutex::new(Ok(NotifyFileData::new())));
        let data2 = data.dupe();
        let root2 = root.dupe();
        let cells_for_callback = cells.dupe();
        let ignore_specs_for_callback = ignore_specs.clone();
        let mut watcher = notify::recommended_watcher(move |event| {
            let mut guard = data2.lock().unwrap();
            if let Ok(state) = &mut *guard {
                if let Err(e) = state.process(
                    event,
                    &root2,
                    &cells_for_callback,
                    &ignore_specs_for_callback,
                ) {
                    *guard = Err(e);
                }
            }
        })
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;

        let root_state = if watch_included_root_dirs_only {
            // Seed root state and register a recursive watch for each
            // non-ignored top-level subdirectory. The project root itself
            // is not watched; root-level files are picked up by `rescan_root`
            // on each sync. DICE is cold at startup so we discard the
            // synthesized events from the initial scan.
            let mut state = HashMap::new();
            let (_, new_watches) = rescan_root(&mut state, root, &cells, &ignore_specs)?;
            for path in new_watches {
                watcher
                    .watch(&path, notify::RecursiveMode::Recursive)
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
            }
            Some(Mutex::new(state))
        } else {
            watcher
                .watch(root.root().as_path(), notify::RecursiveMode::Recursive)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
            None
        };

        Ok(Self {
            watcher: Mutex::new(watcher),
            data,
            root: root.dupe(),
            cells,
            ignore_specs,
            root_state,
        })
    }

    fn sync2(
        &self,
        mut dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, DiceTransactionUpdater)> {
        let synth_events = if let Some(state_mu) = &self.root_state {
            let (events, new_watches) = {
                let mut state = state_mu.lock().unwrap();
                rescan_root(&mut state, &self.root, &self.cells, &self.ignore_specs)?
            };
            if !new_watches.is_empty() {
                let mut watcher = self.watcher.lock().unwrap();
                for path in new_watches {
                    debug!("FileWatcher: Watching new root subdirectory: {:?}", path);
                    watcher
                        .watch(&path, notify::RecursiveMode::Recursive)
                        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
                }
            }
            events
        } else {
            Vec::new()
        };

        let old = {
            let mut guard = self.data.lock().unwrap();
            if let Ok(data) = &mut *guard {
                for ev in synth_events {
                    data.events.insert(ev);
                }
            }
            mem::replace(&mut *guard, Ok(NotifyFileData::new()))
        };
        let (stats, changes) = old?.sync();
        if let Some(changes) = changes {
            changes.write_to_dice(&mut dice)?;
        } else {
            // We missed some file system notifications, so we drop everything
            dice = dice.unstable_take();
        }
        Ok((stats, dice))
    }
}

/// Re-enumerate top-level entries under the project root and update `state`
/// in place. Returns synthetic events for any deltas vs. the previous state
/// and paths of newly-discovered directories that need a recursive watch
/// registered by the caller. Ignored entries are not tracked.
fn rescan_root(
    state: &mut HashMap<PathBuf, RootEntryState>,
    root: &ProjectRoot,
    cells: &CellResolver,
    ignore_specs: &StdBuckHashMap<CellName, IgnoreSet>,
) -> buck2_error::Result<(Vec<(CellPath, EventKind)>, Vec<PathBuf>)> {
    let mut events = Vec::new();
    let mut new_watches = Vec::new();
    let mut seen = HashSet::new();

    for entry in std::fs::read_dir(root.root().as_path())
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Environment))?
    {
        let entry = entry.map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Environment))?;
        let file_type = entry
            .file_type()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Environment))?;
        if file_type.is_symlink() {
            continue;
        }
        let path = entry.path();
        let cell_path = cells.get_cell_path(&root.relativize(AbsNormPath::new(&path)?)?);
        if ignore_specs
            .get(&cell_path.cell())
            .is_some_and(|i| i.is_match(cell_path.path()))
        {
            continue;
        }
        seen.insert(path.clone());

        if file_type.is_dir() {
            if !state.contains_key(&path) {
                new_watches.push(path.clone());
                events.push((cell_path, EventKind::Create(CreateKind::Folder)));
            }
            state.insert(path, RootEntryState::Dir);
        } else if file_type.is_file() {
            let metadata = entry
                .metadata()
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Environment))?;
            let new_state = RootEntryState::File {
                size: metadata.len(),
                mtime: metadata.modified().ok(),
            };
            match state.get(&path) {
                Some(prev) if *prev == new_state => {}
                Some(_) => {
                    debug!("FileWatcher: Root file modified: {:?}", path);
                    events.push((
                        cell_path,
                        EventKind::Modify(ModifyKind::Data(DataChange::Any)),
                    ));
                }
                None => {
                    events.push((cell_path, EventKind::Create(CreateKind::File)));
                }
            }
            state.insert(path, new_state);
        }
    }

    state.retain(|path, entry| {
        if seen.contains(path) {
            return true;
        }
        let cell_path = match AbsNormPath::new(path)
            .and_then(|abs| root.relativize(abs))
            .map(|rel| cells.get_cell_path(&rel))
        {
            Ok(p) => p,
            Err(_) => return false,
        };
        let kind = match entry {
            RootEntryState::File { .. } => EventKind::Remove(RemoveKind::File),
            RootEntryState::Dir => EventKind::Remove(RemoveKind::Folder),
        };
        events.push((cell_path, kind));
        false
    });

    Ok((events, new_watches))
}

#[async_trait]
impl FileWatcher for NotifyFileWatcher {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(DiceTransactionUpdater, Mergebase)> {
        span_async(
            buck2_data::FileWatcherStart {
                provider: buck2_data::FileWatcherProvider::RustNotify as i32,
            },
            async {
                let (stats, res) = match self.sync2(dice) {
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

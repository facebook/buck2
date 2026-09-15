/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::mem;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::dice::FileChangeTracker;
use buck2_common::file_ops::invalidation::set_filesystem_invalidation_token;
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

        // Checked before the path loop: the kernel-overflow rescan signal can arrive with no
        // paths attached (platform dependent), and a missed rescan means the daemon keeps
        // answering from a stale graph.
        if event.need_rescan() {
            self.missed_events = true;
            debug!("FileWatcher: File change events were missed");
        }

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
        // Missing notifications make the entire incremental event batch unreliable.
        let base = if self.missed_events {
            buck2_data::FileWatcherStats {
                filesystem_inputs_invalidated: true,
                incomplete_events_reason: Some(
                    "notify requested a rescan after missed filesystem events".to_owned(),
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

#[derive(Allocative)]
pub struct NotifyFileWatcher {
    invalidation_token: AtomicU64,
    #[allocative(skip)]
    #[expect(unused)]
    // FIXME(JakobDegen): Clarify if this just needs to be kept alive or can be removed?
    watcher: RecommendedWatcher,
    data: Arc<Mutex<buck2_error::Result<NotifyFileData>>>,
}

impl NotifyFileWatcher {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: StdBuckHashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
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
        })
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
        watcher
            .watch(root.root().as_path(), notify::RecursiveMode::Recursive)
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
        Ok(Self {
            invalidation_token: AtomicU64::new(0),
            watcher,
            data,
        })
    }

    fn sync2(
        &self,
        mut dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, DiceTransactionUpdater)> {
        let old = {
            let mut guard = self.data.lock().unwrap();
            mem::replace(&mut *guard, Ok(NotifyFileData::new()))
        };
        let (stats, changes) = old?.sync();
        // Watcher updates/commits are serialized by the concurrency manager.
        // Retain the advanced token even if this command aborts before commit.
        let token = if changes.is_none() {
            self.invalidation_token
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |value| {
                    value.checked_add(1)
                })
                .map_err(|_| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "Filesystem invalidation token exhausted"
                    )
                })?
                + 1
        } else {
            self.invalidation_token.load(Ordering::Relaxed)
        };
        set_filesystem_invalidation_token(&mut dice, token)?;
        if let Some(changes) = changes {
            changes.write_to_dice(&mut dice)?;
        }
        Ok((stats, dice))
    }
}

#[async_trait]
impl FileWatcher for NotifyFileWatcher {
    fn uses_filesystem_invalidation(&self) -> bool {
        true
    }

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

#[cfg(test)]
mod tests {
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use notify::event::CreateKind;
    use notify::event::Flag;

    use super::*;

    fn fixture() -> (
        ProjectRoot,
        CellResolver,
        StdBuckHashMap<CellName, IgnoreSet>,
        tempfile::TempDir,
    ) {
        let cells = CellResolver::testing_with_name_and_path(
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        );
        let tempdir = tempfile::tempdir().unwrap();
        let root_path =
            fs_util::canonicalize(AbsNormPathBuf::new(tempdir.path().to_owned()).unwrap()).unwrap();
        let root = ProjectRoot::new(root_path).unwrap();
        (root, cells, StdBuckHashMap::default(), tempdir)
    }

    /// The kernel-overflow signal can arrive as a rescan-flagged event with no paths attached
    /// (platform dependent). The flag must still be recorded, otherwise lost notifications go
    /// undetected and the daemon keeps answering from a stale graph, which is the cache
    /// inconsistency the missed-events handling exists to close.
    #[test]
    fn rescan_event_with_no_paths_sets_missed_events() {
        let (root, cells, ignores, _t) = fixture();
        let mut state = NotifyFileData::new();
        let event = notify::Event::new(EventKind::Any).set_flag(Flag::Rescan);
        state.process(Ok(event), &root, &cells, &ignores).unwrap();
        assert!(
            state.missed_events,
            "a pathless rescan event must set missed_events"
        );
    }

    #[test]
    fn rescan_event_with_paths_sets_missed_events() {
        let (root, cells, ignores, _t) = fixture();
        let mut state = NotifyFileData::new();
        let path = root.resolve(ProjectRelativePath::new("f").unwrap());
        let event = notify::Event::new(EventKind::Create(CreateKind::File))
            .add_path(path.into_abs_path_buf().into_path_buf())
            .set_flag(Flag::Rescan);
        state.process(Ok(event), &root, &cells, &ignores).unwrap();
        assert!(state.missed_events);
    }

    #[test]
    fn sync_with_missed_events_reports_input_invalidation() {
        let mut state = NotifyFileData::new();
        state.missed_events = true;
        let (stats, changes) = state.sync();
        assert!(changes.is_none(), "missed events must drop the tracker");
        assert!(stats.filesystem_inputs_invalidated);
        assert!(!stats.fresh_instance);
        assert!(stats.fresh_instance_data.is_none());
        assert!(stats.incomplete_events_reason.is_some());
    }

    #[test]
    fn sync_without_missed_events_returns_changes() {
        let (stats, changes) = NotifyFileData::new().sync();
        assert!(
            changes.is_some(),
            "no missed events: incremental tracker must survive"
        );
        assert!(!stats.fresh_instance);
        assert!(!stats.filesystem_inputs_invalidated);
        assert!(stats.fresh_instance_data.is_none());
        assert!(stats.incomplete_events_reason.is_none());
    }
}

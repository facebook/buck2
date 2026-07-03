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
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::dice::FileChangeTracker;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
use crate::watchman::utils::find_first_valid_parent;

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

        if event.need_rescan() {
            self.missed_events = true;
            debug!("FileWatcher: File change events were missed");
        }

        for path in &event.paths {
            // We ignore the buck-out prefix, as those are uninteresting events caused by us.
            // We also ignore other buck-out directories, as if you have two isolation dirs running at once, they are not interesting.
            // We do this in the notify-watcher, rather than a generic layer, as watchman users should configure
            // to ignore buck-out, to reduce the number of events, rather than hiding them later.
            //
            // Checked on the raw path so this dominant event class is discarded
            // cheaply, whatever bytes the path contains.
            if let Ok(rel) = path.strip_prefix(root.root().as_path())
                && rel.starts_with(InvocationPaths::buck_out_dir_prefix().as_str())
            {
                // We don't want to event add them as ignored events, since they are super common
                // and very boring
                continue;
            }

            // Uninteresting event kinds don't need the path at all.
            if ignore_event_kind(event.kind) {
                self.ignored += 1;
                continue;
            }

            // Testing shows that we get absolute paths back from the `notify` library.
            // It's not documented though.
            //
            // Relativized leniently because ignored directories can transiently
            // contain names `ProjectRelativePath` rejects (e.g. a literal backslash),
            // and those must reach the ignore check below rather than fail: an error
            // would poison the watcher permanently.
            let rel = match AbsNormPath::new(&path).and_then(|path| root.relativize_relaxed(path)) {
                Ok(rel) => rel,
                Err(e) => match path.strip_prefix(root.root().as_path()) {
                    // Not relativizable at all (in practice: a non-UTF-8 name).
                    Ok(raw_rel) => {
                        self.degrade_to_parent(raw_rel, cells, ignore_specs);
                        continue;
                    }
                    // Outside the project root: a genuine error.
                    Err(_) => return Err(e),
                },
            };

            // The relaxed path may violate the `ProjectRelativePath` invariants; it
            // is only used to match the ignores, never to identify a file.
            let cell_path = cells.get_cell_path(ProjectRelativePath::unchecked_new(&rel));
            let ignore = ignore_specs
                .get(&cell_path.cell())
                // See the comment on the analogous code in `watchman/interface.rs`
                .is_some_and(|ignore| ignore.is_match(cell_path.path()));

            info!(
                "FileWatcher: {:?} {:?} (ignore = {})",
                rel, &event.kind, ignore
            );

            if ignore {
                self.ignored += 1;
            } else if ProjectRelativePath::new(&*rel).is_ok() {
                self.events.insert((cell_path, event.kind));
            } else {
                // Interesting, but not representable (e.g. an embedded backslash).
                self.degrade_to_parent(Path::new(&*rel), cells, ignore_specs);
            }
        }
        Ok(())
    }

    /// The event path cannot be represented as a `ProjectRelativePath`. Buck
    /// cannot read such paths anyway, so record a change of the nearest
    /// representable parent directory instead — mirroring the watchman and
    /// edenfs watchers — rather than erroring, which would poison the watcher.
    fn degrade_to_parent(
        &mut self,
        rel: &Path,
        cells: &CellResolver,
        ignore_specs: &StdBuckHashMap<CellName, IgnoreSet>,
    ) {
        let parent = find_first_valid_parent(rel).unwrap_or(ProjectRelativePath::empty());
        let cell_path = cells.get_cell_path(parent);
        let ignore = ignore_specs
            .get(&cell_path.cell())
            .is_some_and(|ignore| ignore.is_match(cell_path.path()));

        info!(
            "FileWatcher: {:?} -> {:?} (unrepresentable path, ignore = {})",
            rel, parent, ignore
        );

        if ignore {
            self.ignored += 1;
        } else {
            // Maps to `dir_added_or_removed` in `sync`, invalidating the
            // parent's directory listing.
            self.events
                .insert((cell_path, EventKind::Create(CreateKind::Folder)));
        }
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

#[derive(Allocative)]
pub struct NotifyFileWatcher {
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
        Ok(Self { watcher, data })
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
        if let Some(changes) = changes {
            changes.write_to_dice(&mut dice)?;
        } else {
            // We missed some file system notifications, so we drop everything
            dice = dice.unstable_take();
        }
        Ok((stats, dice))
    }
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

#[cfg(all(test, unix))]
mod tests {
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;

    fn process_path(
        fs: &ProjectRootTemp,
        rel: impl AsRef<Path>,
    ) -> buck2_error::Result<NotifyFileData> {
        let cells = CellResolver::testing_with_name_and_path(
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        );
        let mut ignore_specs = StdBuckHashMap::default();
        ignore_specs.insert(
            CellName::testing_new("root"),
            IgnoreSet::from_ignore_spec("ignored", true)?,
        );

        let event = notify::Event::new(EventKind::Create(CreateKind::File))
            .add_path(fs.path().root().as_path().join(rel.as_ref()));
        let mut data = NotifyFileData::new();
        data.process(Ok(event), fs.path(), &cells, &ignore_specs)?;
        Ok(data)
    }

    #[test]
    fn test_ignores_apply_before_path_validation() -> buck2_error::Result<()> {
        let fs = ProjectRootTemp::new()?;

        // A regular event is recorded.
        let data = process_path(&fs, "src/file")?;
        assert_eq!(1, data.events.len());
        assert_eq!(0, data.ignored);

        // Ignored paths are discarded even when they contain components
        // `ProjectRelativePath` rejects, such as a literal backslash.
        let data = process_path(&fs, r"buck-out/foo\bar")?;
        assert_eq!(0, data.events.len());
        assert_eq!(0, data.ignored);

        let data = process_path(&fs, r"ignored/foo\bar")?;
        assert_eq!(0, data.events.len());
        assert_eq!(1, data.ignored);

        Ok(())
    }

    #[test]
    fn test_unrepresentable_paths_never_error() -> buck2_error::Result<()> {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        let fs = ProjectRootTemp::new()?;

        // An unrepresentable name outside any ignored directory degrades to a
        // change of the nearest representable parent directory instead of
        // erroring, like the watchman and edenfs watchers.
        let data = process_path(&fs, r"src/foo\bar")?;
        assert_eq!(0, data.ignored);
        let (cell_path, kind) = data.events.iter().next().unwrap();
        assert_eq!("root//src", cell_path.to_string());
        assert_eq!(EventKind::Create(CreateKind::Folder), *kind);

        // Same for non-UTF-8 names, which cannot even be relativized.
        let non_utf8 = OsStr::from_bytes(b"foo\xff");
        let data = process_path(&fs, Path::new("src").join(non_utf8))?;
        assert_eq!(0, data.ignored);
        let (cell_path, kind) = data.events.iter().next().unwrap();
        assert_eq!("root//src", cell_path.to_string());
        assert_eq!(EventKind::Create(CreateKind::Folder), *kind);

        // Under buck-out and ignored directories they are discarded like any
        // other path there.
        let data = process_path(&fs, Path::new("buck-out").join(non_utf8))?;
        assert_eq!(0, data.events.len());
        assert_eq!(0, data.ignored);

        let data = process_path(&fs, Path::new("ignored").join(non_utf8))?;
        assert_eq!(0, data.events.len());
        assert_eq!(1, data.ignored);

        Ok(())
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashSet;
use std::mem;
use std::path::PathBuf;
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
        let event = match event {
            Ok(event) => event,
            // The watcher failed at something, typically installing a watch for a directory that
            // just appeared. It is not fatal, but from here on the watch tree covers less than the
            // whole project, and a directory nobody watches produces no events at all: the daemon
            // would keep serving whatever it read last. Treat it exactly like dropped events, which
            // clears DICE and registers the tree again.
            Err(e) => {
                self.missed_events = true;
                info!("FileWatcher: watcher error, coverage may be incomplete: {e:?}");
                return Ok(());
            }
        };

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
        // If we missed events, sync2() will drop the entire DICE graph and register the watch
        // tree again. Surface that to telemetry/UI by reusing the fresh-instance fields the
        // watchman path uses when it clears DICE for the same reason.
        let base = if self.missed_events {
            buck2_data::FileWatcherStats {
                fresh_instance: true,
                fresh_instance_data: Some(buck2_data::FreshInstance {
                    new_mergebase: false,
                    cleared_dice: true,
                    cleared_dep_files: false,
                }),
                incomplete_events_reason: Some(
                    "notify dropped events or failed to watch a directory".to_owned(),
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

/// What it takes to register the watch tree, kept so that it can be built again.
struct Registration {
    root: ProjectRoot,
    cells: CellResolver,
    ignore_specs: StdBuckHashMap<CellName, IgnoreSet>,
}

/// Paths whose watch could not be installed.
///
/// A path that failed once fails again on every registration, a directory the user has no
/// permission for being the obvious case. Remembering them keeps such a directory from making every
/// single command drop DICE and walk the tree again; they cost that once.
type FailedWatches = Arc<Mutex<HashSet<PathBuf>>>;

#[derive(Allocative)]
pub struct NotifyFileWatcher {
    /// Never used directly, but must be kept alive: dropping the watcher removes all its watches.
    /// Replaced wholesale when the tree has to be registered again.
    #[allocative(skip)]
    watcher: Mutex<RecommendedWatcher>,
    #[allocative(skip)]
    registration: Registration,
    #[allocative(skip)]
    failed: FailedWatches,
    data: Arc<Mutex<buck2_error::Result<NotifyFileData>>>,
}

impl NotifyFileWatcher {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: StdBuckHashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
        let data = Arc::new(Mutex::new(Ok(NotifyFileData::new())));
        let registration = Registration {
            root: root.dupe(),
            cells,
            ignore_specs,
        };
        let failed: FailedWatches = Default::default();
        let watcher = Self::register(&registration, data.dupe(), failed.dupe())?;
        Ok(Self {
            watcher: Mutex::new(watcher),
            registration,
            failed,
            data,
        })
    }

    /// Watch the whole project.
    fn register(
        registration: &Registration,
        data: Arc<Mutex<buck2_error::Result<NotifyFileData>>>,
        failed: FailedWatches,
    ) -> buck2_error::Result<RecommendedWatcher> {
        let root = registration.root.dupe();
        let cells = registration.cells.dupe();
        let ignore_specs = registration.ignore_specs.clone();
        let mut watcher =
            notify::recommended_watcher(move |event: notify::Result<notify::Event>| {
                // A path we already know we cannot watch is not news, and reacting to it again would
                // make every command drop DICE for as long as it exists.
                if let Err(e) = &event {
                    let mut failed = failed.lock().unwrap();
                    let novel: Vec<_> = e.paths.iter().map(|p| failed.insert(p.clone())).collect();
                    if !e.paths.is_empty() && !novel.contains(&true) {
                        debug!("FileWatcher: {:?} failed to watch again", e.paths);
                        return;
                    }
                }
                let mut guard = data.lock().unwrap();
                if let Ok(state) = &mut *guard {
                    if let Err(e) = state.process(event, &root, &cells, &ignore_specs) {
                        *guard = Err(e);
                    }
                }
            })
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
        watcher
            .watch(
                registration.root.root().as_path(),
                notify::RecursiveMode::Recursive,
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::NotifyWatcher))?;
        Ok(watcher)
    }

    /// Register the tree again, after events were dropped or a watch failed to install.
    ///
    /// Dropping DICE recovers from the changes we did not see, but not from a directory that has no
    /// watch at all: nothing would ever report a change there again. The new registration walks the
    /// tree and covers whatever the old one is missing. It is built before the old one is dropped,
    /// so no window opens where the project is unwatched; the overlap only duplicates events.
    fn reregister(&self) -> buck2_error::Result<()> {
        let watcher = Self::register(&self.registration, self.data.dupe(), self.failed.dupe())?;
        *self.watcher.lock().unwrap() = watcher;
        Ok(())
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
            // We missed some file system notifications, so we drop everything and make sure the
            // watch tree covers the project again before we read it back.
            dice = dice.unstable_take();
            self.reregister()?;
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
    use std::fs;
    use std::os::unix::fs::PermissionsExt;
    use std::thread::sleep;
    use std::time::Duration;
    use std::time::Instant;

    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;

    use super::*;

    /// Wait for the watcher thread to catch up with what the test did.
    fn wait_for(watcher: &NotifyFileWatcher, done: impl Fn(&NotifyFileData) -> bool) -> bool {
        let deadline = Instant::now() + Duration::from_secs(5);
        while Instant::now() < deadline {
            if let Ok(data) = &*watcher.data.lock().unwrap() {
                if done(data) {
                    return true;
                }
            }
            sleep(Duration::from_millis(50));
        }
        false
    }

    /// A watch that could not be installed has to leave the daemon knowing that its coverage is
    /// incomplete, so that the next sync clears DICE and registers the tree again.
    ///
    /// Ignored because notify reports the failure only with
    /// <https://github.com/notify-rs/notify/pull/970>; run with `--ignored` against a notify that
    /// carries it.
    #[test]
    #[ignore = "needs notify-rs/notify#970 for the failure to be reported at all"]
    fn a_failed_watch_counts_as_missed_events() {
        let tempdir = tempfile::tempdir().unwrap();
        let project = tempdir.path().join("project");
        let staging = tempdir.path().join("staging");
        fs::create_dir(&project).unwrap();
        fs::create_dir_all(staging.join("readable")).unwrap();
        let unwatchable = staging.join("unwatchable");
        fs::create_dir(&unwatchable).unwrap();
        fs::set_permissions(&unwatchable, fs::Permissions::from_mode(0o000)).unwrap();
        if fs::read_dir(&unwatchable).is_ok() {
            return; // running as root, which can watch a directory it cannot read
        }

        let root = ProjectRoot::new(
            fs_util::canonicalize(AbsNormPathBuf::new(project.clone()).unwrap()).unwrap(),
        )
        .unwrap();
        let cells = CellResolver::testing_with_name_and_path(
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        );
        let watcher = NotifyFileWatcher::new(&root, cells, StdBuckHashMap::default()).unwrap();

        // Moved in whole, so the walk it triggers is certain to meet the unwatchable directory.
        let appearing = project.join("appearing");
        fs::rename(&staging, &appearing).unwrap();
        let missed = wait_for(&watcher, |data| data.missed_events);
        // Before the asserts: a directory the test cannot read is one tempfile cannot remove.
        fs::set_permissions(
            appearing.join("unwatchable"),
            fs::Permissions::from_mode(0o755),
        )
        .unwrap();
        assert!(
            missed,
            "expected the failed watch to count as missed events"
        );

        // Registering again is what buys back the coverage the failure cost us.
        watcher.reregister().unwrap();
        fs::write(appearing.join("readable").join("file"), "x").unwrap();
        assert!(
            wait_for(&watcher, |data| data
                .events
                .iter()
                .any(|(path, _)| path.to_string().ends_with("file"))),
            "expected a change under the sibling of the unwatchable directory to be seen"
        );
    }
}

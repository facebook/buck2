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
use std::fs::File;
use std::io::Read;
use std::mem;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use blake3::Hash;
use buck2_common::file_ops::dice::FileChangeTracker;
use buck2_common::file_ops::metadata::FileType;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_data::FileWatcherEventType;
use buck2_data::FileWatcherKind;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_events::dispatch::span_async;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::file_name::FileNameBuf;
use compact_str::CompactString;
use dice::DiceTransactionUpdater;
use dupe::Dupe;

use crate::file_watcher::FileWatcher;
use crate::mergebase::Mergebase;
use crate::stats::FileWatcherStats;

// On each sync, recomputes hashes of all files in the repository.
// Useful for tests on unreliable filesystems,
// but probably not much elsewhere.
#[derive(Allocative)]
pub struct FsHashCrawler {
    root: ProjectRoot,
    cells: CellResolver,
    ignore_specs: HashMap<CellName, IgnoreSet>,
    snapshot: Arc<Mutex<FsSnapshot>>,
}

impl FsHashCrawler {
    pub fn new(
        root: &ProjectRoot,
        cells: CellResolver,
        ignore_specs: HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<Self> {
        let snapshot = Arc::new(Mutex::new(FsSnapshot::build(root, &cells)?));
        Ok(Self {
            root: root.dupe(),
            cells,
            ignore_specs,
            snapshot,
        })
    }

    async fn update(
        &self,
        mut dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, DiceTransactionUpdater)> {
        let root = self.root.dupe();
        let cells = self.cells.dupe();
        let new_snapshot =
            tokio::task::spawn_blocking(move || FsSnapshot::build(&root, &cells)).await??;
        let mut guard = self.snapshot.lock().unwrap();
        let old_snapshot = mem::replace(&mut *guard, new_snapshot);
        let (stats, changes) = old_snapshot.get_updates_for_dice(&guard, &self.ignore_specs)?;
        changes.write_to_dice(&mut dice)?;
        Ok((stats, dice))
    }
}

#[async_trait]
impl FileWatcher for FsHashCrawler {
    async fn sync(
        &self,
        dice: DiceTransactionUpdater,
    ) -> buck2_error::Result<(DiceTransactionUpdater, Mergebase)> {
        span_async(
            buck2_data::FileWatcherStart {
                provider: buck2_data::FileWatcherProvider::FsHashCrawler as i32,
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

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug)]
struct FsEvent {
    cell_path: CellPath,
    event: FileWatcherEventType,
    kind: FileWatcherKind,
}

#[derive(Allocative)]
enum EntryInfo {
    #[allocative(skip)]
    File(Hash),
    Directory,
    Symlink,
}

impl EntryInfo {
    fn to_file_watcher_kind(&self) -> FileWatcherKind {
        match self {
            EntryInfo::File(_) => FileWatcherKind::File,
            EntryInfo::Directory => FileWatcherKind::Directory,
            EntryInfo::Symlink => FileWatcherKind::Symlink,
        }
    }
}

#[derive(Allocative)]
struct FsSnapshot(HashMap<CellPath, EntryInfo>);

impl FsSnapshot {
    fn build(root: &ProjectRoot, cells: &CellResolver) -> buck2_error::Result<Self> {
        let mut snapshot = FsSnapshot(HashMap::new());
        snapshot.build_fs_snapshot(root, cells, root.root())?;
        Ok(snapshot)
    }

    fn add_entry(&mut self, cell: CellPath, info: EntryInfo) {
        self.0.insert(cell, info);
    }

    fn get_updates(&self, new_snapshot: &FsSnapshot) -> buck2_error::Result<Vec<FsEvent>> {
        let mut events = Vec::new();
        for (cell_path, prev_info) in self.0.iter() {
            if let Some(current_info) = new_snapshot.0.get(cell_path) {
                match (current_info, prev_info) {
                    (EntryInfo::File(cur), EntryInfo::File(prev)) => {
                        if cur != prev {
                            events.push(FsEvent {
                                cell_path: cell_path.to_owned(),
                                event: FileWatcherEventType::Modify,
                                kind: prev_info.to_file_watcher_kind(),
                            });
                        }
                    }
                    (EntryInfo::Directory, EntryInfo::Directory) => (),
                    // FIXME(JakobDegen): Track symlink targets
                    (EntryInfo::Symlink, EntryInfo::Symlink) => (),
                    (current_info, prev_info) => {
                        events.push(FsEvent {
                            cell_path: cell_path.to_owned(),
                            event: FileWatcherEventType::Delete,
                            kind: prev_info.to_file_watcher_kind(),
                        });
                        events.push(FsEvent {
                            cell_path: cell_path.to_owned(),
                            event: FileWatcherEventType::Create,
                            kind: current_info.to_file_watcher_kind(),
                        });
                    }
                }
            } else {
                events.push(FsEvent {
                    cell_path: cell_path.to_owned(),
                    event: FileWatcherEventType::Delete,
                    kind: prev_info.to_file_watcher_kind(),
                });
            }
        }
        let new_entries = new_snapshot
            .0
            .iter()
            .filter(|(path, _)| !self.0.contains_key(*path));
        for (cell_path, info) in new_entries {
            events.push(FsEvent {
                cell_path: cell_path.to_owned(),
                event: FileWatcherEventType::Create,
                kind: info.to_file_watcher_kind(),
            });
        }
        Ok(events)
    }

    fn get_updates_for_dice(
        &self,
        new_snapshot: &FsSnapshot,
        ignore_specs: &HashMap<CellName, IgnoreSet>,
    ) -> buck2_error::Result<(buck2_data::FileWatcherStats, FileChangeTracker)> {
        let events = self.get_updates(new_snapshot)?;
        let mut changed = FileChangeTracker::new();
        let mut stats = FileWatcherStats::new(Default::default(), events.len());
        let mut ignored = 0;
        for event in events.into_iter() {
            let ignore = ignore_specs
                .get(&event.cell_path.cell())
                .is_some_and(|i| i.is_match(event.cell_path.path()));

            if ignore {
                ignored += 1;
                continue;
            }

            stats.add(event.cell_path.to_string(), event.event, event.kind);
            match (event.event, event.kind) {
                (
                    FileWatcherEventType::Create,
                    FileWatcherKind::File | FileWatcherKind::Symlink,
                ) => {
                    changed.file_added_or_removed(event.cell_path);
                }
                (FileWatcherEventType::Create, FileWatcherKind::Directory) => {
                    changed.dir_added_or_removed(event.cell_path);
                }
                (
                    FileWatcherEventType::Modify,
                    FileWatcherKind::File | FileWatcherKind::Symlink,
                ) => {
                    changed.file_contents_changed(event.cell_path);
                }
                (FileWatcherEventType::Modify, FileWatcherKind::Directory) => {
                    // FIXME(JakobDegen): This should not be needed
                    changed.dir_entries_changed_force_invalidate(event.cell_path);
                }
                (
                    FileWatcherEventType::Delete,
                    FileWatcherKind::File | FileWatcherKind::Symlink,
                ) => {
                    changed.file_added_or_removed(event.cell_path);
                }
                (FileWatcherEventType::Delete, FileWatcherKind::Directory) => {
                    changed.dir_added_or_removed(event.cell_path);
                }
            }
        }
        stats.add_ignored(ignored);
        Ok((stats.finish(), changed))
    }

    fn build_fs_snapshot(
        &mut self,
        root: &ProjectRoot,
        cells: &CellResolver,
        disk_path: &AbsNormPath,
    ) -> buck2_error::Result<()> {
        for file in fs_util::read_dir(disk_path).categorize_internal()? {
            let file = file?;
            let filetype = file.file_type()?;
            let filename = file.file_name();

            let filename = FileNameBuf::try_from(CompactString::new(
                filename
                    .to_str()
                    .ok_or_else(|| internal_error!("Filename is not UTF-8"))?,
            ))
            .with_buck_error_context(|| format!("Invalid filename: {}", disk_path.display()))?;

            let disk_path = disk_path.join(filename);
            let rel_path = root.relativize(&disk_path)?;
            let cell_path = cells.get_cell_path(&rel_path);

            // We ignore buck-out and .hg dirs, as those are uninteresting events caused by us.
            if rel_path.starts_with(InvocationPaths::buck_out_dir_prefix())
                || rel_path.starts_with(ProjectRelativePath::unchecked_new(".hg"))
            {
                continue;
            }

            let filetype = FileType::from(filetype);
            match filetype {
                FileType::File => {
                    let hash = file_hash(disk_path.as_maybe_relativized())?;
                    self.add_entry(cell_path, EntryInfo::File(hash));
                }
                FileType::Directory => {
                    self.build_fs_snapshot(root, cells, &disk_path)?;
                    self.add_entry(cell_path, EntryInfo::Directory);
                }
                FileType::Symlink => {
                    self.add_entry(cell_path, EntryInfo::Symlink);
                }
                FileType::Unknown => (),
            }
        }
        Ok(())
    }
}

fn file_hash(path: &Path) -> buck2_error::Result<Hash> {
    let mut reader = File::open(path)?;
    let mut hasher = blake3::Hasher::new();

    let mut buffer = [0; 16 * 1024];
    loop {
        let count = reader.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }

    Ok(hasher.finalize())
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use buck2_core::cells::CellResolver;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_data::FileWatcherEventType;
    use buck2_data::FileWatcherKind;
    use buck2_fs::fs_util::uncategorized as fs_util;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_fs::paths::abs_path::AbsPathBuf;

    use crate::fs_hash_crawler::FsEvent;
    use crate::fs_hash_crawler::FsSnapshot;

    #[tokio::test]
    async fn test_fs_snapshot() -> buck2_error::Result<()> {
        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        );
        let tempdir = tempfile::tempdir()?;
        let root_path = fs_util::canonicalize(AbsNormPathBuf::new(tempdir.path().to_owned())?)?;
        let proj_root = ProjectRoot::new(root_path)?;

        let get_path = |path| -> buck2_error::Result<(AbsPathBuf, CellPath)> {
            let path = ProjectRelativePath::new(path).unwrap();
            let cell_path = cell_resolver.get_cell_path(path);
            Ok((proj_root.resolve(path).into_abs_path_buf(), cell_path))
        };
        let dir1 = proj_root.resolve(ProjectRelativePath::new("dir1")?);
        let (file1, file1_cell) = get_path("dir1/file1")?;
        let (dir2, dir2_cell) = get_path("dir2")?;
        let (file2, file2_cell) = get_path("dir2/file2")?;
        let (file3, file3_cell) = get_path("dir1/file3")?;
        fs_util::create_dir_all(dir1)?;
        fs_util::write(&file1, "old content")?;
        fs_util::create_dir_all(&dir2)?;
        fs_util::write(file2, "old content")?;

        let old_snapshot = FsSnapshot::build(&proj_root, &cell_resolver)?;
        fs_util::write(file1, "new content")?;
        fs_util::remove_all(dir2)?;
        fs_util::write(file3, "new content")?;
        let new_snapshot = FsSnapshot::build(&proj_root, &cell_resolver)?;
        let events = old_snapshot.get_updates(&new_snapshot)?;

        let expected = [
            FsEvent {
                cell_path: file1_cell,
                event: FileWatcherEventType::Modify,
                kind: FileWatcherKind::File,
            },
            FsEvent {
                cell_path: file3_cell,
                event: FileWatcherEventType::Create,
                kind: FileWatcherKind::File,
            },
            FsEvent {
                cell_path: dir2_cell,
                event: FileWatcherEventType::Delete,
                kind: FileWatcherKind::Directory,
            },
            FsEvent {
                cell_path: file2_cell,
                event: FileWatcherEventType::Delete,
                kind: FileWatcherKind::File,
            },
        ];

        let events = events.iter().collect::<BTreeSet<_>>();
        let expected = expected.iter().collect::<BTreeSet<_>>();
        assert_eq!(events, expected);
        Ok(())
    }
}

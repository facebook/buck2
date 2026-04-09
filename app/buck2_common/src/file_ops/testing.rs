/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_fs::paths::file_name::FileNameBuf;
use cmp_any::PartialEqAny;
use dice::DiceComputations;
use dice::testing::DiceBuilder;
use dupe::Dupe;
use itertools::Itertools;
use pagable::PagablePanic;
use pagable::pagable_typetag;

use crate::cas_digest::CasDigestConfig;
use crate::external_symlink::ExternalSymlink;
use crate::file_ops::delegate::FileOpsDelegate;
use crate::file_ops::delegate::FileOpsDelegateWithIgnores;
use crate::file_ops::delegate::testing::FileOpsKey;
use crate::file_ops::delegate::testing::FileOpsValue;
use crate::file_ops::dice::CheckIgnores;
use crate::file_ops::dice::ReadFileProxy;
use crate::file_ops::metadata::FileMetadata;
use crate::file_ops::metadata::FileType;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::RawSymlink;
use crate::file_ops::metadata::ReadDirOutput;
use crate::file_ops::metadata::SimpleDirEntry;
use crate::file_ops::metadata::TrackedFileDigest;
use crate::file_ops::trait_::FileOps;
use crate::ignores::file_ignores::FileIgnoreResult;

#[derive(Clone)]
enum TestFileOpsEntry {
    File(String /*data*/, FileMetadata),
    ExternalSymlink(Arc<ExternalSymlink>, Option<String> /*data*/),
    /// A symlink to another path within the project. The CellPath is the
    /// resolved target (what `at` points to after resolving), and the Symlink
    /// holds the raw relative target string.
    InternalSymlink(CellPath, Arc<crate::file_ops::metadata::Symlink>),
    Directory(BTreeSet<SimpleDirEntry>),
}

/// Immutable snapshot of test filesystem state. Captured in `ReadFileProxy`
/// closures so that cached proxies keep their original data even after the
/// live state is mutated.
#[derive(Clone, Dupe)]
struct TestFileOpsSnapshot(Arc<BTreeMap<CellPath, TestFileOpsEntry>>);

#[async_trait]
impl FileOps for TestFileOpsSnapshot {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<String>> {
        Ok(self.0.get(&path.to_owned()).and_then(|e| match e {
            TestFileOpsEntry::File(data, ..) => Some(data.clone()),
            TestFileOpsEntry::ExternalSymlink(_, data) => data.clone(),
            TestFileOpsEntry::InternalSymlink(target, _) => {
                self.0.get(target).and_then(|e| match e {
                    TestFileOpsEntry::File(data, ..) => Some(data.clone()),
                    _ => None,
                })
            }
            _ => None,
        }))
    }

    async fn read_dir(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<ReadDirOutput> {
        let included = self
            .0
            .get(&path.to_owned())
            .and_then(|e| match e {
                TestFileOpsEntry::Directory(listing) => {
                    Some(listing.iter().cloned().sorted().collect::<Vec<_>>().into())
                }
                _ => None,
            })
            .ok_or_else(|| {
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "couldn't find dir {:?}",
                    path
                )
            })?;
        Ok(ReadDirOutput { included })
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        // Walk ancestors to find directory symlinks, like the real IO layer.
        let cell = path.cell();
        let mut components = path.path().iter();
        let mut current = CellPath::new(cell, CellRelativePathBuf::unchecked_new(String::new()));
        while let Some(component) = components.next() {
            current = current.join(component);
            if let Some(TestFileOpsEntry::InternalSymlink(target, symlink)) = self.0.get(&current) {
                let rest: buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf =
                    components.collect();
                let mut resolved = target.clone();
                if !rest.is_empty() {
                    resolved = resolved.join(&rest);
                }
                return Ok(Some(RawPathMetadata::Symlink {
                    at: Arc::new(current),
                    to: RawSymlink::Relative(Arc::new(resolved), symlink.dupe()),
                }));
            }
        }

        self.0.get(&path.to_owned()).map_or(Ok(None), |e| {
            match e {
                TestFileOpsEntry::File(_data, metadata) => {
                    Ok(RawPathMetadata::File(metadata.to_owned()))
                }
                TestFileOpsEntry::ExternalSymlink(sym, _) => Ok(RawPathMetadata::Symlink {
                    at: Arc::new(path.to_owned()),
                    to: RawSymlink::External(sym.dupe()),
                }),
                TestFileOpsEntry::InternalSymlink(..) => {
                    unreachable!("should have been handled in ancestor walk above")
                }
                TestFileOpsEntry::Directory(..) => Ok(RawPathMetadata::Directory),
            }
            .map(Some)
        })
    }

    async fn is_ignored(
        &self,
        _path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<FileIgnoreResult> {
        Ok(FileIgnoreResult::Ok)
    }

    async fn buildfiles<'a>(&self, _cell: CellName) -> buck2_error::Result<Arc<[FileNameBuf]>> {
        Ok(Arc::from_iter([FileNameBuf::unchecked_new("BUCK")]))
    }
}

/// Mutable handle to test filesystem state. Holds a shared slot that
/// `TestCellFileOps` snapshots from on each DICE recomputation.
#[derive(Allocative, Clone, Dupe)]
pub struct TestFileOps {
    #[allocative(skip)]
    live: Arc<Mutex<Arc<BTreeMap<CellPath, TestFileOpsEntry>>>>,
}

/// Different cells are presumed to be adjacent on disk.
fn compute_raw_target(from: &CellPath, to: &CellPath) -> buck2_fs::paths::RelativePathBuf {
    let from_parts: Vec<&str> = std::iter::once(from.cell().as_str())
        .chain(from.path().iter().map(|c| c.as_str()))
        .collect();
    let to_parts: Vec<&str> = std::iter::once(to.cell().as_str())
        .chain(to.path().iter().map(|c| c.as_str()))
        .collect();
    let common = from_parts
        .iter()
        .zip(to_parts.iter())
        .take_while(|(a, b)| a == b)
        .count();
    let up_count = from_parts.len() - 1 - common;
    let mut parts: Vec<&str> = std::iter::repeat_n("..", up_count).collect();
    parts.extend_from_slice(&to_parts[common..]);
    buck2_fs::paths::RelativePathBuf::from(parts.join("/"))
}

fn build_entries(
    inputs: BTreeMap<CellPath, TestFileOpsEntry>,
) -> BTreeMap<CellPath, TestFileOpsEntry> {
    let mut entries = BTreeMap::new();
    for (path, entry) in inputs {
        let mut file_type = match entry {
            TestFileOpsEntry::Directory(..) => FileType::Directory,
            TestFileOpsEntry::ExternalSymlink(..) | TestFileOpsEntry::InternalSymlink(..) => {
                FileType::Symlink
            }
            TestFileOpsEntry::File(..) => FileType::File,
        };
        // make sure the test setup is correct and concise
        assert!(
            entries.insert(path.to_owned(), entry).is_none(),
            "Adding `{path}`, it already exists."
        );

        let mut path = path.as_ref();

        // now add to / create the parent directories
        while let (Some(dir), Some(name)) = (path.parent(), path.path().file_name()) {
            let dir_entry = entries
                .entry(dir.to_owned())
                .or_insert_with(|| TestFileOpsEntry::Directory(BTreeSet::new()));
            match dir_entry {
                TestFileOpsEntry::Directory(listing) => {
                    listing.insert(SimpleDirEntry {
                        file_type,
                        file_name: name.to_owned(),
                    });
                    file_type = FileType::Directory;
                    path = dir;
                }
                _ => panic!("Adding `{path}`, but `{dir}` exists and is not a dir"),
            };
        }
    }
    entries
}

impl TestFileOps {
    fn new(inputs: BTreeMap<CellPath, TestFileOpsEntry>) -> Self {
        TestFileOps {
            live: Arc::new(Mutex::new(Arc::new(build_entries(inputs)))),
        }
    }

    pub fn new_with_files(files: BTreeMap<CellPath, String>) -> Self {
        let cas_digest_config = CasDigestConfig::testing_default();

        Self::new(
            files
                .into_iter()
                .map(|(path, data)| {
                    (
                        path,
                        TestFileOpsEntry::File(
                            data.clone(),
                            FileMetadata {
                                digest: TrackedFileDigest::from_content(
                                    data.as_bytes(),
                                    cas_digest_config,
                                ),
                                is_executable: false,
                            },
                        ),
                    )
                })
                .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
        )
    }

    pub fn new_with_files_metadata(files: BTreeMap<CellPath, FileMetadata>) -> Self {
        Self::new(
            files
                .into_iter()
                .map(|(path, m)| (path, TestFileOpsEntry::File("".to_owned(), m)))
                .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
        )
    }

    pub fn new_with_symlinks(symlinks: BTreeMap<CellPath, Arc<ExternalSymlink>>) -> Self {
        Self::new(
            symlinks
                .into_iter()
                .map(|(path, s)| (path, TestFileOpsEntry::ExternalSymlink(s, None)))
                .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
        )
    }

    pub fn new_with_symlinks_and_external_content(
        symlinks: BTreeMap<CellPath, (Arc<ExternalSymlink>, String)>,
    ) -> Self {
        Self::new(
            symlinks
                .into_iter()
                .map(|(path, (s, data))| (path, TestFileOpsEntry::ExternalSymlink(s, Some(data))))
                .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
        )
    }

    /// Creates a TestFileOps from a mix of files and internal (relative) symlinks.
    /// `symlinks`: (symlink_path, resolved_target_path). The raw relative
    /// target is computed automatically, including cross-cell symlinks.
    pub fn new_with_files_and_symlinks(
        files: BTreeMap<CellPath, String>,
        symlinks: Vec<(CellPath, CellPath)>,
    ) -> Self {
        let cas_digest_config = CasDigestConfig::testing_default();

        let mut entries = BTreeMap::new();
        for (path, data) in files {
            entries.insert(
                path,
                TestFileOpsEntry::File(
                    data.clone(),
                    FileMetadata {
                        digest: TrackedFileDigest::from_content(data.as_bytes(), cas_digest_config),
                        is_executable: false,
                    },
                ),
            );
        }
        for (link_path, resolved) in symlinks {
            let raw = compute_raw_target(&link_path, &resolved);
            entries.insert(
                link_path,
                TestFileOpsEntry::InternalSymlink(
                    resolved,
                    Arc::new(crate::file_ops::metadata::Symlink::new(raw)),
                ),
            );
        }
        Self::new(entries)
    }

    /// Get a snapshot of the current filesystem state
    fn snapshot(&self) -> TestFileOpsSnapshot {
        TestFileOpsSnapshot(self.live.lock().unwrap().dupe())
    }

    /// Replace the whole filesystem with another
    pub fn replace_live(&self, new: &TestFileOps) {
        *self.live.lock().unwrap() = new.live.lock().unwrap().dupe();
    }

    /// Sets the content of a particular file. Does NOT invalidate the path, you must also call
    /// `dirty_and_commit`.
    pub fn set_file_content(&self, path: &CellPath, content: &str) {
        let cas_digest_config = CasDigestConfig::testing_default();
        let mut live = self.live.lock().unwrap();
        let mut entries = (**live).clone();
        match entries.get_mut(path) {
            Some(TestFileOpsEntry::File(data, metadata)) => {
                *data = content.to_owned();
                *metadata = FileMetadata {
                    digest: TrackedFileDigest::from_content(content.as_bytes(), cas_digest_config),
                    is_executable: false,
                };
            }
            _ => panic!("set_file_content: path {path} not found or not a file"),
        }
        *live = Arc::new(entries);
    }

    pub fn set_external_symlink_content(&self, path: &CellPath, content: &str) {
        let mut live = self.live.lock().unwrap();
        let mut entries = (**live).clone();
        match entries.get_mut(path) {
            Some(TestFileOpsEntry::ExternalSymlink(_, data)) => {
                *data = Some(content.to_owned());
            }
            _ => panic!(
                "set_external_symlink_content: path {path} not found or not an external symlink"
            ),
        }
        *live = Arc::new(entries);
    }

    pub fn mock_in_cell(&self, cell: CellName, builder: DiceBuilder) -> DiceBuilder {
        let data = Ok(FileOpsValue(FileOpsDelegateWithIgnores::new(
            None,
            Arc::new(TestCellFileOps(
                cell,
                Self {
                    live: Arc::clone(&self.live),
                },
            )),
        )));
        builder
            .mock_and_return(
                FileOpsKey {
                    cell,
                    check_ignores: CheckIgnores::Yes,
                },
                data.dupe(),
            )
            .mock_and_return(
                FileOpsKey {
                    cell,
                    check_ignores: CheckIgnores::No,
                },
                data,
            )
    }
}

#[async_trait]
impl FileOps for TestFileOps {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<String>> {
        self.snapshot().read_file_if_exists(path).await
    }

    async fn read_dir(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<ReadDirOutput> {
        self.snapshot().read_dir(path).await
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        self.snapshot().read_path_metadata_if_exists(path).await
    }

    async fn is_ignored(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<FileIgnoreResult> {
        self.snapshot().is_ignored(path).await
    }

    async fn buildfiles<'a>(&self, cell: CellName) -> buck2_error::Result<Arc<[FileNameBuf]>> {
        self.snapshot().buildfiles(cell).await
    }
}

#[derive(PagablePanic)]
pub struct TestCellFileOps(CellName, TestFileOps);

#[pagable_typetag]
#[async_trait]
impl FileOpsDelegate for TestCellFileOps {
    async fn read_file_if_exists(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy> {
        let snap = self.1.snapshot();
        Ok(ReadFileProxy::new_with_captures(
            (CellPath::new(self.0, path.to_owned()), snap),
            move |(path, snap)| async move { FileOps::read_file_if_exists(&snap, path.as_ref()).await },
        ))
    }

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        let snap = self.1.snapshot();
        let path = CellPath::new(self.0, path.to_owned());
        let simple_entries = FileOps::read_dir(&snap, path.as_ref()).await?.included;
        Ok(simple_entries
            .iter()
            .map(|e| RawDirEntry {
                file_name: e.file_name.clone().into_inner(),
                file_type: e.file_type,
            })
            .collect())
    }

    async fn read_path_metadata_if_exists(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let snap = self.1.snapshot();
        let path = CellPath::new(self.0, path.to_owned());
        FileOps::read_path_metadata_if_exists(&snap, path.as_ref()).await
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::always_false()
    }
}

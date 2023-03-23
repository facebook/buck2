/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::hash::Hash;
use std::path::Path;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use compact_str::CompactString;
use derive_more::Display;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;

use crate::cas_digest::CasDigest;
use crate::cas_digest::CasDigestConfig;
use crate::cas_digest::TrackedCasDigest;
use crate::cas_digest::TrackedCasDigestKind;
use crate::external_symlink::ExternalSymlink;

#[derive(Debug, thiserror::Error)]
enum FileOpsError {
    #[error("File not found: `{0}`")]
    FileNotFound(String),
}

/// std::fs::FileType is an opaque type that isn't constructible. This is
/// basically the equivalent.
#[derive(Clone, Dupe, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Allocative)]
pub enum FileType {
    Directory,
    File,
    Symlink,
    /// Some special files (for example, unix domain sockets) will have a
    /// filetype that we don't recognize.
    Unknown,
}

impl From<std::fs::FileType> for FileType {
    fn from(fs_type: std::fs::FileType) -> Self {
        if fs_type.is_file() {
            FileType::File
        } else if fs_type.is_dir() {
            FileType::Directory
        } else if fs_type.is_symlink() {
            FileType::Symlink
        } else {
            FileType::Unknown
        }
    }
}

impl FileType {
    pub fn is_dir(&self) -> bool {
        matches!(self, FileType::Directory)
    }

    pub fn is_file(&self) -> bool {
        matches!(self, FileType::File)
    }

    pub fn is_symlink(&self) -> bool {
        matches!(self, FileType::Symlink)
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Allocative)]
pub struct SimpleDirEntry {
    // Put the `file_name` first so we sort by it (which is what people expect)
    pub file_name: FileNameBuf,
    pub file_type: FileType,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Allocative)]
pub struct RawDirEntry {
    // Put the `file_name` first so we sort by it (which is what people expect)
    /// Not all file names are accepted as file names in Buck. Such files are automatically ignored.
    /// This should probably be something like `CompactOsString`.
    pub file_name: CompactString,
    pub file_type: FileType,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Dupe, Allocative)]
pub struct ReadDirOutput {
    /// Sorted.
    pub included: Arc<[SimpleDirEntry]>,
    /// Sorted.
    pub ignored: Arc<[RawDirEntry]>,
}

impl ReadDirOutput {
    /// Is the file name in the directory listing. Ignores files that were explicitly ignored.
    pub fn contains(&self, file_name: &FileName) -> bool {
        self.included.iter().any(|x| x.file_name == file_name)
    }
}

#[derive(Allocative)]
pub struct FileDigestKind {
    _private: (),
}

impl TrackedCasDigestKind for FileDigestKind {
    fn empty_digest(config: CasDigestConfig) -> Option<TrackedCasDigest<Self>> {
        Some(config.empty_file_digest())
    }
}

pub type FileDigest = CasDigest<FileDigestKind>;

pub type TrackedFileDigest = TrackedCasDigest<FileDigestKind>;

impl FileDigest {
    /// Obtain the digest of the file if you can.
    pub fn from_file<P>(file: P, config: CasDigestConfig) -> anyhow::Result<Self>
    where
        P: AsRef<Path>,
    {
        let file = file.as_ref();
        match Self::from_file_attr(file, config) {
            Some(x) => Ok(x),
            None => Self::from_file_disk(file, config),
        }
    }

    /// Read the file from the xattr, or skip if it's not available.
    #[cfg(unix)]
    fn from_file_attr(file: &Path, config: CasDigestConfig) -> Option<Self> {
        use std::borrow::Cow;
        use std::collections::HashSet;

        use buck2_core::fs::fs_util;

        use crate::cas_digest::RawDigest;

        if !config.allows_sha1() {
            return None;
        }

        let mut file = Cow::Borrowed(file);
        let mut meta;
        let mut visited = HashSet::new();

        // NOTE: We have to look through symlinks first because `xattr::get` doesn't (at least not
        // on MacOS, it seems), and instead we end up with the hash of the symlink destination as a
        // string.
        loop {
            meta = fs_util::symlink_metadata(&file).ok()?;
            if !meta.is_symlink() {
                break;
            }

            let dest = fs_util::read_link(&file).ok()?;
            let dest = if dest.is_absolute() {
                dest
            } else {
                match file.parent() {
                    Some(parent) => parent.join(dest),
                    None => dest,
                }
            };

            let prev = std::mem::replace(&mut file, Cow::Owned(dest));

            if !visited.insert(prev.into_owned()) {
                // We hit a loop.
                return None;
            }
        }

        match xattr::get(&file, "user.sha1") {
            Ok(Some(v)) => {
                let sha1 = RawDigest::parse_sha1(&v).ok()?;
                let size = meta.len();
                Some(Self::new(sha1, size))
            }
            _ => None,
        }
    }

    /// Windows doesn't support extended file attributes.
    #[cfg(windows)]
    fn from_file_attr(_file: &Path, _config: CasDigestConfig) -> Option<Self> {
        None
    }

    /// Get the digest from disk. You should usually prefer `from_file`
    /// which also uses faster methods of getting the SHA1 if it can.
    pub fn from_file_disk(file: &Path, config: CasDigestConfig) -> anyhow::Result<Self> {
        let f = File::open(file)?;
        FileDigest::from_reader(f, config)
    }
}

/// Stores the relevant metadata for a file.
// New fields should be added as needed, and unused fields removed.
#[derive(Debug, Dupe, Hash, PartialEq, Eq, Clone, Display, Allocative)]
#[display(fmt = "File({})", digest)]
pub struct FileMetadata {
    pub digest: TrackedFileDigest,
    pub is_executable: bool,
}

impl FileMetadata {
    /// Metadata of an empty file
    pub fn empty(config: CasDigestConfig) -> Self {
        Self {
            digest: TrackedFileDigest::empty(config),
            is_executable: false,
        }
    }
}

/// Stores the relevant metadata for a path.
#[derive(Debug, Dupe, PartialEq, Eq, Clone)]
pub enum PathMetadata {
    File(FileMetadata),
    ExternalSymlink(Arc<ExternalSymlink>),
    Directory,
}

#[derive(Debug, PartialEq, Dupe, Eq, Clone, Allocative)]
pub enum RawPathMetadata<T = Arc<CellPath>> {
    Symlink { at: T, to: RawSymlink<T> },
    File(FileMetadata),
    Directory,
}

#[derive(Debug, Dupe, PartialEq, Eq, Clone, Allocative)]
pub enum RawSymlink<T> {
    Relative(T),
    External(Arc<ExternalSymlink>),
}

/// Stores the relevant metadata for a path.
#[derive(Debug, Dupe, PartialEq, Eq, Clone)]
pub enum PathMetadataOrRedirection<T = Arc<CellPath>> {
    PathMetadata(PathMetadata),
    Redirection(T),
}

impl<T> PathMetadataOrRedirection<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> PathMetadataOrRedirection<O> {
        match self {
            Self::PathMetadata(meta) => PathMetadataOrRedirection::PathMetadata(meta),
            Self::Redirection(r) => PathMetadataOrRedirection::Redirection(f(r)),
        }
    }
}

impl<T> From<RawPathMetadata<T>> for PathMetadataOrRedirection<T> {
    fn from(meta: RawPathMetadata<T>) -> Self {
        match meta {
            RawPathMetadata::Directory => {
                PathMetadataOrRedirection::PathMetadata(PathMetadata::Directory)
            }
            RawPathMetadata::File(file) => {
                PathMetadataOrRedirection::PathMetadata(PathMetadata::File(file))
            }
            RawPathMetadata::Symlink {
                at: _,
                to: RawSymlink::Relative(r),
            } => PathMetadataOrRedirection::Redirection(r),
            RawPathMetadata::Symlink {
                at: _,
                to: RawSymlink::External(e),
            } => PathMetadataOrRedirection::PathMetadata(PathMetadata::ExternalSymlink(e)),
        }
    }
}

impl<T> RawPathMetadata<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> RawPathMetadata<O> {
        match Self::try_map::<O, RawPathMetadata<O>>(self, |v| Ok(f(v))) {
            Ok(out) => out,
            Err(e) => e,
        }
    }

    pub fn try_map<O, E>(self, f: impl Fn(T) -> Result<O, E>) -> Result<RawPathMetadata<O>, E> {
        match self {
            Self::Directory => Ok(RawPathMetadata::Directory),
            Self::File(file) => Ok(RawPathMetadata::File(file)),
            Self::Symlink {
                at,
                to: RawSymlink::Relative(dest),
            } => Ok(RawPathMetadata::Symlink {
                at: f(at)?,
                to: RawSymlink::Relative(f(dest)?),
            }),
            Self::Symlink {
                at,
                to: RawSymlink::External(e),
            } => Ok(RawPathMetadata::Symlink {
                at: f(at)?,
                to: RawSymlink::External(e),
            }),
        }
    }
}

impl<T> From<PathMetadata> for PathMetadataOrRedirection<T> {
    fn from(meta: PathMetadata) -> Self {
        Self::PathMetadata(meta)
    }
}

#[async_trait]
pub trait FileOps: Allocative + Send + Sync {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<String>>;

    async fn read_dir_with_ignores(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<ReadDirOutput>;

    async fn is_ignored(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<bool>;

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<RawPathMetadata>>;

    fn eq_token(&self) -> PartialEqAny;
}

impl dyn FileOps + '_ {
    pub async fn read_file(&self, path: CellPathRef<'_>) -> anyhow::Result<String> {
        self.read_file_if_exists(path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }

    /// Return the list of file outputs, sorted.
    pub async fn read_dir(&self, path: CellPathRef<'_>) -> anyhow::Result<Arc<[SimpleDirEntry]>> {
        Ok(self.read_dir_with_ignores(path).await?.included)
    }

    pub async fn try_exists(&self, path: CellPathRef<'_>) -> anyhow::Result<bool> {
        Ok(self.read_path_metadata_if_exists(path).await?.is_some())
    }

    pub async fn read_path_metadata(
        &self,
        path: CellPathRef<'_>,
    ) -> anyhow::Result<RawPathMetadata> {
        self.read_path_metadata_if_exists(path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }
}

impl PartialEq for dyn FileOps {
    fn eq(&self, other: &dyn FileOps) -> bool {
        self.eq_token() == other.eq_token()
    }
}

pub mod testing {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_path::CellPathRef;
    use dupe::Dupe;
    use gazebo::cmp::PartialEqAny;
    use itertools::Itertools;

    use crate::cas_digest::CasDigestConfig;
    use crate::external_symlink::ExternalSymlink;
    use crate::file_ops::FileMetadata;
    use crate::file_ops::FileOps;
    use crate::file_ops::FileType;
    use crate::file_ops::RawPathMetadata;
    use crate::file_ops::RawSymlink;
    use crate::file_ops::ReadDirOutput;
    use crate::file_ops::SimpleDirEntry;
    use crate::file_ops::TrackedFileDigest;

    enum TestFileOpsEntry {
        File(String /*data*/, FileMetadata),
        ExternalSymlink(Arc<ExternalSymlink>),
        Directory(BTreeSet<SimpleDirEntry>),
    }

    #[derive(Allocative)]
    pub struct TestFileOps {
        #[allocative(skip)]
        entries: BTreeMap<CellPath, TestFileOpsEntry>,
    }

    impl TestFileOps {
        fn new(entries: BTreeMap<CellPath, TestFileOpsEntry>) -> Self {
            let mut file_ops = Self {
                entries: BTreeMap::new(),
            };
            for (path, entry) in entries {
                file_ops.add_entry(path, entry);
            }
            file_ops
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
                    .map(|(path, s)| (path, TestFileOpsEntry::ExternalSymlink(s)))
                    .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
            )
        }

        fn add_entry(&mut self, path: CellPath, entry: TestFileOpsEntry) {
            let mut file_type = match entry {
                TestFileOpsEntry::Directory(..) => FileType::Directory,
                TestFileOpsEntry::ExternalSymlink(..) => FileType::Symlink,
                TestFileOpsEntry::File(..) => FileType::File,
            };
            // make sure the test setup is correct and concise
            assert!(
                self.entries.insert(path.to_owned(), entry).is_none(),
                "Adding `{}`, it already exists.",
                path
            );

            let mut path = path.as_ref();

            // now add to / create the parent directories
            while let (Some(dir), Some(name)) = (path.parent(), path.path().file_name()) {
                let dir_entry = self
                    .entries
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
                    _ => panic!("Adding `{}`, but `{}` exists and is not a dir", path, dir),
                };
            }
        }
    }

    #[async_trait]
    impl FileOps for TestFileOps {
        async fn read_file_if_exists(
            &self,
            path: CellPathRef<'async_trait>,
        ) -> anyhow::Result<Option<String>> {
            Ok(self.entries.get(&path.to_owned()).and_then(|e| match e {
                TestFileOpsEntry::File(data, ..) => Some(data.clone()),
                _ => None,
            }))
        }

        async fn read_dir_with_ignores(
            &self,
            path: CellPathRef<'async_trait>,
        ) -> anyhow::Result<ReadDirOutput> {
            let included = self
                .entries
                .get(&path.to_owned())
                .and_then(|e| match e {
                    TestFileOpsEntry::Directory(listing) => {
                        Some(listing.iter().cloned().sorted().collect::<Vec<_>>().into())
                    }
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!("couldn't find dir {:?}", path))?;
            Ok(ReadDirOutput {
                included,
                ignored: Vec::new().into(),
            })
        }

        async fn read_path_metadata_if_exists(
            &self,
            path: CellPathRef<'async_trait>,
        ) -> anyhow::Result<Option<RawPathMetadata>> {
            self.entries.get(&path.to_owned()).map_or(Ok(None), |e| {
                match e {
                    TestFileOpsEntry::File(_data, metadata) => {
                        Ok(RawPathMetadata::File(metadata.to_owned()))
                    }
                    TestFileOpsEntry::ExternalSymlink(sym) => Ok(RawPathMetadata::Symlink {
                        at: Arc::new(path.to_owned()),
                        to: RawSymlink::External(sym.dupe()),
                    }),
                    _ => Err(anyhow::anyhow!("couldn't get metadata for {:?}", path)),
                }
                .map(Some)
            })
        }

        async fn is_ignored(&self, _path: CellPathRef<'async_trait>) -> anyhow::Result<bool> {
            Ok(false)
        }

        fn eq_token(&self) -> PartialEqAny {
            PartialEqAny::always_false()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Borrow;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;

    use super::*;

    #[cfg(unix)]
    mod unix {
        use std::os::unix::fs::symlink;

        use anyhow::Context;
        use buck2_core::fs::fs_util;

        use super::*;
        use crate::cas_digest::SHA1_SIZE;

        #[test]
        fn test_from_file_attr() -> anyhow::Result<()> {
            let config = CasDigestConfig::testing_default();
            let tempdir = tempfile::tempdir()?;

            let file = tempdir.path().join("dest");
            fs_util::write(&file, "foo")?;
            xattr::set(
                &file,
                "user.sha1",
                b"0000000000000000000000000000000000000000",
            )?;

            symlink("dest", tempdir.path().join("link"))?;
            symlink(tempdir.path().join("dest"), tempdir.path().join("abs_link"))?;
            symlink("link", tempdir.path().join("recurse_link"))?;
            symlink("recurse_link", tempdir.path().join("recurse_recurse_link"))?;

            let d1 = FileDigest::from_file(&file, config).context("file")?;
            let d2 = FileDigest::from_file(tempdir.path().join("link"), config).context("file")?;
            let d3 = FileDigest::from_file(tempdir.path().join("abs_link"), config)
                .context("abs_link")?;
            let d4 = FileDigest::from_file(tempdir.path().join("recurse_link"), config)
                .context("recurse_link")?;
            let d5 = FileDigest::from_file(tempdir.path().join("recurse_recurse_link"), config)
                .context("recurse_recurse_link")?;

            assert_eq!(d1.raw_digest().as_bytes(), &[0; SHA1_SIZE][..]);
            assert_eq!(d1.size(), 3);
            assert_eq!(d1, d2);
            assert_eq!(d1, d3);
            assert_eq!(d1, d4);
            assert_eq!(d1, d5);

            let dx = FileDigest::from_file(&file, crate::cas_digest::testing::sha256())
                .context("disable xattr")?;

            assert_ne!(d1, dx);

            Ok(())
        }

        #[test]
        fn test_from_file_attr_loop() -> anyhow::Result<()> {
            let tempdir = tempfile::tempdir()?;

            symlink("loop1", tempdir.path().join("loop2"))?;
            symlink("loop2", tempdir.path().join("loop1"))?;

            Ok(())
        }
    }

    #[test]
    fn test_tracked_file_digest_equivalence() {
        let digest_config = CasDigestConfig::testing_default();
        let digest = FileDigest::from_content(b"foo", digest_config);
        let tracked_digest = TrackedFileDigest::new(digest.dupe(), digest_config);

        assert_eq!(&digest, tracked_digest.borrow());

        let mut hasher_digest = DefaultHasher::new();
        digest.hash(&mut hasher_digest);

        let mut hasher_tracked_digest = DefaultHasher::new();
        tracked_digest.hash(&mut hasher_tracked_digest);

        assert_eq!(hasher_digest.finish(), hasher_tracked_digest.finish());
        assert_eq!(digest.to_string(), tracked_digest.to_string());
    }
}

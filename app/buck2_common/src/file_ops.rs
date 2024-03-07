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
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::buck2_env;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use cmp_any::PartialEqAny;
use compact_str::CompactString;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::VariantName;

use crate::cas_digest::CasDigest;
use crate::cas_digest::CasDigestConfig;
use crate::cas_digest::CasDigestKind;
use crate::cas_digest::TrackedCasDigest;
use crate::external_symlink::ExternalSymlink;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum FileOpsError {
    #[error("File not found: `{0}`")]
    // File not found errors are not inherently always user errors; however, we only use these
    // methods with source files, and in that case this is correct
    #[buck2(user)]
    #[buck2(tag = IoNotFound)]
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

impl CasDigestKind for FileDigestKind {
    fn empty_digest(config: CasDigestConfig) -> Option<TrackedCasDigest<Self>> {
        Some(config.empty_file_digest())
    }
}

pub type FileDigest = CasDigest<FileDigestKind>;

pub type TrackedFileDigest = TrackedCasDigest<FileDigestKind>;

#[derive(Copy, Dupe, Clone)]
pub struct FileDigestConfig {
    config: CasDigestConfig,
}

impl FileDigestConfig {
    /// Obtain a FileDigestConfig for hashing source files.
    pub fn source(config: CasDigestConfig) -> Self {
        Self {
            config: config.source_files_config(),
        }
    }

    /// Obtain a FileDigestConfig for hashing output files.
    pub fn build(config: CasDigestConfig) -> Self {
        Self { config }
    }

    /// Obtain the underlying CasDigestConfig.
    pub fn as_cas_digest_config(&self) -> CasDigestConfig {
        self.config
    }
}

impl FileDigest {
    /// Obtain the digest of the file if you can.
    pub fn from_file(file: &AbsPath, config: FileDigestConfig) -> anyhow::Result<Self> {
        if !buck2_env!("BUCK2_DISABLE_FILE_ATTR", bool)? {
            if let Some(digest) = Self::from_file_attr(file, config) {
                return Ok(digest);
            }
        }

        Self::from_file_disk(file, config)
    }

    /// Read the file from the xattr, or skip if it's not available.
    #[cfg(unix)]
    fn from_file_attr(file: &AbsPath, config: FileDigestConfig) -> Option<Self> {
        use buck2_core::fs::fs_util;

        use crate::cas_digest::RawDigest;

        enum Digest {
            Sha1,
            Blake3Keyed,
        }

        let digest = if config.as_cas_digest_config().allows_sha1() {
            Digest::Sha1
        } else if config.as_cas_digest_config().allows_blake3_keyed() {
            Digest::Blake3Keyed
        } else {
            return None;
        };

        let meta = fs_util::symlink_metadata(file).ok()?;

        // This really shouldn't happen (and it better not because we're about to read the xattr of
        // a symlink otherwise) but this method doesn't have the ability to return an error.
        if meta.is_symlink() {
            return None;
        }

        let raw_digest = match digest {
            Digest::Sha1 => xattr::get(file.as_maybe_relativized(), "user.sha1")
                .ok()
                .flatten()
                .and_then(|v| RawDigest::parse_sha1(&v).ok()),
            // NOTE: Eden returns *keyed* blake3 in user.blake3, so we use that.
            Digest::Blake3Keyed => xattr::get(file.as_maybe_relativized(), "user.blake3")
                .ok()
                .flatten()
                .and_then(|v| RawDigest::parse_blake3_keyed(&v).ok()),
        };

        raw_digest.map(|raw| Self::new(raw, meta.len()))
    }

    /// Windows doesn't support extended file attributes.
    #[cfg(windows)]
    fn from_file_attr(_file: &AbsPath, _config: FileDigestConfig) -> Option<Self> {
        None
    }

    /// Get the digest from disk. You should usually prefer `from_file`
    /// which also uses faster methods of getting the SHA1 if it can.
    pub fn from_file_disk(file: &AbsPath, config: FileDigestConfig) -> anyhow::Result<Self> {
        let f = File::open(file.as_maybe_relativized())?;
        FileDigest::from_reader(f, config.as_cas_digest_config())
    }
}

/// Stores the relevant metadata for a file.
// New fields should be added as needed, and unused fields removed.
#[derive(Debug, Dupe, Hash, PartialEq, Eq, Clone, Display, Allocative)]
#[display(fmt = "File(digest={}, is_executable={})", digest, is_executable)]
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

#[derive(Debug, PartialEq, Dupe, Eq, Clone, Allocative, VariantName)]
pub enum RawPathMetadata<T = Arc<CellPath>> {
    Symlink { at: T, to: RawSymlink<T> },
    File(FileMetadata),
    Directory,
}

#[derive(Debug, Dupe, Hash, PartialEq, Eq, Clone, Allocative, VariantName)]
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
pub trait FileOps: Send + Sync {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<String>>;

    /// Return the list of file outputs, sorted.
    async fn read_dir(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<ReadDirOutput>;

    async fn is_ignored(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<bool>;

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> anyhow::Result<Option<RawPathMetadata>>;

    fn eq_token(&self) -> PartialEqAny;

    async fn buildfiles<'a>(&self, cell: CellName) -> anyhow::Result<Arc<[FileNameBuf]>>;
}

impl dyn FileOps + '_ {
    pub async fn read_file(&self, path: CellPathRef<'_>) -> anyhow::Result<String> {
        self.read_file_if_exists(path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
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
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::fs::paths::file_name::FileNameBuf;
    use cmp_any::PartialEqAny;
    use dupe::Dupe;
    use itertools::Itertools;

    use crate::cas_digest::CasDigestConfig;
    use crate::dice::file_ops::keys::FileOpsDelegate;
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
        entries: Arc<BTreeMap<CellPath, TestFileOpsEntry>>,
    }

    impl TestFileOps {
        fn new(inputs: BTreeMap<CellPath, TestFileOpsEntry>) -> Self {
            let mut entries = BTreeMap::new();
            for (path, entry) in inputs {
                let mut file_type = match entry {
                    TestFileOpsEntry::Directory(..) => FileType::Directory,
                    TestFileOpsEntry::ExternalSymlink(..) => FileType::Symlink,
                    TestFileOpsEntry::File(..) => FileType::File,
                };
                // make sure the test setup is correct and concise
                assert!(
                    entries.insert(path.to_owned(), entry).is_none(),
                    "Adding `{}`, it already exists.",
                    path
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
                        _ => panic!("Adding `{}`, but `{}` exists and is not a dir", path, dir),
                    };
                }
            }
            TestFileOps {
                entries: Arc::new(entries),
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
                    .map(|(path, s)| (path, TestFileOpsEntry::ExternalSymlink(s)))
                    .collect::<BTreeMap<CellPath, TestFileOpsEntry>>(),
            )
        }

        pub fn for_cell(&self, cell: CellName) -> TestCellFileOps {
            TestCellFileOps(
                cell,
                Self {
                    entries: Arc::clone(&self.entries),
                },
            )
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

        async fn read_dir(&self, path: CellPathRef<'async_trait>) -> anyhow::Result<ReadDirOutput> {
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
            Ok(ReadDirOutput { included })
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

        async fn buildfiles<'a>(&self, _cell: CellName) -> anyhow::Result<Arc<[FileNameBuf]>> {
            Ok(Arc::from_iter([FileNameBuf::unchecked_new("BUCK")]))
        }
    }

    pub struct TestCellFileOps(CellName, TestFileOps);

    #[async_trait]
    impl FileOpsDelegate for TestCellFileOps {
        async fn read_file_if_exists(
            &self,
            path: &'async_trait CellRelativePath,
        ) -> anyhow::Result<Option<String>> {
            let path = CellPath::new(self.0, path.to_owned());
            FileOps::read_file_if_exists(&self.1, path.as_ref()).await
        }

        /// Return the list of file outputs, sorted.
        async fn read_dir(
            &self,
            path: &'async_trait CellRelativePath,
        ) -> anyhow::Result<ReadDirOutput> {
            let path = CellPath::new(self.0, path.to_owned());
            FileOps::read_dir(&self.1, path.as_ref()).await
        }

        async fn is_ignored(&self, path: &'async_trait CellRelativePath) -> anyhow::Result<bool> {
            let path = CellPath::new(self.0, path.to_owned());
            FileOps::is_ignored(&self.1, path.as_ref()).await
        }

        async fn read_path_metadata_if_exists(
            &self,
            path: &'async_trait CellRelativePath,
        ) -> anyhow::Result<Option<RawPathMetadata>> {
            let path = CellPath::new(self.0, path.to_owned());
            FileOps::read_path_metadata_if_exists(&self.1, path.as_ref()).await
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

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
use std::io::Read;
use std::path::Path;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use derive_more::Display;
use gazebo::cmp::PartialEqAny;
use gazebo::prelude::*;
use globset::Candidate;
use globset::GlobSetBuilder;
use once_cell::sync::Lazy;
use once_cell::sync::OnceCell;
use regex::Regex;
use sha1::Digest;
use sha1::Sha1;
use thiserror::Error;

use crate::cas_digest::CasDigest;
use crate::cas_digest::TrackedCasDigest;
use crate::cas_digest::TrackedCasDigestKind;
use crate::external_symlink::ExternalSymlink;
use crate::io::IoProvider;
use crate::result::SharedResult;

#[derive(Debug, Error)]
enum FileOpsError {
    #[error("Tried to read ignored dir `{0}` (reason: {1}).")]
    ReadIgnoredDir(CellRelativePathBuf, String),
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
    pub file_type: FileType,
    pub file_name: FileNameBuf,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Dupe, Allocative)]
pub struct ReadDirOutput {
    pub included: Arc<Vec<SimpleDirEntry>>,
    pub ignored: Arc<Vec<SimpleDirEntry>>,
}

#[derive(Allocative)]
pub struct FileDigestKind {
    _private: (),
}

impl TrackedCasDigestKind for FileDigestKind {
    fn cell_for_empty_digest() -> Option<&'static OnceCell<TrackedCasDigest<Self>>> {
        static EMPTY_DIGEST: OnceCell<TrackedCasDigest<FileDigestKind>> = OnceCell::new();
        Some(&EMPTY_DIGEST)
    }
}

pub type FileDigest = CasDigest<FileDigestKind>;

pub type TrackedFileDigest = TrackedCasDigest<FileDigestKind>;

impl FileDigest {
    /// Obtain the digest of the file if you can.
    pub fn from_file<P>(file: P) -> anyhow::Result<Self>
    where
        P: AsRef<Path>,
    {
        let file = file.as_ref();
        match Self::from_file_attr(file) {
            Some(x) => Ok(x),
            None => Self::from_file_disk(file),
        }
    }

    /// Read the file from the xattr, or skip if it's not available.
    #[cfg(unix)]
    fn from_file_attr(file: &Path) -> Option<Self> {
        use std::borrow::Cow;
        use std::collections::HashSet;

        use buck2_core::fs::fs_util;

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
                let sha1 = Self::parse_digest_sha1_without_size(&v).ok()?;
                let size = meta.len();
                Some(Self::new_sha1(sha1, size))
            }
            _ => None,
        }
    }

    /// Windows doesn't support extended file attributes.
    #[cfg(windows)]
    fn from_file_attr(_file: &Path) -> Option<Self> {
        None
    }

    /// Get the digest from disk. You should usually prefer `from_file`
    /// which also uses faster methods of getting the SHA1 if it can.
    pub fn from_file_disk(file: &Path) -> anyhow::Result<Self> {
        let mut f = File::open(file)?;
        let mut h = Sha1::new();
        let mut size = 0;

        // Buffer size chosen based on benchmarks at D26176645
        let mut buffer = [0; 16 * 1024];
        loop {
            let count = f.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            size += count as u64;
            h.update(&buffer[..count]);
        }
        let sha1 = h.finalize().into();
        Ok(Self::new_sha1(sha1, size))
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
    pub fn empty() -> Self {
        Self {
            digest: TrackedFileDigest::empty(),
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
    async fn read_file(&self, path: &CellPath) -> anyhow::Result<String>;

    async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>>;

    async fn read_dir_with_ignores(&self, path: &CellPath) -> SharedResult<ReadDirOutput>;

    async fn is_ignored(&self, path: &CellPath) -> anyhow::Result<bool>;

    async fn try_exists(&self, path: &CellPath) -> anyhow::Result<bool> {
        Ok(self.read_path_metadata_if_exists(path).await?.is_some())
    }

    async fn read_path_metadata(&self, path: &CellPath) -> SharedResult<RawPathMetadata> {
        self.read_path_metadata_if_exists(path)
            .await?
            .ok_or_else(|| anyhow::anyhow!("file `{}` not found", path).into())
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<RawPathMetadata>>;

    fn eq_token(&self) -> PartialEqAny;
}

impl PartialEq for dyn FileOps {
    fn eq(&self, other: &dyn FileOps) -> bool {
        self.eq_token() == other.eq_token()
    }
}

pub trait DefaultFileOpsDelegate: Allocative + PartialEq + Send + Sync + 'static {
    fn check_ignored(&self, path: &CellPath) -> anyhow::Result<FileIgnoreResult>;
    fn resolve_cell_root(&self, cell: &CellName) -> anyhow::Result<CellRootPathBuf>;
    fn resolve(&self, path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf> {
        let cell_root = self.resolve_cell_root(path.cell())?;
        Ok(cell_root.project_relative_path().join(path.path()))
    }
    fn get_cell_path(&self, path: &ProjectRelativePath) -> anyhow::Result<CellPath>;
    fn io_provider(&self) -> &dyn IoProvider;
}

#[async_trait]
impl<T: DefaultFileOpsDelegate> FileOps for T {
    async fn read_file(&self, path: &CellPath) -> anyhow::Result<String> {
        // TODO(cjhopman): error on ignored paths, maybe.
        let project_path = self.resolve(path)?;
        self.io_provider().read_file(project_path).await
    }

    async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>> {
        Ok(self.read_dir_with_ignores(path).await?.included)
    }

    async fn read_dir_with_ignores(&self, path: &CellPath) -> SharedResult<ReadDirOutput> {
        // TODO(cjhopman): This should also probably verify that the parent chain is not ignored.
        self.check_ignored(path)?
            .into_result()
            .with_context(|| format!("Error checking whether dir `{}` is ignored", path))?;

        let project_path = self.resolve(path)?;
        let mut entries = self
            .io_provider()
            .read_dir(project_path)
            .await
            .with_context(|| format!("Error listing dir `{}`", path))?;

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));

        let is_ignored = |entry: &SimpleDirEntry| {
            let entry_path = path.join(&entry.file_name);
            let is_ignored = DefaultFileOpsDelegate::check_ignored(self, &entry_path)?.is_ignored();
            anyhow::Ok(is_ignored)
        };

        let mut ignored_entries = Vec::new();

        // Filter out any entries that are ignored.
        let mut filtering_error = None;
        let (included_entries, ignored_entries): (Vec<_>, Vec<_>) =
            entries.into_iter().partition(|e| match is_ignored(e) {
                Ok(ignored) => {
                    ignored_entries.push(e.clone());
                    !ignored
                }
                Err(e) => {
                    filtering_error = Some(e);
                    true
                }
            });

        if let Some(err) = filtering_error {
            return Err(err.into());
        }
        Ok(ReadDirOutput {
            included: Arc::new(included_entries),
            ignored: Arc::new(ignored_entries),
        })
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<RawPathMetadata>> {
        let project_path = self.resolve(path)?;

        let res = self
            .io_provider()
            .read_path_metadata_if_exists(project_path)
            .await
            .with_context(|| format!("Error accessing metadata for path `{}`", path))?;
        res.map(|meta| meta.try_map(|path| Ok(Arc::new(self.get_cell_path(&path)?))))
            .transpose()
    }

    async fn is_ignored(&self, path: &CellPath) -> anyhow::Result<bool> {
        Ok(self.check_ignored(path)?.is_ignored())
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

pub enum FileIgnoreResult {
    Ok,
    IgnoredByPattern(CellRelativePathBuf, String),
    IgnoredByCell(CellRelativePathBuf, String),
}

impl FileIgnoreResult {
    /// Converts the FileIgnoreResult to a Result<()> where any ignored case is converted to an Err
    /// with appropriate message. This should be used when it would be an error to interact with an
    /// ignored file.
    pub fn into_result(self) -> anyhow::Result<()> {
        match self {
            FileIgnoreResult::Ok => Ok(()),
            FileIgnoreResult::IgnoredByPattern(path, pattern) => {
                Err(anyhow::anyhow!(FileOpsError::ReadIgnoredDir(
                    path,
                    format!("file is matched by pattern `{}`", pattern)
                )))
            }
            FileIgnoreResult::IgnoredByCell(path, cell) => Err(anyhow::anyhow!(
                FileOpsError::ReadIgnoredDir(path, format!("file is part of cell `{}`", cell))
            )),
        }
    }

    /// Returns true if the file is ignored, false otherwise.
    pub fn is_ignored(&self) -> bool {
        match self {
            FileIgnoreResult::Ok => false,
            _ => true,
        }
    }
}

#[derive(Allocative)]
pub struct IgnoreSet {
    #[allocative(skip)]
    globset: globset::GlobSet,
    // We keep patterns so that error messages can refer to the specific pattern that was matched.
    // This should be in the same order as the strings were added to the GlobSet to match the indices returned from it.
    patterns: Vec<String>,
}

impl PartialEq for IgnoreSet {
    fn eq(&self, other: &Self) -> bool {
        // Only compare patterns because globset is derived from patterns.
        self.patterns == other.patterns
    }
}

impl IgnoreSet {
    /// Creates an IgnoreSet from an "ignore spec".
    ///
    /// This is modeled after buck1's parsing of project.ignores.
    ///
    /// An ignore spec is a comma-separated list of ignore patterns. If an ignore pattern
    /// contains a glob character, then it uses java.nio.file.FileSystem.getPathMatcher,
    /// otherwise it creates a com.facebook.buck.io.filesystem.RecursivePathMatcher
    ///
    /// Java's path matcher does not allow  '*' to cross directory boundaries. We get
    /// the RecursivePathMatcher behavior by identifying non-globby things and appending
    /// a '/**'.
    pub fn from_ignore_spec(spec: &str) -> anyhow::Result<Self> {
        // TODO(cjhopman): There's opportunity to greatly improve the performance of IgnoreSet by
        // constructing special cases for a couple of common patterns we see in ignore specs. We
        // know that these can get large wins in some places where we've done this same ignore (watchman, buck1's ignores).
        // `**/filename`: a filename filter. These can all be merged into one hashset lookup.
        // `**/*.ext`: an extension filter. These can all be merged into one hashset lookup.
        // `**/*x*x*`: just some general glob on the filename alone, can merge these into one GlobSet that just needs to check against the filename.
        // `some/prefix/**`: a directory prefix. These can all be merged into one trie lookup.
        let mut patterns_builder = GlobSetBuilder::new();
        let mut patterns = Vec::new();
        for val in spec.split(',') {
            let val = val.trim();
            if val.is_empty() {
                continue;
            }

            let val = val.trim_end_matches('/');

            static GLOB_CHARS: Lazy<Regex> = Lazy::new(|| Regex::new(r"[*?{\[]").unwrap());

            if GLOB_CHARS.is_match(val) {
                patterns_builder.add(
                    globset::GlobBuilder::new(val)
                        .literal_separator(true)
                        .build()?,
                );
            } else {
                patterns_builder.add(globset::Glob::new(&format!("{{{},{}/**}}", val, val))?);
            }
            patterns.push(val.to_owned());
        }

        Ok(Self {
            globset: patterns_builder.build()?,
            patterns,
        })
    }

    /// Constructs an IgnoreSet that will ignore anything contained in a deeper cell.
    ///
    /// Ex. if this cell's path is `some/cell` and other cells are at `.`, `other`,
    /// `some/cell/deeper`, this would construct an IgnoreSet to ignore `deeper/**`
    /// (note that these ignores are expected to receive cell-relative paths.)
    fn from_cell_roots(
        all_cells: &[(&CellName, &CellRootPath)],
        this_cell: &CellRootPath,
    ) -> anyhow::Result<Self> {
        let mut cells_builder = GlobSetBuilder::new();
        let mut cell_names = Vec::new();
        for (name, path) in all_cells {
            if *path == this_cell {
                continue;
            }

            if !path.starts_with(this_cell) {
                continue;
            }

            let relative = path.strip_prefix(this_cell).unwrap();
            cells_builder.add(globset::Glob::new(&format!(
                "{{{},{}/**}}",
                relative, relative
            ))?);
            cell_names.push(name.as_str().to_owned());
        }

        Ok(Self {
            globset: cells_builder.build()?,
            patterns: cell_names,
        })
    }

    /// Returns a pattern that matches the candidate if there is one.
    fn matches_candidate(&self, candidate: &Candidate) -> Option<&str> {
        match self.globset.matches_candidate(candidate).as_slice() {
            [] => None,
            [v, ..] => Some(&self.patterns[*v]),
        }
    }

    /// Returns whether any pattern matches.
    pub fn is_match(&self, path: &CellRelativePath) -> bool {
        self.globset.is_match(path.as_str())
    }
}

/// Ignores files based on configured ignore patterns and cell paths.
#[derive(PartialEq, Allocative)]
pub struct FileIgnores {
    ignores: IgnoreSet,
    cell_ignores: IgnoreSet,
}

impl FileIgnores {
    /// Creates a new FileIgnores intended for use by the interpreter.
    ///
    /// This will ignore files/dirs in the ignore spec and those in other cells.
    pub fn new_for_interpreter(
        ignore_spec: &str,
        all_cells: &[(&CellName, &CellRootPath)],
        this_cell: &CellRootPath,
    ) -> anyhow::Result<FileIgnores> {
        Ok(FileIgnores {
            ignores: IgnoreSet::from_ignore_spec(ignore_spec)?,
            cell_ignores: IgnoreSet::from_cell_roots(all_cells, this_cell)?,
        })
    }

    pub fn check(&self, path: &CellRelativePath) -> FileIgnoreResult {
        let candidate = globset::Candidate::new(path.as_str());

        if let Some(pattern) = self.ignores.matches_candidate(&candidate) {
            return FileIgnoreResult::IgnoredByPattern(path.to_owned(), pattern.to_owned());
        }

        if let Some(pattern) = self.cell_ignores.matches_candidate(&candidate) {
            return FileIgnoreResult::IgnoredByCell(path.to_owned(), pattern.to_owned());
        }

        FileIgnoreResult::Ok
    }
}

pub mod testing {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_core::cells::cell_path::CellPath;
    use gazebo::cmp::PartialEqAny;
    use gazebo::prelude::*;
    use itertools::Itertools;

    use crate::external_symlink::ExternalSymlink;
    use crate::file_ops::FileDigest;
    use crate::file_ops::FileMetadata;
    use crate::file_ops::FileOps;
    use crate::file_ops::FileType;
    use crate::file_ops::RawPathMetadata;
    use crate::file_ops::RawSymlink;
    use crate::file_ops::ReadDirOutput;
    use crate::file_ops::SimpleDirEntry;
    use crate::file_ops::TrackedFileDigest;
    use crate::result::SharedResult;
    use crate::result::ToSharedResultExt;

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
            Self::new(
                files
                    .into_iter()
                    .map(|(path, data)| {
                        (
                            path,
                            TestFileOpsEntry::File(
                                data.clone(),
                                FileMetadata {
                                    digest: TrackedFileDigest::new(FileDigest::from_bytes_sha1(
                                        data.as_bytes(),
                                    )),
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

        fn add_entry(&mut self, mut path: CellPath, entry: TestFileOpsEntry) {
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
        async fn read_file(&self, path: &CellPath) -> anyhow::Result<String> {
            self.entries
                .get(path)
                .and_then(|e| match e {
                    TestFileOpsEntry::File(data, ..) => Some(data.clone()),
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!("couldn't find file {:?}", path))
        }

        async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>> {
            self.entries
                .get(path)
                .and_then(|e| match e {
                    TestFileOpsEntry::Directory(listing) => Some(Arc::new(
                        listing.iter().cloned().sorted().collect::<Vec<_>>(),
                    )),
                    _ => None,
                })
                .ok_or_else(|| anyhow::anyhow!("couldn't find dir {:?}", path))
                .shared_error()
        }

        async fn read_dir_with_ignores(&self, path: &CellPath) -> SharedResult<ReadDirOutput> {
            Ok(ReadDirOutput {
                included: self.read_dir(path).await?,
                ignored: Arc::new(Vec::new()),
            })
        }

        async fn read_path_metadata_if_exists(
            &self,
            path: &CellPath,
        ) -> SharedResult<Option<RawPathMetadata>> {
            self.entries
                .get(path)
                .map_or(Ok(None), |e| {
                    match e {
                        TestFileOpsEntry::File(_data, metadata) => {
                            Ok(RawPathMetadata::File(metadata.dupe()))
                        }
                        TestFileOpsEntry::ExternalSymlink(sym) => Ok(RawPathMetadata::Symlink {
                            at: Arc::new(path.clone()),
                            to: RawSymlink::External(sym.dupe()),
                        }),
                        _ => Err(anyhow::anyhow!("couldn't get metadata for {:?}", path)),
                    }
                    .map(Some)
                })
                .shared_error()
        }

        async fn is_ignored(&self, _path: &CellPath) -> anyhow::Result<bool> {
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

    use buck2_core::fs::project::ProjectRelativePath;

    use super::*;

    #[test]
    fn file_ignores() -> anyhow::Result<()> {
        let cells = &[
            (
                &CellName::unchecked_new("root".to_owned()),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root")),
            ),
            (
                &CellName::unchecked_new("other".to_owned()),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root/other_cell")),
            ),
            (
                &CellName::unchecked_new("third".to_owned()),
                CellRootPath::new(ProjectRelativePath::unchecked_new("third")),
            ),
        ];
        let ignores = FileIgnores::new_for_interpreter(
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            cells,
            CellRootPath::new(ProjectRelativePath::unchecked_new("root")),
        )?;

        assert_eq!(
            true,
            ignores
                .check(CellRelativePath::unchecked_new("some/long/path/Class.java"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(CellRelativePath::unchecked_new("other_cell"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(CellRelativePath::unchecked_new("other_cell/some/lib"))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(CellRelativePath::unchecked_new("third"))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(CellRelativePath::unchecked_new("one/two/three"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(CellRelativePath::unchecked_new("recursive/two/three"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(CellRelativePath::unchecked_new("trailing_slash/BUCK"))
                .is_ignored()
        );

        Ok(())
    }

    #[cfg(unix)]
    mod unix {
        use std::os::unix::fs::symlink;

        use buck2_core::fs::fs_util;

        use super::*;
        use crate::cas_digest::SHA1_SIZE;

        #[test]
        fn test_from_file_attr() -> anyhow::Result<()> {
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

            let d1 = FileDigest::from_file(&file).context("file")?;
            let d2 = FileDigest::from_file(&tempdir.path().join("link")).context("file")?;
            let d3 = FileDigest::from_file(&tempdir.path().join("abs_link")).context("abs_link")?;
            let d4 = FileDigest::from_file(&tempdir.path().join("recurse_link"))
                .context("recurse_link")?;
            let d5 = FileDigest::from_file(&tempdir.path().join("recurse_recurse_link"))
                .context("recurse_recurse_link")?;

            assert_eq!(d1.sha1(), &[0; SHA1_SIZE]);
            assert_eq!(d1.size(), 3);
            assert_eq!(d1, d2);
            assert_eq!(d1, d3);
            assert_eq!(d1, d4);
            assert_eq!(d1, d5);

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
        let digest = FileDigest::from_bytes_sha1(b"foo");
        let tracked_digest = TrackedFileDigest::new(digest.dupe());

        assert_eq!(&digest, tracked_digest.borrow());

        let mut hasher_digest = DefaultHasher::new();
        digest.hash(&mut hasher_digest);

        let mut hasher_tracked_digest = DefaultHasher::new();
        tracked_digest.hash(&mut hasher_tracked_digest);

        assert_eq!(hasher_digest.finish(), hasher_tracked_digest.finish());
        assert_eq!(digest.to_string(), tracked_digest.to_string());
    }
}

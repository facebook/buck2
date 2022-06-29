/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::fmt;
use std::fs::File;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::Read;
use std::path::Path;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::cells::paths::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::result::SharedResult;
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

use crate::external_symlink::ExternalSymlink;
use crate::io::IoProvider;

#[derive(Debug, Error)]
enum FileOpsError {
    #[error("Tried to read ignored dir `{0}` (reason: {1}).")]
    ReadIgnoredDir(CellRelativePathBuf, String),
}

/// std::fs::FileType is an opaque type that isn't constructible. This is
/// basically the equivalent.
#[derive(Clone, Dupe, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
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

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct SimpleDirEntry {
    pub file_type: FileType,
    pub file_name: FileNameBuf,
}

// The number of bytes required by a SHA1 hash
const SHA1_SIZE: usize = 20;

/// The bytes that make up a file digest.
#[derive(Display, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[display(fmt = "{}:{}", "hex::encode(sha1)", size)]
pub struct FileDigest {
    pub size: u64,
    pub sha1: [u8; SHA1_SIZE],
}

impl fmt::Debug for FileDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Dupe for FileDigest {}

impl FileDigest {
    // Precondition: Must be exactly SHA1_SIZE bytes or will panic
    fn mk_sha1(data: &[u8]) -> [u8; SHA1_SIZE] {
        let mut sha1 = [0; SHA1_SIZE];
        sha1.copy_from_slice(data);
        sha1
    }

    pub fn parse_digest(data: &[u8]) -> Option<[u8; SHA1_SIZE]> {
        if data.len() != SHA1_SIZE * 2 {
            return None;
        }
        Some(Self::mk_sha1(&hex::decode(data).ok()?))
    }

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
        use std::fs;

        let mut file = Cow::Borrowed(file);
        let mut meta;
        let mut visited = HashSet::new();

        // NOTE: We have to look through symlinks first because `xattr::get` doesn't (at least not
        // on MacOS, it seems), and instead we end up with the hash of the symlink destination as a
        // string.
        loop {
            meta = fs::symlink_metadata(&file).ok()?;
            if !meta.is_symlink() {
                break;
            }

            let dest = fs::read_link(&file).ok()?;
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
                let sha1 = Self::parse_digest(&v)?;
                let size = meta.len();
                Some(Self { size, sha1 })
            }
            _ => None,
        }
    }

    /// Windows doesn't support extended file attributes.
    #[cfg(windows)]
    fn from_file_attr(_file: &Path) -> Option<Self> {
        None
    }

    fn from_file_disk(file: &Path) -> anyhow::Result<Self> {
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
            h.input(&buffer[..count]);
        }
        let sha1 = h.result().into();
        Ok(Self { size, sha1 })
    }

    /// Return the digest of an empty string
    pub fn empty() -> Self {
        Self::from_bytes(&[])
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        let sha1 = Sha1::digest(bytes).into();
        Self {
            sha1,
            size: bytes.len() as u64,
        }
    }
}

/// A digest to interact with RE. This, despite the name, can be a file or a directory. We track
/// the sha1 and the size of the underlying blob. We *also* keep track of its expiry in the CAS.
/// Note that for directory, the expiry represents that of the directory's blob, not its underlying
/// contents.
struct FileDigestInner {
    data: FileDigest,
    expires: AtomicU64,
}

#[derive(Display, Clone, Dupe)]
#[display(fmt = "{}", "self.data()")]
pub struct TrackedFileDigest {
    inner: Arc<FileDigestInner>,
}

impl Borrow<FileDigest> for TrackedFileDigest {
    fn borrow(&self) -> &FileDigest {
        self.data()
    }
}

impl<'a> Borrow<FileDigest> for &'a TrackedFileDigest {
    fn borrow(&self) -> &FileDigest {
        self.data()
    }
}

impl PartialOrd for TrackedFileDigest {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data().partial_cmp(other.data())
    }
}

impl Ord for TrackedFileDigest {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data().cmp(other.data())
    }
}

impl PartialEq for TrackedFileDigest {
    fn eq(&self, other: &Self) -> bool {
        self.data().eq(other.data())
    }
}

impl Eq for TrackedFileDigest {}

impl Hash for TrackedFileDigest {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().hash(state)
    }
}

impl fmt::Debug for TrackedFileDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{} expires at {}]",
            self,
            self.inner.expires.load(Ordering::Relaxed)
        )
    }
}

impl TrackedFileDigest {
    pub fn new(data: FileDigest) -> Self {
        if data.size == 0 {
            return Self::empty();
        }

        Self {
            inner: Arc::new(FileDigestInner {
                data,
                expires: AtomicU64::new(0),
            }),
        }
    }

    pub fn new_expires(data: FileDigest, expiry: SystemTime) -> Self {
        let res = Self::new(data);
        res.update_expires(expiry);
        res
    }

    pub fn empty() -> Self {
        static EMPTY_DIGEST: OnceCell<TrackedFileDigest> = OnceCell::new();

        return EMPTY_DIGEST
            .get_or_init(|| Self {
                inner: Arc::new(FileDigestInner {
                    data: FileDigest::empty(),
                    expires: AtomicU64::new(0),
                }),
            })
            .dupe();
    }

    pub fn data(&self) -> &FileDigest {
        &self.inner.data
    }

    pub fn sha1(&self) -> &[u8; SHA1_SIZE] {
        &self.inner.data.sha1
    }

    pub fn size(&self) -> u64 {
        self.inner.data.size
    }

    pub fn expires(&self) -> SystemTime {
        SystemTime::UNIX_EPOCH + Duration::from_secs(self.inner.expires.load(Ordering::Relaxed))
    }

    pub fn update_expires(&self, time: SystemTime) {
        self.inner.expires.store(
            time.duration_since(SystemTime::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0),
            Ordering::Relaxed,
        )
    }
}

/// Stores the relevant metadata for a file.
// New fields should be added as needed, and unused fields removed.
#[derive(Debug, Dupe, Hash, PartialEq, Eq, Clone, Display)]
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

#[async_trait]
pub trait FileOps: Send + Sync {
    async fn read_file(&self, path: &CellPath) -> anyhow::Result<String>;

    async fn read_dir(&self, path: &CellPath) -> SharedResult<Arc<Vec<SimpleDirEntry>>>;

    async fn is_ignored(&self, path: &CellPath) -> anyhow::Result<bool>;

    async fn try_exists(&self, path: &CellPath) -> anyhow::Result<bool> {
        Ok(self.read_path_metadata_if_exists(path).await?.is_some())
    }

    async fn read_path_metadata(&self, path: &CellPath) -> SharedResult<PathMetadata> {
        self.read_path_metadata_if_exists(path)
            .await?
            .ok_or_else(|| anyhow::anyhow!("file `{}` not found", path).into())
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<PathMetadata>>;

    fn eq_token(&self) -> PartialEqAny;
}

impl PartialEq for dyn FileOps {
    fn eq(&self, other: &dyn FileOps) -> bool {
        self.eq_token() == other.eq_token()
    }
}

pub trait DefaultFileOpsDelegate: PartialEq + Send + Sync + 'static {
    fn check_ignored(&self, path: &CellPath) -> anyhow::Result<FileIgnoreResult>;
    fn resolve(&self, path: &CellPath) -> anyhow::Result<ProjectRelativePathBuf>;
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

        let is_ignored = |entry: &SimpleDirEntry| {
            let entry_path = path.join_unnormalized(&entry.file_name);
            let is_ignored = DefaultFileOpsDelegate::check_ignored(self, &entry_path)?.is_ignored();
            anyhow::Ok(is_ignored)
        };

        // Filter out any entries that are ignored.
        let mut filtering_error = None;
        entries.retain(|e| match is_ignored(e) {
            Ok(ignored) => !ignored,
            Err(e) => {
                filtering_error = Some(e);
                true
            }
        });
        if let Some(err) = filtering_error {
            return Err(err.into());
        }

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));

        Ok(Arc::new(entries))
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &CellPath,
    ) -> SharedResult<Option<PathMetadata>> {
        let project_path = self.resolve(path)?;
        let cell_project_path = self.resolve(&CellPath::new(
            path.cell().clone(),
            CellRelativePathBuf::unchecked_new("".to_owned()),
        ))?;

        let res = self
            .io_provider()
            .read_path_metadata_if_exists(cell_project_path, project_path)
            .await
            .with_context(|| format!("Error accessing metadata for path `{}`", path))?;

        Ok(res)
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

pub struct IgnoreSet {
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
        all_cells: &[(&CellName, &ProjectRelativePath)],
        this_cell: &ProjectRelativePath,
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
#[derive(PartialEq)]
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
        all_cells: &[(&CellName, &ProjectRelativePath)],
        this_cell: &ProjectRelativePath,
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

    use anyhow::anyhow;
    use async_trait::async_trait;
    use buck2_core::cells::paths::CellPath;
    use buck2_core::result::SharedResult;
    use buck2_core::result::ToSharedResultExt;
    use gazebo::cmp::PartialEqAny;
    use gazebo::prelude::*;
    use itertools::Itertools;

    use crate::external_symlink::ExternalSymlink;
    use crate::file_ops::FileDigest;
    use crate::file_ops::FileMetadata;
    use crate::file_ops::FileOps;
    use crate::file_ops::FileType;
    use crate::file_ops::PathMetadata;
    use crate::file_ops::SimpleDirEntry;
    use crate::file_ops::TrackedFileDigest;

    enum TestFileOpsEntry {
        File(String /*data*/, FileMetadata),
        ExternalSymlink(Arc<ExternalSymlink>),
        Directory(BTreeSet<SimpleDirEntry>),
    }

    pub struct TestFileOps {
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
                                    digest: TrackedFileDigest::new(FileDigest::from_bytes(
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
                .ok_or_else(|| anyhow!("couldn't find file {:?}", path))
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
                .ok_or_else(|| anyhow!("couldn't find dir {:?}", path))
                .shared_error()
        }

        async fn read_path_metadata_if_exists(
            &self,
            path: &CellPath,
        ) -> SharedResult<Option<PathMetadata>> {
            self.entries
                .get(path)
                .map_or(Ok(None), |e| {
                    match e {
                        TestFileOpsEntry::File(_data, metadata) => {
                            Ok(PathMetadata::File(metadata.dupe()))
                        }
                        TestFileOpsEntry::ExternalSymlink(sym) => {
                            Ok(PathMetadata::ExternalSymlink(sym.dupe()))
                        }
                        _ => Err(anyhow!("couldn't get metadata for {:?}", path)),
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
    use std::collections::hash_map::DefaultHasher;

    use super::*;

    #[test]
    fn file_ignores() -> anyhow::Result<()> {
        let cells = &[
            (
                &CellName::unchecked_new("root".to_owned()),
                ProjectRelativePath::unchecked_new("root"),
            ),
            (
                &CellName::unchecked_new("other".to_owned()),
                ProjectRelativePath::unchecked_new("root/other_cell"),
            ),
            (
                &CellName::unchecked_new("third".to_owned()),
                ProjectRelativePath::unchecked_new("third"),
            ),
        ];
        let ignores = FileIgnores::new_for_interpreter(
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            cells,
            ProjectRelativePath::unchecked_new("root"),
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
        use std::fs;
        use std::os::unix::fs::symlink;

        use super::*;

        #[test]
        fn test_from_file_attr() -> anyhow::Result<()> {
            let tempdir = tempfile::tempdir()?;

            let file = tempdir.path().join("dest");
            fs::write(&file, "foo")?;
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

            assert_eq!(d1.sha1, [0; SHA1_SIZE]);
            assert_eq!(d1.size, 3);
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
        let digest = FileDigest::from_bytes(b"foo");
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

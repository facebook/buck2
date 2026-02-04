/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::sync::OnceLock;

use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_common::file_ops::delegate::FileOpsDelegate;
use buck2_common::file_ops::dice::ReadFileProxy;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::FileType;
use buck2_common::file_ops::metadata::RawDirEntry;
use buck2_common::file_ops::metadata::RawPathMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_common::io::fs::is_executable;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::builder::DirectoryBuilder;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_hasher::DirectoryDigester;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_ref::DirectoryRef;
use buck2_directory::directory::directory_ref::FingerprintedDirectoryRef;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::find::DirectoryFindError;
use buck2_directory::directory::find::find;
use buck2_directory::directory::immutable_directory::ImmutableDirectory;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_external_cells_bundled::BundledCell;
use buck2_external_cells_bundled::BundledFile;
use buck2_external_cells_bundled::get_bundled_data;
use buck2_fs::fs_util::uncategorized as fs_util;
use buck2_fs::paths::abs_path::AbsPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_util::strong_hasher::Blake3StrongHasher;
use cmp_any::PartialEqAny;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

fn load_nano_prelude() -> buck2_error::Result<BundledCell> {
    let path = env::var("NANO_PRELUDE")
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Input))
        .buck_error_context(
            "NANO_PRELUDE env var must be set to the location of nano prelude\n\
        Consider `export NANO_PRELUDE=$HOME/fbsource/fbcode/buck2/tests/e2e_util/nano_prelude`",
        )?;
    if path.is_empty() {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "NANO_PRELUDE env var must not be empty"
        ));
    }
    let path = AbsPathBuf::new(Path::new(&path))
        .buck_error_context("NANO_PRELUDE env var must point to absolute path")?;

    let mut files = Vec::new();
    let mut dir_stack = Vec::new();
    dir_stack.push((path, ForwardRelativePathBuf::empty()));
    while let Some((dir, rel_path)) = dir_stack.pop() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let entry_path = AbsPathBuf::new(entry.path())?;
            let entry_rel_path = rel_path.join(FileName::new(
                entry
                    .file_name()
                    .to_str()
                    .buck_error_context("not UTF-8 string")?,
            )?);
            match FileType::from(entry.file_type()?) {
                FileType::Directory => dir_stack.push((entry_path, entry_rel_path)),
                FileType::File => {
                    let contents = fs_util::read(&entry_path)?;
                    files.push(BundledFile {
                        path: entry_rel_path.as_str().to_owned().leak(),
                        contents: contents.leak(),
                        is_executable: is_executable(&entry.metadata()?),
                    });
                }
                FileType::Symlink | FileType::Unknown => {
                    // We don't have these in nano-prelude.
                }
            }
        }
    }

    Ok(BundledCell {
        name: "nano_prelude",
        files: files.leak(),
        is_testing: true,
    })
}

fn nano_prelude() -> buck2_error::Result<BundledCell> {
    static NANO_PRELUDE: OnceLock<BundledCell> = OnceLock::new();
    Ok(*NANO_PRELUDE
        .get_or_try_init(|| load_nano_prelude().buck_error_context("loading nano_prelude"))?)
}

pub(crate) fn find_bundled_data(cell_name: CellName) -> buck2_error::Result<BundledCell> {
    #[derive(buck2_error::Error, Debug)]
    #[error("No bundled cell named `{0}`, options are `{}`", _1.join(", "))]
    #[buck2(tag = Input)]
    struct CellNotBundled(String, Vec<&'static str>);

    let cell_name = cell_name.as_str();

    if cell_name == "nano_prelude" {
        return nano_prelude();
    }

    get_bundled_data()
        .iter()
        .find(|data| data.name == cell_name)
        .copied()
        .ok_or_else(|| {
            CellNotBundled(
                cell_name.to_owned(),
                get_bundled_data()
                    .iter()
                    .filter(|data| !data.is_testing)
                    .map(|data| data.name)
                    .collect(),
            )
            .into()
        })
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, allocative::Allocative)]
struct ContentsAndMetadata {
    contents: &'static [u8],
    metadata: FileMetadata,
}

/// We don't actually need the directory digest, but unfortunately the directory tooling kind of
/// requires us to have one.
#[derive(
    allocative::Allocative,
    derive_more::Display,
    Debug,
    PartialEq,
    Eq,
    Hash,
    Copy,
    Clone
)]
struct BundledDirectoryDigest(#[allocative(skip)] blake3::Hash);

impl Dupe for BundledDirectoryDigest {
    fn dupe(&self) -> Self {
        *self
    }
}

impl DirectoryDigest for BundledDirectoryDigest {}

struct BundledDirectoryDigester;

impl DirectoryDigester<ContentsAndMetadata, BundledDirectoryDigest> for BundledDirectoryDigester {
    fn hash_entries<'a, D, I>(&self, entries: I) -> BundledDirectoryDigest
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<D, &'a ContentsAndMetadata>)>,
        D: FingerprintedDirectoryRef<
                'a,
                Leaf = ContentsAndMetadata,
                DirectoryDigest = BundledDirectoryDigest,
            > + 'a,
        Self: Sized,
    {
        use std::hash::Hash;

        let mut hasher = Blake3StrongHasher::default();
        for (name, entry) in entries {
            name.hash(&mut hasher);
            match entry {
                DirectoryEntry::Dir(dir) => {
                    dir.as_fingerprinted_dyn().fingerprint().hash(&mut hasher);
                }
                DirectoryEntry::Leaf(leaf) => {
                    leaf.metadata.hash(&mut hasher);
                }
            }
        }
        BundledDirectoryDigest(hasher.finalize())
    }

    fn leaf_size(&self, leaf: &ContentsAndMetadata) -> u64 {
        leaf.contents.len() as u64
    }
}

#[derive(allocative::Allocative)]
pub(crate) struct BundledFileOpsDelegate {
    dir: ImmutableDirectory<ContentsAndMetadata, BundledDirectoryDigest>,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Environment)]
enum BundledPathSearchError {
    #[error("Expected a directory at `{0}` but found a file")]
    ExpectedDirectory(String),
    #[error("Path not found: `{0}`")]
    MissingFile(CellRelativePathBuf),
    #[error("Expected file at `{0}` but found a directory")]
    ExpectedFile(CellRelativePathBuf),
}

impl BundledFileOpsDelegate {
    fn get_entry_at_path_if_exists(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<
        Option<
            DirectoryEntry<
                impl DirectoryRef<
                    '_,
                    Leaf = ContentsAndMetadata,
                    DirectoryDigest = BundledDirectoryDigest,
                > + use<'_>,
                &ContentsAndMetadata,
            >,
        >,
    > {
        match find(self.dir.as_ref(), path.iter()) {
            Ok(entry) => Ok(entry),
            Err(DirectoryFindError::CannotTraverseLeaf { path }) => {
                Err(BundledPathSearchError::ExpectedDirectory(path.to_string()).into())
            }
        }
    }

    fn get_entry_at_path(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<
        DirectoryEntry<
            impl DirectoryRef<'_, Leaf = ContentsAndMetadata, DirectoryDigest = BundledDirectoryDigest>
            + use<'_>,
            &ContentsAndMetadata,
        >,
    > {
        self.get_entry_at_path_if_exists(path)?
            .ok_or_else(|| BundledPathSearchError::MissingFile(path.to_owned()).into())
    }

    /// Return the list of file outputs, sorted.
    async fn read_dir(&self, path: &CellRelativePath) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        let dir = match self.get_entry_at_path(path)? {
            DirectoryEntry::Dir(dir) => dir,
            DirectoryEntry::Leaf(_) => {
                return Err(BundledPathSearchError::ExpectedDirectory(path.to_string()).into());
            }
        };

        let entries = dir
            .entries()
            .map(|(name, entry)| RawDirEntry {
                file_name: name.to_owned().into_inner(),
                file_type: match entry {
                    DirectoryEntry::Leaf(_) => FileType::File,
                    DirectoryEntry::Dir(_) => FileType::Directory,
                },
            })
            .collect();

        Ok(entries)
    }

    fn read_file_if_exists(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<Option<&'static str>> {
        match self.get_entry_at_path_if_exists(path)? {
            Some(DirectoryEntry::Leaf(leaf)) => Ok(Some(str::from_utf8(leaf.contents)?)),
            Some(DirectoryEntry::Dir(_)) => {
                Err(BundledPathSearchError::ExpectedFile(path.to_owned()).into())
            }
            None => Ok(None),
        }
    }

    fn read_path_metadata_if_exists(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        match self.get_entry_at_path_if_exists(path)? {
            Some(DirectoryEntry::Leaf(leaf)) => {
                Ok(Some(RawPathMetadata::File(leaf.metadata.clone())))
            }
            Some(DirectoryEntry::Dir(_)) => Ok(Some(RawPathMetadata::Directory)),
            None => Ok(None),
        }
    }
}

#[async_trait::async_trait]
impl FileOpsDelegate for BundledFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy> {
        let res = self.read_file_if_exists(path)?;
        Ok(ReadFileProxy::new_with_captures(res, |res| async move {
            Ok(res.map(|s| s.to_owned()))
        }))
    }

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        self.read_dir(path).await
    }

    async fn read_path_metadata_if_exists(
        &self,
        _ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        self.read_path_metadata_if_exists(path)
    }

    fn eq_token(&self) -> PartialEqAny<'_> {
        PartialEqAny::always_false()
    }
}

fn get_file_ops_delegate_impl(
    data: BundledCell,
    digest_config: DigestConfig,
) -> buck2_error::Result<BundledFileOpsDelegate> {
    let mut builder: DirectoryBuilder<ContentsAndMetadata, BundledDirectoryDigest> =
        DirectoryBuilder::empty();
    let source_digest_config = digest_config.cas_digest_config().source_files_config();
    for file in data.files {
        let path = ForwardRelativePath::new(file.path)
            .internal_error("non-forward relative bundled path")?;
        let metadata = FileMetadata {
            digest: TrackedFileDigest::from_content(file.contents, source_digest_config),
            is_executable: file.is_executable,
        };

        builder
            .insert(
                path,
                DirectoryEntry::Leaf(ContentsAndMetadata {
                    contents: file.contents,
                    metadata,
                }),
            )
            .internal_error("conflicting bundled source paths")?;
    }
    let builder = builder.fingerprint(&BundledDirectoryDigester);
    Ok(BundledFileOpsDelegate { dir: builder })
}

async fn declare_all_source_artifacts(
    ctx: &mut DiceComputations<'_>,
    cell_name: CellName,
    ops: &BundledFileOpsDelegate,
) -> buck2_error::Result<()> {
    let mut requests = Vec::new();
    let artifact_fs = ctx.get_artifact_fs().await?;
    let buck_out_resolver = artifact_fs.buck_out_path_resolver();

    for (path, entry) in ops.dir.unordered_walk_leaves().with_paths() {
        let path = buck_out_resolver.resolve_external_cell_source(
            CellRelativePath::new(path.as_ref()),
            ExternalCellOrigin::Bundled(cell_name),
        );
        requests.push(WriteRequest {
            path,
            content: entry.contents.to_vec(),
            is_executable: entry.metadata.is_executable,
        });
    }

    let materializer = ctx.per_transaction_data().get_materializer();
    materializer
        .declare_write(Box::new(move || Ok(requests)))
        .await
        .map(|_| ())
}

pub(crate) async fn get_file_ops_delegate(
    ctx: &mut DiceComputations<'_>,
    cell_name: CellName,
) -> buck2_error::Result<Arc<BundledFileOpsDelegate>> {
    #[derive(
        dupe::Dupe,
        Clone,
        Copy,
        Debug,
        derive_more::Display,
        PartialEq,
        Eq,
        Hash,
        allocative::Allocative
    )]
    struct BundledFileOpsDelegateKey(CellName);

    #[async_trait::async_trait]
    impl Key for BundledFileOpsDelegateKey {
        type Value = buck2_error::Result<Arc<BundledFileOpsDelegate>>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            let data = find_bundled_data(self.0)?;
            let ops = get_file_ops_delegate_impl(data, ctx.global_data().get_digest_config())?;
            declare_all_source_artifacts(ctx, self.0, &ops).await?;
            Ok(Arc::new(ops))
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            // No need for non-trivial equality, because this has no deps and is never recomputed
            false
        }
    }

    ctx.compute(&BundledFileOpsDelegateKey(cell_name)).await?
}

pub(crate) async fn materialize_all(
    ctx: &mut DiceComputations<'_>,
    cell: CellName,
) -> buck2_error::Result<ProjectRelativePathBuf> {
    let artifact_fs = ctx.get_artifact_fs().await?;
    let buck_out_resolver = artifact_fs.buck_out_path_resolver();

    let ops = get_file_ops_delegate(ctx, cell).await?;
    let materializer = ctx.per_transaction_data().get_materializer();
    let mut paths = Vec::new();
    for (path, _entry) in ops.dir.unordered_walk_leaves().with_paths() {
        let path = buck_out_resolver.resolve_external_cell_source(
            CellRelativePath::new(path.as_ref()),
            ExternalCellOrigin::Bundled(cell),
        );
        paths.push(path);
    }

    materializer.ensure_materialized(paths).await?;
    Ok(buck_out_resolver.resolve_external_cell_source(
        CellRelativePath::unchecked_new(""),
        ExternalCellOrigin::Bundled(cell),
    ))
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    fn testing_ops() -> BundledFileOpsDelegate {
        let data = find_bundled_data(CellName::testing_new("test_bundled_cell")).unwrap();
        get_file_ops_delegate_impl(data, DigestConfig::testing_default()).unwrap()
    }

    #[tokio::test]
    async fn test_smoke_read() {
        let ops = testing_ops();
        let content = ops
            .read_file_if_exists(&CellRelativePath::unchecked_new("dir/src.txt"))
            .unwrap()
            .unwrap();
        let content = if cfg!(windows) {
            // Git may check out files on Windows with \r\n as line separator.
            // We could configure git, but it's more reliable to handle it in the test.
            content.replace("\r\n", "\n")
        } else {
            content.to_owned()
        };
        assert_eq!(content, "foobar\n");
        assert!(
            ops.read_file_if_exists(&CellRelativePath::unchecked_new("dir/does_not_exist.txt"))
                .unwrap()
                .is_none()
        );
    }

    #[tokio::test]
    async fn test_executable_bit() {
        let ops = testing_ops();
        assert_matches!(
            ops.read_path_metadata_if_exists(&CellRelativePath::unchecked_new("dir/src.txt"))
                .unwrap()
                .unwrap(),
            RawPathMetadata::File(FileMetadata {
                digest: _,
                is_executable: false,
            }),
        );
        assert_matches!(
            ops.read_path_metadata_if_exists(&CellRelativePath::unchecked_new("dir/src2.txt"))
                .unwrap()
                .unwrap(),
            RawPathMetadata::File(FileMetadata {
                digest: _,
                is_executable: true,
            }),
        );
    }

    #[tokio::test]
    async fn test_dir_listing() {
        let ops = testing_ops();

        let root = CellRelativePath::unchecked_new("");
        let root_metadata = ops.read_path_metadata_if_exists(root).unwrap().unwrap();
        assert_matches!(root_metadata, RawPathMetadata::Directory);
        let root_entries = ops.read_dir(root).await.unwrap();
        assert!(root_entries.is_sorted());
        assert_eq!(
            &*root_entries,
            &[
                RawDirEntry {
                    file_name: ".buckconfig".into(),
                    file_type: FileType::File
                },
                RawDirEntry {
                    file_name: "BUCK_TREE".into(),
                    file_type: FileType::File
                },
                RawDirEntry {
                    file_name: "dir".into(),
                    file_type: FileType::Directory
                },
            ],
        );

        let dir = CellRelativePath::unchecked_new("dir");
        let dir_metadata = ops.read_path_metadata_if_exists(dir).unwrap().unwrap();
        assert_matches!(dir_metadata, RawPathMetadata::Directory);
        let dir_entries = ops.read_dir(dir).await.unwrap();
        assert!(dir_entries.is_sorted());
        assert_eq!(dir_entries.len(), 5);
    }

    #[test]
    fn test_load_all_bundled_cells() {
        for c in get_bundled_data() {
            get_file_ops_delegate_impl(*c, DigestConfig::testing_default()).unwrap();
        }
    }
}

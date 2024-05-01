/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_common::dice::file_ops::delegate::FileOpsDelegate;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::FileType;
use buck2_common::file_ops::RawDirEntry;
use buck2_common::file_ops::RawPathMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::external::ExternalCellOrigin;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::directory::find;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryBuilder;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryFindError;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::ImmutableDirectory;
use buck2_core::directory::NoDigest;
use buck2_core::directory::NoDigestDigester;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_external_cells_bundled::get_bundled_data;
use buck2_external_cells_bundled::BundledCell;
use cmp_any::PartialEqAny;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;

pub(crate) fn find_bundled_data(cell_name: CellName) -> anyhow::Result<BundledCell> {
    #[derive(buck2_error::Error, Debug)]
    #[error("No bundled cell named `{0}`, options are `{}`", _1.join(", "))]
    struct CellNotBundled(String, Vec<&'static str>);

    let cell_name = cell_name.as_str();

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

#[derive(allocative::Allocative, PartialEq, Eq)]
pub(crate) struct BundledFileOpsDelegate {
    dir: ImmutableDirectory<ContentsAndMetadata, NoDigest>,
}

#[derive(buck2_error::Error, Debug)]
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
    ) -> anyhow::Result<
        Option<DirectoryEntry<&dyn Directory<ContentsAndMetadata, NoDigest>, &ContentsAndMetadata>>,
    > {
        if path.is_empty() {
            return Ok(Some(DirectoryEntry::Dir(&self.dir)));
        }
        match find(&self.dir, path.iter()) {
            Ok(entry) => Ok(entry),
            Err(DirectoryFindError::EmptyPath) => Ok(None),
            Err(DirectoryFindError::CannotTraverseLeaf { path }) => {
                Err(BundledPathSearchError::ExpectedDirectory(path.to_string()).into())
            }
        }
    }

    fn get_entry_at_path(
        &self,
        path: &CellRelativePath,
    ) -> anyhow::Result<
        DirectoryEntry<&dyn Directory<ContentsAndMetadata, NoDigest>, &ContentsAndMetadata>,
    > {
        self.get_entry_at_path_if_exists(path)?
            .ok_or_else(|| BundledPathSearchError::MissingFile(path.to_owned()).into())
    }
}

#[async_trait::async_trait]
impl FileOpsDelegate for BundledFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Option<String>> {
        match self.get_entry_at_path_if_exists(path)? {
            Some(DirectoryEntry::Leaf(leaf)) => {
                Ok(Some(String::from_utf8(leaf.contents.to_vec())?))
            }
            Some(DirectoryEntry::Dir(_)) => {
                Err(BundledPathSearchError::ExpectedFile(path.to_owned()).into())
            }
            None => Ok(None),
        }
    }

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Vec<RawDirEntry>> {
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

    async fn read_path_metadata_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> anyhow::Result<Option<RawPathMetadata>> {
        match self.get_entry_at_path_if_exists(path)? {
            Some(DirectoryEntry::Leaf(leaf)) => {
                Ok(Some(RawPathMetadata::File(leaf.metadata.clone())))
            }
            Some(DirectoryEntry::Dir(_)) => Ok(Some(RawPathMetadata::Directory)),
            None => Ok(None),
        }
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

fn get_file_ops_delegate_impl(
    data: BundledCell,
    digest_config: DigestConfig,
) -> anyhow::Result<BundledFileOpsDelegate> {
    let mut builder: DirectoryBuilder<ContentsAndMetadata, NoDigest> = DirectoryBuilder::empty();
    let digest_config = digest_config.cas_digest_config().source_files_config();
    for file in data.files {
        let path = ForwardRelativePath::new(file.path)
            .internal_error("non-forward relative bundled path")?;
        let metadata = FileMetadata {
            digest: TrackedFileDigest::from_content(file.contents, digest_config),
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
    Ok(BundledFileOpsDelegate {
        dir: builder.fingerprint(&NoDigestDigester),
    })
}

async fn declare_all_source_artifacts(
    ctx: &mut DiceComputations<'_>,
    cell_name: CellName,
    ops: &BundledFileOpsDelegate,
) -> anyhow::Result<()> {
    let mut requests = Vec::new();
    let artifact_fs = ctx.get_artifact_fs().await?;
    let buck_out_resolver = artifact_fs.buck_out_path_resolver();

    for (path, entry) in ops.dir.unordered_walk().with_paths() {
        let DirectoryEntry::Leaf(entry) = entry else {
            continue;
        };
        let path = buck_out_resolver.resolve_external_cell_source(
            CellPathRef::new(cell_name, CellRelativePath::new(path.as_ref())),
            ExternalCellOrigin::Bundled,
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
) -> anyhow::Result<Arc<BundledFileOpsDelegate>> {
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

    Ok(ctx.compute(&BundledFileOpsDelegateKey(cell_name)).await??)
}

pub(crate) async fn materialize_all(
    ctx: &mut DiceComputations<'_>,
    cell: CellName,
) -> anyhow::Result<ProjectRelativePathBuf> {
    let artifact_fs = ctx.get_artifact_fs().await?;
    let buck_out_resolver = artifact_fs.buck_out_path_resolver();

    let ops = get_file_ops_delegate(ctx, cell).await?;
    let materializer = ctx.per_transaction_data().get_materializer();
    let mut paths = Vec::new();
    for (path, entry) in ops.dir.unordered_walk().with_paths() {
        let DirectoryEntry::Leaf(_) = entry else {
            continue;
        };
        let path = buck_out_resolver.resolve_external_cell_source(
            CellPathRef::new(cell, CellRelativePath::new(path.as_ref())),
            ExternalCellOrigin::Bundled,
        );
        paths.push(path);
    }

    materializer.ensure_materialized(paths).await?;
    Ok(buck_out_resolver.resolve_external_cell_source(
        CellPathRef::new(cell, CellRelativePath::unchecked_new("")),
        ExternalCellOrigin::Bundled,
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
        assert_eq!(
            ops.read_file_if_exists(&CellRelativePath::unchecked_new("dir/src.txt"))
                .await
                .unwrap()
                .unwrap(),
            "foobar\n"
        );
        assert!(
            ops.read_file_if_exists(&CellRelativePath::unchecked_new("dir/does_not_exist.txt"))
                .await
                .unwrap()
                .is_none()
        );
    }

    #[tokio::test]
    async fn test_executable_bit() {
        let ops = testing_ops();
        assert_matches!(
            ops.read_path_metadata_if_exists(&CellRelativePath::unchecked_new("dir/src.txt"))
                .await
                .unwrap()
                .unwrap(),
            RawPathMetadata::File(FileMetadata {
                digest: _,
                is_executable: false,
            }),
        );
        assert_matches!(
            ops.read_path_metadata_if_exists(&CellRelativePath::unchecked_new("dir/src2.txt"))
                .await
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
        let root_metadata = ops
            .read_path_metadata_if_exists(root)
            .await
            .unwrap()
            .unwrap();
        assert_matches!(root_metadata, RawPathMetadata::Directory);
        let root_entries = ops.read_dir(root).await.unwrap();
        assert!(root_entries.is_sorted());
        assert_eq!(
            &root_entries,
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
        let dir_metadata = ops
            .read_path_metadata_if_exists(dir)
            .await
            .unwrap()
            .unwrap();
        assert_matches!(dir_metadata, RawPathMetadata::Directory);
        let dir_entries = ops.read_dir(dir).await.unwrap();
        assert!(dir_entries.is_sorted());
        assert_eq!(dir_entries.len(), 5);
    }
}

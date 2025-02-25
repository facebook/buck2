/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_futures::cancellation::CancellationContext;
use cmp_any::PartialEqAny;
use derivative::Derivative;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::dice::cells::HasCellResolver;
use crate::dice::data::HasIoProvider;
use crate::dice::file_ops::delegate::keys::FileOpsKey;
use crate::dice::file_ops::delegate::keys::FileOpsValue;
use crate::dice::file_ops::CheckIgnores;
use crate::external_cells::EXTERNAL_CELLS_IMPL;
use crate::file_ops::RawDirEntry;
use crate::file_ops::RawPathMetadata;
use crate::file_ops::ReadDirOutput;
use crate::file_ops::SimpleDirEntry;
use crate::ignores::all_cells::HasCellFileIgnores;
use crate::ignores::file_ignores::CellFileIgnores;
use crate::ignores::file_ignores::FileIgnoreResult;
use crate::io::IoProvider;

/// Note: Everything in this mini-module exists only so that it can be replaced by a `TestFileOps`
/// in unittests
mod keys {
    use allocative::Allocative;
    use buck2_core::cells::name::CellName;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::dice::file_ops::delegate::FileOpsDelegateWithIgnores;
    use crate::dice::file_ops::CheckIgnores;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    #[display("{:?}", self)]
    pub(crate) struct FileOpsKey {
        pub cell: CellName,
        pub check_ignores: CheckIgnores,
    }

    #[derive(Dupe, Clone, Allocative)]
    pub(crate) struct FileOpsValue(#[allocative(skip)] pub FileOpsDelegateWithIgnores);
}

#[async_trait]
pub trait FileOpsDelegate: Send + Sync {
    async fn read_file_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<String>>;

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Vec<RawDirEntry>>;

    async fn read_path_metadata_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>>;

    fn eq_token(&self) -> PartialEqAny;
}

/// A `FileOpsDelegate` implementation that calls out to the `IoProvider` to read files.
///
/// This is used for everything except 1) tests, and 2) external cells.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(PartialEq)]
struct IoFileOpsDelegate {
    // Safe to ignore because `io` does not change during the lifetime of the daemon.
    #[derivative(PartialEq = "ignore")]
    io: Arc<dyn IoProvider>,
    cells: CellResolver,
    cell: CellName,
}

impl IoFileOpsDelegate {
    fn resolve(&self, path: &CellRelativePath) -> ProjectRelativePathBuf {
        let cell_root = self.cells.get(self.cell).unwrap().path();
        cell_root.as_project_relative_path().join(path)
    }

    fn get_cell_path(&self, path: &ProjectRelativePath) -> buck2_error::Result<CellPath> {
        self.cells.get_cell_path(path)
    }

    fn io_provider(&self) -> &dyn IoProvider {
        self.io.as_ref()
    }
}

#[async_trait]
impl FileOpsDelegate for IoFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<String>> {
        let project_path = self.resolve(path);
        self.io_provider().read_file_if_exists(project_path).await
    }

    async fn read_dir(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        let project_path = self.resolve(path);
        let mut entries = self
            .io_provider()
            .read_dir(project_path)
            .await
            .with_buck_error_context(|| format!("Error listing dir `{}`", path))?;

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));

        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let project_path = self.resolve(path);

        let res = self
            .io_provider()
            .read_path_metadata_if_exists(project_path)
            .await
            .with_buck_error_context(|| format!("Error accessing metadata for path `{}`", path))?;
        res.map(|meta| meta.try_map(|path| Ok(Arc::new(self.get_cell_path(&path)?))))
            .transpose()
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

#[async_trait]
impl Key for FileOpsKey {
    type Value = buck2_error::Result<FileOpsValue>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let cells = ctx.get_cell_resolver().await?;
        let ignores = if self.check_ignores == CheckIgnores::Yes {
            Some(ctx.new_cell_ignores(self.cell).await?)
        } else {
            None
        };

        let out = if let Some(origin) = cells.get(self.cell)?.external() {
            let delegate = EXTERNAL_CELLS_IMPL
                .get()?
                .get_file_ops_delegate(ctx, self.cell, origin.dupe())
                .await?;
            FileOpsDelegateWithIgnores::new(ignores, delegate)
        } else {
            let io = ctx.global_data().get_io_provider();
            let delegate = IoFileOpsDelegate {
                io,
                cells,
                cell: self.cell,
            };
            FileOpsDelegateWithIgnores::new(ignores, Arc::new(delegate))
        };

        Ok(FileOpsValue(out))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x.0 == y.0,
            _ => false,
        }
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }
}

pub(crate) async fn get_delegated_file_ops(
    dice: &mut DiceComputations<'_>,
    cell: CellName,
    check_ignores: CheckIgnores,
) -> buck2_error::Result<FileOpsDelegateWithIgnores> {
    Ok(dice
        .compute(&FileOpsKey {
            cell,
            check_ignores,
        })
        .await??
        .0)
}

#[derive(Clone, Dupe)]
pub struct FileOpsDelegateWithIgnores {
    ignores: Option<Arc<CellFileIgnores>>,
    delegate: Arc<dyn FileOpsDelegate>,
}

impl PartialEq for FileOpsDelegateWithIgnores {
    fn eq(&self, other: &Self) -> bool {
        self.ignores == other.ignores && self.delegate.eq_token() == other.delegate.eq_token()
    }
}

impl FileOpsDelegateWithIgnores {
    pub(crate) fn new(
        ignores: Option<Arc<CellFileIgnores>>,
        delegate: Arc<dyn FileOpsDelegate>,
    ) -> Self {
        Self { ignores, delegate }
    }
}

impl FileOpsDelegateWithIgnores {
    fn check_ignores(&self, path: &UncheckedCellRelativePath) -> FileIgnoreResult {
        match self.ignores.as_ref() {
            Some(ignores) => ignores.check(path),
            None => FileIgnoreResult::Ok,
        }
    }

    pub async fn read_file_if_exists(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<Option<String>> {
        self.delegate.read_file_if_exists(path).await
    }

    /// Return the list of file outputs, sorted.
    pub async fn read_dir(&self, path: &CellRelativePath) -> buck2_error::Result<ReadDirOutput> {
        // TODO(cjhopman): This should also probably verify that the parent chain is not ignored.
        self.check_ignores(UncheckedCellRelativePath::new(path))
            .into_result()?;

        let entries = self.delegate.read_dir(path).await?;

        let is_ignored = |file_name: &str| {
            let mut cell_relative_path_buf;
            let cell_relative_path: &str = if path.is_empty() {
                file_name
            } else {
                cell_relative_path_buf =
                    String::with_capacity(path.as_str().len() + 1 + file_name.len());
                cell_relative_path_buf.push_str(path.as_str());
                cell_relative_path_buf.push('/');
                cell_relative_path_buf.push_str(file_name);
                &cell_relative_path_buf
            };

            let cell_relative_path = UncheckedCellRelativePath::unchecked_new(cell_relative_path);
            let is_ignored = self.check_ignores(cell_relative_path).is_ignored();
            buck2_error::Ok(is_ignored)
        };

        // Filter out any entries that are ignored.
        let mut included_entries = Vec::new();
        for e in entries {
            let RawDirEntry {
                file_type,
                file_name,
            } = e;

            if !is_ignored(&file_name)? {
                let file_name = match FileNameBuf::try_from_or_get_back(file_name) {
                    Ok(file_name) => file_name,
                    Err(file_name) => {
                        console_message(format!(
                            "File name `{file_name}` is not valid. \
                                    Add the path to `project.ignore` to mute this message",
                        ));
                        continue;
                    }
                };
                included_entries.push(SimpleDirEntry {
                    file_name,
                    file_type,
                });
            }
        }

        Ok(ReadDirOutput {
            included: included_entries.into(),
        })
    }

    pub async fn read_path_metadata_if_exists(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        self.delegate.read_path_metadata_if_exists(path).await
    }

    pub async fn is_ignored(
        &self,
        path: &CellRelativePath,
    ) -> buck2_error::Result<FileIgnoreResult> {
        Ok(self.check_ignores(UncheckedCellRelativePath::new(path)))
    }
}

pub(crate) mod testing {
    pub(crate) use super::keys::FileOpsKey;
    pub(crate) use super::keys::FileOpsValue;
}

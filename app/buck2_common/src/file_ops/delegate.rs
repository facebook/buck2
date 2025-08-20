/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_events::dispatch::console_message;
use buck2_futures::cancellation::CancellationContext;
use cmp_any::PartialEqAny;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::dice::cells::HasCellResolver;
use crate::external_cells::EXTERNAL_CELLS_IMPL;
use crate::file_ops::delegate::keys::FileOpsKey;
use crate::file_ops::delegate::keys::FileOpsValue;
use crate::file_ops::dice::CheckIgnores;
use crate::file_ops::dice::ReadFileProxy;
use crate::file_ops::io::IoFileOpsDelegate;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::ReadDirOutput;
use crate::file_ops::metadata::SimpleDirEntry;
use crate::ignores::all_cells::HasCellFileIgnores;
use crate::ignores::file_ignores::CellFileIgnores;
use crate::ignores::file_ignores::FileIgnoreResult;

/// Note: Everything in this mini-module exists only so that it can be replaced by a `TestFileOps`
/// in unittests
mod keys {
    use allocative::Allocative;
    use buck2_core::cells::name::CellName;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::file_ops::delegate::FileOpsDelegateWithIgnores;
    use crate::file_ops::dice::CheckIgnores;

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
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy>;

    /// Return the list of file outputs, sorted.
    async fn read_dir(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>>;

    async fn read_path_metadata_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>>;

    /// Check if a path exists in the exact case given.
    ///
    /// The default implementation assumes that `read_path_metadata_if_exists` is already correctly
    /// case-sensitive - to the extent that that's not the case, this must be overridden.
    async fn exists_matching_exact_case(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<bool> {
        let metadata = self.read_path_metadata_if_exists(ctx, path).await?;
        Ok(metadata.is_some())
    }

    fn eq_token(&self) -> PartialEqAny<'_>;
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
            let delegate = IoFileOpsDelegate {
                cells: cells.dupe(),
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
        ctx: &mut DiceComputations<'_>,
        path: &CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy> {
        self.delegate.read_file_if_exists(ctx, path).await
    }

    /// Return the list of file outputs, sorted.
    pub(crate) async fn read_dir(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &CellRelativePath,
    ) -> buck2_error::Result<ReadDirOutput> {
        // TODO(cjhopman): This should also probably verify that the parent chain is not ignored.
        self.check_ignores(UncheckedCellRelativePath::new(path))
            .into_result()?;

        let entries = self.delegate.read_dir(ctx, path).await?;

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
        for e in entries.iter() {
            let RawDirEntry {
                file_type,
                file_name,
            } = e;

            if !is_ignored(file_name)? {
                let file_name = match FileNameBuf::try_from_or_get_back(file_name.to_owned()) {
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
                    file_type: *file_type,
                });
            }
        }
        let read_dir_output = ReadDirOutput {
            included: included_entries.into(),
        };
        Ok(read_dir_output)
    }

    pub(crate) async fn exists_matching_exact_case(
        &self,
        path: &CellRelativePath,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<bool> {
        if self
            .check_ignores(UncheckedCellRelativePath::new(path))
            .is_ignored()
        {
            return Ok(false);
        }

        self.delegate.exists_matching_exact_case(dice, path).await
    }

    pub async fn read_path_metadata_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        self.delegate.read_path_metadata_if_exists(ctx, path).await
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

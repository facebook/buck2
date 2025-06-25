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
use buck2_core::cells::CellResolver;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use cmp_any::PartialEqAny;
use dashmap::DashMap;
use derivative::Derivative;
use dice::DiceComputations;
use dice::UserComputationData;
use dupe::Dupe;

use crate::file_ops::delegate::FileOpsDelegate;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;
use crate::io::IoProvider;

/// A `FileOpsDelegate` implementation that calls out to the `IoProvider` to read files.
///
/// This is used for everything except 1) tests, and 2) external cells.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(PartialEq)]
pub(super) struct IoFileOpsDelegate {
    // Safe to ignore because `io` does not change during the lifetime of the daemon.
    #[derivative(PartialEq = "ignore")]
    pub(super) io: Arc<dyn IoProvider>,
    pub(super) cells: CellResolver,
    pub(super) cell: CellName,
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
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Vec<RawDirEntry>> {
        let project_path = self.resolve(path);
        let read_dir_cache = ctx
            .per_transaction_data()
            .data
            .get::<ReadDirCache>()
            .expect("ReadDirCache is expected to be set.");
        if let Some(cached) = read_dir_cache.0.get(&project_path) {
            return Ok(cached.clone());
        };
        let mut entries = self
            .io_provider()
            .read_dir(project_path.clone())
            .await
            .with_buck_error_context(|| format!("Error listing dir `{}`", path))?;

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));
        read_dir_cache.0.insert(project_path, entries.clone());

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

pub struct ReadDirCache(DashMap<ProjectRelativePathBuf, Vec<RawDirEntry>>);

pub trait HasReadDirCache {
    fn set_read_dir_cache(&mut self, cache: DashMap<ProjectRelativePathBuf, Vec<RawDirEntry>>);
}

impl HasReadDirCache for UserComputationData {
    fn set_read_dir_cache(&mut self, cache: DashMap<ProjectRelativePathBuf, Vec<RawDirEntry>>) {
        self.data.set(ReadDirCache(cache));
    }
}

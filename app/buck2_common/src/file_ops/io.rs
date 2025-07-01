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

use crate::dice::data::HasIoProvider;
use crate::file_ops::delegate::FileOpsDelegate;
use crate::file_ops::dice::ReadFileProxy;
use crate::file_ops::metadata::RawDirEntry;
use crate::file_ops::metadata::RawPathMetadata;

/// A `FileOpsDelegate` implementation that calls out to the `IoProvider` to read files.
///
/// This is used for everything except 1) tests, and 2) external cells.
#[derive(Clone, Dupe, Derivative, Allocative)]
#[derivative(PartialEq)]
pub(super) struct IoFileOpsDelegate {
    pub(super) cells: CellResolver,
    pub(super) cell: CellName,
}

impl IoFileOpsDelegate {
    fn resolve(&self, path: &CellRelativePath) -> ProjectRelativePathBuf {
        let cell_root = self.cells.get(self.cell).unwrap().path();
        cell_root.as_project_relative_path().join(path)
    }

    fn get_cell_path(&self, path: &ProjectRelativePath) -> CellPath {
        self.cells.get_cell_path(path)
    }
}

#[async_trait]
impl FileOpsDelegate for IoFileOpsDelegate {
    async fn read_file_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<ReadFileProxy> {
        Ok(ReadFileProxy::new_with_captures(
            (self.resolve(path), ctx.global_data().get_io_provider()),
            |(project_path, io)| async move { io.read_file_if_exists(project_path).await },
        ))
    }

    async fn read_dir(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Arc<[RawDirEntry]>> {
        let project_path = self.resolve(path);
        let read_dir_cache = ctx
            .per_transaction_data()
            .data
            .get::<ReadDirCache>()
            .expect("ReadDirCache is expected to be set.");
        if let Some(cached) = read_dir_cache.0.get(&project_path) {
            return Ok(cached.clone());
        };
        let mut entries = ctx
            .global_data()
            .get_io_provider()
            .read_dir(project_path.clone())
            .await
            .with_buck_error_context(|| format!("Error listing dir `{}`", path))?;

        // Make sure entries are deterministic, since read_dir isn't.
        entries.sort_by(|a, b| a.file_name.cmp(&b.file_name));
        let entries: Arc<[RawDirEntry]> = Arc::from(entries);
        read_dir_cache.0.insert(project_path, entries.clone());

        Ok(entries)
    }

    async fn read_path_metadata_if_exists(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<Option<RawPathMetadata>> {
        let project_path = self.resolve(path);

        let res = ctx
            .global_data()
            .get_io_provider()
            .read_path_metadata_if_exists(project_path)
            .await
            .with_buck_error_context(|| format!("Error accessing metadata for path `{}`", path))?;
        Ok(res.map(|meta| meta.map(|path| Arc::new(self.get_cell_path(&path)))))
    }

    async fn exists_matching_exact_case(
        &self,
        ctx: &mut DiceComputations<'_>,
        path: &'async_trait CellRelativePath,
    ) -> buck2_error::Result<bool> {
        let Some(dir) = path.parent() else {
            // FIXME(JakobDegen): Blindly assuming that cell roots exist isn't quite right, I'll fix
            // this later in the stack
            return Ok(true);
        };
        // FIXME(JakobDegen): Unwrap is ok because a parent exists, but there should be a better API
        // for this
        let entry = path.file_name().unwrap();
        let dir = self.read_dir(ctx, dir).await?;
        Ok(dir.iter().any(|f| &*f.file_name == entry))
    }

    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }
}

struct ReadDirCache(DashMap<ProjectRelativePathBuf, Arc<[RawDirEntry]>>);

pub fn initialize_read_dir_cache(data: &mut UserComputationData) {
    data.data.set(ReadDirCache(Default::default()));
}

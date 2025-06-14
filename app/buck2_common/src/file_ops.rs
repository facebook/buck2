/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use dashmap::DashMap;
use dice::UserComputationData;

use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::ReadDirOutput;
use crate::ignores::file_ignores::FileIgnoreResult;

pub mod error;
pub mod metadata;
pub mod testing;

#[async_trait]
pub trait FileOps: Send + Sync {
    async fn read_file_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<String>>;

    /// Return the list of file outputs, sorted.
    async fn read_dir(&self, path: CellPathRef<'async_trait>)
    -> buck2_error::Result<ReadDirOutput>;

    async fn is_ignored(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<FileIgnoreResult>;

    async fn read_path_metadata_if_exists(
        &self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<RawPathMetadata>>;

    async fn buildfiles<'a>(&self, cell: CellName) -> buck2_error::Result<Arc<[FileNameBuf]>>;
}

pub struct ReadDirCache(DashMap<CellPath, ReadDirOutput>);

impl ReadDirCache {
    pub fn get(&self, key: &CellPath) -> Option<ReadDirOutput> {
        self.0.get(key).map(|entry| entry.clone())
    }
}

pub trait HasReadDirCache {
    fn set_read_dir_cache(&mut self, cache: DashMap<CellPath, ReadDirOutput>);

    fn get_read_dir_cache(&self) -> &ReadDirCache;

    fn update_read_dir_cache(&self, cell_path: CellPath, read_dir_output: &ReadDirOutput);
}

impl HasReadDirCache for UserComputationData {
    fn set_read_dir_cache(&mut self, cache: DashMap<CellPath, ReadDirOutput>) {
        self.data.set(ReadDirCache(cache));
    }

    fn get_read_dir_cache(&self) -> &ReadDirCache {
        &self
            .data
            .get::<ReadDirCache>()
            .expect("ReadDirCache is expected to be set.")
    }

    fn update_read_dir_cache(&self, cell_path: CellPath, read_dir_output: &ReadDirOutput) {
        let updated_cache = self.get_read_dir_cache();
        updated_cache
            .0
            .insert(cell_path.to_owned(), read_dir_output.clone());
    }
}

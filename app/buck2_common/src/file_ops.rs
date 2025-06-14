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
use dupe::Dupe;

use crate::TIME_TO_FIX_USE_BETTER_ERROR;
use crate::file_ops::metadata::RawPathMetadata;
use crate::file_ops::metadata::ReadDirOutput;
use crate::ignores::file_ignores::FileIgnoreResult;

pub mod metadata;
pub mod testing;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum FileOpsError {
    #[error("File not found: {0}")]
    // File not found errors are not inherently always user errors; however, we only use these
    // methods with source files, and in that case this is correct
    #[buck2(input)]
    #[buck2(tag = IoNotFound)]
    FileNotFound(String),
}

pub enum FileReadError {
    NotFound(String),
    Buck(buck2_error::Error),
}

impl FileReadError {
    pub fn with_package_context_information(self, package_path: String) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => {
                let err_message = if *TIME_TO_FIX_USE_BETTER_ERROR.get().unwrap() {
                    format!(
                        "`{}`.\n     Included in `{}` but does not exist",
                        path, package_path
                    )
                } else {
                    path
                };

                FileOpsError::FileNotFound(err_message).into()
            }
            FileReadError::Buck(err) => err.dupe(),
        }
    }

    pub fn without_package_context_information(self) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => FileOpsError::FileNotFound(path).into(),
            FileReadError::Buck(err) => err.dupe(),
        }
    }
}

pub trait FileReadErrorContext<T> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T>;
    fn without_package_context_information(self) -> buck2_error::Result<T>;
}

impl<T> FileReadErrorContext<T> for std::result::Result<T, FileReadError> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T> {
        self.map_err(|e| e.with_package_context_information(package_path))
    }

    fn without_package_context_information(self) -> buck2_error::Result<T> {
        self.map_err(|e| e.without_package_context_information())
    }
}

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

impl dyn FileOps + '_ {
    pub async fn read_file(&self, path: CellPathRef<'_>) -> buck2_error::Result<String> {
        self.read_file_if_exists(path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }

    pub async fn read_path_metadata(
        &self,
        path: CellPathRef<'_>,
    ) -> buck2_error::Result<RawPathMetadata> {
        self.read_path_metadata_if_exists(path)
            .await?
            .ok_or_else(|| FileOpsError::FileNotFound(path.to_string()).into())
    }
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::cell_path::CellPath;
use dashmap::DashMap;
use dice::UserComputationData;

use crate::file_ops::metadata::ReadDirOutput;

pub mod error;
pub mod metadata;
pub mod testing;
pub mod trait_;

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

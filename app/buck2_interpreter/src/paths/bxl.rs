/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use thiserror::Error;

/// Path of a `bxl` file for `bxl` commands
#[derive(
    Clone,
    Hash,
    Eq,
    PartialEq,
    Debug,
    derive_more::Display,
    Ord,
    PartialOrd,
    Allocative
)]
#[display(fmt = "{}", path)]
pub struct BxlFilePath {
    /// The path of this bxl file, including the `bxl` extension
    path: CellPath,
}

#[derive(Debug, Error)]
#[error("Expected a cell path to a `.bxl` file, but got `{0}`")]
struct BxlPathError(CellPath);

impl BxlFilePath {
    pub fn new(path: CellPath) -> anyhow::Result<Self> {
        let err = || BxlPathError(path.clone());

        if path.path().file_name().ok_or_else(err)?.extension() != Some("bxl") {
            return Err(anyhow::anyhow!(err()));
        }

        Ok(Self::unverified_new(path))
    }

    fn unverified_new(path: CellPath) -> Self {
        BxlFilePath { path }
    }

    pub fn testing_new(cell: &str, cell_relative_path: &str) -> Self {
        let path = CellPath::new(
            CellName::testing_new(cell),
            CellRelativePathBuf::try_from(cell_relative_path.to_owned()).unwrap(),
        );
        Self::unverified_new(path)
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        BuildFileCell::new(self.cell())
    }
}

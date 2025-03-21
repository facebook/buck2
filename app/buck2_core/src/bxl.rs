/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

use crate::cells::build_file_cell::BuildFileCell;
use crate::cells::cell_path::CellPath;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePathBuf;

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
#[display("{}", path)]
pub struct BxlFilePath {
    /// The path of this bxl file, including the `bxl` extension
    path: CellPath,
}

#[derive(Debug, buck2_error::Error)]
#[error("Expected a cell path to a `.bxl` file, but got `{0}`")]
#[buck2(tag = Input)]
struct BxlPathError(CellPath);

impl BxlFilePath {
    pub fn new(path: CellPath) -> buck2_error::Result<Self> {
        let err = || BxlPathError(path.clone());

        if path.path().file_name().ok_or_else(err)?.extension() != Some("bxl") {
            return Err(err().into());
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

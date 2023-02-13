/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;

use crate::cells::build_file_cell::BuildFileCell;
use crate::cells::cell_path::CellPath;
use crate::cells::cell_path::CellPathRef;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::fs::paths::file_name::FileName;

#[derive(Debug, thiserror::Error)]
enum ImportPathError {
    #[error("Invalid import path `{0}`")]
    Invalid(CellPath),
}

/// Path of a `.bzl` file.
#[derive(Clone, Hash, Eq, PartialEq, Debug, Allocative)]
pub struct ImportPath {
    /// The path to the import as a 'CellPath', which contains the cell
    /// information and the cell relative path to the bzl file itself, including the bzl suffix
    path: CellPath,
    /// The cell of the top-level build module that this is being loaded
    /// (perhaps transitively) into.
    build_file_cell: BuildFileCell,
}

impl ImportPath {
    pub fn new(path: CellPath, build_file_cell: BuildFileCell) -> anyhow::Result<Self> {
        if path.parent().is_none() {
            return Err(ImportPathError::Invalid(path).into());
        }

        if path.path().as_str().contains('?') {
            return Err(ImportPathError::Invalid(path).into());
        }

        Ok(Self {
            path,
            build_file_cell,
        })
    }

    pub fn testing_new(cell: &str, cell_relative_path: &str, filename: &str) -> Self {
        let cell = cell.to_owned();
        Self::testing_new_cross_cell(&cell, cell_relative_path, filename, &cell)
    }

    pub fn testing_new_cross_cell(
        cell: &str,
        cell_relative_path: &str,
        filename: &str,
        build_file_cell: &str,
    ) -> Self {
        let cell_path = CellPath::new(
            CellName::testing_new(cell),
            CellRelativePath::unchecked_new(cell_relative_path)
                .join(FileName::unchecked_new(filename)),
        );
        Self::new(
            cell_path,
            BuildFileCell::new(CellName::testing_new(build_file_cell)),
        )
        .unwrap()
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        self.build_file_cell
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    /// Parent directory of the import path.
    pub fn path_parent(&self) -> CellPathRef {
        self.path
            .parent()
            .expect("constructor verified path has parent")
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.build_file_cell.name() == self.path.cell() {
            write!(f, "{}", self.path)
        } else {
            write!(f, "{}@{}", self.path, self.build_file_cell.name())
        }
    }
}

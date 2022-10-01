/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::fmt::Display;
use std::fmt::Formatter;

use crate::cells::build_file_cell::BuildFileCell;
use crate::cells::cell_path::CellPath;
use crate::cells::paths::CellRelativePath;
use crate::cells::CellName;
use crate::fs::paths::FileName;

/// The starlark interpreter expects imports
/// to be identified by a String and requires using this id in some cases.
/// The id will contain both the package and filename. For a cross-cell load,
/// it will include an `@cell_name` suffix to indicate the top-level cell
/// being loaded into.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ModuleID(pub String);

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Borrow<str> for ModuleID {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl ModuleID {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Path of a `.bzl` file.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct ImportPath {
    /// The path to the import as a 'CellPath', which contains the cell
    /// information and the cell relative path to the bzl file itself, including the bzl suffix
    path: CellPath,
    /// The cell of the top-level build module that this is being loaded
    /// (perhaps transitively) into.
    build_file_cell: BuildFileCell,
    /// A ModuleID for the import.
    id: ModuleID,
}

impl ImportPath {
    pub fn new(path: CellPath, build_file_cell: BuildFileCell) -> anyhow::Result<Self> {
        Ok(Self::unverified_new(path, build_file_cell))
    }

    pub fn unverified_new(path: CellPath, build_file_cell: BuildFileCell) -> Self {
        let id = ModuleID(if build_file_cell.name() == path.cell() {
            format!("{}", path)
        } else {
            format!("{}@{}", path, build_file_cell.name())
        });
        Self {
            path,
            build_file_cell,
            id,
        }
    }

    pub fn unchecked_new(cell: &str, cell_relative_path: &str, filename: &str) -> Self {
        let cell = cell.to_owned();
        Self::unchecked_new_cross_cell(&cell, cell_relative_path, filename, &cell)
    }

    pub fn unchecked_new_cross_cell(
        cell: &str,
        cell_relative_path: &str,
        filename: &str,
        build_file_cell: &str,
    ) -> Self {
        let cell_path = CellPath::new(
            CellName::unchecked_new(cell.to_owned()),
            CellRelativePath::unchecked_new(cell_relative_path)
                .join(FileName::unchecked_new(filename)),
        );
        Self::unverified_new(
            cell_path,
            BuildFileCell::new(CellName::unchecked_new(build_file_cell.to_owned())),
        )
    }

    pub fn cell(&self) -> &CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
        &self.build_file_cell
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn id(&self) -> &ModuleID {
        &self.id
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

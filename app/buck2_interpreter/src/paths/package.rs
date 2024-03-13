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
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::file_name::FileName;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative, derive_more::Display)]
#[display(fmt = "{}", path)]
pub struct PackageFilePath {
    /// Including `/PACKAGE`.
    path: CellPath,
}

impl PackageFilePath {
    pub const PACKAGE_FILE_NAME: &'static FileName = FileName::unchecked_new("PACKAGE");

    /// Create for directory containing `PACKAGE` file.
    pub fn for_dir(path: CellPathRef) -> PackageFilePath {
        PackageFilePath {
            path: path.join(Self::PACKAGE_FILE_NAME),
        }
    }

    pub fn from_file_path(path: CellPathRef) -> Option<PackageFilePath> {
        if path.ends_with(PackageFilePath::PACKAGE_FILE_NAME.as_ref()) {
            Some(PackageFilePath {
                path: path.to_owned(),
            })
        } else {
            None
        }
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        BuildFileCell::new(self.cell())
    }

    /// Directory containing this `PACKAGE` file.
    pub fn dir(&self) -> CellPathRef {
        self.path
            .parent()
            .expect("constructor verifies that path is not root")
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }
}

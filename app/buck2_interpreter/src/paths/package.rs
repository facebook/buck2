/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_fs::paths::file_name::FileName;
use pagable::Pagable;

/// Represents the path to a PACKAGE file.
///
/// Each package can define local configuration
/// by providing a PACKAGE file. This file is evaluated prior to the BUCK file and
/// can specify per-package values accessible via Starlark.
///
/// Example of a valid PACKAGE file path: `fbsource//path/to/PACKAGE`
///
/// Find more details in the [Buck2 documentation](https://buck2.build/docs/rule_authors/package_files/).
#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Allocative,
    derive_more::Display,
    Pagable
)]
#[display("{}", path)]
pub struct PackageFilePath {
    /// Including `/PACKAGE`.
    path: CellPath,
}

impl PackageFilePath {
    pub fn package_file_names() -> impl Iterator<Item = &'static FileName> {
        [
            FileName::unchecked_new("BUCK_TREE"),
            FileName::unchecked_new("PACKAGE"),
        ]
        .into_iter()
    }

    /// Files which could be `PACKAGE` files.
    pub fn for_dir(path: CellPathRef<'_>) -> impl Iterator<Item = PackageFilePath> + '_ {
        Self::package_file_names().map(move |name| PackageFilePath {
            path: path.join(name),
        })
    }

    pub fn package_file_for_dir(path: CellPathRef) -> PackageFilePath {
        PackageFilePath {
            path: path.join(FileName::unchecked_new("PACKAGE")),
        }
    }

    pub fn from_file_path(path: CellPathRef) -> Option<PackageFilePath> {
        for file_name in Self::package_file_names() {
            if path.ends_with(file_name.as_ref()) {
                return Some(PackageFilePath {
                    path: path.to_owned(),
                });
            }
        }
        None
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        BuildFileCell::new(self.cell())
    }

    /// Directory containing this `PACKAGE` file.
    pub fn dir(&self) -> CellPathRef<'_> {
        self.path
            .parent()
            .expect("constructor verifies that path is not root")
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn file_name(&self) -> &FileName {
        self.path.path().file_name().unwrap()
    }
}

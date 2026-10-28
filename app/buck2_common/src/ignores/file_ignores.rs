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
use buck2_core::cells::name::CellName;
use buck2_core::cells::nested::NestedCells;
use buck2_core::cells::paths::CellRelativePath;
use pagable::Pagable;

use crate::ignores::ignore_set::IgnoreSet;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum FileOpsError {
    #[error("Tried to read ignored dir `{0}` (reason: {1}).")]
    ReadIgnoredDir(String, String),
}

#[derive(Debug, Allocative)]
pub enum FileIgnoreReason {
    IgnoredByPattern { path: String, pattern: String },
    IgnoredByCell { path: String, cell_name: CellName },
}

impl FileIgnoreReason {
    pub fn describe(&self) -> String {
        match self {
            FileIgnoreReason::IgnoredByPattern { pattern, .. } => {
                format!("config project.ignore contains `{pattern}`")
            }
            FileIgnoreReason::IgnoredByCell { cell_name, .. } => {
                format!("path is contained in cell `{cell_name}`")
            }
        }
    }
}

#[derive(Debug, Allocative)]
pub enum FileIgnoreResult {
    Ok,
    Ignored(FileIgnoreReason),
}

impl FileIgnoreResult {
    /// Converts the FileIgnoreResult to a Result<()> where any ignored case is converted to an Err
    /// with appropriate message. This should be used when it would be an error to interact with an
    /// ignored file.
    pub fn into_result(self) -> buck2_error::Result<()> {
        match self {
            FileIgnoreResult::Ok => Ok(()),
            FileIgnoreResult::Ignored(FileIgnoreReason::IgnoredByPattern { path, pattern }) => {
                Err(FileOpsError::ReadIgnoredDir(
                    path,
                    format!("file is matched by pattern `{pattern}`"),
                )
                .into())
            }
            FileIgnoreResult::Ignored(FileIgnoreReason::IgnoredByCell { path, cell_name }) => Err(
                FileOpsError::ReadIgnoredDir(path, format!("file is part of cell `{cell_name}`"))
                    .into(),
            ),
        }
    }

    /// Returns true if the file is ignored, false otherwise.
    pub fn is_ignored(&self) -> bool {
        !matches!(self, FileIgnoreResult::Ok)
    }
}

/// Ignores files based on configured ignore patterns and cell paths.
#[derive(PartialEq, Eq, Allocative, Debug, Pagable)]
pub struct CellFileIgnores {
    ignores: Arc<IgnoreSet>,
    cell_ignores: NestedCells,
    cell: CellName,
}

impl CellFileIgnores {
    pub fn new(ignores: Arc<IgnoreSet>, nested_cells: NestedCells, cell: CellName) -> Self {
        CellFileIgnores {
            ignores,
            cell_ignores: nested_cells,
            cell,
        }
    }

    pub(crate) fn check(&self, path: &CellRelativePath) -> FileIgnoreResult {
        if let Some(pattern) = self.ignores.matches_candidate(self.cell, path) {
            return FileIgnoreResult::Ignored(FileIgnoreReason::IgnoredByPattern {
                path: path.as_str().to_owned(),
                pattern: pattern.to_owned(),
            });
        }

        if let Some((_, cell_name, _)) = self.cell_ignores.matches(path) {
            return FileIgnoreResult::Ignored(FileIgnoreReason::IgnoredByCell {
                path: path.as_str().to_owned(),
                cell_name,
            });
        }

        FileIgnoreResult::Ok
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::nested::NestedCells;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;

    use crate::ignores::file_ignores::CellFileIgnores;
    use crate::ignores::ignore_set::IgnoreSet;

    #[test]
    fn file_ignores() -> buck2_error::Result<()> {
        let cells = &[
            (
                CellName::testing_new("root"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root")),
            ),
            (
                CellName::testing_new("other"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("root/other_cell")),
            ),
            (
                CellName::testing_new("third"),
                CellRootPath::new(ProjectRelativePath::unchecked_new("third")),
            ),
        ];
        let nested_cells = NestedCells::from_cell_roots(cells, CellRootPath::testing_new("root"));
        let root_cell = CellName::testing_new("root");
        let ignore_set = Arc::new(IgnoreSet::testing_new_single_cell(
            root_cell,
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            true,
        )?);
        let ignores = CellFileIgnores::new(ignore_set, nested_cells, root_cell);

        assert!(
            ignores
                .check(CellRelativePath::unchecked_new("some/long/path/Class.java"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(CellRelativePath::unchecked_new("other_cell"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(CellRelativePath::unchecked_new("other_cell/some/lib"))
                .is_ignored()
        );

        assert!(
            !ignores
                .check(CellRelativePath::unchecked_new("third"))
                .is_ignored()
        );

        assert!(
            !ignores
                .check(CellRelativePath::unchecked_new("one/two/three"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(CellRelativePath::unchecked_new("recursive/two/three"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(CellRelativePath::unchecked_new("trailing_slash/BUCK"))
                .is_ignored()
        );

        Ok(())
    }
}

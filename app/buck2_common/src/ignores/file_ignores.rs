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
use buck2_core::cells::name::CellName;
use buck2_core::cells::nested::NestedCells;
use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;

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
        match self {
            FileIgnoreResult::Ok => false,
            _ => true,
        }
    }
}

/// Ignores files based on configured ignore patterns and cell paths.
#[derive(PartialEq, Eq, Allocative, Debug)]
pub struct CellFileIgnores {
    ignores: IgnoreSet,
    cell_ignores: NestedCells,
}

impl CellFileIgnores {
    /// Creates a new FileIgnores intended for use by the interpreter.
    ///
    /// This will ignore files/dirs in the ignore spec and those in other cells.
    pub fn new_for_interpreter(
        ignore_spec: &str,
        nested_cells: NestedCells,
        root_cell: bool,
    ) -> buck2_error::Result<CellFileIgnores> {
        Ok(CellFileIgnores {
            ignores: IgnoreSet::from_ignore_spec(ignore_spec, root_cell)?,
            cell_ignores: nested_cells,
        })
    }

    pub(crate) fn check(&self, path: &UncheckedCellRelativePath) -> FileIgnoreResult {
        let candidate = globset::Candidate::new(path.as_str());

        if let Some(pattern) = self.ignores.matches_candidate(&candidate) {
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
    use buck2_core::cells::cell_root_path::CellRootPath;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::nested::NestedCells;
    use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;

    use crate::ignores::file_ignores::CellFileIgnores;

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
        let ignores = CellFileIgnores::new_for_interpreter(
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            nested_cells,
            true,
        )?;

        assert!(
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "some/long/path/Class.java"
                ))
                .is_ignored()
        );

        assert!(
            ignores
                .check(UncheckedCellRelativePath::unchecked_new("other_cell"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "other_cell/some/lib"
                ))
                .is_ignored()
        );

        assert!(
            !ignores
                .check(UncheckedCellRelativePath::unchecked_new("third"))
                .is_ignored()
        );

        assert!(
            !ignores
                .check(UncheckedCellRelativePath::unchecked_new("one/two/three"))
                .is_ignored()
        );

        assert!(
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "recursive/two/three"
                ))
                .is_ignored()
        );

        assert!(
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "trailing_slash/BUCK"
                ))
                .is_ignored()
        );

        Ok(())
    }
}

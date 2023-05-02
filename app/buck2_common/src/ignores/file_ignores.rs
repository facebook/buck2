/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::cells::name::CellName;
use buck2_core::cells::nested::NestedCells;
use buck2_core::cells::unchecked_cell_rel_path::UncheckedCellRelativePath;

use crate::ignores::ignore_set::IgnoreSet;

#[derive(Debug, thiserror::Error)]
enum FileOpsError {
    #[error("Tried to read ignored dir `{0}` (reason: {1}).")]
    ReadIgnoredDir(String, String),
}

pub(crate) enum FileIgnoreResult {
    Ok,
    IgnoredByPattern(String, String),
    IgnoredByCell(String, CellName),
}

impl FileIgnoreResult {
    /// Converts the FileIgnoreResult to a Result<()> where any ignored case is converted to an Err
    /// with appropriate message. This should be used when it would be an error to interact with an
    /// ignored file.
    pub(crate) fn into_result(self) -> anyhow::Result<()> {
        match self {
            FileIgnoreResult::Ok => Ok(()),
            FileIgnoreResult::IgnoredByPattern(path, pattern) => {
                Err(anyhow::anyhow!(FileOpsError::ReadIgnoredDir(
                    path,
                    format!("file is matched by pattern `{}`", pattern)
                )))
            }
            FileIgnoreResult::IgnoredByCell(path, cell_name) => Err(anyhow::anyhow!(
                FileOpsError::ReadIgnoredDir(path, format!("file is part of cell `{}`", cell_name))
            )),
        }
    }

    /// Returns true if the file is ignored, false otherwise.
    pub(crate) fn is_ignored(&self) -> bool {
        match self {
            FileIgnoreResult::Ok => false,
            _ => true,
        }
    }
}

/// Ignores files based on configured ignore patterns and cell paths.
#[derive(PartialEq, Eq, Allocative, Debug)]
pub struct FileIgnores {
    ignores: IgnoreSet,
    cell_ignores: NestedCells,
}

impl FileIgnores {
    /// Creates a new FileIgnores intended for use by the interpreter.
    ///
    /// This will ignore files/dirs in the ignore spec and those in other cells.
    pub fn new_for_interpreter(
        ignore_spec: &str,
        nested_cells: NestedCells,
        root_cell: bool,
    ) -> anyhow::Result<FileIgnores> {
        Ok(FileIgnores {
            ignores: IgnoreSet::from_ignore_spec(ignore_spec, root_cell)?,
            cell_ignores: nested_cells,
        })
    }

    pub(crate) fn check(&self, path: &UncheckedCellRelativePath) -> FileIgnoreResult {
        let candidate = globset::Candidate::new(path.as_str());

        if let Some(pattern) = self.ignores.matches_candidate(&candidate) {
            return FileIgnoreResult::IgnoredByPattern(
                path.as_str().to_owned(),
                pattern.to_owned(),
            );
        }

        if let Some((_, cell_name, _)) = self.cell_ignores.matches(path) {
            return FileIgnoreResult::IgnoredByCell(path.as_str().to_owned(), cell_name);
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

    use crate::ignores::file_ignores::FileIgnores;

    #[test]
    fn file_ignores() -> anyhow::Result<()> {
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
        let ignores = FileIgnores::new_for_interpreter(
            "**/*.java , some/dir/**, one/*, \n    recursive, trailing_slash/",
            nested_cells,
            true,
        )?;

        assert_eq!(
            true,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "some/long/path/Class.java"
                ))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new("other_cell"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "other_cell/some/lib"
                ))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new("third"))
                .is_ignored()
        );

        assert_eq!(
            false,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new("one/two/three"))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "recursive/two/three"
                ))
                .is_ignored()
        );

        assert_eq!(
            true,
            ignores
                .check(UncheckedCellRelativePath::unchecked_new(
                    "trailing_slash/BUCK"
                ))
                .is_ignored()
        );

        Ok(())
    }
}

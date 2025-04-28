/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use relative_path::Component;
use relative_path::RelativePath;
use relative_path::RelativePathBuf;

use crate::cells::cell_path::CellPath;
use crate::cells::paths::CellRelativePathBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum RelativeImportParseError {
    #[error("Relative import path `{0}` is not allowed at the current location.")]
    InvalidRelativeImport(String),
    #[error("Relative import path `{0}` with non-leading parent dir component is not allowed.")]
    NonLeadingParentDirRelativeImport(String),
    #[error("Relative import path `{0}` with current dir component `.` is not allowed.")]
    CurrentDirRelativeImport(String),
    #[error("Invalid path `{0}` for file relative import path. Should be a forward relative path.")]
    InvalidCurrentPathWhenFileRelativeImport(String),
}

#[derive(Debug, Hash, Eq, PartialEq, PartialOrd, Ord, Allocative)]
pub struct CellPathWithAllowedRelativeDir {
    current_dir: CellPath,
    allowed_relative_dir: Option<CellPath>,
}

impl CellPathWithAllowedRelativeDir {
    pub fn new(current_dir: CellPath, allowed_relative_dir: Option<CellPath>) -> Self {
        if let Some(ref allowed_relative_dir_value) = allowed_relative_dir {
            assert!(
                current_dir.starts_with(allowed_relative_dir_value.as_ref()),
                "current_dir: `{}` must be a subpath of allowed_relative_dir: `{}`",
                current_dir,
                allowed_relative_dir_value,
            );
        }

        Self {
            current_dir,
            allowed_relative_dir,
        }
    }

    pub fn join_normalized(&self, path: &RelativePath) -> buck2_error::Result<CellPath> {
        let Some(allowed_relative_dir) = &self.allowed_relative_dir else {
            let rel_path = <&ForwardRelativePath>::try_from(path).map_err(|_e| {
                RelativeImportParseError::InvalidCurrentPathWhenFileRelativeImport(path.to_string())
            })?;
            return Ok(self.current_dir.join(rel_path));
        };

        let mut resolved_path = RelativePathBuf::from_path(self.current_dir.path().to_string())?;
        let mut num_allowed_parents =
            RelativePath::from_path(&self.current_dir.path().to_string())?
                .components()
                .count()
                - RelativePath::from_path(&allowed_relative_dir.path().to_string())?
                    .components()
                    .count();
        let mut components = path.components();
        while let Some(c) = components.next() {
            match c {
                Component::CurDir => {
                    return Err(RelativeImportParseError::CurrentDirRelativeImport(
                        path.to_string(),
                    )
                    .into());
                }
                Component::ParentDir => {
                    if num_allowed_parents == 0 {
                        return Err(RelativeImportParseError::InvalidRelativeImport(
                            path.to_string(),
                        )
                        .into());
                    }
                    num_allowed_parents -= 1;
                    resolved_path.pop();
                }
                Component::Normal(file_name) => {
                    resolved_path.push(file_name);
                    let remaining = components.as_relative_path();
                    let Ok(remaining) = <&ForwardRelativePath>::try_from(remaining) else {
                        return Err(RelativeImportParseError::NonLeadingParentDirRelativeImport(
                            path.to_string(),
                        )
                        .into());
                    };
                    if !remaining.is_empty() {
                        // If we join an empty ForwardRelativePath, it will add an unwanted trailing slash
                        resolved_path = resolved_path.join(remaining);
                    }
                    break;
                }
            }
        }
        Ok(CellPath::new(
            self.current_dir.cell(),
            CellRelativePathBuf::try_from(resolved_path)?,
        ))
    }

    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellPath {
        self.current_dir.join(path)
    }
}

#[cfg(test)]
mod tests {
    use relative_path::RelativePath;

    use crate::cells::cell_path::CellPath;
    use crate::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
    use crate::cells::cell_path_with_allowed_relative_dir::RelativeImportParseError;
    use crate::cells::name::CellName;
    use crate::cells::paths::CellRelativePath;
    use crate::fs::paths::file_name::FileName;

    fn path(cell: &str, dir: &str, filename: &str) -> CellPath {
        CellPath::new(
            CellName::testing_new(cell),
            CellRelativePath::unchecked_new(dir).join(FileName::unchecked_new(filename)),
        )
    }

    #[test]
    fn parent_directory_traversal() -> buck2_error::Result<()> {
        let import = "../sibling.bzl";
        let cell_path_with_allowed_relative_dir = CellPathWithAllowedRelativeDir {
            current_dir: CellPath::testing_new("cell1//package/path"),
            allowed_relative_dir: Some(CellPath::testing_new("cell1//package")),
        };
        assert_eq!(
            path("cell1", "package", "sibling.bzl"),
            cell_path_with_allowed_relative_dir
                .join_normalized(RelativePath::from_path(import)?)?
        );
        Ok(())
    }

    #[test]
    fn multiple_parent_directory_traversal() -> buck2_error::Result<()> {
        let import = "../../foo.bzl";
        let cell_path_with_allowed_relative_dir = CellPathWithAllowedRelativeDir {
            current_dir: CellPath::testing_new("cell1//root/a/b"),
            allowed_relative_dir: Some(CellPath::testing_new("cell1//root")),
        };
        assert_eq!(
            path("cell1", "root", "foo.bzl"),
            cell_path_with_allowed_relative_dir
                .join_normalized(RelativePath::from_path(import)?)?
        );
        Ok(())
    }

    #[test]
    fn parent_directory_with_subpath() -> buck2_error::Result<()> {
        let import = "../bar/zoo.bzl";
        let cell_path_with_allowed_relative_dir = CellPathWithAllowedRelativeDir {
            current_dir: CellPath::testing_new("cell1//package/foo/baz"),
            allowed_relative_dir: Some(CellPath::testing_new("cell1//package")),
        };
        assert_eq!(
            path("cell1", "package/foo/bar", "zoo.bzl"),
            cell_path_with_allowed_relative_dir
                .join_normalized(RelativePath::from_path(import)?)?
        );
        Ok(())
    }

    #[test]
    fn traversal_out_of_allowed_dir() -> buck2_error::Result<()> {
        let import = "../../package1/too_far_up.bzl";
        let cell_path_with_allowed_relative_dir = CellPathWithAllowedRelativeDir {
            current_dir: CellPath::testing_new("cell1//package1/path"),
            allowed_relative_dir: Some(CellPath::testing_new("cell1//package1")),
        };
        assert_eq!(
            format!(
                "{:#}",
                cell_path_with_allowed_relative_dir
                    .join_normalized(RelativePath::from_path(import)?)
                    .unwrap_err()
            ),
            RelativeImportParseError::InvalidRelativeImport(import.to_owned()).to_string()
        );
        Ok(())
    }

    #[test]
    fn nonleading_parent_dir_component() -> buck2_error::Result<()> {
        let import = "some_dir/../../package2/wrong_package.bzl";
        let cell_path_with_allowed_relative_dir = CellPathWithAllowedRelativeDir {
            current_dir: CellPath::testing_new("cell1//package1/path"),
            allowed_relative_dir: Some(CellPath::testing_new("cell1//package1")),
        };
        assert_eq!(
            format!(
                "{:#}",
                cell_path_with_allowed_relative_dir
                    .join_normalized(RelativePath::from_path(import)?)
                    .unwrap_err()
            ),
            RelativeImportParseError::NonLeadingParentDirRelativeImport(import.to_owned())
                .to_string()
        );
        Ok(())
    }
}

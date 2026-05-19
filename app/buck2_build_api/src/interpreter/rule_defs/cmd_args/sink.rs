/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

/// CommandLineSink is the destination into which `CommandLineFormatter` writes the fully prepared
/// command line.
///
/// This is almost always just `Vec<String>`.
pub trait CommandLineSink {
    fn push_arg(&mut self, s: Cow<'_, str>);
}

impl CommandLineSink for Vec<String> {
    fn push_arg(&mut self, s: Cow<'_, str>) {
        self.push(s.into_owned())
    }
}

/// A command line is normally a list of strings; this type is to be used when you expect just one.
///
/// This should normally be used with a `CommandLineFormatter` on which you expect to immediately
/// call `push_scope_delimiter`.
pub enum SingletonCommandLineSink {
    None,
    Finished(String),
    Error(buck2_error::Error),
}

impl SingletonCommandLineSink {
    pub fn new() -> Self {
        Self::None
    }

    pub fn finalize(self) -> buck2_error::Result<String> {
        match self {
            Self::None => Err(buck2_error::internal_error!(
                "SingletonCommandLineSink not written",
            )),
            Self::Finished(s) => Ok(s),
            Self::Error(e) => Err(e),
        }
    }
}

impl CommandLineSink for SingletonCommandLineSink {
    fn push_arg(&mut self, s: Cow<'_, str>) {
        match self {
            Self::None => {
                *self = Self::Finished(s.into_owned());
            }
            Self::Finished(_) => {
                *self = Self::Error(buck2_error::internal_error!(
                    "SingletonCommandLineSink written more than once"
                ));
            }
            Self::Error(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use buck2_core::execution_types::executor_config::PathSeparatorKind;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_fs::paths::RelativePathBuf;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;

    use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;

    #[test]
    fn test_command_line_location() {
        let unix_path_sep = PathSeparatorKind::Unix;
        let windows_path_sep = PathSeparatorKind::Windows;
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::empty(), unix_path_sep)
                .into_string(),
            "."
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::empty(), windows_path_sep)
                .into_string(),
            "."
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a"), unix_path_sep)
                .into_string(),
            "./a"
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a"), windows_path_sep)
                .into_string(),
            ".\\a"
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a/b"), unix_path_sep)
                .into_string(),
            "a/b"
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a/b"), windows_path_sep)
                .into_string(),
            "a\\b"
        );
    }

    #[test]
    fn test_abs_command_line_location_windows() {
        if !cfg!(windows) {
            return;
        }

        let root = ProjectRoot::new_unchecked(AbsNormPathBuf::unchecked_new(PathBuf::from(
            "C:\\foo\\bar",
        )));

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePath::empty().to_buf(),
                PathSeparatorKind::Windows
            )
            .into_string(),
            "C:\\foo\\bar",
        );

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePathBuf::testing_new("baz"),
                PathSeparatorKind::Windows
            )
            .into_string(),
            "C:\\foo\\bar\\baz",
        );

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePathBuf::testing_new("baz/qux"),
                PathSeparatorKind::Windows
            )
            .into_string(),
            "C:\\foo\\bar\\baz\\qux",
        );
    }

    #[test]
    fn test_abs_command_line_location_unix() {
        if !cfg!(unix) {
            return;
        }

        let root =
            ProjectRoot::new_unchecked(AbsNormPathBuf::unchecked_new(PathBuf::from("/foo/bar")));

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePath::empty().to_buf(),
                PathSeparatorKind::Unix
            )
            .into_string(),
            "/foo/bar",
        );

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePathBuf::testing_new("baz"),
                PathSeparatorKind::Unix
            )
            .into_string(),
            "/foo/bar/baz",
        );

        assert_eq!(
            CommandLineLocation::from_root(
                &root,
                ProjectRelativePathBuf::testing_new("baz/qux"),
                PathSeparatorKind::Unix
            )
            .into_string(),
            "/foo/bar/baz/qux",
        );
    }
}

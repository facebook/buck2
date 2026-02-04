/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_fs::paths::RelativePathBuf;
use indexmap::IndexSet;

use crate::interpreter::rule_defs::cmd_args::traits::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineLocation;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
pub enum CommandLineBuilderErrors {
    #[error(
        "write-to-file macro is only supported as a part of command line argument which is written to a file"
    )]
    WriteToFileMacroNotSupported,
    #[error(
        "Number of write-to-file macro artifacts during analysis time should be consistent with when the command line is formed."
    )]
    InconsistentNumberOfMacroArtifacts,
}

/// Builds up arguments needed to construct a command line
pub struct DefaultCommandLineContext<'v> {
    fs: &'v ExecutorFs<'v>,
    // First element is list of artifacts, each corresponding to a file with macro contents. Ordering is very important.
    // Second element is a current position in that list.
    maybe_macros_state: Option<(
        &'v IndexSet<(&'v Artifact, Option<&'v ContentBasedPathHash>)>,
        usize,
    )>,
}

impl<'v> DefaultCommandLineContext<'v> {
    /// Create a new builder
    ///
    /// `fs`: The `ExecutorFs` that things like `Artifact` can use to generate strings
    pub fn new(fs: &'v ExecutorFs) -> Self {
        Self {
            fs,
            maybe_macros_state: None,
        }
    }

    pub fn new_with_write_to_file_macros_support(
        fs: &'v ExecutorFs,
        macro_files: &'v IndexSet<(&'v Artifact, Option<&'v ContentBasedPathHash>)>,
    ) -> Self {
        Self {
            fs,
            maybe_macros_state: Some((macro_files, 0)),
        }
    }

    /// The `ArtifactFilesystem` to resolve `Artifact`s
    pub fn fs(&self) -> &ExecutorFs<'_> {
        self.fs
    }
}

impl CommandLineContext for DefaultCommandLineContext<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<CommandLineLocation<'_>> {
        Ok(CommandLineLocation::from_relative_path(
            path.into(),
            self.fs.path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs<'_> {
        self.fs
    }

    fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf> {
        if let Some((files, pos)) = self.maybe_macros_state {
            if pos >= files.len() {
                return Err(CommandLineBuilderErrors::InconsistentNumberOfMacroArtifacts.into());
            }
            self.maybe_macros_state = Some((files, pos + 1));
            Ok(self
                .resolve_project_path(files[pos].0.resolve_path(self.fs.fs(), files[pos].1)?)?
                .into_relative())
        } else {
            Err(CommandLineBuilderErrors::WriteToFileMacroNotSupported.into())
        }
    }
}

pub struct AbsCommandLineContext<'v>(DefaultCommandLineContext<'v>);

impl<'v> AbsCommandLineContext<'v> {
    pub fn new(executor_fs: &'v ExecutorFs) -> Self {
        Self::wrap(DefaultCommandLineContext::<'v>::new(executor_fs))
    }

    pub fn wrap(default: DefaultCommandLineContext<'v>) -> Self {
        Self(default)
    }
}

impl CommandLineContext for AbsCommandLineContext<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<CommandLineLocation<'_>> {
        Ok(CommandLineLocation::from_root(
            self.0.fs().fs().fs(),
            path,
            self.fs().path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs<'_> {
        self.0.fs()
    }

    fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf> {
        let executor_fs = self.0.fs();
        let mut path = executor_fs.fs().fs().root().to_path_buf();
        path.extend(self.0.next_macro_file_path()?.iter());
        RelativePathBuf::from_path(path).map_err(|e| {
            buck2_error::buck2_error!(buck2_error::ErrorTag::Tier0, "{}", e.to_string())
        })
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use buck2_core::cells::CellResolver;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::execution_types::executor_config::PathSeparatorKind;
    use buck2_core::fs::artifact_path_resolver::ArtifactFs;
    use buck2_core::fs::buck_out_path::BuckOutPathResolver;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use fxhash::FxHashMap;

    use super::*;
    use crate::interpreter::rule_defs::cmd_args::traits::CommandLineArgLike;

    #[test]
    fn adds_args_and_builds() -> buck2_error::Result<()> {
        let project_fs =
            ProjectRoot::new(AbsNormPathBuf::try_from(std::env::current_dir().unwrap()).unwrap())
                .unwrap();
        let fs = ArtifactFs::new(
            CellResolver::testing_with_name_and_path(
                CellName::testing_new("cell"),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            ),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs,
        );
        let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);

        let mut cli = Vec::<String>::new();
        let mut ctx = DefaultCommandLineContext::new(&executor_fs);

        "foo".add_to_command_line(&mut cli, &mut ctx, &FxHashMap::default())?;

        assert_eq!(&["foo".to_owned()], cli.as_slice());
        Ok(())
    }

    #[test]
    fn test_command_line_location() {
        let unix_path_sep = PathSeparatorKind::Unix;
        let windows_path_sep = PathSeparatorKind::Windows;
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::new(), unix_path_sep)
                .into_string(),
            "."
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::new(), windows_path_sep)
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use anyhow::anyhow;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use indexmap::IndexSet;
use thiserror::Error;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::ExecutorFs;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineLocation;

#[derive(Error, Debug)]
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
pub struct BaseCommandLineBuilder<'v> {
    fs: &'v ExecutorFs<'v>,
    command_line: Vec<String>,
    // First element is list of artifacts, each corresponding to a file with macro contents. Ordering is very important.
    // Second element is a current position in that list.
    maybe_macros_state: Option<(&'v IndexSet<Artifact>, usize)>,
}

impl<'v> BaseCommandLineBuilder<'v> {
    /// Create a new builder
    ///
    /// `fs`: The `ExecutorFs` that things like `Artifact` can use to generate strings
    pub fn new(fs: &'v ExecutorFs) -> Self {
        Self {
            fs,
            command_line: Vec::new(),
            maybe_macros_state: None,
        }
    }

    pub fn new_with_write_to_file_macros_support(
        fs: &'v ExecutorFs,
        macro_files: &'v IndexSet<Artifact>,
    ) -> Self {
        Self {
            fs,
            command_line: Vec::new(),
            maybe_macros_state: Some((macro_files, 0)),
        }
    }

    /// The `ArtifactFilesystem` to resolve `Artifact`s
    pub fn fs(&self) -> &ExecutorFs {
        self.fs
    }

    /// Obtain the arguments that the command line describes.
    pub fn build(self) -> Vec<String> {
        self.command_line
    }
}

impl CommandLineBuilder for BaseCommandLineBuilder<'_> {
    fn add_arg_string(&mut self, s: String) {
        self.command_line.push(s)
    }
}

impl CommandLineBuilderContext for BaseCommandLineBuilder<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        Ok(CommandLineLocation::from_relative_path(
            path.into(),
            self.fs.path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs {
        self.fs
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        if let Some((files, pos)) = self.maybe_macros_state {
            if pos >= files.len() {
                return Err(anyhow!(
                    CommandLineBuilderErrors::InconsistentNumberOfMacroArtifacts
                ));
            }
            self.maybe_macros_state = Some((files, pos + 1));
            Ok(self
                .resolve_project_path(self.fs.fs().resolve(&files[pos])?)?
                .into_relative())
        } else {
            Err(anyhow!(
                CommandLineBuilderErrors::WriteToFileMacroNotSupported
            ))
        }
    }
}

pub struct AbsCommandLineBuilder<'v>(BaseCommandLineBuilder<'v>);

impl<'v> AbsCommandLineBuilder<'v> {
    pub fn new(executor_fs: &'v ExecutorFs) -> Self {
        Self(BaseCommandLineBuilder::<'v>::new(executor_fs))
    }

    pub fn build(self) -> Vec<String> {
        self.0.build()
    }
}

impl CommandLineBuilderContext for AbsCommandLineBuilder<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        Ok(CommandLineLocation::from_root(
            &self.0.fs().fs().fs().root,
            path.into(),
            self.fs().path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs {
        self.0.fs()
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        let executor_fs = self.0.fs();
        let mut path = executor_fs.fs().fs().root.to_path_buf();
        path.extend(self.0.next_macro_file_path()?.iter());
        RelativePathBuf::from_path(path).map_err(|e| anyhow!(e))
    }
}

impl CommandLineBuilder for AbsCommandLineBuilder<'_> {
    fn add_arg_string(&mut self, s: String) {
        self.0.add_arg_string(s);
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_node::execute::config::PathSeparatorKind;

    use super::*;
    use crate::actions::artifact::ArtifactFs;
    use crate::interpreter::rule_defs::cmd_args::builder::BaseCommandLineBuilder;
    use crate::interpreter::rule_defs::cmd_args::traits::CommandLineArgLike;
    use crate::path::BuckOutPathResolver;
    use crate::path::BuckPathResolver;

    #[test]
    fn adds_args_and_builds() -> anyhow::Result<()> {
        let project_fs =
            ProjectRoot::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap());
        let fs = ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs,
        );
        let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);

        let mut builder = BaseCommandLineBuilder::new(&executor_fs);

        "foo".add_to_command_line(&mut builder)?;
        "bar".to_owned().add_to_command_line(&mut builder)?;

        assert_eq!(
            &["foo".to_owned(), "bar".to_owned()],
            builder.build().as_slice()
        );
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
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a"), unix_path_sep)
                .into_string(),
            "a"
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
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fmt::Debug, path::Path};

use anyhow::{anyhow, Context as _};
use buck2_core::{
    cells::paths::CellPath,
    fs::{paths::RelativePathBuf, project::ProjectRelativePathBuf},
};
use indexmap::IndexSet;
use starlark::values::string::StarlarkStr;
use thiserror::Error;

use crate::{
    actions::artifact::{Artifact, ArtifactFs, OutputArtifact},
    artifact_groups::ArtifactGroup,
    attrs::attr_type::arg::value::ResolvedMacro,
    interpreter::rule_defs::artifact_tagging::ArtifactTag,
};

pub trait CommandLineArtifactVisitor {
    fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>);

    fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>);
}

/// A CommandLineArtifactVisitor that gathers inputs and outputs.
pub struct SimpleCommandLineArtifactVisitor {
    pub inputs: IndexSet<ArtifactGroup>,
    pub outputs: IndexSet<OutputArtifact>,
}

impl SimpleCommandLineArtifactVisitor {
    pub fn new() -> Self {
        Self {
            inputs: IndexSet::new(),
            outputs: IndexSet::new(),
        }
    }
}

impl CommandLineArtifactVisitor for SimpleCommandLineArtifactVisitor {
    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
        self.inputs.insert(input);
    }

    fn visit_output(&mut self, artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {
        self.outputs.insert(artifact);
    }
}

pub trait WriteToFileMacroVisitor {
    fn visit_write_to_file_macro(&mut self, m: &ResolvedMacro) -> anyhow::Result<()>;

    /// Generator produces a 'RelativePathBuf' relative to the directory which owning command will run in.
    fn set_current_relative_to_path(
        &mut self,
        gen: &dyn Fn(&dyn CommandLineBuilderContext) -> anyhow::Result<Option<RelativePathBuf>>,
    ) -> anyhow::Result<()>;
}

/// Implemented by anything that can show up in a command line. This method adds any args and env vars that are needed
///
/// Certain operations on `CommandLineBuilder` can fail, so propagate those upward
pub trait CommandLineArgLike {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()>;

    fn visit_artifacts(&self, _visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        Ok(())
    }

    /// Allows to query if object contains a value resolved from `attr.arg()`
    fn contains_arg_attr(&self) -> bool;

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()>;
}

impl CommandLineArgLike for &str {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        cli.add_arg_string((*self).to_owned());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

impl CommandLineArgLike for StarlarkStr {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        cli.add_arg_string(self.as_str().to_owned());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

impl CommandLineArgLike for String {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        cli.add_arg_string(self.clone());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

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

pub trait CommandLineBuilderContext {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation>;

    fn fs(&self) -> &ArtifactFs;

    /// Resolves the 'Artifact's to a 'CommandLineLocation' relative to the directory this command will run in.
    fn resolve_artifact(&self, artifact: &Artifact) -> anyhow::Result<CommandLineLocation> {
        self.resolve_project_path(self.fs().resolve(artifact)?)
            .with_context(|| format!("Error resolving artifact: {}", artifact))
    }

    fn resolve_cell_path(&self, path: &CellPath) -> anyhow::Result<CommandLineLocation> {
        self.resolve_project_path(self.fs().resolve_cell_path(path)?)
            .with_context(|| format!("Error resolving cell path: {}", path))
    }

    /// Result is 'RelativePathBuf' relative to the directory this command will run in. The path points to the file containing expanded macro.
    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf>;
}

pub trait CommandLineBuilder: CommandLineBuilderContext {
    /// Add the string representation to the list of command line arguments.
    fn add_arg_string(&mut self, s: String);
}

/// Builds up arguments needed to construct a command line
pub struct BaseCommandLineBuilder<'v> {
    fs: &'v ArtifactFs,
    command_line: Vec<String>,
    // First element is list of artifacts, each corresponding to a file with macro contents. Ordering is very important.
    // Second element is a current position in that list.
    maybe_macros_state: Option<(&'v IndexSet<Artifact>, usize)>,
}

impl<'v> BaseCommandLineBuilder<'v> {
    /// Create a new builder
    ///
    /// `fs`: The `ArtifactFs` that things like `Artifact` can use to generate strings
    pub fn new(fs: &'v ArtifactFs) -> Self {
        Self {
            fs,
            command_line: Vec::new(),
            maybe_macros_state: None,
        }
    }

    pub fn new_with_write_to_file_macros_support(
        fs: &'v ArtifactFs,
        macro_files: &'v IndexSet<Artifact>,
    ) -> Self {
        Self {
            fs,
            command_line: Vec::new(),
            maybe_macros_state: Some((macro_files, 0)),
        }
    }

    /// The `ArtifactFilesystem` to resolve `Artifact`s
    pub fn fs(&self) -> &ArtifactFs {
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
        Ok(CommandLineLocation::from_relative_path(path.into()))
    }

    fn fs(&self) -> &ArtifactFs {
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
                .resolve_project_path(self.fs.resolve(&files[pos])?)?
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
    pub fn new(artifact_fs: &'v ArtifactFs) -> Self {
        Self(BaseCommandLineBuilder::<'v>::new(artifact_fs))
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
        Ok(CommandLineLocation {
            root: Some(&self.0.fs().fs().root),
            path: path.into(),
        })
    }

    fn fs(&self) -> &ArtifactFs {
        self.0.fs()
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        let artifact_fs = self.0.fs();
        let mut path = artifact_fs.fs().root.to_path_buf();
        path.extend(self.0.next_macro_file_path()?.iter());
        RelativePathBuf::from_path(path).map_err(|e| anyhow!(e))
    }
}

impl CommandLineBuilder for AbsCommandLineBuilder<'_> {
    fn add_arg_string(&mut self, s: String) {
        self.0.add_arg_string(s);
    }
}

/// CommandLineLocation represents the path to a resolved artifact. If the root is present, the
/// path is udnerstood to be relative to the root. If no root is present, the path is relative to
/// some contextual location that depends on the CommandLineBuilderContext that produced the
/// CommandLineLocation.
#[derive(Debug, Clone)]
pub struct CommandLineLocation<'a> {
    root: Option<&'a Path>,
    path: RelativePathBuf,
}

impl CommandLineLocation<'_> {
    pub fn into_relative(self) -> RelativePathBuf {
        self.path
    }

    pub fn into_string(self) -> String {
        let Self { root, path } = self;

        let res = match root {
            Some(root) => {
                let mut root = root.to_path_buf();
                root.extend(path.iter());
                root.to_string_lossy().into_owned()
            }
            None => path.as_str().to_owned(),
        };
        // In command lines, the empty path is the current directory, so use that instead
        // so we don't have to deal with empty strings being implicit current directory.
        if res == "" { ".".to_owned() } else { res }
    }
}

impl CommandLineLocation<'static> {
    pub fn from_relative_path(path: RelativePathBuf) -> Self {
        Self { root: None, path }
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use buck2_core::{
        cells::{testing::CellResolverExt, CellName, CellResolver},
        fs::{
            paths::AbsPathBuf,
            project::{ProjectFilesystem, ProjectRelativePathBuf},
        },
    };

    use super::*;
    use crate::{
        actions::artifact::ArtifactFs,
        interpreter::rule_defs::cmd_args::builder::{BaseCommandLineBuilder, CommandLineArgLike},
        path::{BuckOutPathResolver, BuckPathResolver},
    };

    #[test]
    fn adds_args_and_builds() -> anyhow::Result<()> {
        let project_fs =
            ProjectFilesystem::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap());
        let fs = ArtifactFs::new(
            BuckPathResolver::new(CellResolver::of_names_and_paths(&[(
                CellName::unchecked_new("cell".into()),
                ProjectRelativePathBuf::unchecked_new("cell_path".into()),
            )])),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new("buck_out".into())),
            project_fs,
        );

        let mut builder = BaseCommandLineBuilder::new(&fs);

        "foo".add_to_command_line(&mut builder)?;
        "bar".to_owned().add_to_command_line(&mut builder)?;

        assert_eq!(
            &["foo".to_owned(), "bar".to_owned()],
            builder.build().as_slice()
        );
        Ok(())
    }

    #[test]
    fn test_empty_command_line() {
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::new()).into_string(),
            "."
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a")).into_string(),
            "a"
        );
        assert_eq!(
            CommandLineLocation::from_relative_path(RelativePathBuf::from("a/b")).into_string(),
            "a/b"
        );
    }
}

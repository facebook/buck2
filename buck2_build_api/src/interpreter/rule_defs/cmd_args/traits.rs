/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt::{Debug, Display},
    path::Path,
    ptr,
};

use anyhow::Context as _;
use buck2_core::{
    cells::paths::CellPath,
    fs::{paths::RelativePathBuf, project::ProjectRelativePathBuf},
};
use indexmap::IndexSet;
use starlark::values::string::StarlarkStr;

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

impl<'a> CommandLineLocation<'a> {
    pub fn from_root(root: &'a Path, path: RelativePathBuf) -> Self {
        Self {
            root: Some(root),
            path,
        }
    }

    pub fn from_relative_path(path: RelativePathBuf) -> Self {
        Self { root: None, path }
    }
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

pub trait FrozenCommandLineArgLike:
    CommandLineArgLike + Send + Sync + Debug + Display + 'static
{
    fn ptr_eq(&self, other: &dyn FrozenCommandLineArgLike) -> bool;
}

impl<T> FrozenCommandLineArgLike for T
where
    T: CommandLineArgLike + Send + Sync + Debug + Display + 'static,
{
    fn ptr_eq(&self, other: &dyn FrozenCommandLineArgLike) -> bool {
        ptr::eq(
            self as *const T,
            other as *const dyn FrozenCommandLineArgLike as *const T,
        )
    }
}

impl PartialEq for dyn FrozenCommandLineArgLike {
    fn eq(&self, other: &Self) -> bool {
        // use simple ptr eq, which is the default behaviour for starlark values
        self.ptr_eq(other)
    }
}

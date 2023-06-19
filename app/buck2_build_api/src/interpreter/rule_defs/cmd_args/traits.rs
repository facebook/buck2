/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt::Debug;

use anyhow::Context as _;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use indexmap::IndexSet;
use starlark::any::ProvidesStaticType;
use starlark::values::string::StarlarkStr;

use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::resolved_macro::ResolvedMacro;

pub trait CommandLineArtifactVisitor {
    fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>);

    fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>);

    /// Those two functions can be used to keep track of recursion when visiting artifacts.
    fn push_frame(&mut self) -> anyhow::Result<()> {
        Ok(())
    }

    fn pop_frame(&mut self) {}
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
        gen: &dyn Fn(&dyn CommandLineContext) -> anyhow::Result<Option<RelativePathBuf>>,
    ) -> anyhow::Result<()>;
}

/// Implemented by anything that can show up in a command line. This method adds any args and env vars that are needed
///
/// Certain operations on `CommandLineBuilder` can fail, so propagate those upward
pub trait CommandLineArgLike {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()>;

    fn visit_artifacts(&self, _visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        Ok(())
    }

    /// Allows to query if object contains a value resolved from `attrs.arg()`
    fn contains_arg_attr(&self) -> bool;

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()>;
}

unsafe impl<'v> ProvidesStaticType<'v> for &'v dyn CommandLineArgLike {
    type StaticType = &'static dyn CommandLineArgLike;
}

impl CommandLineArgLike for &str {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        cli.push_arg((*self).to_owned());
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
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        cli.push_arg(self.as_str().to_owned());
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
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        cli.push_arg(self.clone());
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

impl CommandLineArgLike for StarlarkTargetLabel {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        cli.push_arg(self.to_string());
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
/// some contextual location that depends on the CommandLineContext that produced the
/// CommandLineLocation.
#[derive(Debug, Clone)]
pub struct CommandLineLocation<'a> {
    root: Option<&'a ProjectRoot>,
    path: RelativePathBuf,
    path_separator: PathSeparatorKind,
}

impl CommandLineLocation<'_> {
    pub fn into_relative(self) -> RelativePathBuf {
        self.path
    }

    pub fn into_string(self) -> String {
        let Self {
            root,
            path,
            path_separator,
        } = self;

        let mut root_buf;
        let res = match root {
            Some(root) => {
                root_buf = root.root().to_path_buf();
                root_buf.extend(path.iter());
                root_buf.to_string_lossy()
            }
            None => Cow::Borrowed(path.as_str()),
        };
        // In command lines, the empty path is the current directory, so use that instead
        // so we don't have to deal with empty strings being implicit current directory.
        if res.is_empty() {
            ".".to_owned()
        } else if path_separator == PathSeparatorKind::Windows && res.contains('/') {
            res.replace('/', "\\")
        } else {
            res.into_owned()
        }
    }
}

impl<'a> CommandLineLocation<'a> {
    pub fn from_root(
        root: &'a ProjectRoot,
        path: RelativePathBuf,
        path_separator: PathSeparatorKind,
    ) -> Self {
        Self {
            root: Some(root),
            path,
            path_separator,
        }
    }

    pub fn from_relative_path(path: RelativePathBuf, path_separator: PathSeparatorKind) -> Self {
        Self {
            root: None,
            path,
            path_separator,
        }
    }
}

pub trait CommandLineContext {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation>;

    fn fs(&self) -> &ExecutorFs;

    /// Resolves the 'Artifact's to a 'CommandLineLocation' relative to the directory this command will run in.
    fn resolve_artifact(&self, artifact: &Artifact) -> anyhow::Result<CommandLineLocation> {
        self.resolve_project_path(artifact.resolve_path(self.fs().fs())?)
            .with_context(|| format!("Error resolving artifact: {}", artifact))
    }

    fn resolve_cell_path(&self, path: CellPathRef) -> anyhow::Result<CommandLineLocation> {
        self.resolve_project_path(self.fs().fs().resolve_cell_path(path)?)
            .with_context(|| format!("Error resolving cell path: {}", path))
    }

    /// Result is 'RelativePathBuf' relative to the directory this command will run in. The path points to the file containing expanded macro.
    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf>;
}

/// CommandLineBuilder accumulates elements into some form of list (which might be an actual Vec, a
/// space-separated list, etc.). An API is provided to add individual elements. Lower-level APIs
/// are exposed to allow access to a buffer and control end-of-element.
pub trait CommandLineBuilder {
    /// Add a standalone element to this command line builder. This element
    fn push_arg(&mut self, s: String);
}

impl CommandLineBuilder for Vec<String> {
    fn push_arg(&mut self, s: String) {
        self.push(s)
    }
}

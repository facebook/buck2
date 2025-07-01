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
use std::fmt::Debug;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::DeclaredArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::cell_root::CellRoot;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::project_root::StarlarkProjectRoot;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use indexmap::IndexMap;
use indexmap::IndexSet;
use starlark::any::ProvidesStaticType;
use starlark::typing::Ty;
use starlark::values::string::StarlarkStr;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::resolved_macro::ResolvedMacro;

pub trait CommandLineArtifactVisitor<'v> {
    fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>);

    fn visit_declared_output(&mut self, artifact: OutputArtifact<'v>, tag: Option<&ArtifactTag>);

    fn visit_frozen_output(&mut self, artifact: Artifact, tag: Option<&ArtifactTag>);

    /// Those two functions can be used to keep track of recursion when visiting artifacts.
    fn push_frame(&mut self) -> buck2_error::Result<()> {
        Ok(())
    }

    fn pop_frame(&mut self) {}

    fn visit_declared_artifact(
        &mut self,
        declared_artifact: DeclaredArtifact<'v>,
        tag: Option<&ArtifactTag>,
    ) -> buck2_error::Result<()> {
        self.visit_input(
            ArtifactGroup::Artifact(declared_artifact.ensure_bound()?.into_artifact()),
            tag,
        );
        Ok(())
    }
}

/// A CommandLineArtifactVisitor that gathers inputs and outputs.
pub struct SimpleCommandLineArtifactVisitor<'v> {
    pub inputs: IndexSet<ArtifactGroup>,
    pub declared_outputs: IndexSet<OutputArtifact<'v>>,
    pub frozen_outputs: IndexSet<Artifact>,
}

impl SimpleCommandLineArtifactVisitor<'_> {
    pub fn new() -> Self {
        Self {
            inputs: IndexSet::new(),
            declared_outputs: IndexSet::new(),
            frozen_outputs: IndexSet::new(),
        }
    }
}

impl<'v> CommandLineArtifactVisitor<'v> for SimpleCommandLineArtifactVisitor<'v> {
    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
        self.inputs.insert(input);
    }

    fn visit_declared_output(&mut self, artifact: OutputArtifact<'v>, _tag: Option<&ArtifactTag>) {
        self.declared_outputs.insert(artifact);
    }

    fn visit_frozen_output(&mut self, artifact: Artifact, _tag: Option<&ArtifactTag>) {
        self.frozen_outputs.insert(artifact);
    }
}

pub trait WriteToFileMacroVisitor {
    fn visit_write_to_file_macro(
        &mut self,
        m: &ResolvedMacro,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()>;

    /// Generator produces a 'RelativePathBuf' relative to the directory which owning command will run in.
    fn set_current_relative_to_path(
        &mut self,
        generate: &dyn Fn(&dyn CommandLineContext) -> buck2_error::Result<Option<RelativePathBuf>>,
    ) -> buck2_error::Result<()>;
}

/// Used to provide a mapping from artifacts to the content-based hash that should be used when
/// resolving their path.
///
/// In general, for artifacts that have been built (e.g. inputs to an action), we use a hash based
/// on the content of the artifact. For some other situations where we do not yet have the content
/// hash available, this mapper may choose to resolve artifacts to a different "hash". For example,
/// prior to being built, output artifacts always resolve to a known constant value.  
///
/// Only artifacts that are specified to use content-based paths actually use the value that is
/// returned here. Any other artifacts will continue to resolve their path using the configuration
/// hash.
pub trait ArtifactPathMapper {
    fn get(&self, artifact: &Artifact) -> Option<&ContentBasedPathHash>;
}

impl ArtifactPathMapper for IndexMap<&Artifact, ContentBasedPathHash> {
    fn get(&self, artifact: &Artifact) -> Option<&ContentBasedPathHash> {
        self.get(artifact)
    }
}

pub struct ArtifactPathMapperImpl<'a> {
    pub map: IndexMap<&'a Artifact, ContentBasedPathHash>,
}

impl<'a> From<&'a Vec<(ArtifactGroup, ArtifactGroupValues)>> for ArtifactPathMapperImpl<'a> {
    fn from(ensured_inputs: &'a Vec<(ArtifactGroup, ArtifactGroupValues)>) -> Self {
        Self {
            map: ensured_inputs
                .iter()
                .flat_map(|(_, v)| v.iter())
                .map(|(a, v)| (a, v.content_based_path_hash()))
                .collect(),
        }
    }
}

impl ArtifactPathMapper for ArtifactPathMapperImpl<'_> {
    fn get(&self, artifact: &Artifact) -> Option<&ContentBasedPathHash> {
        self.map.get(artifact)
    }
}

/// Implemented by anything that can show up in a command line. This method adds any args and env vars that are needed
///
/// Certain operations on `CommandLineBuilder` can fail, so propagate those upward
pub trait CommandLineArgLike<'v> {
    /// Call `command_line_arg_like_impl!` to register the type with the interpreter typechecker.
    fn register_me(&self);

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()>;

    fn visit_artifacts(
        &self,
        _visitor: &mut dyn CommandLineArtifactVisitor<'v>,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    /// Allows to query if object contains a value resolved from `attrs.arg()`
    fn contains_arg_attr(&self) -> bool;

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()>;
}

unsafe impl<'v> ProvidesStaticType<'v> for &'v dyn CommandLineArgLike<'v> {
    type StaticType = &'static dyn CommandLineArgLike<'static>;
}

impl<'v> CommandLineArgLike<'v> for &str {
    fn register_me(&self) {
        command_line_arg_like_impl!(Ty::string());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_arg((*self).to_owned());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

impl<'v> CommandLineArgLike<'v> for StarlarkStr {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkStr::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_arg(self.as_str().to_owned());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

impl<'v> CommandLineArgLike<'v> for StarlarkTargetLabel {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkTargetLabel::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_arg(self.to_string());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

impl<'v> CommandLineArgLike<'v> for StarlarkConfiguredProvidersLabel {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkConfiguredProvidersLabel::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        _context: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_arg(self.to_string());
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

impl<'v> CommandLineArgLike<'v> for CellRoot {
    fn register_me(&self) {
        command_line_arg_like_impl!(CellRoot::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_location(ctx.resolve_cell_path(self.cell_path())?);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

impl<'v> CommandLineArgLike<'v> for StarlarkProjectRoot {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkProjectRoot::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        cli.push_location(ctx.resolve_project_path(ProjectRelativePath::empty().to_owned())?);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
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
            None => {
                let path = path.as_str();

                if path.contains('/') {
                    Cow::Borrowed(path)
                } else if path.is_empty() {
                    // In command lines, the empty path is the current directory, so use that instead
                    // so we don't have to deal with empty strings being implicit current directory.
                    Cow::Borrowed(".")
                } else {
                    // If the path isn't empty, but doesn't have any path separators, add `./` This
                    // ensures that if we have an executable relative to the repo root, we can
                    // execute it (since `execvp`'s behavior is dependent on the presence of a `/`
                    // in the path).
                    let mut res = String::with_capacity(path.len() + 2);
                    res.push_str("./");
                    res.push_str(path);
                    Cow::Owned(res)
                }
            }
        };

        if path_separator == PathSeparatorKind::Windows {
            res.replace('/', "\\")
        } else {
            res.into_owned()
        }
    }
}

impl<'a> CommandLineLocation<'a> {
    pub fn from_root(
        root: &'a ProjectRoot,
        path: ProjectRelativePathBuf,
        path_separator: PathSeparatorKind,
    ) -> Self {
        Self {
            root: Some(root),
            path: path.into(),
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
    ) -> buck2_error::Result<CommandLineLocation>;

    fn fs(&self) -> &ExecutorFs;

    /// Resolves the 'Artifact's to a 'CommandLineLocation' relative to the directory this command will run in.
    fn resolve_artifact(
        &self,
        artifact: &Artifact,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<CommandLineLocation> {
        self.resolve_project_path(
            artifact.resolve_path(self.fs().fs(), artifact_path_mapping.get(artifact))?,
        )
        .with_buck_error_context(|| format!("Error resolving artifact: {}", artifact))
    }

    /// Resolves the OutputArtifact to a 'CommandLineLocation' relative to the directory this command will run in.
    /// For content-based paths, this will resolve to a "constant" path, i.e. one that is known in advance and is
    /// not actually dependent upon the artifact's content.
    fn resolve_output_artifact(
        &self,
        artifact: &Artifact,
    ) -> buck2_error::Result<CommandLineLocation> {
        self.resolve_project_path(artifact.get_path().resolve(
            self.fs().fs(),
            Some(&ContentBasedPathHash::for_output_artifact()),
        )?)
        .with_buck_error_context(|| format!("Error resolving output artifact: {}", artifact))
    }

    fn resolve_cell_path(&self, path: CellPathRef) -> buck2_error::Result<CommandLineLocation> {
        self.resolve_project_path(self.fs().fs().resolve_cell_path(path)?)
            .with_buck_error_context(|| format!("Error resolving cell path: {}", path))
    }

    /// Result is 'RelativePathBuf' relative to the directory this command will run in. The path points to the file containing expanded macro.
    fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf>;
}

/// CommandLineBuilder accumulates elements into some form of list (which might be an actual Vec, a
/// space-separated list, etc.). An API is provided to add individual elements. Lower-level APIs
/// are exposed to allow access to a buffer and control end-of-element.
pub trait CommandLineBuilder {
    /// Add a standalone element to this command line builder. This element
    fn push_arg(&mut self, s: String);

    fn push_location(&mut self, location: CommandLineLocation<'_>) {
        self.push_arg(location.into_string());
    }
}

impl CommandLineBuilder for Vec<String> {
    fn push_arg(&mut self, s: String) {
        self.push(s)
    }
}

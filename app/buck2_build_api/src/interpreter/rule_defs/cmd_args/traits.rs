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
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_fs::paths::RelativePath;
use buck2_fs::paths::RelativePathBuf;
use buck2_hash::BuckHashMap;
use buck2_hash::BuckIndexSet;
use buck2_interpreter::types::cell_root::CellRoot;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::project_root::StarlarkProjectRoot;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use starlark::any::ProvidesStaticType;
use starlark::typing::Ty;
use starlark::values::string::StarlarkStr;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::format::CommandLineFormatter;
use crate::interpreter::rule_defs::resolved_macro::ResolvedMacro;

pub trait CommandLineArtifactVisitor<'v> {
    fn visit_input(&mut self, input: ArtifactGroup, tags: Vec<&ArtifactTag>);

    fn visit_declared_output(&mut self, artifact: OutputArtifact<'v>, tags: Vec<&ArtifactTag>);

    fn visit_frozen_output(&mut self, artifact: Artifact, tags: Vec<&ArtifactTag>);

    /// Those two functions can be used to keep track of recursion when visiting artifacts.
    fn push_frame(&mut self) -> buck2_error::Result<()> {
        Ok(())
    }

    fn pop_frame(&mut self) {}

    fn visit_declared_artifact(
        &mut self,
        declared_artifact: DeclaredArtifact<'v>,
        tags: Vec<&ArtifactTag>,
    ) -> buck2_error::Result<()> {
        self.visit_input(
            ArtifactGroup::Artifact(declared_artifact.ensure_bound()?.into_artifact()),
            tags,
        );
        Ok(())
    }

    fn skip_hidden(&self) -> bool {
        false
    }
}

/// A CommandLineArtifactVisitor that gathers inputs and outputs.
pub struct SimpleCommandLineArtifactVisitor<'v> {
    pub inputs: BuckIndexSet<ArtifactGroup>,
    pub declared_outputs: BuckIndexSet<OutputArtifact<'v>>,
    pub frozen_outputs: BuckIndexSet<Artifact>,
}

impl SimpleCommandLineArtifactVisitor<'_> {
    pub fn new() -> Self {
        Self {
            inputs: BuckIndexSet::default(),
            declared_outputs: BuckIndexSet::default(),
            frozen_outputs: BuckIndexSet::default(),
        }
    }

    pub fn inputs(&self) -> impl Iterator<Item = &ArtifactGroup> {
        self.inputs.iter()
    }
}

impl<'v> CommandLineArtifactVisitor<'v> for SimpleCommandLineArtifactVisitor<'v> {
    fn visit_input(&mut self, input: ArtifactGroup, _tags: Vec<&ArtifactTag>) {
        self.inputs.insert(input);
    }

    fn visit_declared_output(&mut self, artifact: OutputArtifact<'v>, _tags: Vec<&ArtifactTag>) {
        self.declared_outputs.insert(artifact);
    }

    fn visit_frozen_output(&mut self, artifact: Artifact, _tags: Vec<&ArtifactTag>) {
        self.frozen_outputs.insert(artifact);
    }
}

pub trait WriteToFileMacroVisitor {
    fn visit_write_to_file_macro(
        &mut self,
        m: &ResolvedMacro,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()>;

    fn set_current_relative_to_path(&mut self, p: ProjectRelativePathBuf);

    fn fs(&self) -> Option<&ExecutorFs<'_>>;
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

impl ArtifactPathMapper for BuckHashMap<&Artifact, ContentBasedPathHash> {
    fn get(&self, artifact: &Artifact) -> Option<&ContentBasedPathHash> {
        self.get(artifact)
    }
}

pub struct ArtifactPathMapperImpl<'a> {
    pub map: BuckHashMap<&'a Artifact, ContentBasedPathHash>,
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
        fmt: &mut CommandLineFormatter<'v, '_>,
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_str(self);
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_str(self.as_str());
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_string(self.to_string());
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_string(self.to_string());
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_cell_path(self.cell_path())?;
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
        fmt: &mut CommandLineFormatter<'v, '_>,
    ) -> buck2_error::Result<()> {
        fmt.push_project_path(ProjectRelativePath::empty().to_owned())?;
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

        match root {
            Some(root) => Self::format_absolute_inner(&path, path_separator, root),
            None => Self::format_inner(&path, path_separator),
        }
        .into_owned()
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

    /// Returns the path rendered in a way that is appropriate for use in command lines.
    pub fn format<'p>(path: &'p RelativePath, fs: &ExecutorFs) -> Cow<'p, str> {
        Self::format_inner(path, fs.path_separator())
    }

    fn format_inner<'p>(path: &'p RelativePath, sep: PathSeparatorKind) -> Cow<'p, str> {
        let parts = path.components().count();
        if parts == 0 {
            // In command lines, the empty path is the current directory, so use that instead
            // so we don't have to deal with empty strings being implicit current directory.
            Cow::Borrowed(".")
        } else if parts == 1 {
            // If the path isn't empty, but doesn't have any path separators, add `./` This
            // ensures that if we have an executable relative to the repo root, we can
            // execute it (since `execvp`'s behavior is dependent on the presence of a `/`
            // in the path).
            let mut res = String::with_capacity(path.as_str().len() + 2);
            res.push('.');
            res.push(match sep {
                PathSeparatorKind::Unix => '/',
                PathSeparatorKind::Windows => '\\',
            });
            res.push_str(path.as_str());
            Cow::Owned(res)
        } else {
            match sep {
                PathSeparatorKind::Unix => Cow::Borrowed(path.as_str()),
                PathSeparatorKind::Windows => Cow::Owned(path.as_str().replace('/', "\\")),
            }
        }
    }

    /// Similar to `format` but produces an absolute path
    pub fn format_absolute<'p>(path: &'p ProjectRelativePath, fs: &ExecutorFs) -> Cow<'p, str> {
        Self::format_absolute_inner(path.as_ref(), fs.path_separator(), fs.fs().fs())
    }

    fn format_absolute_inner<'p>(
        // FIXME(JakobDegen): This is completely wrong if it's not a `ProjectRelativePath`
        path: &'p RelativePath,
        sep: PathSeparatorKind,
        root: &ProjectRoot,
    ) -> Cow<'p, str> {
        let mut p = root.root().to_path_buf();
        p.extend(path.iter());
        // FIXME(JakobDegen): Seriously?
        let s = p.to_string_lossy();
        // FIXME(JakobDegen): This is pretty confused. `PathBuf` and our absolute path types
        // use the host's path separators. Fix, and also write a test, and also fix the path
        // types to make these sorts of bugs not possible
        if sep == PathSeparatorKind::Windows {
            Cow::Owned(s.as_ref().replace('/', "\\"))
        } else {
            Cow::Owned(s.into_owned())
        }
    }
}

/// CommandLineBuilder accumulates elements into some form of list (which might be an actual Vec, a
/// space-separated list, etc.). An API is provided to add individual elements. Lower-level APIs
/// are exposed to allow access to a buffer and control end-of-element.
pub trait CommandLineBuilder {
    /// Add a standalone element to this command line builder. This element
    fn push_arg(&mut self, s: Cow<'_, str>);
}

impl CommandLineBuilder for Vec<String> {
    fn push_arg(&mut self, s: Cow<'_, str>) {
        self.push(s.into_owned())
    }
}

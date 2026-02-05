/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsInputArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_execute::execute::request::OutputType;
use dupe::OptionDupedExt;
use indexmap::indexset;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::ValueTyped;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::none::NoneOr;

use crate::actions::impls::copy::CopyMode;
use crate::actions::impls::copy::UnregisteredCopyAction;
use crate::actions::impls::symlinked_dir::UnregisteredSymlinkedDirAction;

fn create_dir_tree<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    this: &AnalysisActions<'v>,
    output: OutputArtifactArg<'v>,
    srcs: UnpackDictEntries<&'v str, ValueAsInputArtifactLike<'v>>,
    copy: CopyMode,
    has_content_based_path: Option<bool>,
) -> buck2_error::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
    let action = UnregisteredSymlinkedDirAction::new(copy, srcs)?;
    let unioned_associated_artifacts = action.unioned_associated_artifacts();

    let mut this = this.state()?;
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, output, OutputType::Directory, has_content_based_path)?;
    this.register_action(indexset![output_artifact], action, None, None)?;

    Ok(declaration.into_declared_artifact(unioned_associated_artifacts))
}

fn copy_file_impl<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    this: &AnalysisActions<'v>,
    dest: OutputArtifactArg<'v>,
    src: ValueAsInputArtifactLike<'v>,
    copy: CopyMode,
    output_type: OutputType,
    has_content_based_path: Option<bool>,
) -> buck2_error::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
    let src = src.0;

    let artifact = src.get_artifact_group()?;
    let associated_artifacts = src.get_associated_artifacts();
    let mut this = this.state()?;
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, dest, output_type, has_content_based_path)?;

    this.register_action(
        indexset![output_artifact],
        UnregisteredCopyAction::new(artifact, copy),
        None,
        None,
    )?;

    Ok(declaration.into_declared_artifact(
        associated_artifacts
            .duped()
            .unwrap_or_else(AssociatedArtifacts::new),
    ))
}

#[starlark_module]
pub(crate) fn analysis_actions_methods_copy(methods: &mut MethodsBuilder) {
    /// Copies the source `artifact` to the destination (which can be a string representing a
    /// filename or an output `artifact`) and returns the output `artifact`. The copy works for
    /// files or directories.
    fn copy_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsInputArtifactLike<'v>,
        #[starlark(require = named, default = NoneOr::None)] has_content_based_path: NoneOr<bool>,
        #[starlark(require = named, default = NoneOr::None)] executable_bit_override: NoneOr<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word
        // `file` in it
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Copy {
                executable_bit_override: executable_bit_override.into_option(),
            },
            OutputType::FileOrDirectory,
            has_content_based_path.into_option(),
        )?)
    }

    /// Creates a symlink to the source `artifact` at the destination (which can be a string
    /// representing a filename or an output `artifact`) and returns the output `artifact`. The
    /// symlink works for files or directories.
    fn symlink_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsInputArtifactLike<'v>,
        #[starlark(require = named, default = NoneOr::None)] has_content_based_path: NoneOr<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word
        // `file` in it
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Symlink,
            OutputType::FileOrDirectory,
            has_content_based_path.into_option(),
        )?)
    }

    /// Make a copy of a directory.
    fn copy_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsInputArtifactLike<'v>,
        #[starlark(require = named, default = NoneOr::None)] has_content_based_path: NoneOr<bool>,
        #[starlark(require = named, default = NoneOr::None)] executable_bit_override: NoneOr<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Copy {
                executable_bit_override: executable_bit_override.into_option(),
            },
            OutputType::Directory,
            has_content_based_path.into_option(),
        )?)
    }

    /// Returns an `artifact` that is a directory containing symlinks.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to bound `artifact`, which will be laid out in the directory.
    fn symlinked_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: UnpackDictEntries<&'v str, ValueAsInputArtifactLike<'v>>,
        #[starlark(require = named, default = NoneOr::None)] has_content_based_path: NoneOr<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
        Ok(create_dir_tree(
            eval,
            this,
            output,
            srcs,
            CopyMode::Symlink,
            has_content_based_path.into_option(),
        )?)
    }

    /// Returns an `artifact` which is a directory containing copied files.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to the bound `artifact`, which will be laid out in the directory.
    fn copied_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: UnpackDictEntries<&'v str, ValueAsInputArtifactLike<'v>>,
        #[starlark(require = named, default = NoneOr::None)] has_content_based_path: NoneOr<bool>,
        #[starlark(require = named, default = NoneOr::None)] executable_bit_override: NoneOr<bool>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact<'v>>> {
        Ok(create_dir_tree(
            eval,
            this,
            output,
            srcs,
            CopyMode::Copy {
                executable_bit_override: executable_bit_override.into_option(),
            },
            has_content_based_path.into_option(),
        )?)
    }
}

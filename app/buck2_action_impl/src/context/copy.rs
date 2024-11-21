/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_execute::execute::request::OutputType;
use dupe::OptionDupedExt;
use indexmap::indexset;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::ValueTyped;

use crate::actions::impls::copy::CopyMode;
use crate::actions::impls::copy::UnregisteredCopyAction;
use crate::actions::impls::symlinked_dir::UnregisteredSymlinkedDirAction;

fn create_dir_tree<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    this: &AnalysisActions<'v>,
    output: OutputArtifactArg<'v>,
    srcs: UnpackDictEntries<&'v str, ValueAsArtifactLike<'v>>,
    copy: bool,
) -> buck2_error::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
    // validate that the moves are valid, and move them into inputs
    let action = UnregisteredSymlinkedDirAction::new(copy, srcs)?;
    let inputs = action.inputs();
    let unioned_associated_artifacts = action.unioned_associated_artifacts();

    let mut this = this.state()?;
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, output, OutputType::Directory)?;
    this.register_action(inputs, indexset![output_artifact], action, None, None)?;

    Ok(declaration.into_declared_artifact(unioned_associated_artifacts))
}

fn copy_file_impl<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    this: &AnalysisActions<'v>,
    dest: OutputArtifactArg<'v>,
    src: ValueAsArtifactLike<'v>,
    copy: CopyMode,
    output_type: OutputType,
) -> buck2_error::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
    let src = src.0;

    let artifact = src.get_artifact_group()?;
    let associated_artifacts = src.get_associated_artifacts();
    let mut this = this.state()?;
    let (declaration, output_artifact) = this.get_or_declare_output(eval, dest, output_type)?;

    this.register_action(
        indexset![artifact],
        indexset![output_artifact],
        UnregisteredCopyAction::new(copy),
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
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word
        // `file` in it
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Copy,
            OutputType::FileOrDirectory,
        )?)
    }

    /// Creates a symlink to the source `artifact` at the destination (which can be a string
    /// representing a filename or an output `artifact`) and returns the output `artifact`. The
    /// symlink works for files or directories.
    fn symlink_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word
        // `file` in it
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Symlink,
            OutputType::FileOrDirectory,
        )?)
    }

    /// Make a copy of a directory.
    fn copy_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        Ok(copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Copy,
            OutputType::Directory,
        )?)
    }

    /// Returns an `artifact` that is a directory containing symlinks.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to bound `artifact`, which will be laid out in the directory.
    fn symlinked_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: UnpackDictEntries<&'v str, ValueAsArtifactLike<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        Ok(create_dir_tree(eval, this, output, srcs, false)?)
    }

    /// Returns an `artifact` which is a directory containing copied files.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to the bound `artifact`, which will be laid out in the directory.
    fn copied_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: UnpackDictEntries<&'v str, ValueAsArtifactLike<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        Ok(create_dir_tree(eval, this, output, srcs, true)?)
    }
}

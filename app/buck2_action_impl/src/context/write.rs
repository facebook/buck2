/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_build_api::actions::impls::json::JsonUnpack;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::value::CommandLineArg;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCommandLineValueUnpack;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use buck2_execute::execute::request::OutputType;
use dupe::Dupe;
use either::Either;
use indexmap::indexset;
use indexmap::IndexSet;
use relative_path::RelativePathBuf;
use sha1::Digest;
use sha1::Sha1;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::UnpackValue;
use starlark::values::ValueOf;
use starlark::values::ValueTyped;
use starlark_map::small_set::SmallSet;

use crate::actions::impls::write::UnregisteredWriteAction;
use crate::actions::impls::write_json::UnregisteredWriteJsonAction;
use crate::actions::impls::write_macros::UnregisteredWriteMacrosToFileAction;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum WriteActionError {
    #[error(
        "Argument type attributes detected in a content to be written into a file, but support for arguments was not turned on. Use `allow_args` parameter to turn on the support for arguments."
    )]
    ArgAttrsDetectedButNotAllowed,
}

#[derive(UnpackValue, StarlarkTypeRepr)]
enum WriteContentArg<'v> {
    CommandLineArg(CommandLineArg<'v>),
    StarlarkCommandLineValueUnpack(StarlarkCommandLineValueUnpack<'v>),
}

#[starlark_module]
pub(crate) fn analysis_actions_methods_write(methods: &mut MethodsBuilder) {
    /// Returns an `artifact` whose contents are `content` written as a JSON value.
    ///
    /// * `output`: can be a string, or an existing artifact created with `declare_output`
    /// * `content`:  must be composed of the basic json types (boolean, number, string, list/tuple,
    ///   dictionary) plus artifacts and command lines
    ///     * An artifact will be written as a string containing the path
    ///     * A command line will be written as a list of strings, unless `joined=True` is set, in
    ///       which case it will be a string
    /// * If you pass `with_inputs = True`, you'll get back a `cmd_args` that expands to the JSON
    ///   file but carries all the underlying inputs as dependencies (so you don't have to use, for
    ///   example, `hidden` for them to be added to an action that already receives the JSON file)
    /// * `pretty` (optional): write formatted JSON (defaults to `False`)
    /// * `absolute` (optional): if set, this action will produce absolute paths in its output when
    ///   rendering artifact paths. You generally shouldn't use this if you plan to use this action
    ///   as the input for anything else, as this would effectively result in losing all shared
    ///   caching. (defaults to `False`)
    fn write_json<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] content: ValueOf<'v, JsonUnpack<'v>>,
        #[starlark(require = named, default = false)] with_inputs: bool,
        #[starlark(require = named, default = false)] pretty: bool,
        #[starlark(require = named, default = false)] absolute: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<impl AllocValue<'v>> {
        let mut this = this.state()?;
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, OutputType::File)?;

        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredWriteJsonAction::new(pretty, absolute),
            Some(content.value),
            None,
        )?;

        let value = declaration.into_declared_artifact(AssociatedArtifacts::new());
        // TODO(cjhopman): The with_inputs thing can go away once we have artifact dependencies (we'll still
        // need the UnregisteredWriteJsonAction::cli() to represent the dependency though).
        if with_inputs {
            // TODO(nga): we use `AllocValue`, so this function return type for this branch
            //   is `write_json_cli_args`. We want just `cmd_args`,
            //   because users don't care about precise type.
            //   Do it when we migrate to new types not based on strings.
            let cli = UnregisteredWriteJsonAction::cli(value.to_value(), content.value)?;
            Ok(Either::Right(cli))
        } else {
            Ok(Either::Left(value))
        }
    }

    /// Returns an `artifact` whose contents are `content`
    ///
    /// * `is_executable` (optional): indicates whether the resulting file should be marked with
    ///   executable permissions
    /// * `allow_args` (optional): must be set to `True` if you want to write parameter arguments to
    ///   the file (in particular, macros that write to file)
    ///     * If it is true, the result will be a pair of the `artifact` containing content and a
    ///       list of artifact values that were written by macros, which should be used in hidden
    ///       fields or similar
    /// * `with_inputs` (optional): if set, add artifacts in `content` as associated artifacts of the return `artifact`.
    /// * `absolute` (optional): if set, this action will produce absolute paths in its output when
    ///   rendering artifact paths. You generally shouldn't use this if you plan to use this action
    ///   as the input for anything else, as this would effectively result in losing all shared
    ///   caching.
    ///
    /// The content is often a string, but can be any `ArgLike` value. This is occasionally useful
    /// for generating scripts to run as a part of another action. `cmd_args` in the content are
    /// newline separated unless another delimiter is explicitly specified.
    fn write<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] content: WriteContentArg<'v>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] allow_args: bool,
        // If set, add artifacts in content as associated artifacts of the output. This will only work for bound artifacts.
        #[starlark(require = named, default = false)] with_inputs: bool,
        #[starlark(require = named, default = false)] absolute: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<
        Either<
            ValueTyped<'v, StarlarkDeclaredArtifact>,
            (
                ValueTyped<'v, StarlarkDeclaredArtifact>,
                Vec<StarlarkDeclaredArtifact>,
            ),
        >,
    > {
        fn count_write_to_file_macros(
            args_allowed: bool,
            cli: &dyn CommandLineArgLike,
        ) -> buck2_error::Result<u32> {
            if !args_allowed && cli.contains_arg_attr() {
                return Err(WriteActionError::ArgAttrsDetectedButNotAllowed.into());
            }

            struct WriteToFileMacrosCounter {
                count: u32,
            }

            impl WriteToFileMacroVisitor for WriteToFileMacrosCounter {
                fn visit_write_to_file_macro(
                    &mut self,
                    _m: &ResolvedMacro,
                ) -> buck2_error::Result<()> {
                    self.count += 1;
                    Ok(())
                }

                fn set_current_relative_to_path(
                    &mut self,
                    _gen: &dyn Fn(
                        &dyn CommandLineContext,
                    )
                        -> buck2_error::Result<Option<RelativePathBuf>>,
                ) -> buck2_error::Result<()> {
                    Ok(())
                }
            }

            let mut counter = WriteToFileMacrosCounter { count: 0 };
            cli.visit_write_to_file_macros(&mut counter)?;
            Ok(counter.count)
        }

        fn get_cli_inputs(
            with_inputs: bool,
            cli: &dyn CommandLineArgLike,
        ) -> buck2_error::Result<SmallSet<ArtifactGroup>> {
            if !with_inputs {
                return Ok(Default::default());
            }

            #[derive(Default)]
            struct CommandLineInputVisitor {
                inputs: SmallSet<ArtifactGroup>,
            }
            impl CommandLineArtifactVisitor for CommandLineInputVisitor {
                fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
                    self.inputs.insert(input);
                }

                fn visit_output(&mut self, _artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {}
            }

            let mut visitor = CommandLineInputVisitor::default();
            cli.visit_artifacts(&mut visitor)?;
            Ok(visitor.inputs)
        }

        let mut this = this.state()?;
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, OutputType::File)?;

        let (content_cli, written_macro_count, mut associated_artifacts) = match content {
            WriteContentArg::CommandLineArg(content) => {
                let content_arg = content.as_command_line_arg();
                let count = count_write_to_file_macros(allow_args, content_arg)?;
                let cli_inputs = get_cli_inputs(with_inputs, content_arg)?;
                (content, count, cli_inputs)
            }
            WriteContentArg::StarlarkCommandLineValueUnpack(content) => {
                let cli = StarlarkCmdArgs::try_from_value_typed(content)?;
                let count = count_write_to_file_macros(allow_args, &cli)?;
                let cli_inputs = get_cli_inputs(with_inputs, &cli)?;
                (
                    CommandLineArg::from_cmd_args(eval.heap().alloc_typed(cli)),
                    count,
                    cli_inputs,
                )
            }
        };

        let written_macro_files = if written_macro_count > 0 {
            let macro_directory_path = {
                // There might be several write actions at once, use write action output hash to deterministically avoid collisions for .macro files.
                let digest = output_artifact
                    .get_path()
                    .with_full_path(|path| Sha1::digest(path.as_str().as_bytes()));
                let sha = hex::encode(digest);
                format!("__macros/{}", sha)
            };

            let mut written_macro_files = indexset![];
            for i in 0..written_macro_count {
                let macro_file = this.declare_output(
                    None,
                    &format!("{}/{}.macro", &macro_directory_path, i),
                    OutputType::File,
                    eval.call_stack_top_location(),
                )?;
                written_macro_files.insert(macro_file);
            }

            let state = &mut *this;
            let action = UnregisteredWriteMacrosToFileAction::new(
                output_artifact
                    .get_path()
                    .with_short_path(|p| p.to_string()),
            );
            state.register_action(
                indexset![],
                written_macro_files.iter().map(|a| a.as_output()).collect(),
                action,
                Some(content_cli.to_value()),
                None,
            )?;

            written_macro_files
        } else {
            indexset![]
        };

        let action = {
            let maybe_macro_files = if allow_args {
                let mut macro_files = indexset![];
                for a in &written_macro_files {
                    macro_files.insert(a.dupe().ensure_bound()?.into_artifact());
                }
                Some(macro_files)
            } else {
                None
            };
            UnregisteredWriteAction {
                is_executable,
                macro_files: maybe_macro_files,
                absolute,
            }
        };
        this.register_action(
            indexset![],
            indexset![output_artifact],
            action,
            Some(content_cli.to_value()),
            None,
        )?;

        if allow_args {
            for a in &written_macro_files {
                associated_artifacts.insert(ArtifactGroup::Artifact(
                    a.dupe().ensure_bound()?.into_artifact(),
                ));
            }
        }

        let value =
            declaration.into_declared_artifact(AssociatedArtifacts::from(associated_artifacts));
        if allow_args {
            let macro_files: Vec<StarlarkDeclaredArtifact> = written_macro_files
                .into_iter()
                .map(|a| StarlarkDeclaredArtifact::new(None, a, AssociatedArtifacts::new()))
                .collect();
            Ok(Either::Right((value, macro_files)))
        } else {
            // Prefer simpler API when there is no possibility for write-to-file macros to be present in a content
            Ok(Either::Left(value))
        }
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_build_api::actions::impls::json::validate_json;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkOutputOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkPromiseArtifact;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::context::ANALYSIS_ACTIONS_METHODS_ACTIONS;
use buck2_build_api::interpreter::rule_defs::digest_config::StarlarkDigestConfig;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_run_info::WorkerRunInfo;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSetDefinition;
use buck2_common::cas_digest::CasDigest;
use buck2_core::category::Category;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_execute::execute::request::OutputType;
use buck2_execute::materialize::http::Checksum;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use chrono::TimeZone;
use chrono::Utc;
use dupe::Dupe;
use dupe::OptionDupedExt;
use either::Either;
use host_sharing::WeightClass;
use host_sharing::WeightPercentage;
use indexmap::indexset;
use indexmap::IndexSet;
use relative_path::RelativePathBuf;
use sha1::Digest;
use sha1::Sha1;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::dict::DictOf;
use starlark::values::function::FUNCTION_TYPE;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::typing::StarlarkIter;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::Value;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark_map::small_map;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::actions::impls::cas_artifact::ArtifactKind;
use crate::actions::impls::cas_artifact::DirectoryKind;
use crate::actions::impls::cas_artifact::UnregisteredCasArtifactAction;
use crate::actions::impls::copy::CopyMode;
use crate::actions::impls::copy::UnregisteredCopyAction;
use crate::actions::impls::download_file::UnregisteredDownloadFileAction;
use crate::actions::impls::run::dep_files::RunActionDepFiles;
use crate::actions::impls::run::new_executor_preference;
use crate::actions::impls::run::MetadataParameter;
use crate::actions::impls::run::StarlarkRunActionValues;
use crate::actions::impls::run::UnregisteredRunAction;
use crate::actions::impls::symlinked_dir::UnregisteredSymlinkedDirAction;
use crate::actions::impls::write::UnregisteredWriteAction;
use crate::actions::impls::write_json::UnregisteredWriteJsonAction;
use crate::actions::impls::write_macros::UnregisteredWriteMacrosToFileAction;

#[derive(thiserror::Error, Debug)]
enum DownloadFileError {
    #[error("Must pass in at least one checksum (e.g. `sha1 = ...`)")]
    MissingChecksum,
}

#[derive(thiserror::Error, Debug)]
enum DynamicOutputError {
    #[error("Output list may not be empty")]
    EmptyOutput,
    #[error("List of dynamic inputs may not be empty")]
    EmptyDynamic,
    #[error("Final argument must be a function, got `{0}`")]
    NotAFunction(String),
}

#[derive(thiserror::Error, Debug)]
enum CasArtifactError {
    #[error("Not a valid RE digest: `{0}`")]
    InvalidDigest(String),
    #[error("is_tree and is_directory are mutually exclusive")]
    TreeAndDirectory,
}

#[derive(Debug, thiserror::Error)]
enum RunActionError {
    #[error("expected at least one output artifact, did not get any")]
    NoOutputsSpecified,
    #[error("`weight` must be a positive integer, got `{0}`")]
    InvalidWeight(i32),
    #[error("`weight` and `weight_percentage` cannot both be passed")]
    DuplicateWeightsSpecified,
    #[error("`dep_files` value with key `{}` has an invalid count of associated outputs. Expected 1, got {}.", .key, .count)]
    InvalidDepFileOutputs { key: String, count: usize },
    #[error("`dep_files` with keys `{}` and {} are using the same tag", .first, .second)]
    ConflictingDepFiles { first: String, second: String },
    #[error(
        "missing `metadata_path` parameter which is required when `metadata_env_var` parameter is present"
    )]
    MetadataPathMissing,
    #[error(
        "missing `metadata_env_var` parameter which is required when `metadata_path` parameter is present"
    )]
    MetadataEnvVarMissing,
    #[error(
        "Recursion limit exceeded when visiting artifacts: do you have a cycle in your inputs or outputs?"
    )]
    ArtifactVisitRecursionLimitExceeded,
}

#[derive(Debug, thiserror::Error)]
enum WriteActionError {
    #[error(
        "Argument type attributes detected in a content to be written into a file, but support for arguments was not turned on. Use `allow_args` parameter to turn on the support for arguments."
    )]
    ArgAttrsDetectedButNotAllowed,
}

fn create_dir_tree<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: &AnalysisActions<'v>,
    output: OutputArtifactArg<'v>,
    srcs: DictOf<'v, &'v str, ValueAsArtifactLike<'v>>,
    copy: bool,
) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
    // validate that the moves are valid, and move them into inputs
    let action = UnregisteredSymlinkedDirAction::new(copy, srcs)?;
    let inputs = action.inputs();
    let unioned_associated_artifacts = action.unioned_associated_artifacts();

    let mut this = this.state();
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, output, OutputType::Directory)?;
    this.register_action(inputs, indexset![output_artifact], action, None)?;

    Ok(declaration.into_declared_artifact(unioned_associated_artifacts))
}

fn copy_file_impl<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: &AnalysisActions<'v>,
    dest: OutputArtifactArg<'v>,
    src: ValueAsArtifactLike<'v>,
    copy: CopyMode,
    output_type: OutputType,
) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
    let src = src.0;

    let artifact = src.get_bound_artifact()?;
    let associated_artifacts = src.get_associated_artifacts();
    let mut this = this.state();
    let (declaration, output_artifact) = this.get_or_declare_output(eval, dest, output_type)?;

    this.register_action(
        indexset![ArtifactGroup::Artifact(artifact)],
        indexset![output_artifact],
        UnregisteredCopyAction::new(copy),
        None,
    )?;

    Ok(declaration.into_declared_artifact(
        associated_artifacts
            .duped()
            .unwrap_or_else(AssociatedArtifacts::new),
    ))
}

/// Functions to allow users to interact with the Actions registry.
/// Accessed via `ctx.actions.<function>`.
///
/// Actions take inputs and produce outputs, mostly using the `artifact` type.
/// Most output filenames can either be artifacts created with `declare_output` or strings that are implicitly converted to output artifacts.
#[starlark_module]
fn analysis_actions_methods_actions(builder: &mut MethodsBuilder) {
    /// Returns an unbound `artifact` which must be bound before analysis terminates. The usual way of binding an artifact is
    /// with `ctx.actions.run`.
    ///
    /// To construct an artifact with the name `foo`, call `ctx.actions.declare_output("foo")`. Artifacts from a single target may not
    /// have the same name, so if you then want a second artifact also named `foo` you need to supply a prefix, e.g.
    /// `ctx.actions.declare_output("directory", "foo")`. The artifact will still report it has name `foo`, but will be located at
    /// `directory/foo`.
    ///
    /// The `dir` argument should be set to `True` if the binding will be a directory.
    fn declare_output<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] prefix: &str,
        #[starlark(require = pos)] filename: Option<&str>,
        #[starlark(require = named, default = false)] dir: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        // We take either one or two positional arguments, namely (filename) or (prefix, filename).
        // The prefix argument is optional, but first, so we pretend the filename is optional
        // and fix them up here.
        let (prefix, filename) = match filename {
            None => (None, prefix),
            Some(filename) => (Some(prefix), filename),
        };

        let output_type = if dir {
            OutputType::Directory
        } else {
            OutputType::FileOrDirectory
        };
        let artifact = this.state().declare_output(
            prefix,
            filename,
            output_type,
            eval.call_stack_top_location(),
        )?;

        Ok(StarlarkDeclaredArtifact::new(
            eval.call_stack_top_location(),
            artifact,
            AssociatedArtifacts::new(),
        ))
    }

    /// Returns an `artifact` whose contents are content written as a JSON value.
    ///
    /// * `filename`: can be a string, or an existing artifact created with `declare_output`
    /// * `content`:  must be composed of the basic json types (boolean, number, string, list/tuple, dictionary) plus artifacts and command lines
    ///     * An artifact will be written as a string containing the path
    ///     * A command line will be written as a list of strings, unless `joined=True` is set, in which case it will be a string
    /// * If you pass `with_inputs = True`, you'll get back a `cmd_args` that expands to the JSON file but carries all the underlying inputs as dependencies (so you don't have to use, for example, `hidden` for them to be added to an action that already receives the JSON file)
    fn write_json<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] content: Value<'v>,
        #[starlark(require = named, default = false)] with_inputs: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<impl AllocValue<'v>> {
        let mut this = this.state();
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, OutputType::File)?;

        validate_json(content)?;
        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredWriteJsonAction::new(),
            Some(content),
        )?;

        let value = declaration.into_declared_artifact(AssociatedArtifacts::new());
        // TODO(cjhopman): The with_inputs thing can go away once we have artifact dependencies (we'll still
        // need the UnregisteredWriteJsonAction::cli() to represent the dependency though).
        if with_inputs {
            // TODO(nga): we use `AllocValue`, so this function return type for this branch
            //   is `write_json_cli_args`. We want just `cmd_args`,
            //   because users don't care about precise type.
            //   Do it when we migrate to new types not based on strings.
            let cli = UnregisteredWriteJsonAction::cli(value.to_value(), content)?;
            Ok(Either::Right(cli))
        } else {
            Ok(Either::Left(value))
        }
    }

    /// Returns an `artifact` whose contents are content
    ///
    /// * `is_executable` (optional): indicates whether the resulting file should be marked with executable permissions
    /// * `allow_args` (optional): must be set to `True` if you want to write parameter arguments to the file (in particular, macros that write to file)
    ///     * If it is true, the result will be a pair of the `artifact` containing content and a list of artifact values that were written by macros, which should be used in hidden fields or similar
    /// * `absolute` (optional): if set, this action will produce absolute paths in its output when
    ///   rendering artifact paths. You generally shouldn't use this if you plan to use this action
    ///   as the input for anything else, as this would effectively result in losing all shared
    ///   caching.
    fn write<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] content: Value<'v>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] allow_args: bool,
        // If set, add artifacts in content as associated artifacts of the output. This will only work for bound artifacts.
        #[starlark(require = named, default = false)] with_inputs: bool,
        #[starlark(require = named, default = false)] absolute: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<
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
        ) -> anyhow::Result<u32> {
            if !args_allowed && cli.contains_arg_attr() {
                return Err(anyhow::anyhow!(
                    WriteActionError::ArgAttrsDetectedButNotAllowed
                ));
            }

            struct WriteToFileMacrosCounter {
                count: u32,
            }

            impl WriteToFileMacroVisitor for WriteToFileMacrosCounter {
                fn visit_write_to_file_macro(&mut self, _m: &ResolvedMacro) -> anyhow::Result<()> {
                    self.count += 1;
                    Ok(())
                }

                fn set_current_relative_to_path(
                    &mut self,
                    _gen: &dyn Fn(
                        &dyn CommandLineContext,
                    ) -> anyhow::Result<Option<RelativePathBuf>>,
                ) -> anyhow::Result<()> {
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
        ) -> anyhow::Result<SmallSet<ArtifactGroup>> {
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

        let mut this = this.state();
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, OutputType::File)?;

        let (content_cli, written_macro_count, mut associated_artifacts) =
            if let Some(content_arg) = content.as_command_line() {
                let count = count_write_to_file_macros(allow_args, content_arg)?;
                let cli_inputs = get_cli_inputs(with_inputs, content_arg)?;
                (content, count, cli_inputs)
            } else {
                let cli = StarlarkCmdArgs::try_from_value(content)?;
                let count = count_write_to_file_macros(allow_args, &cli)?;
                let cli_inputs = get_cli_inputs(with_inputs, &cli)?;
                (eval.heap().alloc(cli), count, cli_inputs)
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
                Some(eval.heap().alloc(content_cli)),
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
            Some(content_cli),
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

    /// Copies the source `artifact` to the destination (which can be a string representing a filename or an output `artifact`) and returns the output `artifact`.
    /// The copy works for files or directories.
    fn copy_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word `file` in it
        copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Copy,
            OutputType::FileOrDirectory,
        )
    }

    /// Creates a symlink to the source `artifact` at the destination (which can be a string representing a filename or an output `artifact`) and returns the output `artifact`.
    /// The symlink works for files or directories.
    fn symlink_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word `file` in it
        copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Symlink,
            OutputType::FileOrDirectory,
        )
    }

    /// Make a copy of a directory.
    fn copy_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        copy_file_impl(eval, this, dest, src, CopyMode::Copy, OutputType::Directory)
    }

    /// Create a symlink to a directory.
    fn symlink_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] dest: OutputArtifactArg<'v>,
        #[starlark(require = pos)] src: ValueAsArtifactLike<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        copy_file_impl(
            eval,
            this,
            dest,
            src,
            CopyMode::Symlink,
            OutputType::Directory,
        )
    }

    /// Returns an `artifact` that is a directory containing symlinks.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to bound `artifact`, which will be laid out in the directory.
    fn symlinked_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: DictOf<'v, &'v str, ValueAsArtifactLike<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        create_dir_tree(eval, this, output, srcs, false)
    }

    /// Returns an `artifact` which is a directory containing copied files.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to the bound `artifact`, which will be laid out in the directory.
    fn copied_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] srcs: DictOf<'v, &'v str, ValueAsArtifactLike<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        create_dir_tree(eval, this, output, srcs, true)
    }

    /// Runs a command
    ///
    /// * `arguments`: must be of type `cmd_args`, or a type convertible to such (such as a list of strings and artifacts) and must contain at least one `.as_output()` artifact
    /// * `category`: category and identifier - when used together, identify the action in Buck2's event stream, and must be unique for a given target
    /// * `weight`: used to note how heavy the command is and will typically be set to a higher value to indicate that less such commands should be run in parallel (if running locally)
    /// * `no_outputs_cleanup`: if this flag is set then Buck2 won't clean the outputs of a previous build that might be present on a disk; in which case, command from arguments should be responsible for the cleanup (that is useful, for example, when an action is supporting incremental mode and its outputs are based on result from a previous build)
    /// * `metadata_env_var` and `meadata_path` should be used together: both set or both unset
    ///     * `metadata_path`: defines a path relative to the result directory for a file with action metadata, which will be created right before the command will be run.
    ///     * Metadata contains the path relative to the Buck2 project root and hash digest for every action input (this excludes symlinks as they could be resolved by a user script if needed). The resolved path relative to the Buck2 project for the metadata file will be passed to command from arguments, via the environment variable, with its name set by `metadata_env_var`
    ///     * Both `metadata_env_var` and `metadata_path` are useful when making actions behave in an incremental manner (for details, see [Incremental Actions](https://buck2.build/docs/rule_authors/incremental_actions/))
    /// * The `prefer_local`, `prefer_remote` and `local_only` options allow selecting where the
    /// action should run if the executor selected for this target is a hybrid executor.
    ///     * All those options disable concurrent execution: the action will run on the preferred
    ///     platform first (concurrent execution only happens with a "full" hybrid executor).
    ///     * Execution may be retried on the "non-preferred" platform if it fails due to a
    ///     transient error, except for `local_only`, which does not allow this.
    ///     * If the executor selected is a remote-only executor and you use `local_only`, that's
    ///     an error. The other options will not raise errors.
    ///     * Setting more than one of those options is an error.
    ///     * Those flags behave the same way as the equivalent `--prefer-remote`, `--prefer-local`
    ///     and `--local-only` CLI flags. The CLI flags take precedence.
    ///     * The `force_full_hybrid_if_capable` option overrides the `use_limited_hybrid` hybrid.
    ///     The options listed above take precedence if set.
    ///
    /// When actions execute, they'll do so from the root of the repository. As they execute,
    /// actions have exclusive access to their output directory.
    ///
    /// Actions also get exclusive access to a "scratch" path that is exposed via the environment
    /// variable `BUCK_SCRATCH_PATH`. This path is expressed as a path relative to the working
    /// directory (i.e. relative to the project).
    ///
    /// The scratch path is not guaranteed to exist when the action executes. If an action wants to
    /// use the scratch path, the action should create that directory (Buck does guarantee that the
    /// directory can be created).
    fn run<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] arguments: Value<'v>,
        #[starlark(require = named)] category: String,
        #[starlark(require = named, default = NoneOr::None)] identifier: NoneOr<String>,
        #[starlark(require = named)] env: Option<ValueOf<'v, SmallMap<&'v str, Value<'v>>>>,
        #[starlark(require = named, default = false)] local_only: bool,
        #[starlark(require = named, default = false)] prefer_local: bool,
        #[starlark(require = named, default = false)] prefer_remote: bool,
        #[starlark(require = named, default = true)] low_pass_filter: bool,
        #[starlark(require = named, default = false)] always_print_stderr: bool,
        #[starlark(require = named)] weight: Option<i32>,
        #[starlark(require = named)] weight_percentage: Option<i32>,
        #[starlark(require = named)] dep_files: Option<SmallMap<&'v str, &'v ArtifactTag>>,
        #[starlark(require = named)] metadata_env_var: Option<String>,
        #[starlark(require = named)] metadata_path: Option<String>,
        // TODO(scottcao): Refactor `no_outputs_cleanup` to `outputs_cleanup`
        #[starlark(require = named, default = false)] no_outputs_cleanup: bool,
        #[starlark(require = named, default = false)] allow_cache_upload: bool,
        #[starlark(require = named, default = false)] allow_dep_file_cache_upload: bool,
        #[starlark(require = named, default = false)] force_full_hybrid_if_capable: bool,
        #[starlark(require = named)] exe: Option<
            Either<ValueOf<'v, &'v WorkerRunInfo<'v>>, ValueOf<'v, &'v RunInfo<'v>>>,
        >,
        #[starlark(require = named, default = false)] unique_input_inodes: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        struct RunCommandArtifactVisitor {
            inner: SimpleCommandLineArtifactVisitor,
            tagged_outputs: HashMap<ArtifactTag, Vec<OutputArtifact>>,
            depth: u64,
        }

        impl RunCommandArtifactVisitor {
            fn new() -> Self {
                Self {
                    inner: SimpleCommandLineArtifactVisitor::new(),
                    tagged_outputs: HashMap::new(),
                    depth: 0,
                }
            }
        }

        impl CommandLineArtifactVisitor for RunCommandArtifactVisitor {
            fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
                self.inner.visit_input(input, tag);
            }

            fn visit_output(&mut self, artifact: OutputArtifact, tag: Option<&ArtifactTag>) {
                match tag {
                    None => {}
                    Some(tag) => {
                        self.tagged_outputs
                            .entry(tag.dupe())
                            .or_default()
                            .push(artifact.dupe());
                    }
                }

                self.inner.visit_output(artifact, tag);
            }

            fn push_frame(&mut self) -> anyhow::Result<()> {
                self.depth += 1;
                if self.depth > 1000 {
                    return Err(RunActionError::ArtifactVisitRecursionLimitExceeded.into());
                }
                Ok(())
            }

            fn pop_frame(&mut self) {
                self.depth = self.depth.saturating_sub(1);
            }
        }

        let executor_preference = new_executor_preference(local_only, prefer_local, prefer_remote)?;

        let mut artifact_visitor = RunCommandArtifactVisitor::new();

        let starlark_args = StarlarkCmdArgs::try_from_value(arguments)?;
        starlark_args.visit_artifacts(&mut artifact_visitor)?;

        let (starlark_exe, starlark_worker) = match exe {
            Some(Either::Left(worker_run)) => {
                let worker: ValueOf<&WorkerInfo> = worker_run.typed.worker();
                let worker_exe = worker_run.typed.exe();
                worker_exe.as_ref().visit_artifacts(&mut artifact_visitor)?;
                let starlark_exe = StarlarkCmdArgs::try_from_value(worker_exe.to_value())?;
                starlark_exe.visit_artifacts(&mut artifact_visitor)?;
                (starlark_exe, NoneOr::Other(worker))
            }
            Some(Either::Right(exe)) => {
                let starlark_exe = StarlarkCmdArgs::try_from_value(*exe)?;
                starlark_exe.visit_artifacts(&mut artifact_visitor)?;
                (starlark_exe, NoneOr::None)
            }
            None => (StarlarkCmdArgs::default(), NoneOr::None),
        };

        let weight = match (weight, weight_percentage) {
            (None, None) => WeightClass::Permits(1),
            (Some(v), None) => {
                if v < 1 {
                    return Err(RunActionError::InvalidWeight(v).into());
                } else {
                    WeightClass::Permits(v as usize)
                }
            }
            (None, Some(v)) => WeightClass::Percentage(
                WeightPercentage::try_new(v).context("Invalid `weight_percentage`")?,
            ),
            (Some(..), Some(..)) => {
                return Err(RunActionError::DuplicateWeightsSpecified.into());
            }
        };

        let starlark_env = match env {
            None => Value::new_none(),
            Some(env) => {
                for v in env.typed.values() {
                    v.as_command_line_err()?
                        .visit_artifacts(&mut artifact_visitor)?;
                }
                env.value
            }
        };

        let RunCommandArtifactVisitor {
            inner: artifacts,
            tagged_outputs,
            depth: _,
        } = artifact_visitor;

        let mut dep_files_configuration = RunActionDepFiles::new();

        if let Some(dep_files) = dep_files {
            for (key, tag) in dep_files {
                let tagged = tagged_outputs.get(tag);
                let count = tagged.map_or(0, |t| t.len());

                if count != 1 {
                    return Err(RunActionError::InvalidDepFileOutputs {
                        key: (*key).to_owned(),
                        count,
                    }
                    .into());
                }

                match dep_files_configuration.labels.entry(tag.dupe()) {
                    small_map::Entry::Vacant(v) => {
                        v.insert(Arc::from(key));
                    }
                    small_map::Entry::Occupied(o) => {
                        return Err(RunActionError::ConflictingDepFiles {
                            first: (**o.get()).to_owned(),
                            second: (*key).to_owned(),
                        }
                        .into());
                    }
                }
            }
        }

        let category = Category::try_from(category)?;
        let identifier = identifier.into_option();

        let metadata_param = match (metadata_env_var, metadata_path) {
            (Some(env_var), Some(path)) => {
                let path: ForwardRelativePathBuf = path.try_into()?;
                this.state().claim_output_path(eval, &path)?;
                Ok(Some(MetadataParameter { env_var, path }))
            }
            (Some(_), None) => Err(anyhow::anyhow!(RunActionError::MetadataPathMissing)),
            (None, Some(_)) => Err(anyhow::anyhow!(RunActionError::MetadataEnvVarMissing)),
            (None, None) => Ok(None),
        }?;

        if artifacts.outputs.is_empty() {
            return Err(RunActionError::NoOutputsSpecified.into());
        }
        let heap = eval.heap();

        let starlark_values = heap.alloc(StarlarkRunActionValues {
            exe: heap.alloc(starlark_exe),
            args: heap.alloc(starlark_args),
            env: starlark_env,
            worker: heap.alloc(starlark_worker),
        });

        let action = UnregisteredRunAction {
            category,
            identifier,
            executor_preference,
            always_print_stderr,
            weight,
            low_pass_filter,
            dep_files: dep_files_configuration,
            metadata_param,
            no_outputs_cleanup,
            allow_cache_upload,
            allow_dep_file_cache_upload,
            force_full_hybrid_if_capable,
            unique_input_inodes,
        };
        this.state().register_action(
            artifacts.inputs,
            artifacts.outputs,
            action,
            Some(starlark_values),
        )?;
        Ok(NoneType)
    }

    /// Downloads a URL to an output (filename as string or output artifact).
    /// The file at the URL must have the given sha1 or the command will fail.
    /// The optional parameter is_executable indicates whether the resulting file should be marked with executable permissions.
    /// (Meta-internal) The optional parameter vpnless_url indicates a url from which this resource can be downloaded off VPN; this has the same restrictions as `url` above.
    fn download_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] url: &str,
        #[starlark(require = named, default = NoneOr::None)] vpnless_url: NoneOr<&str>,
        #[starlark(require = named, default = NoneOr::None)] sha1: NoneOr<&str>,
        #[starlark(require = named, default = NoneOr::None)] sha256: NoneOr<&str>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] is_deferrable: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        let mut this = this.state();
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, OutputType::File)?;

        let checksum = match (
            sha1.into_option().map(Arc::from),
            sha256.into_option().map(Arc::from),
        ) {
            (Some(sha1), None) => Checksum::Sha1(sha1),
            (None, Some(sha256)) => Checksum::Sha256(sha256),
            (Some(sha1), Some(sha256)) => Checksum::Both { sha1, sha256 },
            (None, None) => return Err(DownloadFileError::MissingChecksum.into()),
        };

        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredDownloadFileAction::new(
                checksum,
                Arc::from(url),
                vpnless_url.into_option().map(Arc::from),
                is_executable,
                is_deferrable,
            ),
            None,
        )?;

        Ok(declaration.into_declared_artifact(AssociatedArtifacts::new()))
    }

    /// Downloads a CAS artifact to an output
    ///
    /// * `digest`: must look like `SHA1:SIZE`
    /// * `use_case`: your RE use case
    /// * `expires_after_timestamp`: must be a UNIX timestamp. Your digest's TTL must exceed this timestamp. Your build will break once the digest expires, so make sure the expiry is long enough (preferably, in years).
    /// * `is_executable` (optional): indicates the resulting file should be marked with executable permissions
    fn cas_artifact<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] output: OutputArtifactArg<'v>,
        #[starlark(require = pos)] digest: &str,
        #[starlark(require = pos)] use_case: &str,
        #[starlark(require = named)] expires_after_timestamp: i64,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] is_tree: bool,
        #[starlark(require = named, default = false)] is_directory: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkDeclaredArtifact>> {
        let mut registry = this.state();

        let digest = CasDigest::parse_digest(digest, this.digest_config.cas_digest_config())
            .with_context(|| CasArtifactError::InvalidDigest(digest.to_owned()))?
            .0;

        let use_case = RemoteExecutorUseCase::new(use_case.to_owned());

        let expires_after_timestamp = Utc.timestamp_opt(expires_after_timestamp, 0).unwrap();

        let kind = match (is_tree, is_directory) {
            (true, true) => return Err(CasArtifactError::TreeAndDirectory.into()),
            (false, true) => ArtifactKind::Directory(DirectoryKind::Directory),
            (true, false) => ArtifactKind::Directory(DirectoryKind::Tree),
            (false, false) => ArtifactKind::File,
        };

        let output_type = match kind {
            ArtifactKind::Directory(_) => OutputType::Directory,
            ArtifactKind::File => OutputType::File,
        };
        let (output_value, output_artifact) =
            registry.get_or_declare_output(eval, output, output_type)?;

        registry.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredCasArtifactAction {
                digest,
                re_use_case: use_case,
                expires_after: expires_after_timestamp,
                executable: is_executable,
                kind,
            },
            None,
        )?;

        Ok(output_value.into_declared_artifact(AssociatedArtifacts::new()))
    }

    /// Creates a new transitive set. For details, see https://buck2.build/docs/rule_authors/transitive_sets/.
    fn tset<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] definition: ValueTypedComplex<'v, TransitiveSetDefinition<'v>>,
        value: Option<Value<'v>>,
        children: Option<ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        this.create_transitive_set(
            definition.to_value(),
            value,
            children.map(|v| v.get()),
            eval,
        )
    }

    /// `dynamic_output` allows a rule to use information that was not available when the rule was first run at analysis time.
    /// Examples include things like Distributed ThinLTO (where the index file is created by another action) or OCaml builds
    /// (where the dependencies are created by `ocamldeps`).
    ///
    /// The arguments are:
    ///
    /// * `dynamic` - a list of artifacts whose values will be available in the function. These will be built before the function is run.
    /// * `inputs` - a container of artifacts (`cmd_args`, list of artifacts, and so on).
    ///   * These inputs must include all the inputs that are referenced by the body of the function argument, apart from those listed in `dynamic` and `outputs`: extra inputs may be passed that are not used.
    ///   * The inputs are used for `buck2 aquery` functionality, but do not cause speculative building. In fact, these inputs may form a cycle with other `dynamic_output` actions if they were all required.
    ///   * In the future, it may be possible to not pass all the inputs if the repo is set to permissive mode, allowing a more powerful form of dynamic dependencies.
    /// * `outputs` - a list of unbound artifacts (created with `declare_artifact`) which will be bound by the function.
    /// * The function argument is given 3 arguments:
    ///   * `ctx` (context) - which is the same as that passed to the initial rule analysis.
    ///   * `outputs` - using one of the artifacts from the `dynamic_output`'s `outputs` (example usage: `outputs[artifact_from_dynamic_output_outputs]`) gives an unbounded artifact. The function argument must use its `outputs` argument to bind output artifacts, rather than reusing artifacts from the outputs passed into `dynamic_output` directly.
    ///   * `artifacts` - using one of the artifacts from `dynamic` (example usage: `artifacts[artifact_from_dynamic])` gives an artifact value containing the methods `read_string`, `read_lines`, and `read_json` to obtain the values from the disk in various formats.  Anything too complex should be piped through a Python script for transformation to JSON.
    /// * The function must call `ctx.actions` (probably `ctx.actions.run`) to bind all outputs. It can examine the values of the dynamic variables and depends on the inputs.
    ///   * The function will usually be a `def`, as `lambda` in Starlark does not allow statements, making it quite underpowered.
    /// For full details see http://localhost:3000/docs/rule_authors/dynamic_dependencies/.
    fn dynamic_output<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = named)] dynamic: Vec<StarlarkArtifact>,
        #[starlark(require = named)] inputs: Vec<StarlarkArtifact>,
        #[starlark(require = named)] outputs: Vec<StarlarkOutputOrDeclaredArtifact>,
        #[starlark(require = named)] f: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<NoneType> {
        // Parameter validation
        let lambda_type = f.get_type();
        if lambda_type != FUNCTION_TYPE {
            return Err(DynamicOutputError::NotAFunction(lambda_type.to_owned()).into());
        }
        if dynamic.is_empty() {
            return Err(DynamicOutputError::EmptyDynamic.into());
        }
        if outputs.is_empty() {
            return Err(DynamicOutputError::EmptyOutput.into());
        }

        // Conversion
        let dynamic = dynamic.iter().map(|x| x.artifact()).collect();
        let inputs = inputs.iter().map(|x| x.artifact()).collect();
        let outputs = outputs.iter().map(|x| x.0.artifact()).collect();

        // Registration
        let attributes_plugins_lambda = heap.alloc((this.attributes, this.plugins, f));
        let mut this = this.state();
        this.register_dynamic_output(dynamic, inputs, outputs, attributes_plugins_lambda)?;
        Ok(NoneType)
    }

    /// Allocate a new input tag. Used with the `dep_files` argument to `run`.
    fn artifact_tag<'v>(this: &AnalysisActions<'v>) -> anyhow::Result<ArtifactTag> {
        let _ = this;
        Ok(ArtifactTag::new())
    }

    /// Converts a promise to an artifact. If the promise later resolves to a non-artifact, it is an error. Takes
    /// in an optional named parameter `short_path` that can be used to access the short path before the promise is
    /// resolved. It will be validated that the provided short path matches the built artifact's short path after
    /// analysis happens and the promise has been resolved.
    ///
    /// For more details see https://buck2.build/docs/rule_authors/anon_targets/.
    fn artifact_promise<'v>(
        this: &AnalysisActions<'v>,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        #[starlark(require = named, default = NoneOr::None)] short_path: NoneOr<StringValue<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkPromiseArtifact> {
        let short_path = if let Some(short_path) = short_path.into_option() {
            Some(ForwardRelativePathBuf::new(short_path.as_str().to_owned())?)
        } else {
            None
        };

        let declaration_location = eval.call_stack_top_location();
        let mut this = this.state();
        let artifact = this.register_artifact_promise(
            promise,
            declaration_location.clone(),
            short_path.clone(),
        )?;
        let res = StarlarkPromiseArtifact::new(declaration_location, artifact, short_path);
        Ok(res)
    }

    /// Obtain this daemon's digest configuration. This allows rules to discover what digests the
    /// daemon may be able to e.g. defer download because they conform to its RE backend's expected
    /// digest format.
    fn digest_config<'v>(this: &AnalysisActions<'v>) -> anyhow::Result<StarlarkDigestConfig> {
        Ok(StarlarkDigestConfig {
            digest_config: this.digest_config,
        })
    }
}

pub(crate) fn init_analysis_action_methods_actions() {
    ANALYSIS_ACTIONS_METHODS_ACTIONS.init(analysis_actions_methods_actions);
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::actions::artifact::artifact_type::OutputArtifact;
use buck2_build_api::actions::impls::json::validate_json;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::attrs::resolve::attr_type::arg::value::ResolvedMacro;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkPromiseArtifact;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::context::REGISTER_CONTEXT_ACTIONS;
use buck2_common::cas_digest::CasDigest;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_core::category::Category;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_execute::execute::request::OutputType;
use buck2_execute::materialize::http::Checksum;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter_for_build::rule::FrozenRuleCallable;
use chrono::TimeZone;
use chrono::Utc;
use ctor::ctor;
use dupe::Dupe;
use dupe::OptionDupedExt;
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
use starlark::values::Heap;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueTyped;
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
    #[error("`dep_files` values must be artifact tags, got `{}` for key `{}`", .value, .key)]
    InvalidDepFileTag { key: String, value: String },
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
    output: Value<'v>,
    srcs: Value<'v>,
    copy: bool,
) -> anyhow::Result<Value<'v>> {
    // validate that the moves are valid, and move them into inputs
    let action = UnregisteredSymlinkedDirAction::new(copy, srcs)?;
    let inputs = action.inputs();
    let unioned_associated_artifacts = action.unioned_associated_artifacts();

    let mut this = this.state();
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, output, "output", OutputType::Directory)?;
    this.register_action(inputs, indexset![output_artifact], action, None)?;

    let value = declaration
        .into_declared_artifact(unioned_associated_artifacts)
        .to_value();
    Ok(value)
}

fn copy_file<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: &AnalysisActions<'v>,
    dest: Value<'v>,
    src: Value<'v>,
    copy: CopyMode,
    output_type: OutputType,
) -> anyhow::Result<Value<'v>> {
    let src = src
        .as_artifact()
        .ok_or_else(|| ValueError::IncorrectParameterTypeNamed("src".to_owned()))?;

    let artifact = src.get_bound_artifact()?;
    let associated_artifacts = src.get_associated_artifacts();
    let mut this = this.state();
    let (declaration, output_artifact) =
        this.get_or_declare_output(eval, dest, "dest", output_type)?;

    this.register_action(
        indexset![ArtifactGroup::Artifact(artifact)],
        indexset![output_artifact],
        UnregisteredCopyAction::new(copy),
        None,
    )?;

    let value = declaration.into_declared_artifact(
        associated_artifacts
            .duped()
            .unwrap_or_else(AssociatedArtifacts::new),
    );
    Ok(value.to_value())
}

// Type literals that we use
const TYPE_INPUT_ARTIFACT: &str = "[str.type, \"output_artifact\", \"artifact\"]";
const TYPE_ARTIFACT: &str = "\"artifact\"";
const TYPE_CMD_ARG_LIKE: &str = "\"_arglike\"";

/// Functions to allow users to interact with the Actions registry.
/// Accessed via `ctx.actions.<function>`.
///
/// Actions take inputs and produce outputs, mostly using the `artifact` type.
/// Most output filenames can either be artifacts created with `declare_output` or strings that are implicitly converted to output artifacts.
#[starlark_module]
fn register_context_actions(builder: &mut MethodsBuilder) {
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
    #[starlark(return_type = "[\"artifact\", \"cmd_args\"]")]
    fn write_json<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos)] content: Value<'v>,
        #[starlark(require = named, default = false)] with_inputs: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, "output", OutputType::File)?;

        validate_json(content)?;
        this.register_action(
            IndexSet::new(),
            indexset![output_artifact],
            UnregisteredWriteJsonAction::new(),
            Some(content),
        )?;

        let value = declaration
            .into_declared_artifact(AssociatedArtifacts::new())
            .to_value();
        // TODO(cjhopman): The with_inputs thing can go away once we have artifact dependencies (we'll still
        // need the UnregisteredWriteJsonAction::cli() to represent the dependency though).
        if with_inputs {
            let cli = UnregisteredWriteJsonAction::cli(value, content)?;
            Ok(eval.heap().alloc(cli))
        } else {
            Ok(value)
        }
    }

    /// Returns an `artifact` whose contents are content
    ///
    /// * `is_executable` (optional): indicates whether the resulting file should be marked with executable permissions
    /// * `allow_args` (optional): must be set to `True` if you want to write parameter arguments to the file (in particular, macros that write to file)
    ///     * If it is true, the result will be a pair of the `artifact` containing content and a list of artifact values that were written by macros, which should be used in hidden fields or similar
    #[starlark(return_type = "[\"artifact\", (\"artifact\", [\"artifact\"])]")]
    fn write<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos, type = TYPE_CMD_ARG_LIKE)] content: Value<'v>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] allow_args: bool,
        // If set, add artifacts in content as associated artifacts of the output. This will only work for bound artifacts.
        #[starlark(require = named, default = false)] with_inputs: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
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
            this.get_or_declare_output(eval, output, "output", OutputType::File)?;

        let (content_cli, written_macro_count, mut associated_artifacts) =
            if let Some(content_arg) = content.as_command_line() {
                let count = count_write_to_file_macros(allow_args, content_arg)?;
                let cli_inputs = get_cli_inputs(with_inputs, content_arg)?;
                (content, count, cli_inputs)
            } else {
                let cli = StarlarkCommandLine::try_from_value(content)?;
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
            UnregisteredWriteAction::new(is_executable, maybe_macro_files)
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

        let value = declaration
            .into_declared_artifact(AssociatedArtifacts::from(associated_artifacts))
            .to_value();
        if allow_args {
            let macro_files: Vec<StarlarkDeclaredArtifact> = written_macro_files
                .into_iter()
                .map(|a| StarlarkDeclaredArtifact::new(None, a, AssociatedArtifacts::new()))
                .collect();
            Ok(eval.heap().alloc((value, macro_files)))
        } else {
            // Prefer simpler API when there is no possibility for write-to-file macros to be present in a content
            Ok(value)
        }
    }

    /// Copies the source `artifact` to the destination (which can be a string representing a filename or an output `artifact`) and returns the output `artifact`.
    /// The copy works for files or directories.
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn copy_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] dest: Value<'v>,
        #[starlark(require = pos, type = TYPE_ARTIFACT)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word `file` in it
        copy_file(
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
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn symlink_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] dest: Value<'v>,
        #[starlark(require = pos, type = TYPE_ARTIFACT)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        // `copy_file` can copy either a file or a directory, even though its name has the word `file` in it
        copy_file(
            eval,
            this,
            dest,
            src,
            CopyMode::Symlink,
            OutputType::FileOrDirectory,
        )
    }

    /// Make a copy of a directory.
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn copy_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] dest: Value<'v>,
        #[starlark(require = pos, type = TYPE_ARTIFACT)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        copy_file(eval, this, dest, src, CopyMode::Copy, OutputType::Directory)
    }

    /// Create a symlink to a directory.
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn symlink_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] dest: Value<'v>,
        #[starlark(require = pos, type = TYPE_ARTIFACT)] src: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        copy_file(
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
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn symlinked_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos, type = "{str.type, \"artifact\"}")] srcs: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        create_dir_tree(eval, this, output, srcs, false)
    }

    /// Returns an `artifact` which is a directory containing copied files.
    /// The srcs must be a dictionary of path (as string, relative to the result directory) to the bound `artifact`, which will be laid out in the directory.
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn copied_dir<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos, type = "{str.type, \"artifact\"}")] srcs: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
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
    fn run<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_CMD_ARG_LIKE)] arguments: Value<'v>,
        #[starlark(require = named)] category: String,
        #[starlark(require = named, default = NoneOr::None)] identifier: NoneOr<String>,
        #[starlark(require = named)] env: Option<ValueOf<'v, SmallMap<&'v str, Value<'v>>>>,
        #[starlark(require = named, default = false)] local_only: bool,
        #[starlark(require = named, default = false)] prefer_local: bool,
        #[starlark(require = named, default = false)] prefer_remote: bool,
        #[starlark(require = named, default = false)] always_print_stderr: bool,
        #[starlark(require = named)] weight: Option<i32>,
        #[starlark(require = named)] weight_percentage: Option<i32>,
        #[starlark(require = named, type = "{str.type, \"artifact_tag\"}")] dep_files: Option<
            ValueOf<'v, SmallMap<&'v str, Value<'v>>>,
        >,
        #[starlark(require = named)] metadata_env_var: Option<String>,
        #[starlark(require = named)] metadata_path: Option<String>,
        // TODO(scottcao): Refactor `no_outputs_cleanup` to `outputs_cleanup`
        #[starlark(require = named, default = false)] no_outputs_cleanup: bool,
        #[starlark(require = named, default = false)] allow_cache_upload: bool,
        #[starlark(require = named, default = false)] force_full_hybrid_if_capable: bool,
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

        let starlark_cli = StarlarkCommandLine::try_from_value(arguments)?;
        starlark_cli.visit_artifacts(&mut artifact_visitor)?;

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
            for (key, value) in dep_files.typed.iter() {
                let tag = value.downcast_ref::<ArtifactTag>().ok_or_else(|| {
                    RunActionError::InvalidDepFileTag {
                        key: (*key).to_owned(),
                        value: value.to_string(),
                    }
                })?;

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
                    Entry::Vacant(v) => {
                        v.insert(Arc::from(*key));
                    }
                    Entry::Occupied(o) => {
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
        let starlark = eval.heap().alloc((starlark_cli, starlark_env));

        let action = UnregisteredRunAction {
            category,
            identifier,
            executor_preference,
            always_print_stderr,
            weight,
            dep_files: dep_files_configuration,
            metadata_param,
            no_outputs_cleanup,
            allow_cache_upload,
            force_full_hybrid_if_capable,
        };
        this.state().register_action(
            artifacts.inputs,
            artifacts.outputs,
            action,
            Some(starlark),
        )?;
        Ok(NoneType)
    }

    /// Downloads a URL to an output (filename as string or output artifact).
    /// The file at the URL must have the given sha1 or the command will fail.
    /// The optional parameter is_executable indicates whether the resulting file should be marked with executable permissions.
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn download_file<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos)] url: &str,
        #[starlark(require = named, default = NoneOr::None)] sha1: NoneOr<&str>,
        #[starlark(require = named, default = NoneOr::None)] sha256: NoneOr<&str>,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] is_deferrable: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        let (declaration, output_artifact) =
            this.get_or_declare_output(eval, output, "output", OutputType::File)?;

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
                is_executable,
                is_deferrable,
            ),
            None,
        )?;

        let value = declaration
            .into_declared_artifact(AssociatedArtifacts::new())
            .to_value();
        Ok(value)
    }

    /// Downloads a CAS artifact to an output
    ///
    /// * `digest`: must look like `SHA1:SIZE`
    /// * `use_case`: your RE use case
    /// * `expires_after_timestamp`: must be a UNIX timestamp. Your digest's TTL must exceed this timestamp. Your build will break once the digest expires, so make sure the expiry is long enough (preferably, in years).
    /// * `is_executable` (optional): indicates the resulting file should be marked with executable permissions
    #[starlark(return_type = TYPE_ARTIFACT)]
    fn cas_artifact<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = TYPE_INPUT_ARTIFACT)] output: Value<'v>,
        #[starlark(require = pos)] digest: &str,
        #[starlark(require = pos)] use_case: &str,
        #[starlark(require = named)] expires_after_timestamp: i64,
        #[starlark(require = named, default = false)] is_executable: bool,
        #[starlark(require = named, default = false)] is_tree: bool,
        #[starlark(require = named, default = false)] is_directory: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
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
            registry.get_or_declare_output(eval, output, "output", output_type)?;

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

        Ok(output_value
            .into_declared_artifact(AssociatedArtifacts::new())
            .to_value())
    }

    /// Creates a new transitive set. For details, see https://buck2.build/docs/rule_authors/transitive_sets/.
    fn tset<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos, type = "\"transitive_set_definition\"")] definition: Value<'v>,
        value: Option<Value<'v>>,
        #[starlark(type = "iter(\"\")")] children: Option<Value<'v>>, // An iterable.
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let mut this = this.state();
        this.create_transitive_set(definition, value, children, eval)
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
    ///
    /// For full details see http://localhost:3000/docs/rule_authors/dynamic_dependencies/.
    fn dynamic_output<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = named)] dynamic: Vec<StarlarkArtifact>,
        #[starlark(require = named)] inputs: Vec<StarlarkArtifact>,
        #[starlark(require = named)] outputs: Vec<StarlarkOutputArtifact>,
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
        let outputs = outputs.iter().map(|x| x.artifact()).collect();

        // Registration
        let attributes_lambda = heap.alloc((this.attributes, f));
        let mut this = this.state();
        this.register_dynamic_output(dynamic, inputs, outputs, attributes_lambda)?;
        Ok(NoneType)
    }

    /// Allocate a new input tag. Used with the `dep_files` argument to `run`.
    fn artifact_tag<'v>(this: &AnalysisActions<'v>) -> anyhow::Result<ArtifactTag> {
        let _ = this;
        Ok(ArtifactTag::new())
    }

    /// An anonymous target is defined by the hash of its attributes, rather than its name.
    /// During analysis, rules can define and access the providers of anonymous targets before producing their own providers.
    /// Two distinct rules might ask for the same anonymous target, sharing the work it performs.
    ///
    /// For more details see https://buck2.build/docs/rule_authors/anon_targets/
    fn anon_target<'v>(
        this: &AnalysisActions<'v>,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attrs: DictOf<'v, &'v str, Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        let res = heap.alloc_typed(StarlarkPromise::new_unresolved());
        let mut this = this.state();
        this.register_anon_target(res, rule, attrs)?;
        Ok(res)
    }

    /// Converts a promise to an artifact. If the promise later resolves to a non-artifact, it is an error.
    ///
    /// For more details see https://buck2.build/docs/rule_authors/anon_targets/.
    fn artifact_promise<'v>(
        this: &AnalysisActions<'v>,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkPromiseArtifact> {
        let declaration_location = eval.call_stack_top_location();
        let mut this = this.state();
        let artifact = this.register_artifact_promise(promise, declaration_location.clone())?;
        let res = StarlarkPromiseArtifact::new(declaration_location, artifact);
        Ok(res)
    }

    /// Generate a series of anonymous targets
    fn anon_targets<'v>(
        this: &AnalysisActions<'v>,
        rules: Vec<(
            ValueTyped<'v, FrozenRuleCallable>,
            DictOf<'v, &'v str, Value<'v>>,
        )>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkPromise<'v>>> {
        let res = heap.alloc_typed(StarlarkPromise::new_unresolved());
        let mut this = this.state();
        this.register_anon_targets(res, rules)?;
        Ok(res)
    }
}

#[ctor]
fn init_register_context_actions() {
    REGISTER_CONTEXT_ACTIONS.init(register_context_actions);
}

#[cfg(test)]
mod tests {
    use buck2_build_api::analysis::registry::AnalysisRegistry;
    use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
    use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
    use buck2_build_api::interpreter::rule_defs::register_rule_defs;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::provider::label::ConfiguredProvidersLabel;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::label::TargetLabel;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_interpreter::types::label::Label;
    use buck2_node::configuration::execution::ExecutionPlatformResolution;
    use dupe::Dupe;
    use indoc::indoc;
    use maplit::hashmap;
    use starlark::environment::GlobalsBuilder;
    use starlark::environment::Module;
    use starlark::eval::Evaluator;
    use starlark::eval::ReturnFileLoader;
    use starlark::syntax::AstModule;
    use starlark::syntax::Dialect;
    use starlark::values::structs::AllocStruct;
    use starlark::values::UnpackValue;
    use starlark::values::Value;

    fn run_ctx_test(
        content: &str,
        result_handler: impl FnOnce(anyhow::Result<Value>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let func_mod = Module::new();
        let globals = GlobalsBuilder::extended().with(register_rule_defs).build();
        let prelude = indoc!(
            r#"
             def assert_eq(a, b):
                 if a != b:
                     fail("Expected {}, got {}".format(a, b))
             "#
        );
        let full_content = format!("{}\n{}", prelude, content);

        {
            let mut eval = Evaluator::new(&func_mod);
            let ast = AstModule::parse("foo.bzl", full_content, &Dialect::Extended).unwrap();
            eval.eval_module(ast, &globals).unwrap();
        };
        let frozen_func_mod = func_mod.freeze()?;
        let test_function = frozen_func_mod.get("test").unwrap();

        let modules = hashmap!["func_mod" => &frozen_func_mod];

        let env = Module::new();
        let file_loader = ReturnFileLoader { modules: &modules };
        let test_function = test_function.owned_value(env.frozen_heap());
        let mut eval = Evaluator::new(&env);
        eval.set_loader(&file_loader);
        let label = TargetLabel::testing_parse("root//foo/bar:some_name")
            .configure(ConfigurationData::testing_new());
        let registry = AnalysisRegistry::new_from_owner(
            BaseDeferredKey::TargetLabel(label.dupe()),
            ExecutionPlatformResolution::unspecified(),
        );
        let attributes = eval.heap().alloc(AllocStruct([("name", "some_name")]));

        let ctx = eval.heap().alloc(AnalysisContext::new(
            eval.heap(),
            attributes,
            Some(
                eval.heap()
                    .alloc_typed(Label::new(ConfiguredProvidersLabel::new(
                        label,
                        ProvidersName::Default,
                    ))),
            ),
            registry,
            DigestConfig::testing_default(),
        ));

        let returned = eval.eval_function(test_function, &[ctx], &[]);
        result_handler(returned)
    }

    #[test]
    fn ctx_instantiates() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(ctx):
                 assert_eq("foo/bar", ctx.label.package)
                 assert_eq("some_name", ctx.label.name)
                 assert_eq(None, ctx.label.sub_target)
                 return ctx.attrs.name
             "#
        );
        run_ctx_test(content, |ret| {
            assert_eq!("some_name", ret.unwrap().unpack_str().unwrap());
            Ok(())
        })
    }

    #[test]
    fn declare_output_declares_outputs() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 out = c.actions.declare_output("foo/bar.cpp")
                 return (out.basename, out.short_path)
             "#
        );

        run_ctx_test(content, |ret| {
            let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap();
            assert_eq!("bar.cpp", a.0);
            assert_eq!("foo/bar.cpp", a.1);
            Ok(())
        })
    }

    #[test]
    fn declare_output_with_prefix() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 out = c.actions.declare_output("out/test", "foo/bar.cpp")
                 return (out.basename, out.short_path)
             "#
        );

        run_ctx_test(content, |ret| {
            let a = <(&str, &str)>::unpack_value(ret.unwrap()).unwrap();
            assert_eq!("bar.cpp", a.0);
            assert_eq!("foo/bar.cpp", a.1);
            Ok(())
        })
    }

    #[test]
    fn declare_output_dot() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 return c.actions.declare_output("magic", ".")
             "#
        );

        let expect = "artifact with an empty filename component";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }

    #[test]
    fn declare_output_dot_bad() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 return c.actions.declare_output("..")
             "#
        );

        let expect = "expected a normalized path";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }
    #[test]
    fn declare_output_dotdot() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 return c.actions.declare_output("foo/..")
             "#
        );

        let expect = "expected a normalized path";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }

    #[test]
    fn declare_output_require_bound() -> anyhow::Result<()> {
        let content = indoc!(
            r#"
             def test(c):
                 a = c.actions.declare_output("a")
                 b = c.actions.declare_output("b")
                 c.actions.run([a, b.as_output()], category = "test_category")
             "#
        );

        let expect = "must be bound by now";
        run_ctx_test(content, |ret| match ret {
            Err(e) if e.to_string().contains(expect) => Ok(()),
            _ => panic!(
                "Expected a specific failure containing `{}`, got {:?}",
                expect, ret
            ),
        })
    }
}

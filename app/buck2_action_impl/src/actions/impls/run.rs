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
use std::ops::ControlFlow;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_execution_target::ActionExecutionTarget;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLine;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLineDigest;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLineFingerprinter;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::FrozenStarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::FrozenWorkerInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;
use buck2_core::category::Category;
use buck2_core::category::CategoryRef;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::executor_config::MetaInternalExtraParams;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPathKind;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use buck2_execute::execute::cache_uploader::IntoRemoteDepFile;
use buck2_execute::execute::cache_uploader::force_cache_upload;
use buck2_execute::execute::dep_file_digest::DepFileDigest;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::request::ActionMetadataBlob;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::RemoteWorkerSpec;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::WriteRequest;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;
use host_sharing::WeightClass;
use indexmap::IndexMap;
use indexmap::IndexSet;
use indexmap::indexmap;
use itertools::Itertools;
use serde_json::json;
use sorted_vector_map::SortedVectorMap;
use starlark::collections::SmallSet;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValueOfUnchecked;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::starlark_value;

use self::dep_files::DepFileBundle;
use crate::actions::impls::run::dep_files::DepFilesCommandLineVisitor;
use crate::actions::impls::run::dep_files::RunActionDepFiles;
use crate::actions::impls::run::dep_files::make_dep_file_bundle;
use crate::actions::impls::run::dep_files::populate_dep_files;
use crate::actions::impls::run::metadata::metadata_content;
use crate::context::run::RunActionError;

pub(crate) mod audit_dep_files;
pub(crate) mod dep_files;
mod metadata;

#[derive(Debug, Allocative)]
pub(crate) struct MetadataParameter {
    /// Name of the environment variable which is set to contain
    /// resolved path of the metadata file when requested by user.
    pub(crate) env_var: String,
    /// User-defined path in the output directory of the metadata file.
    pub(crate) path: ForwardRelativePathBuf,
    /// An artifact that is 'tagged' with any of these tags is ignored
    /// when computing the metadata.
    pub(crate) ignore_tags: SmallSet<ArtifactTag>,
}

impl Display for MetadataParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let json = json!({
            "env_var": self.env_var,
            "path": self.path,
            "ignore_tags": self.ignore_tags.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
        });
        write!(f, "{json}")
    }
}

/// A key that uniquely identifies a RunAction.
#[derive(Eq, PartialEq, Hash, Display, Allocative)]
#[display(
    "{} {} {}",
    owner,
    category,
    identifier.as_deref().unwrap_or("<no identifier>")
)]
pub(crate) struct RunActionKey {
    owner: BaseDeferredKey,
    category: Category,
    identifier: Option<String>,
}

impl RunActionKey {
    pub(crate) fn new(
        owner: BaseDeferredKey,
        category: Category,
        identifier: Option<String>,
    ) -> Self {
        Self {
            owner,
            category,
            identifier,
        }
    }

    pub(crate) fn from_action_execution_target(target: ActionExecutionTarget<'_>) -> Self {
        Self {
            owner: target.owner().dupe(),
            category: target.category().to_owned(),
            identifier: target.identifier().map(|t| t.to_owned()),
        }
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum LocalPreferenceError {
    #[error("cannot have `local_only = True` and `prefer_local = True` at the same time")]
    LocalOnlyAndPreferLocal,
    #[error("cannot have `local_only = True` and `prefer_remote = True` at the same time")]
    LocalOnlyAndPreferRemote,
    #[error(
        "cannot have `local_only = True`, `prefer_local = True` and `prefer_remote = True` at the same time"
    )]
    LocalOnlyAndPreferLocalAndPreferRemote,
    #[error("cannot have `prefer_local = True` and `prefer_remote = True` at the same time")]
    PreferLocalAndPreferRemote,
}

pub(crate) fn new_executor_preference(
    local_only: bool,
    prefer_local: bool,
    prefer_remote: bool,
) -> buck2_error::Result<ExecutorPreference> {
    match (local_only, prefer_local, prefer_remote) {
        (true, false, false) => Ok(ExecutorPreference::LocalRequired),
        (true, false, true) => Err(LocalPreferenceError::LocalOnlyAndPreferRemote.into()),
        (false, true, false) => Ok(ExecutorPreference::LocalPreferred),
        (false, true, true) => Err(LocalPreferenceError::PreferLocalAndPreferRemote.into()),
        (false, false, false) => Ok(ExecutorPreference::Default),
        (false, false, true) => Ok(ExecutorPreference::RemotePreferred),
        (true, true, false) => Err(LocalPreferenceError::LocalOnlyAndPreferLocal.into()),
        (true, true, true) => {
            Err(LocalPreferenceError::LocalOnlyAndPreferLocalAndPreferRemote.into())
        }
    }
}

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredRunAction {
    pub(crate) executor_preference: ExecutorPreference,
    pub(crate) always_print_stderr: bool,
    pub(crate) weight: WeightClass,
    pub(crate) low_pass_filter: bool,
    pub(crate) dep_files: RunActionDepFiles,
    pub(crate) metadata_param: Option<MetadataParameter>,
    pub(crate) no_outputs_cleanup: bool,
    pub(crate) incremental_remote_outputs: bool,
    pub(crate) allow_cache_upload: Option<bool>,
    pub(crate) allow_dep_file_cache_upload: bool,
    pub(crate) force_full_hybrid_if_capable: bool,
    pub(crate) unique_input_inodes: bool,
    pub(crate) remote_execution_dependencies: Vec<RemoteExecutorDependency>,
    // Since this is usually None, use a Box to avoid using memory that is the size
    // of RemoteExecutorCustomImage.
    pub(crate) remote_execution_custom_image: Option<Box<RemoteExecutorCustomImage>>,
    pub(crate) meta_internal_extra_params: MetaInternalExtraParams,
}

impl UnregisteredAction for UnregisteredRunAction {
    fn register(
        self: Box<Self>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
        error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        let starlark_values = starlark_data.internal_error("module data to be present")?;
        let run_action = RunAction::new(*self, starlark_values, outputs, error_handler)?;
        Ok(Box::new(run_action))
    }
}

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize, Allocative)]
#[display("RunActionValues")]
pub(crate) struct StarlarkRunActionValues<'v> {
    pub(crate) exe: ValueTyped<'v, StarlarkCmdArgs<'v>>,
    pub(crate) args: ValueTyped<'v, StarlarkCmdArgs<'v>>,
    pub(crate) env: Option<ValueOfUnchecked<'v, DictType<String, ValueAsCommandLineLike<'static>>>>,
    pub(crate) worker: Option<ValueTypedComplex<'v, WorkerInfo<'v>>>,
    pub(crate) remote_worker: Option<ValueTypedComplex<'v, WorkerInfo<'v>>>,
    pub(crate) category: StringValue<'v>,
    pub(crate) identifier: Option<StringValue<'v>>,
    pub(crate) outputs_for_error_handler: Vec<ValueTyped<'v, StarlarkOutputArtifact<'v>>>,
}

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize, Allocative)]
#[display("RunActionValues")]
pub(crate) struct FrozenStarlarkRunActionValues {
    pub(crate) exe: FrozenValueTyped<'static, FrozenStarlarkCmdArgs>,
    pub(crate) args: FrozenValueTyped<'static, FrozenStarlarkCmdArgs>,
    pub(crate) env:
        Option<FrozenValueOfUnchecked<'static, DictType<String, ValueAsCommandLineLike<'static>>>>,
    pub(crate) worker: Option<FrozenValueTyped<'static, FrozenWorkerInfo>>,
    pub(crate) remote_worker: Option<FrozenValueTyped<'static, FrozenWorkerInfo>>,
    pub(crate) category: FrozenStringValue,
    pub(crate) identifier: Option<FrozenStringValue>,
    pub(crate) outputs_for_error_handler:
        Vec<FrozenValueTyped<'static, FrozenStarlarkOutputArtifact>>,
}

#[starlark_value(type = "RunActionValues")]
impl<'v> StarlarkValue<'v> for StarlarkRunActionValues<'v> {}

#[starlark_value(type = "RunActionValues")]
impl<'v> StarlarkValue<'v> for FrozenStarlarkRunActionValues {
    type Canonical = StarlarkRunActionValues<'v>;
}

impl<'v> Freeze for StarlarkRunActionValues<'v> {
    type Frozen = FrozenStarlarkRunActionValues;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let StarlarkRunActionValues {
            exe,
            args,
            env,
            worker,
            remote_worker,
            category,
            identifier,
            outputs_for_error_handler,
        } = self;
        Ok(FrozenStarlarkRunActionValues {
            exe: exe.freeze(freezer)?,
            args: args.freeze(freezer)?,
            env: env.freeze(freezer)?,
            worker: worker.freeze(freezer)?,
            remote_worker: remote_worker.freeze(freezer)?,
            category: category.freeze(freezer)?,
            identifier: identifier.freeze(freezer)?,
            outputs_for_error_handler: outputs_for_error_handler
                .iter()
                .copied()
                .map(|x| x.freeze(freezer))
                .collect::<FreezeResult<_>>()?,
        })
    }
}

impl FrozenStarlarkRunActionValues {
    pub(crate) fn worker<'v>(
        &'v self,
    ) -> buck2_error::Result<Option<ValueOf<'v, &'v WorkerInfo<'v>>>> {
        let Some(worker) = self.worker else {
            return Ok(None);
        };
        ValueOf::unpack_value_err(worker.to_value())
            .map_err(buck2_error::Error::from)
            .map(Some)
    }

    pub(crate) fn remote_worker<'v>(
        &'v self,
    ) -> buck2_error::Result<Option<ValueOf<'v, &'v WorkerInfo<'v>>>> {
        let Some(remote_worker) = self.remote_worker else {
            return Ok(None);
        };
        ValueOf::unpack_value_err(remote_worker.to_value())
            .map_err(buck2_error::Error::from)
            .map(Some)
    }
}

struct UnpackedWorkerValues<'v> {
    exe: &'v dyn CommandLineArgLike<'v>,
    env: Vec<(&'v str, &'v dyn CommandLineArgLike<'v>)>,
    id: WorkerId,
    concurrency: Option<usize>,
    streaming: bool,
    supports_bazel_remote_persistent_worker_protocol: bool,
}

struct UnpackedRunActionValues<'v> {
    exe: &'v dyn CommandLineArgLike<'v>,
    args: &'v dyn CommandLineArgLike<'v>,
    env: Vec<(&'v str, &'v dyn CommandLineArgLike<'v>)>,
    worker: Option<UnpackedWorkerValues<'v>>,
    remote_worker: Option<UnpackedWorkerValues<'v>>,
}

#[derive(Debug, Allocative)]
pub(crate) struct RunAction {
    inner: UnregisteredRunAction,
    starlark_values: OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    outputs: BoxSliceSet<BuildArtifact>,
    error_handler: Option<OwnedFrozenValue>,
    input_files_bytes: u64,
}

#[allow(clippy::large_enum_variant)]
enum ExecuteResult {
    LocalDepFileHit(ActionOutputs, ActionExecutionMetadata),
    ExecutedOrReHit {
        result: CommandExecutionResult,
        dep_file_bundle: DepFileBundle,
        executor_preference: ExecutorPreference,
        action_and_blobs: ActionDigestAndBlobs,
        input_files_bytes: u64,
    },
}

pub struct DepFilesPlaceholderArtifactPathMapper {}

impl ArtifactPathMapper for DepFilesPlaceholderArtifactPathMapper {
    fn get(&self, _artifact: &Artifact) -> Option<&ContentBasedPathHash> {
        Some(&ContentBasedPathHash::DepFilesPlaceholder)
    }
}

type ExpandedCommandLineDigestForDepFiles = ExpandedCommandLineDigest;

/// A CommandLineArtifactVisitor that gathers non-hidden inputs.
pub struct SkipHiddenCommandLineArtifactVisitor {
    pub inputs: IndexSet<ArtifactGroup>,
}

impl SkipHiddenCommandLineArtifactVisitor {
    pub fn new() -> Self {
        Self {
            inputs: IndexSet::new(),
        }
    }
}

impl CommandLineArtifactVisitor<'_> for SkipHiddenCommandLineArtifactVisitor {
    fn visit_input(&mut self, input: ArtifactGroup, _tags: Vec<&ArtifactTag>) {
        self.inputs.insert(input);
    }

    fn visit_declared_output(&mut self, _artifact: OutputArtifact<'_>, _tags: Vec<&ArtifactTag>) {}

    fn visit_frozen_output(&mut self, _artifact: Artifact, _tags: Vec<&ArtifactTag>) {}

    fn skip_hidden(&self) -> bool {
        true
    }
}

impl RunAction {
    fn visit_artifacts<'a>(
        &'a self,
        artifact_visitor: &mut dyn CommandLineArtifactVisitor<'a>,
    ) -> buck2_error::Result<()> {
        let values = Self::unpack(&self.starlark_values)?;
        values.args.visit_artifacts(artifact_visitor)?;
        values.exe.visit_artifacts(artifact_visitor)?;
        if let Some(worker) = values.worker {
            worker.exe.visit_artifacts(artifact_visitor)?;
        }
        if let Some(remote_worker) = values.remote_worker {
            remote_worker.exe.visit_artifacts(artifact_visitor)?;
            for (_, v) in remote_worker.env.iter() {
                v.visit_artifacts(artifact_visitor)?;
            }
        }
        for (_, v) in values.env.iter() {
            v.visit_artifacts(artifact_visitor)?;
        }
        Ok(())
    }

    fn unpack<'v>(
        values: &'v OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    ) -> buck2_error::Result<UnpackedRunActionValues<'v>> {
        let exe: &dyn CommandLineArgLike = &*values.exe;
        let args: &dyn CommandLineArgLike = &*values.args;
        let env = match values.env {
            None => Vec::new(),
            Some(env) => {
                let d = DictRef::from_value(env.to_value().get())
                    .buck_error_context("expecting dict")?;
                let mut res = Vec::with_capacity(d.len());
                for (k, v) in d.iter() {
                    res.push((
                        k.unpack_str().buck_error_context("expecting string")?,
                        ValueAsCommandLineLike::unpack_value_err(v)?.0,
                    ));
                }
                res
            }
        };
        let worker: Option<&WorkerInfo> = values.worker()?.map(|v| v.typed);

        let worker = worker.map(|worker| UnpackedWorkerValues {
            exe: worker.exe_command_line(),
            env: worker.env(),
            id: WorkerId(worker.id),
            concurrency: worker.concurrency(),
            streaming: worker.streaming(),
            supports_bazel_remote_persistent_worker_protocol: worker
                .supports_bazel_remote_persistent_worker_protocol(),
        });

        let remote_worker: Option<&WorkerInfo> = values.remote_worker()?.map(|v| v.typed);

        let remote_worker = remote_worker.map(|remote_worker| UnpackedWorkerValues {
            exe: remote_worker.exe_command_line(),
            env: remote_worker.env(),
            id: WorkerId(remote_worker.id),
            concurrency: remote_worker.concurrency(),
            streaming: false,
            supports_bazel_remote_persistent_worker_protocol: false,
        });

        Ok(UnpackedRunActionValues {
            exe,
            args,
            env,
            worker,
            remote_worker,
        })
    }

    /// Get the command line expansion for this RunAction.
    fn expand_command_line_and_worker<'v>(
        &'v self,
        action_execution_ctx: &dyn ActionExecutionCtx,
        artifact_visitor: &mut RunActionVisitor<'v>,
    ) -> buck2_error::Result<(
        ExpandedCommandLine,
        ExpandedCommandLineDigestForDepFiles,
        Option<WorkerSpec>,
        Option<RemoteWorkerSpec>,
    )> {
        let fs = &action_execution_ctx.executor_fs();
        let mut cli_ctx = DefaultCommandLineContext::new(fs);
        let values = Self::unpack(&self.starlark_values)?;

        let mut command_line_digest_for_dep_files = ExpandedCommandLineFingerprinter::new();

        let mut exe_rendered = Vec::<String>::new();

        // Creating the artifact_path_mapping isn't free, because we have to iterate TSets.
        // Therefore, only create a mapping if we're going to use it - i.e. if the input
        // is not hidden.
        let mut skip_hidden_visitor = SkipHiddenCommandLineArtifactVisitor::new();
        self.visit_artifacts(&mut skip_hidden_visitor)?;
        let artifact_path_mapping =
            action_execution_ctx.artifact_path_mapping(Some(skip_hidden_visitor.inputs));
        let artifact_path_mapping_for_dep_files = DepFilesPlaceholderArtifactPathMapper {};
        values
            .exe
            .add_to_command_line(&mut exe_rendered, &mut cli_ctx, &artifact_path_mapping)?;
        values.exe.add_to_command_line(
            &mut command_line_digest_for_dep_files,
            &mut cli_ctx,
            &artifact_path_mapping_for_dep_files,
        )?;
        values.exe.visit_artifacts(artifact_visitor)?;
        command_line_digest_for_dep_files.push_count();

        let worker = if let Some(worker) = values.worker {
            let mut worker_rendered = Vec::<String>::new();
            let mut local_worker_visitor = SimpleCommandLineArtifactVisitor::new();
            worker.exe.add_to_command_line(
                &mut worker_rendered,
                &mut cli_ctx,
                &artifact_path_mapping,
            )?;
            worker.exe.add_to_command_line(
                &mut command_line_digest_for_dep_files,
                &mut cli_ctx,
                &artifact_path_mapping_for_dep_files,
            )?;
            worker.exe.visit_artifacts(&mut local_worker_visitor)?;
            let worker_env: buck2_error::Result<SortedVectorMap<_, _>> = worker
                .env
                .into_iter()
                .map(|(k, v)| {
                    let mut env = String::new();
                    let mut ctx = DefaultCommandLineContext::new(fs);
                    v.add_to_command_line(
                        &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut env),
                        &mut ctx,
                        &artifact_path_mapping,
                    )?;
                    v.visit_artifacts(&mut local_worker_visitor)?;

                    command_line_digest_for_dep_files.push_arg(k.to_owned());
                    v.add_to_command_line(
                        &mut command_line_digest_for_dep_files,
                        &mut ctx,
                        &artifact_path_mapping_for_dep_files,
                    )?;
                    command_line_digest_for_dep_files.push_count();
                    Ok((k.to_owned(), env))
                })
                .collect();

            let local_worker_inputs: Vec<&ArtifactGroupValues> = local_worker_visitor
                .inputs()
                .map(|group| action_execution_ctx.artifact_values(group))
                .collect();

            let inputs: Vec<CommandExecutionInput> = local_worker_inputs[..]
                .map(|&i| CommandExecutionInput::Artifact(Box::new(i.dupe())));

            let input_paths = CommandExecutionPaths::new(
                inputs,
                IndexSet::new(),
                action_execution_ctx.fs(),
                action_execution_ctx.digest_config(),
                action_execution_ctx
                    .run_action_knobs()
                    .action_paths_interner
                    .as_ref(),
            )?;

            let worker_key = if worker.supports_bazel_remote_persistent_worker_protocol {
                let mut worker_visitor = SimpleCommandLineArtifactVisitor::new();
                worker.exe.visit_artifacts(&mut worker_visitor)?;
                if !worker_visitor.declared_outputs.is_empty()
                    && !worker_visitor.frozen_outputs.is_empty()
                {
                    // TODO[AH] create appropriate error enum value.
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::ActionMismatchedOutputs,
                        "Remote persistent worker command should not produce outputs."
                    ));
                }
                let worker_inputs: Vec<&ArtifactGroupValues> = worker_visitor
                    .inputs()
                    .map(|group| action_execution_ctx.artifact_values(group))
                    .collect();
                let (_, worker_digest) = metadata_content(
                    fs.fs(),
                    &worker_inputs,
                    action_execution_ctx.digest_config(),
                )?;
                Some(worker_digest)
            } else {
                None
            };

            Some(WorkerSpec {
                exe: worker_rendered,
                id: worker.id,
                env: worker_env?,
                concurrency: worker.concurrency,
                streaming: worker.streaming,
                remote_key: worker_key,
                input_paths,
            })
        } else {
            None
        };

        let remote_worker = if let Some(remote_worker) = values.remote_worker {
            let mut remote_worker_init_visitor = SimpleCommandLineArtifactVisitor::new();
            let mut remote_worker_init_rendered = Vec::<String>::new();
            remote_worker.exe.add_to_command_line(
                &mut remote_worker_init_rendered,
                &mut cli_ctx,
                &artifact_path_mapping,
            )?;
            remote_worker.exe.add_to_command_line(
                &mut command_line_digest_for_dep_files,
                &mut cli_ctx,
                &artifact_path_mapping_for_dep_files,
            )?;
            remote_worker
                .exe
                .visit_artifacts(&mut remote_worker_init_visitor)?;

            let remote_worker_env: buck2_error::Result<SortedVectorMap<_, _>> = remote_worker
                .env
                .into_iter()
                .map(|(k, v)| {
                    let mut env = String::new();
                    let mut ctx = DefaultCommandLineContext::new(fs);
                    v.add_to_command_line(
                        &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut env),
                        &mut ctx,
                        &artifact_path_mapping,
                    )?;
                    v.visit_artifacts(&mut remote_worker_init_visitor)?;

                    command_line_digest_for_dep_files.push_arg(k.to_owned());
                    v.add_to_command_line(
                        &mut command_line_digest_for_dep_files,
                        &mut ctx,
                        &artifact_path_mapping_for_dep_files,
                    )?;
                    command_line_digest_for_dep_files.push_count();
                    Ok((k.to_owned(), env))
                })
                .collect();

            let artifact_inputs: Vec<&ArtifactGroupValues> = remote_worker_init_visitor
                .inputs()
                .map(|group| action_execution_ctx.artifact_values(group))
                .collect();

            let inputs: Vec<CommandExecutionInput> =
                artifact_inputs[..].map(|&i| CommandExecutionInput::Artifact(Box::new(i.dupe())));

            let input_paths = CommandExecutionPaths::new(
                inputs,
                IndexSet::new(),
                action_execution_ctx.fs(),
                action_execution_ctx.digest_config(),
                action_execution_ctx
                    .run_action_knobs()
                    .action_paths_interner
                    .as_ref(),
            )?;
            Some(RemoteWorkerSpec {
                id: remote_worker.id,
                init: remote_worker_init_rendered,
                env: remote_worker_env?,
                input_paths,
                concurrency: remote_worker.concurrency,
            })
        } else {
            None
        };

        let mut args_rendered = Vec::<String>::new();
        values.args.add_to_command_line(
            &mut args_rendered,
            &mut cli_ctx,
            &artifact_path_mapping,
        )?;
        values.args.add_to_command_line(
            &mut command_line_digest_for_dep_files,
            &mut cli_ctx,
            &artifact_path_mapping_for_dep_files,
        )?;
        values.args.visit_artifacts(artifact_visitor)?;
        command_line_digest_for_dep_files.push_count();

        let env_len = values.env.len();
        let cli_env: buck2_error::Result<SortedVectorMap<_, _>> = values
            .env
            .into_iter()
            .map(|(k, v)| {
                let mut env = String::new();
                let mut ctx = DefaultCommandLineContext::new(fs);
                v.add_to_command_line(
                    &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut env),
                    &mut ctx,
                    &artifact_path_mapping,
                )?;
                v.visit_artifacts(artifact_visitor)?;

                command_line_digest_for_dep_files.push_arg(k.to_owned());
                v.add_to_command_line(
                    &mut command_line_digest_for_dep_files,
                    &mut ctx,
                    &artifact_path_mapping_for_dep_files,
                )?;
                command_line_digest_for_dep_files.push_count();
                Ok((k.to_owned(), env))
            })
            .collect();

        command_line_digest_for_dep_files.push_arg(env_len.to_string());
        command_line_digest_for_dep_files.push_count();

        Ok((
            ExpandedCommandLine {
                exe: exe_rendered,
                args: args_rendered,
                env: cli_env?,
            },
            command_line_digest_for_dep_files.finalize(),
            worker,
            remote_worker,
        ))
    }

    pub(crate) fn new(
        inner: UnregisteredRunAction,
        starlark_values: OwnedFrozenValue,
        outputs: IndexSet<BuildArtifact>,
        error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Self> {
        let starlark_values = starlark_values
            .downcast_starlark()
            .internal_error("Must be `RunActionValues`")?;

        Self::unpack(&starlark_values)?;

        // This is checked when declared, but we depend on it so make it clear that it's enforced.
        if outputs.is_empty() {
            return Err(RunActionError::NoOutputsSpecified.into());
        }

        Ok(RunAction {
            inner,
            starlark_values,
            outputs: BoxSliceSet::from(outputs),
            error_handler,
            input_files_bytes: 0,
        })
    }

    async fn prepare<'v>(
        &'v self,
        visitor: &mut RunActionVisitor<'v>,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> buck2_error::Result<(
        PreparedRunAction,
        ExpandedCommandLineDigestForDepFiles,
        HostSharingRequirements,
    )> {
        let (expanded, expanded_command_line_digest_for_dep_files, worker, remote_worker) =
            self.expand_command_line_and_worker(ctx, visitor)?;

        let executor_fs = ctx.executor_fs();
        let fs = executor_fs.fs();

        // TODO (@torozco): At this point, might as well just receive the list already. Finding
        // those things in a HashMap is just not very useful.
        let artifact_inputs: Vec<&ArtifactGroupValues> = visitor
            .inputs()
            .map(|group| ctx.artifact_values(group))
            .collect();

        let mut inputs: Vec<CommandExecutionInput> =
            artifact_inputs[..].map(|&i| CommandExecutionInput::Artifact(Box::new(i.dupe())));

        let mut extra_env = Vec::new();
        let cli_ctx = DefaultCommandLineContext::new(&executor_fs);
        self.prepare_action_metadata(ctx, &cli_ctx, fs, visitor, &mut inputs, &mut extra_env)
            .await?;

        let mut shared_content_based_paths = Vec::new();
        self.prepare_scratch_path(
            ctx,
            &cli_ctx,
            fs,
            &mut inputs,
            &mut shared_content_based_paths,
            &mut extra_env,
        )?;

        for output in self.outputs.iter() {
            if output.get_path().is_content_based_path() {
                let full_path = cli_ctx
                    .resolve_project_path(fs.buck_out_path_resolver().resolve_gen(
                        &output.get_path(),
                        Some(&ContentBasedPathHash::for_output_artifact()),
                    )?)?
                    .into_string();
                shared_content_based_paths.push(full_path);
            }
        }

        // TODO(ianc) Only do this if we're actually going to run the action?
        let host_sharing_requirements = if !shared_content_based_paths.is_empty() {
            HostSharingRequirements::OnePerTokens(
                shared_content_based_paths.into(),
                self.inner.weight,
            )
        } else {
            HostSharingRequirements::Shared(self.inner.weight)
        };

        let paths = CommandExecutionPaths::new(
            inputs,
            self.outputs
                .iter()
                .map(|b| CommandExecutionOutput::BuildArtifact {
                    path: b.get_path().dupe(),
                    output_type: b.output_type(),
                    supports_incremental_remote: self.inner.incremental_remote_outputs,
                })
                .collect(),
            ctx.fs(),
            ctx.digest_config(),
            ctx.run_action_knobs().action_paths_interner.as_ref(),
        )?;

        Ok((
            PreparedRunAction {
                expanded,
                extra_env,
                paths,
                worker,
                remote_worker,
            },
            expanded_command_line_digest_for_dep_files,
            host_sharing_requirements,
        ))
    }

    /// Handle case when user requested file with action metadata to be generated.
    /// Generate content and output path for the file. It will be either passed
    /// to RE as a blob or written to disk in local executor.
    /// Path to this file is passed to user in environment variable which is selected by user.
    async fn prepare_action_metadata(
        &self,
        ctx: &dyn ActionExecutionCtx,
        cli_ctx: &DefaultCommandLineContext<'_>,
        fs: &ArtifactFs,
        visitor: &mut RunActionVisitor<'_>,
        inputs: &mut Vec<CommandExecutionInput>,
        extra_env: &mut Vec<(String, String)>,
    ) -> buck2_error::Result<()> {
        if let Some(metadata_param) = &self.inner.metadata_param {
            let path = BuildArtifactPath::new(
                ctx.target().owner().dupe(),
                metadata_param.path.clone(),
                if self.all_outputs_are_content_based() {
                    BuckOutPathKind::ContentHash
                } else {
                    BuckOutPathKind::Configuration
                },
            );

            let artifact_inputs: Vec<&ArtifactGroupValues> = visitor
                .incremental_metadata_inputs
                .iter()
                .map(|group| ctx.artifact_values(group))
                .collect();
            let (data, digest) = metadata_content(fs, &artifact_inputs, ctx.digest_config())?;
            let content_hash = ContentBasedPathHash::new(digest.raw_digest().as_bytes())?;
            let project_rel_path = fs
                .buck_out_path_resolver()
                .resolve_gen(&path, Some(&content_hash))?;

            ctx.materializer()
                .declare_write(Box::new(|| {
                    Ok(vec![WriteRequest {
                        path: project_rel_path.clone(),
                        content: data.0.0,
                        is_executable: false,
                    }])
                }))
                .await
                .buck_error_context("Failed to write action metadata!")?;

            inputs.push(CommandExecutionInput::ActionMetadata(ActionMetadataBlob {
                digest,
                path,
                content_hash,
            }));

            let env = cli_ctx
                .resolve_project_path(project_rel_path)?
                .into_string();
            extra_env.push((metadata_param.env_var.to_owned(), env));
        }
        Ok(())
    }

    fn prepare_scratch_path(
        &self,
        ctx: &dyn ActionExecutionCtx,
        cli_ctx: &DefaultCommandLineContext,
        fs: &ArtifactFs,
        inputs: &mut Vec<CommandExecutionInput>,
        shared_content_based_paths: &mut Vec<String>,
        extra_env: &mut Vec<(String, String)>,
    ) -> buck2_error::Result<()> {
        let scratch = ctx.target().scratch_path();
        let scratch_path = cli_ctx
            .resolve_project_path(fs.buck_out_path_resolver().resolve_scratch(&scratch)?)?
            .into_string();

        if scratch.uses_content_hash() {
            shared_content_based_paths.push(scratch_path.to_owned());
        }

        extra_env.push(("BUCK_SCRATCH_PATH".to_owned(), scratch_path));
        inputs.push(CommandExecutionInput::ScratchPath(scratch));

        Ok(())
    }

    pub(crate) async fn check_cache_result_is_useable(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        request: &CommandExecutionRequest,
        action_digest: &ActionDigest,
        result: CommandExecutionResult,
        dep_file_bundle: &DepFileBundle,
        remote_dep_file_key: &DepFileDigest,
    ) -> buck2_error::Result<ControlFlow<CommandExecutionResult, CommandExecutionManager>> {
        // If it's served by the regular action cache no need to verify anything here.
        if !result.was_served_by_remote_dep_file_cache() {
            return Ok(ControlFlow::Break(result));
        }

        if let Some(found_dep_file_entry) = &result.dep_file_metadata {
            let can_use = span_async_simple(
                buck2_data::MatchDepFilesStart {
                    checking_filtered_inputs: true,
                    remote_cache: true,
                },
                dep_file_bundle.check_remote_dep_file_entry(
                    ctx.digest_config(),
                    ctx.fs(),
                    ctx.materializer(),
                    found_dep_file_entry,
                    &result,
                ),
                buck2_data::MatchDepFilesEnd {},
            )
            .await?;

            if can_use {
                tracing::info!(
                    "Action result is cached via remote dep file cache, skipping execution of :\n```\n$ {}\n```\n for action `{}` with remote dep file key `{}`",
                    request.all_args_str(),
                    action_digest,
                    &remote_dep_file_key,
                );
                return Ok(ControlFlow::Break(result));
            }
        }
        // Continue through other options below
        Ok(ControlFlow::Continue(ctx.command_execution_manager()))
    }

    async fn execute_inner(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<ExecuteResult, ExecuteError> {
        let incremental_action_ignore_tags = self
            .inner
            .metadata_param
            .as_ref()
            .map(|metadata_param| &metadata_param.ignore_tags);
        let mut run_action_visitor =
            RunActionVisitor::new(&self.inner.dep_files, incremental_action_ignore_tags);
        let (prepared_run_action, cmdline_digest_for_dep_files, host_sharing_requirements) =
            self.prepare(&mut run_action_visitor, ctx).await?;

        let dep_file_bundle = make_dep_file_bundle(
            ctx,
            run_action_visitor.dep_files_visitor,
            cmdline_digest_for_dep_files,
            &prepared_run_action.paths,
            prepared_run_action.worker.as_ref().map(|w| &w.input_paths),
        )?;

        // First, check in the local dep file cache if an identical action can be found there.
        // Do this before checking the action cache as we can avoid a potentially large download.
        // Once the action cache lookup misses, we will do the full dep file cache look up.
        let (outputs, should_fully_check_dep_file_cache) = dep_file_bundle
            .check_local_dep_file_cache_for_identical_action(ctx, self.outputs.as_slice())
            .await?;
        if let Some((outputs, metadata)) = outputs {
            return Ok(ExecuteResult::LocalDepFileHit(outputs, metadata));
        }

        let req =
            self.command_execution_request(ctx, prepared_run_action, host_sharing_requirements)?;

        // Prepare the action, check the action cache, fully check the local dep file cache if needed, then execute the command
        let prepared_action = ctx.prepare_action(&req)?;
        let manager = ctx.command_execution_manager();

        let action_cache_result = ctx.action_cache(manager, &req, &prepared_action).await;

        let (req, result) = match action_cache_result {
            ControlFlow::Break(_) => (req, action_cache_result),
            ControlFlow::Continue(manager) => {
                // If we didn't find anything in the action cache, first do a local dep file cache lookup, and if that fails,
                // try to find a remote dep file cache hit.
                if should_fully_check_dep_file_cache {
                    let lookup = dep_file_bundle
                        .check_local_dep_file_cache(ctx, self.outputs.as_slice())
                        .await?;
                    if let Some((outputs, metadata)) = lookup {
                        return Ok(ExecuteResult::LocalDepFileHit(outputs, metadata));
                    }
                }

                let supports_remote_dep_files =
                    self.inner.allow_dep_file_cache_upload && dep_file_bundle.has_dep_files();

                // Enable remote dep file cache lookup for actions that have remote depfile uploads enabled.
                if supports_remote_dep_files {
                    let remote_dep_file_key = dep_file_bundle
                        .remote_dep_file_action(
                            ctx.digest_config(),
                            ctx.mergebase().0.as_ref(),
                            ctx.re_platform(),
                        )
                        .action
                        .coerce();
                    let req = req.with_remote_dep_file_key(&remote_dep_file_key);
                    let remote_dep_file_result = ctx
                        .remote_dep_file_cache(manager, &req, &prepared_action)
                        .await;
                    if let ControlFlow::Break(res) = remote_dep_file_result {
                        // If the result was served by the remote dep file cache, we can't use the result just yet. We need to verify that
                        // the inputs tracked by a depfile that was actually used in the cache hit are identical to the inputs we have for this action.
                        let res = self
                            .check_cache_result_is_useable(
                                ctx,
                                &req,
                                &prepared_action.action_and_blobs.action,
                                res,
                                &dep_file_bundle,
                                &remote_dep_file_key,
                            )
                            .await?;
                        (req, res)
                    } else {
                        (req, remote_dep_file_result)
                    }
                } else {
                    (req, ControlFlow::Continue(manager))
                }
            }
        };

        // If the cache queries did not yield to a result, then we need to execute the action.
        let (result, req, action_and_blobs) = match result {
            ControlFlow::Break(res) => (res, req, prepared_action.action_and_blobs),
            ControlFlow::Continue(manager) => {
                let (req, prepared_action) = if self.inner.incremental_remote_outputs {
                    // For the case of incremental remote outputs, we checked the caches using the action which
                    // does not include the outputs as inputs.
                    // To execute such action we first prepare a different action with the outputs added as inputs.
                    let output_paths_as_inputs = self.output_paths_as_inputs(ctx).await?;
                    if !output_paths_as_inputs.is_empty() {
                        let executor_fs = ctx.executor_fs();
                        let fs = executor_fs.fs();
                        let digest_config = ctx.digest_config();
                        let override_req = req.with_outputs_paths_added_as_inputs(
                            output_paths_as_inputs,
                            fs,
                            digest_config,
                            ctx.run_action_knobs().action_paths_interner.as_ref(),
                        )?;
                        let override_prepared_action = ctx.prepare_action(&override_req)?;
                        (override_req, override_prepared_action)
                    } else {
                        (req, prepared_action)
                    }
                } else {
                    (req, prepared_action)
                };
                let execution_result = ctx.exec_cmd(manager, &req, &prepared_action).await;
                (execution_result, req, prepared_action.action_and_blobs)
            }
        };

        Ok(ExecuteResult::ExecutedOrReHit {
            result,
            dep_file_bundle,
            executor_preference: req.executor_preference,
            action_and_blobs,
            input_files_bytes: req.paths().input_files_bytes(),
        })
    }

    async fn output_paths_as_inputs(
        &self,
        ctx: &dyn ActionExecutionCtx,
    ) -> buck2_error::Result<Vec<CommandExecutionInput>> {
        let executor_fs = ctx.executor_fs();
        let fs = executor_fs.fs();
        let output_paths = {
            let mut output_paths = Vec::new();
            for output in &self.outputs {
                // TODO(T219919866): support content based paths
                let path = fs.resolve_build(output.get_path(), None)?;
                output_paths.push(path);
            }
            output_paths
        };
        let entries = ctx
            .materializer()
            .get_artifact_entries_for_materialized_paths(output_paths)
            .await?;
        // Only proceed with incremental outputs if every output is present
        Ok(entries
            .into_iter()
            .map(|entry| entry.map(|(p, e)| CommandExecutionInput::IncrementalRemoteOutput(p, e)))
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default())
    }

    fn command_execution_request(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        prepared_run_action: PreparedRunAction,
        host_sharing_requirements: HostSharingRequirements,
    ) -> buck2_error::Result<CommandExecutionRequest> {
        let outputs_for_error_handler = self.outputs_for_error_handler()?;
        let mut req = prepared_run_action
            .into_command_execution_request()
            .with_prefetch_lossy_stderr(true)
            .with_executor_preference(self.inner.executor_preference)
            .with_host_sharing_requirements(host_sharing_requirements.into())
            .with_low_pass_filter(self.inner.low_pass_filter)
            .with_outputs_cleanup(!self.inner.no_outputs_cleanup)
            .with_local_environment_inheritance(EnvironmentInheritance::local_command_exclusions())
            .with_force_full_hybrid_if_capable(self.inner.force_full_hybrid_if_capable)
            .with_unique_input_inodes(self.inner.unique_input_inodes)
            .with_remote_execution_dependencies(self.inner.remote_execution_dependencies.clone())
            .with_remote_execution_custom_image(
                self.inner.remote_execution_custom_image.clone().map(|s| *s),
            )
            .with_meta_internal_extra_params(self.inner.meta_internal_extra_params.clone())
            .with_outputs_for_error_handler(outputs_for_error_handler);

        if self.inner.no_outputs_cleanup {
            if self
                .outputs
                .iter()
                .any(|o| o.get_path().is_content_based_path())
            {
                req = req.with_run_action_key(Some(
                    // Using string representation as it is going to be stored in db which requires it to be a string
                    // doing it early here prevents us from exposing RunActionKey type
                    RunActionKey::from_action_execution_target(ctx.target()).to_string(),
                ));
            }
        }

        Ok(req)
    }

    fn outputs_for_error_handler(&self) -> buck2_error::Result<Vec<BuildArtifactPath>> {
        self.starlark_values
            .outputs_for_error_handler
            .iter()
            .map(|artifact| {
                let a = artifact.inner().artifact();

                match a.as_parts().0 {
                    BaseArtifactKind::Source(s) => Err(buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Cannot use source artifact `{}` as output for error handler",
                        s.get_path()
                    )),
                    BaseArtifactKind::Build(b) => Ok(b.get_path().dupe()),
                }
            })
            .collect()
    }
}

pub(crate) struct PreparedRunAction {
    expanded: ExpandedCommandLine,
    /// Environment which is added on top of the one coming from `ExpandedCommandLine::env`
    extra_env: Vec<(String, String)>,
    paths: CommandExecutionPaths,
    worker: Option<WorkerSpec>,
    remote_worker: Option<RemoteWorkerSpec>,
}

impl PreparedRunAction {
    fn into_command_execution_request(self) -> CommandExecutionRequest {
        let Self {
            expanded: ExpandedCommandLine { exe, args, mut env },
            extra_env,
            paths,
            worker,
            remote_worker,
        } = self;

        for (k, v) in extra_env {
            env.insert(k, v);
        }

        CommandExecutionRequest::new(exe, args, paths, env)
            .with_worker(worker)
            .with_remote_worker(remote_worker)
    }
}

pub struct RunActionVisitor<'a> {
    pub(crate) dep_files_visitor: DepFilesCommandLineVisitor<'a>,
    pub(crate) incremental_metadata_inputs: Vec<ArtifactGroup>,
    incremental_metadata_ignore_tags: Option<&'a SmallSet<ArtifactTag>>,
}

impl<'a> RunActionVisitor<'a> {
    pub(crate) fn new(
        dep_files: &'a RunActionDepFiles,
        incremental_metadata_ignore_tags: Option<&'a SmallSet<ArtifactTag>>,
    ) -> Self {
        Self {
            dep_files_visitor: DepFilesCommandLineVisitor::new(dep_files),
            incremental_metadata_inputs: Vec::new(),
            incremental_metadata_ignore_tags,
        }
    }

    pub(crate) fn inputs(&self) -> impl Iterator<Item = &ArtifactGroup> {
        self.dep_files_visitor.inputs()
    }
}

impl<'v> CommandLineArtifactVisitor<'v> for RunActionVisitor<'v> {
    fn visit_input(&mut self, input: ArtifactGroup, tags: Vec<&ArtifactTag>) {
        match self.incremental_metadata_ignore_tags {
            Some(ignore_tags) => {
                if !tags.iter().any(|t| ignore_tags.contains(*t)) {
                    self.incremental_metadata_inputs.push(input.dupe());
                }
            }
            // If incremental_metadata_ignore_tags is None, then we're not going to produce
            // incremental metadata at all, so there's nothing to do here.
            None => {}
        }

        self.dep_files_visitor.visit_input(input, tags);
    }

    fn visit_declared_output(&mut self, _artifact: OutputArtifact<'v>, tags: Vec<&ArtifactTag>) {
        self.dep_files_visitor
            .visit_declared_output(_artifact, tags);
    }

    fn visit_frozen_output(&mut self, artifact: Artifact, tags: Vec<&ArtifactTag>) {
        self.dep_files_visitor.visit_frozen_output(artifact, tags);
    }
}

#[async_trait]
impl Action for RunAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Run
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        self.visit_artifacts(&mut artifact_visitor)?;
        Ok(Cow::Owned(artifact_visitor.inputs.into_iter().collect()))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(self.outputs.as_slice())
    }

    fn first_output(&self) -> &BuildArtifact {
        // Required to have outputs on construction
        &self.outputs.as_slice()[0]
    }

    fn category(&self) -> CategoryRef<'_> {
        CategoryRef::unchecked_new(self.starlark_values.category.as_str())
    }

    fn identifier(&self) -> Option<&str> {
        self.starlark_values.identifier.map(|x| x.as_str())
    }

    fn always_print_stderr(&self) -> bool {
        self.inner.always_print_stderr
    }

    fn aquery_attributes(
        &self,
        fs: &ExecutorFs,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> IndexMap<String, String> {
        let mut cli_rendered = Vec::<String>::new();
        let mut ctx = DefaultCommandLineContext::new(fs);
        let values = Self::unpack(&self.starlark_values).unwrap();
        values
            .exe
            .add_to_command_line(&mut cli_rendered, &mut ctx, artifact_path_mapping)
            .unwrap();
        values
            .args
            .add_to_command_line(&mut cli_rendered, &mut ctx, artifact_path_mapping)
            .unwrap();
        let cmd = format!("[{}]", cli_rendered.iter().join(", "));
        indexmap! {
            "cmd".to_owned() => cmd,
            "executor_preference".to_owned() => self.inner.executor_preference.to_string(),
            "always_print_stderr".to_owned() => self.inner.always_print_stderr.to_string(),
            "weight".to_owned() => self.inner.weight.to_string(),
            "dep_files".to_owned() => self.inner.dep_files.to_string(),
            "metadata_param".to_owned() => match &self.inner.metadata_param {
                None => "None".to_owned(),
                Some(x) => x.to_string(),
            },
            "no_outputs_cleanup".to_owned() => self.inner.no_outputs_cleanup.to_string(),
            "allow_cache_upload".to_owned() => match &self.inner.allow_cache_upload {
                None => "None".to_owned(),
                Some(x) => x.to_string(),
            },
            "allow_dep_file_cache_upload".to_owned() => self.inner.allow_dep_file_cache_upload.to_string(),
        }
    }

    fn error_handler(&self) -> Option<OwnedFrozenValue> {
        self.error_handler.clone()
    }

    fn failed_action_output_artifacts<'v>(
        &self,
        artifact_fs: &ArtifactFs,
        heap: &'v Heap,
        outputs: Option<&ActionOutputs>,
    ) -> buck2_error::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>>
    {
        let mut artifact_value_dict =
            Vec::with_capacity(self.starlark_values.outputs_for_error_handler.len());

        for x in self.starlark_values.outputs_for_error_handler.iter() {
            let artifact = x.inner().artifact();

            let content_based_path_hash = if artifact.has_content_based_path() {
                let outputs = outputs.ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Action failed with no outputs available"
                    )
                })?;
                let artifact_value = outputs
                    .get_from_artifact_path(&artifact.get_path())
                    .ok_or_else(|| {
                        buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "ArtifactValue for artifact `{}` was not found in action outputs",
                            artifact.get_path()
                        )
                    })?;
                Some(artifact_value.content_based_path_hash())
            } else {
                None
            };

            let path = artifact
                .get_path()
                .resolve(artifact_fs, content_based_path_hash.as_ref())?;

            let abs = artifact_fs.fs().resolve(&path);
            // Check if the output file specified exists. We will return an error if it doesn't
            if !fs_util::try_exists(&abs)? {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "Output '{}' defined for error handler does not exist. This is likely due to file not being created, please ensure the action would produce an output",
                    &path
                ));
            }

            let artifact_value = StarlarkArtifactValue::new(
                artifact.dupe(),
                path.to_owned(),
                artifact_fs.fs().dupe(),
            );
            let artifact = StarlarkArtifact::new(artifact);

            artifact_value_dict.push((artifact, artifact_value));
        }

        Ok(heap
            .alloc_typed_unchecked(AllocDict(artifact_value_dict))
            .cast())
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let (
            mut result,
            mut dep_file_bundle,
            executor_preference,
            action_and_blobs,
            input_files_bytes,
        ) = match self.execute_inner(ctx).await? {
            ExecuteResult::LocalDepFileHit(outputs, metadata) => {
                return Ok((outputs, metadata));
            }
            ExecuteResult::ExecutedOrReHit {
                result,
                dep_file_bundle,
                executor_preference,
                action_and_blobs,
                input_files_bytes,
            } => (
                result,
                dep_file_bundle,
                executor_preference,
                action_and_blobs,
                input_files_bytes,
            ),
        };

        let allow_cache_upload = self
            .inner
            .allow_cache_upload
            .unwrap_or_else(|| ctx.run_action_knobs().default_allow_cache_upload);
        let supports_remote_dep_files =
            self.inner.allow_dep_file_cache_upload && dep_file_bundle.has_dep_files();
        let incremental_kind = match (
            self.inner.no_outputs_cleanup,
            self.inner.incremental_remote_outputs,
        ) {
            (true, true) => buck2_data::IncrementalKind::IncrementalLocalAndRemote,
            (false, true) => buck2_data::IncrementalKind::IncrementalRemote,
            (true, false) => buck2_data::IncrementalKind::IncrementalLocal,
            (false, false) => buck2_data::IncrementalKind::NonIncremental,
        };

        // If there is a dep file entry AND if dep file cache upload is enabled, upload it
        if result.was_success()
            && !result.was_served_by_remote_dep_file_cache()
            && (allow_cache_upload || supports_remote_dep_files || force_cache_upload()?)
        {
            let re_result = result.action_result.take();
            let upload_result = ctx
                .cache_upload(
                    &action_and_blobs,
                    &result,
                    re_result,
                    // match needed for coercion, https://github.com/rust-lang/rust/issues/108999
                    if supports_remote_dep_files {
                        Some(&mut dep_file_bundle)
                    } else {
                        None
                    },
                )
                .await?;

            result.did_cache_upload = upload_result.did_cache_upload;
            result.did_dep_file_cache_upload = upload_result.did_dep_file_cache_upload;
            result.dep_file_key = upload_result.dep_file_cache_upload_key;
        }

        let was_locally_executed = result.was_locally_executed();
        let (outputs, metadata) = ctx.unpack_command_execution_result(
            executor_preference,
            result,
            allow_cache_upload,
            self.inner.allow_dep_file_cache_upload,
            Some(input_files_bytes),
            incremental_kind,
        )?;

        populate_dep_files(ctx, dep_file_bundle, &outputs, was_locally_executed).await?;

        Ok((outputs, metadata))
    }
}

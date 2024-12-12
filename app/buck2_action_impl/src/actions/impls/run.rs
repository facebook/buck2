/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::ops::ControlFlow;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLine;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::IncrementalActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::FrozenStarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::FrozenWorkerInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;
use buck2_core::category::CategoryRef;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_error::buck2_error;
use buck2_error::starlark_error::from_starlark;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::cache_uploader::force_cache_upload;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::request::ActionMetadataBlob;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_execute::execute::result::CommandExecutionResult;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;
use host_sharing::WeightClass;
use indexmap::indexmap;
use indexmap::IndexSet;
use itertools::Itertools;
use serde_json::json;
use sorted_vector_map::SortedVectorMap;
use starlark::values::dict::DictRef;
use starlark::values::dict::DictType;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValueOfUnchecked;
use starlark::values::FrozenValueTyped;
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

use self::dep_files::DepFileBundle;
use crate::actions::impls::run::dep_files::make_dep_file_bundle;
use crate::actions::impls::run::dep_files::populate_dep_files;
use crate::actions::impls::run::dep_files::DepFilesCommandLineVisitor;
use crate::actions::impls::run::dep_files::RunActionDepFiles;
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
}

impl Display for MetadataParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let json = json!({
            "env_var": self.env_var,
            "path": self.path,
        });
        write!(f, "{}", json)
    }
}

#[derive(Debug, buck2_error::Error)]
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
    pub(crate) allow_cache_upload: bool,
    pub(crate) allow_dep_file_cache_upload: bool,
    pub(crate) force_full_hybrid_if_capable: bool,
    pub(crate) unique_input_inodes: bool,
    pub(crate) remote_execution_dependencies: Vec<RemoteExecutorDependency>,
    pub(crate) remote_execution_custom_image: Option<RemoteExecutorCustomImage>,
}

impl UnregisteredAction for UnregisteredRunAction {
    fn register(
        self: Box<Self>,
        _: IndexSet<ArtifactGroup>,
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
#[display("run_action_values")]
pub(crate) struct StarlarkRunActionValues<'v> {
    pub(crate) exe: ValueTyped<'v, StarlarkCmdArgs<'v>>,
    pub(crate) args: ValueTyped<'v, StarlarkCmdArgs<'v>>,
    pub(crate) env: Option<ValueOfUnchecked<'v, DictType<String, ValueAsCommandLineLike<'static>>>>,
    pub(crate) worker: Option<ValueTypedComplex<'v, WorkerInfo<'v>>>,
    pub(crate) remote_worker: Option<ValueTypedComplex<'v, WorkerInfo<'v>>>,
    pub(crate) category: StringValue<'v>,
    pub(crate) identifier: Option<StringValue<'v>>,
}

#[derive(Debug, Display, Trace, ProvidesStaticType, NoSerialize, Allocative)]
#[display("run_action_values")]
pub(crate) struct FrozenStarlarkRunActionValues {
    pub(crate) exe: FrozenValueTyped<'static, FrozenStarlarkCmdArgs>,
    pub(crate) args: FrozenValueTyped<'static, FrozenStarlarkCmdArgs>,
    pub(crate) env:
        Option<FrozenValueOfUnchecked<'static, DictType<String, ValueAsCommandLineLike<'static>>>>,
    pub(crate) worker: Option<FrozenValueTyped<'static, FrozenWorkerInfo>>,
    pub(crate) remote_worker: Option<FrozenValueTyped<'static, FrozenWorkerInfo>>,
    pub(crate) category: FrozenStringValue,
    pub(crate) identifier: Option<FrozenStringValue>,
}

#[starlark_value(type = "run_action_values")]
impl<'v> StarlarkValue<'v> for StarlarkRunActionValues<'v> {}

#[starlark_value(type = "run_action_values")]
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
        } = self;
        Ok(FrozenStarlarkRunActionValues {
            exe: FrozenValueTyped::new_err(exe.to_value().freeze(freezer)?)
                .map_err(|e| FreezeError::new(format!("{e}")))?,
            args: FrozenValueTyped::new_err(args.to_value().freeze(freezer)?)
                .map_err(|e| FreezeError::new(format!("{e}")))?,
            env: env.freeze(freezer)?,
            worker: worker.freeze(freezer)?,
            remote_worker: remote_worker.freeze(freezer)?,
            category: category.freeze(freezer)?,
            identifier: identifier.freeze(freezer)?,
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
}

struct UnpackedWorkerValues<'v> {
    exe: &'v dyn CommandLineArgLike,
    id: WorkerId,
    concurrency: Option<usize>,
    remote: bool,
}

struct UnpackedRunActionValues<'v> {
    exe: &'v dyn CommandLineArgLike,
    args: &'v dyn CommandLineArgLike,
    env: Vec<(&'v str, &'v dyn CommandLineArgLike)>,
    worker: Option<UnpackedWorkerValues<'v>>,
}

#[derive(Debug, Allocative)]
pub(crate) struct RunAction {
    inner: UnregisteredRunAction,
    starlark_values: OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    outputs: BoxSliceSet<BuildArtifact>,
    error_handler: Option<OwnedFrozenValue>,
    input_files_bytes: u64,
}

enum ExecuteResult {
    LocalDepFileHit(ActionOutputs, ActionExecutionMetadata),
    ExecutedOrReHit {
        result: CommandExecutionResult,
        dep_file_bundle: Option<DepFileBundle>,
        executor_preference: ExecutorPreference,
        prepared_action: PreparedAction,
        input_files_bytes: u64,
    },
}

impl RunAction {
    fn unpack(
        values: &OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    ) -> buck2_error::Result<UnpackedRunActionValues> {
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
            id: WorkerId(worker.id),
            concurrency: worker.concurrency(),
            remote: worker.remote(),
        });

        Ok(UnpackedRunActionValues {
            exe,
            args,
            env,
            worker,
        })
    }

    /// Get the command line expansion for this RunAction.
    fn expand_command_line_and_worker(
        &self,
        action_execution_ctx: &dyn ActionExecutionCtx,
        artifact_visitor: &mut impl CommandLineArtifactVisitor,
        actx: &dyn ActionExecutionCtx,
    ) -> buck2_error::Result<(ExpandedCommandLine, Option<WorkerSpec>)> {
        let fs = &action_execution_ctx.executor_fs();
        let mut cli_ctx = DefaultCommandLineContext::new(fs);
        let values = Self::unpack(&self.starlark_values)?;

        let mut exe_rendered = Vec::<String>::new();
        values
            .exe
            .add_to_command_line(&mut exe_rendered, &mut cli_ctx)?;
        values.exe.visit_artifacts(artifact_visitor)?;

        let worker = if let Some(worker) = values.worker {
            let mut worker_rendered = Vec::<String>::new();
            worker
                .exe
                .add_to_command_line(&mut worker_rendered, &mut cli_ctx)?;
            worker.exe.visit_artifacts(artifact_visitor)?;
            let worker_key = if worker.remote {
                let mut worker_visitor = SimpleCommandLineArtifactVisitor::new();
                worker.exe.visit_artifacts(&mut worker_visitor)?;
                if !worker_visitor.outputs.is_empty() {
                    // TODO[AH] create appropriate error enum value.
                    return Err(buck2_error!(
                        [],
                        "remote persistent worker command should not produce an output"
                    ));
                }
                let worker_inputs: Vec<&ArtifactGroupValues> = worker_visitor
                    .inputs()
                    .map(|group| actx.artifact_values(group))
                    .collect();
                let (_, worker_digest) =
                    metadata_content(fs.fs(), &worker_inputs, actx.digest_config())?;
                Some(worker_digest)
            } else {
                None
            };
            Some(WorkerSpec {
                exe: worker_rendered,
                id: worker.id,
                concurrency: worker.concurrency,
                remote_key: worker_key,
            })
        } else {
            None
        };

        let mut args_rendered = Vec::<String>::new();
        values
            .args
            .add_to_command_line(&mut args_rendered, &mut cli_ctx)?;
        values.args.visit_artifacts(artifact_visitor)?;

        let cli_env: buck2_error::Result<SortedVectorMap<_, _>> = values
            .env
            .into_iter()
            .map(|(k, v)| {
                let mut env = String::new();
                let mut ctx = DefaultCommandLineContext::new(fs);
                v.add_to_command_line(
                    &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut env),
                    &mut ctx,
                )?;
                v.visit_artifacts(artifact_visitor)?;
                Ok((k.to_owned(), env))
            })
            .collect();

        Ok((
            ExpandedCommandLine {
                exe: exe_rendered,
                args: args_rendered,
                env: cli_env?,
            },
            worker,
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
            .map_err(from_starlark)
            .internal_error("Must be `run_action_values`")?;

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

    fn prepare(
        &self,
        visitor: &mut impl RunActionVisitor,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> buck2_error::Result<PreparedRunAction> {
        let executor_fs = ctx.executor_fs();
        let fs = executor_fs.fs();

        let (expanded, worker) = self.expand_command_line_and_worker(ctx, visitor, ctx)?;

        // TODO (@torozco): At this point, might as well just receive the list already. Finding
        // those things in a HashMap is just not very useful.
        let artifact_inputs: Vec<&ArtifactGroupValues> = visitor
            .inputs()
            .map(|group| ctx.artifact_values(group))
            .collect();

        let mut inputs: Vec<CommandExecutionInput> =
            artifact_inputs[..].map(|&i| CommandExecutionInput::Artifact(Box::new(i.dupe())));

        // Handle case when user requested file with action metadata to be generated.
        // Generate content and output path for the file. It will be either passed
        // to RE as a blob or written to disk in local executor.
        // Path to this file is passed to user in environment variable which is selected by user.
        let cli_ctx = DefaultCommandLineContext::new(&executor_fs);

        let mut extra_env = Vec::new();

        if let Some(metadata_param) = &self.inner.metadata_param {
            let path =
                BuildArtifactPath::new(ctx.target().owner().dupe(), metadata_param.path.clone());
            let env = cli_ctx
                .resolve_project_path(fs.buck_out_path_resolver().resolve_gen(&path))?
                .into_string();
            let (data, digest) = metadata_content(fs, &artifact_inputs, ctx.digest_config())?;
            inputs.push(CommandExecutionInput::ActionMetadata(ActionMetadataBlob {
                data,
                digest,
                path,
            }));
            extra_env.push((metadata_param.env_var.to_owned(), env));
        }

        let scratch = ctx.target().scratch_path();
        let scratch_path = fs.buck_out_path_resolver().resolve_scratch(&scratch);
        extra_env.push((
            "BUCK_SCRATCH_PATH".to_owned(),
            cli_ctx.resolve_project_path(scratch_path)?.into_string(),
        ));
        inputs.push(CommandExecutionInput::ScratchPath(scratch));

        let paths = CommandExecutionPaths::new(
            inputs,
            self.outputs
                .iter()
                .map(|b| CommandExecutionOutput::BuildArtifact {
                    path: b.get_path().dupe(),
                    output_type: b.output_type(),
                })
                .collect(),
            ctx.fs(),
            ctx.digest_config(),
        )?;

        Ok(PreparedRunAction {
            expanded,
            extra_env,
            paths,
            worker,
        })
    }

    pub(crate) async fn check_cache_result_is_useable(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        request: &CommandExecutionRequest,
        action_digest: &ActionDigest,
        result: CommandExecutionResult,
        dep_file_bundle: &Option<DepFileBundle>,
    ) -> buck2_error::Result<ControlFlow<CommandExecutionResult, CommandExecutionManager>> {
        // If it's served by the regular action cache no need to verify anything here.
        if !result.was_served_by_remote_dep_file_cache() {
            return Ok(ControlFlow::Break(result));
        }

        if let Some(bundle) = dep_file_bundle {
            if let Some(found_dep_file_entry) = &result.dep_file_metadata {
                let can_use = span_async_simple(
                    buck2_data::MatchDepFilesStart {
                        checking_filtered_inputs: true,
                        remote_cache: true,
                    },
                    bundle.check_remote_dep_file_entry(
                        ctx.digest_config(),
                        ctx.fs(),
                        ctx.materializer(),
                        found_dep_file_entry,
                    ),
                    buck2_data::MatchDepFilesEnd {},
                )
                .await?;

                if can_use {
                    tracing::info!(
                        "Action result is cached via remote dep file cache, skipping execution of :\n```\n$ {}\n```\n for action `{}` with remote dep file key `{}`",
                        request.all_args_str(),
                        action_digest,
                        bundle.remote_dep_file_action.action,
                    );
                    return Ok(ControlFlow::Break(result));
                }
            } else {
                // This should not happen as we check for the metadata on the cache querier side.
                tracing::debug!(
                    "The remote dep file cache returned a hit for `{}`, but there is no metadata",
                    bundle.remote_dep_file_action.action
                );
            }
        }
        // Continue through other options below
        Ok(ControlFlow::Continue(ctx.command_execution_manager()))
    }

    async fn execute_inner(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<ExecuteResult, ExecuteError> {
        let knobs = ctx.run_action_knobs();
        let process_dep_files = !self.inner.dep_files.labels.is_empty() || knobs.hash_all_commands;
        let (prepared_run_action, dep_file_visitor) = if !process_dep_files {
            (
                self.prepare(&mut SimpleCommandLineArtifactVisitor::new(), ctx)?,
                None,
            )
        } else {
            let mut visitor = DepFilesCommandLineVisitor::new(&self.inner.dep_files);
            let prepared = self.prepare(&mut visitor, ctx)?;
            (prepared, Some(visitor))
        };
        let cmdline_digest = prepared_run_action.expanded.fingerprint();
        let input_files_bytes = prepared_run_action.paths.input_files_bytes();
        // Run actions are assumed to be shared
        let host_sharing_requirements = HostSharingRequirements::Shared(self.inner.weight);

        let req = prepared_run_action
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
            .with_remote_execution_custom_image(self.inner.remote_execution_custom_image.clone());

        let (dep_file_bundle, req) = if let Some(visitor) = dep_file_visitor {
            let bundle = make_dep_file_bundle(ctx, visitor, cmdline_digest, req.paths())?;
            // Enable remote dep file cache lookup for actions that have remote depfile uploads enabled.
            let req = if self.inner.allow_dep_file_cache_upload {
                req.with_remote_dep_file_key(&bundle.remote_dep_file_action.action.coerce())
            } else {
                req
            };
            (Some(bundle), req)
        } else {
            (None, req)
        };

        // First, check in the local dep file cache if an identical action can be found there.
        // Do this before checking the action cache as we can avoid a potentially large download.
        // Once the action cache lookup misses, we will do the full dep file cache look up.
        let should_fully_check_dep_file_cache = if let Some(dep_file_bundle) = &dep_file_bundle {
            let (outputs, should_fully_check_dep_file_cache) = dep_file_bundle
                .check_local_dep_file_cache_for_identical_action(ctx, self.outputs.as_slice())
                .await?;
            if let Some((outputs, metadata)) = outputs {
                return Ok(ExecuteResult::LocalDepFileHit(outputs, metadata));
            }
            should_fully_check_dep_file_cache
        } else {
            false
        };

        // Prepare the action, check the action cache, fully check the local dep file cache if needed, then execute the command
        let prepared_action = ctx.prepare_action(&req)?;
        let manager = ctx.command_execution_manager();

        let action_cache_result = ctx.action_cache(manager, &req, &prepared_action).await;

        // If the result was served by the remote dep file cache, we can't use the result just yet. We need to verify that
        // the inputs tracked by a depfile that was actually used in the cache hit are indentical to the inputs we have for this action.
        let result = if let ControlFlow::Break(res) = action_cache_result {
            self.check_cache_result_is_useable(
                ctx,
                &req,
                &prepared_action.action_and_blobs.action,
                res,
                &dep_file_bundle,
            )
            .await?
        } else {
            action_cache_result
        };

        // If the cache queries did not yield to a result, fallback to local dep file query (continuation), then execution.
        let mut result = match result {
            ControlFlow::Break(res) => res,
            ControlFlow::Continue(manager) => {
                if let Some(dep_file_bundle) = &dep_file_bundle {
                    if should_fully_check_dep_file_cache {
                        let lookup = dep_file_bundle
                            .check_local_dep_file_cache(ctx, self.outputs.as_slice())
                            .await?;
                        if let Some((outputs, metadata)) = lookup {
                            return Ok(ExecuteResult::LocalDepFileHit(outputs, metadata));
                        }
                    }
                };

                ctx.exec_cmd(manager, &req, &prepared_action).await
            }
        };

        // If the action has a dep file, log the remote dep file key so we can look out for collisions
        if let Some(bundle) = &dep_file_bundle {
            result.dep_file_key = Some(bundle.remote_dep_file_action.action.coerce())
        }

        Ok(ExecuteResult::ExecutedOrReHit {
            result,
            dep_file_bundle,
            // Dropping rest of req to avoid holding paths longer than necessary.
            executor_preference: req.executor_preference,
            prepared_action,
            input_files_bytes,
        })
    }
}

pub(crate) struct PreparedRunAction {
    expanded: ExpandedCommandLine,
    extra_env: Vec<(String, String)>,
    paths: CommandExecutionPaths,
    worker: Option<WorkerSpec>,
}

impl PreparedRunAction {
    fn into_command_execution_request(self) -> CommandExecutionRequest {
        let Self {
            expanded: ExpandedCommandLine { exe, args, mut env },
            extra_env,
            paths,
            worker,
        } = self;

        for (k, v) in extra_env {
            env.insert(k, v);
        }

        CommandExecutionRequest::new(exe, args, paths, env).with_worker(worker)
    }
}

trait RunActionVisitor: CommandLineArtifactVisitor {
    type Iter<'a>: Iterator<Item = &'a ArtifactGroup>
    where
        Self: 'a;

    fn inputs<'a>(&'a self) -> Self::Iter<'a>;
}

impl RunActionVisitor for SimpleCommandLineArtifactVisitor {
    type Iter<'a> = impl Iterator<Item = &'a ArtifactGroup> where Self: 'a;

    fn inputs<'a>(&'a self) -> Self::Iter<'a> {
        self.inputs.iter()
    }
}

impl RunActionVisitor for DepFilesCommandLineVisitor<'_> {
    type Iter<'a> = impl Iterator<Item = &'a ArtifactGroup> where Self: 'a;

    fn inputs<'a>(&'a self) -> Self::Iter<'a> {
        self.inputs.iter().flat_map(|g| g.iter())
    }
}

#[async_trait]
impl Action for RunAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Run
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        let values = Self::unpack(&self.starlark_values)?;
        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        values.args.visit_artifacts(&mut artifact_visitor)?;
        values.exe.visit_artifacts(&mut artifact_visitor)?;
        if let Some(worker) = values.worker {
            worker.exe.visit_artifacts(&mut artifact_visitor)?;
        }
        for (_, v) in values.env.iter() {
            v.visit_artifacts(&mut artifact_visitor)?;
        }
        Ok(Cow::Owned(artifact_visitor.inputs.into_iter().collect()))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(self.outputs.as_slice())
    }

    fn first_output(&self) -> &BuildArtifact {
        // Required to have outputs on construction
        &self.outputs.as_slice()[0]
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Incremental(self)
    }

    fn category(&self) -> CategoryRef {
        CategoryRef::unchecked_new(self.starlark_values.category.as_str())
    }

    fn identifier(&self) -> Option<&str> {
        self.starlark_values.identifier.map(|x| x.as_str())
    }

    fn always_print_stderr(&self) -> bool {
        self.inner.always_print_stderr
    }

    fn aquery_attributes(&self, fs: &ExecutorFs) -> indexmap::IndexMap<String, String> {
        let mut cli_rendered = Vec::<String>::new();
        let mut ctx = DefaultCommandLineContext::new(fs);
        let values = Self::unpack(&self.starlark_values).unwrap();
        values
            .exe
            .add_to_command_line(&mut cli_rendered, &mut ctx)
            .unwrap();
        values
            .args
            .add_to_command_line(&mut cli_rendered, &mut ctx)
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
            "allow_cache_upload".to_owned() => self.inner.allow_cache_upload.to_string(),
            "allow_dep_file_cache_upload".to_owned() => self.inner.allow_dep_file_cache_upload.to_string(),
        }
    }

    fn error_handler(&self) -> Option<OwnedFrozenValue> {
        self.error_handler.clone()
    }
}

#[async_trait]
impl IncrementalActionExecutable for RunAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let (
            mut result,
            mut dep_file_bundle,
            executor_preference,
            prepared_action,
            input_files_bytes,
        ) = match self.execute_inner(ctx).await? {
            ExecuteResult::LocalDepFileHit(outputs, metadata) => {
                return Ok((outputs, metadata));
            }
            ExecuteResult::ExecutedOrReHit {
                result,
                dep_file_bundle,
                executor_preference,
                prepared_action,
                input_files_bytes,
            } => (
                result,
                dep_file_bundle,
                executor_preference,
                prepared_action,
                input_files_bytes,
            ),
        };

        // If there is a dep file entry AND if dep file cache upload is enabled, upload it
        let upload_dep_file = self.inner.allow_dep_file_cache_upload && dep_file_bundle.is_some();
        if result.was_success()
            && !result.was_served_by_remote_dep_file_cache()
            && (self.inner.allow_cache_upload || upload_dep_file || force_cache_upload()?)
        {
            let re_result = result.action_result.take();
            let upload_result = ctx
                .cache_upload(
                    &prepared_action.action_and_blobs,
                    &result,
                    re_result,
                    // match needed for coercion, https://github.com/rust-lang/rust/issues/108999
                    match dep_file_bundle.as_mut() {
                        Some(dep_file_bundle) if self.inner.allow_dep_file_cache_upload => {
                            Some(dep_file_bundle)
                        }
                        _ => None,
                    },
                )
                .await?;

            result.did_cache_upload = upload_result.did_cache_upload;
            result.did_dep_file_cache_upload = upload_result.did_dep_file_cache_upload;
        }

        let was_locally_executed = result.was_locally_executed();
        let (outputs, metadata) = ctx.unpack_command_execution_result(
            executor_preference,
            result,
            self.inner.allow_cache_upload,
            self.inner.allow_dep_file_cache_upload,
            Some(input_files_bytes),
        )?;

        if let Some(dep_file_bundle) = dep_file_bundle {
            populate_dep_files(ctx, dep_file_bundle, &outputs, was_locally_executed).await?;
        }

        Ok((outputs, metadata))
    }
}

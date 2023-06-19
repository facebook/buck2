/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt::Display;
use std::ops::ControlFlow;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::impls::expanded_command_line::ExpandedCommandLine;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::IncrementalActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::worker_info::WorkerInfo;
use buck2_core::category::Category;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_events::dispatch::span_async;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::request::ActionMetadataBlob;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
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
use starlark::coerce::Coerce;
use starlark::starlark_complex_value;
use starlark::starlark_type;
use starlark::values::dict::DictRef;
use starlark::values::none::NoneOr;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use thiserror::Error;

use self::dep_files::DeclaredDepFiles;
use self::dep_files::PartitionedInputs;
use crate::actions::impls::run::dep_files::match_or_clear_dep_file;
use crate::actions::impls::run::dep_files::populate_dep_files;
use crate::actions::impls::run::dep_files::CommandDigests;
use crate::actions::impls::run::dep_files::DepFilesCommandLineVisitor;
use crate::actions::impls::run::dep_files::DepFilesKey;
use crate::actions::impls::run::dep_files::RunActionDepFiles;
use crate::actions::impls::run::metadata::metadata_content;

pub(crate) mod audit_dep_files;
pub mod dep_files;
mod metadata;

type DepFileLookUpKey = (
    DepFilesKey,
    CommandDigests,
    PartitionedInputs<Vec<ArtifactGroup>>,
    DeclaredDepFiles,
);

#[derive(Debug, Error)]
enum RunActionValidationError {
    #[error("Expected command line value, got {0}")]
    ContentsNotCommandLineValue(String),
}

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

#[derive(Debug, Error)]
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
) -> anyhow::Result<ExecutorPreference> {
    match (local_only, prefer_local, prefer_remote) {
        (true, false, false) => Ok(ExecutorPreference::LocalRequired),
        (true, false, true) => Err(anyhow::anyhow!(
            LocalPreferenceError::LocalOnlyAndPreferRemote
        )),
        (false, true, false) => Ok(ExecutorPreference::LocalPreferred),
        (false, true, true) => Err(anyhow::anyhow!(
            LocalPreferenceError::PreferLocalAndPreferRemote
        )),
        (false, false, false) => Ok(ExecutorPreference::Default),
        (false, false, true) => Ok(ExecutorPreference::RemotePreferred),
        (true, true, false) => Err(anyhow::anyhow!(
            LocalPreferenceError::LocalOnlyAndPreferLocal
        )),
        (true, true, true) => Err(anyhow::anyhow!(
            LocalPreferenceError::LocalOnlyAndPreferLocalAndPreferRemote
        )),
    }
}

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredRunAction {
    pub(crate) category: Category,
    pub(crate) identifier: Option<String>,
    pub(crate) executor_preference: ExecutorPreference,
    pub(crate) always_print_stderr: bool,
    pub(crate) weight: WeightClass,
    pub(crate) dep_files: RunActionDepFiles,
    pub(crate) metadata_param: Option<MetadataParameter>,
    pub(crate) no_outputs_cleanup: bool,
    pub(crate) allow_cache_upload: bool,
    pub(crate) force_full_hybrid_if_capable: bool,
}

impl UnregisteredAction for UnregisteredRunAction {
    fn register(
        self: Box<Self>,
        _: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        let starlark_values =
            starlark_data.context("module data to be present (internal error)")?;
        let run_action = RunAction::new(*self, starlark_values, outputs)?;
        Ok(Box::new(run_action))
    }
}

#[derive(
    Debug,
    Display,
    Trace,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    Freeze,
    Coerce
)]
#[display(fmt = "run_action_values")]
#[repr(C)]
pub(crate) struct StarlarkRunActionValuesGen<V> {
    pub(crate) exe: V,
    pub(crate) args: V,
    pub(crate) env: V,
    /// `WorkerInfo` or `None`.
    pub(crate) worker: V,
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkRunActionValuesGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    starlark_type!("run_action_values");
}

starlark_complex_value!(pub(crate) StarlarkRunActionValues);

impl<'v, V: ValueLike<'v>> StarlarkRunActionValuesGen<V> {
    pub(crate) fn worker(&self) -> anyhow::Result<ValueOf<'v, NoneOr<&'v WorkerInfo<'v>>>> {
        ValueOf::unpack_value_err(self.worker.to_value())
    }
}

struct UnpackedRunActionValues<'v> {
    exe: &'v dyn CommandLineArgLike,
    args: &'v dyn CommandLineArgLike,
    env: Vec<(&'v str, &'v dyn CommandLineArgLike)>,
    worker: Option<(&'v dyn CommandLineArgLike, WorkerId)>,
}

#[derive(Debug, Allocative)]
pub(crate) struct RunAction {
    inner: UnregisteredRunAction,
    starlark_values: OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    outputs: BoxSliceSet<BuildArtifact>,
}

impl RunAction {
    fn unpack(
        values: &OwnedFrozenValueTyped<FrozenStarlarkRunActionValues>,
    ) -> anyhow::Result<UnpackedRunActionValues> {
        let exe = values.exe.to_value().as_command_line_err()?;
        let args = values.args.to_value().as_command_line_err()?;
        let env = if values.env.is_none() {
            Vec::new()
        } else {
            let d = DictRef::from_value(values.env.to_value()).context("expecting dict")?;
            let mut res = Vec::with_capacity(d.len());
            for (k, v) in d.iter() {
                res.push((
                    k.unpack_str().context("expecting string")?,
                    v.as_command_line_err()?,
                ));
            }
            res
        };
        let worker: NoneOr<&WorkerInfo> = values.worker()?.typed;

        let worker = if let Some(worker) = worker.into_option() {
            let worker_exe = worker.exe_command_line();
            let worker_id = WorkerId(worker.id);
            Some((worker_exe, worker_id))
        } else {
            None
        };

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
        fs: &ExecutorFs,
        artifact_visitor: &mut impl CommandLineArtifactVisitor,
    ) -> anyhow::Result<(ExpandedCommandLine, Option<WorkerSpec>)> {
        let mut ctx = DefaultCommandLineContext::new(fs);
        let values = Self::unpack(&self.starlark_values)?;

        let mut exe_rendered = Vec::<String>::new();
        values
            .exe
            .add_to_command_line(&mut exe_rendered, &mut ctx)?;
        values.exe.visit_artifacts(artifact_visitor)?;

        let worker = if let Some((worker_exe, worker_id)) = values.worker {
            let mut worker_rendered = Vec::<String>::new();
            worker_exe.add_to_command_line(&mut worker_rendered, &mut ctx)?;
            worker_exe.visit_artifacts(artifact_visitor)?;
            Some(WorkerSpec {
                id: worker_id,
                exe: worker_rendered,
            })
        } else {
            None
        };

        let mut args_rendered = Vec::<String>::new();
        values
            .args
            .add_to_command_line(&mut args_rendered, &mut ctx)?;
        values.args.visit_artifacts(artifact_visitor)?;

        let cli_env: anyhow::Result<SortedVectorMap<_, _>> = values
            .env
            .into_iter()
            .map(|(k, v)| {
                let mut env = Vec::<String>::new(); // TODO (torozco): Use a String.
                let mut ctx = DefaultCommandLineContext::new(fs);
                v.add_to_command_line(&mut env, &mut ctx)?;
                v.visit_artifacts(artifact_visitor)?;
                let var = env.join(" ");
                Ok((k.to_owned(), var))
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
    ) -> anyhow::Result<Self> {
        let starlark_values = match starlark_values.downcast() {
            Ok(starlark_values) => starlark_values,
            Err(starlark_values) => {
                return Err(RunActionValidationError::ContentsNotCommandLineValue(
                    starlark_values.value().to_repr(),
                )
                .into());
            }
        };

        Self::unpack(&starlark_values)?;

        Ok(RunAction {
            inner,
            starlark_values,
            outputs: BoxSliceSet::from(outputs),
        })
    }

    fn prepare(
        &self,
        visitor: &mut impl RunActionVisitor,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<PreparedRunAction> {
        let fs = ctx.fs();

        let (expanded, worker) =
            self.expand_command_line_and_worker(&ctx.executor_fs(), visitor)?;

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
        let extra_env = if let Some(metadata_param) = &self.inner.metadata_param {
            let path = BuckOutPath::new(ctx.target().owner().dupe(), metadata_param.path.clone());
            let resolved_path = fs.buck_out_path_resolver().resolve_gen(&path);
            let extra = (metadata_param.env_var.to_owned(), resolved_path.to_string());
            let (data, digest) = metadata_content(fs, &artifact_inputs, ctx.digest_config())?;
            inputs.push(CommandExecutionInput::ActionMetadata(ActionMetadataBlob {
                data,
                digest,
                path,
            }));
            Some(extra)
        } else {
            None
        };

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

    fn make_dep_file_lookup_key(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        visitor: DepFilesCommandLineVisitor<'_>,
        prepared_action: &PreparedRunAction,
    ) -> DepFileLookUpKey {
        let digests = CommandDigests {
            cli: prepared_action.expanded.fingerprint(),
            directory: prepared_action
                .paths
                .input_directory()
                .fingerprint()
                .data()
                .dupe(),
        };
        let dep_files_key = DepFilesKey::from_action_execution_target(ctx.target());

        let DepFilesCommandLineVisitor {
            inputs: declared_inputs,
            outputs: declared_dep_files,
            ..
        } = visitor;
        (dep_files_key, digests, declared_inputs, declared_dep_files)
    }

    async fn check_dep_files(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        dep_files: &DepFileLookUpKey,
    ) -> anyhow::Result<Option<(ActionOutputs, ActionExecutionMetadata)>> {
        let (dep_files_key, digests, declared_inputs, declared_dep_files) = dep_files;

        let matching_result = span_async(buck2_data::MatchDepFilesStart {}, async {
            let res: anyhow::Result<_> = try {
                match_or_clear_dep_file(
                    dep_files_key,
                    digests,
                    declared_inputs,
                    self.outputs.as_slice(),
                    declared_dep_files,
                    ctx,
                )
                .await?
            };

            (res, buck2_data::MatchDepFilesEnd {})
        })
        .await?;

        if let Some(matching_result) = matching_result {
            Ok(Some((
                matching_result,
                ActionExecutionMetadata {
                    execution_kind: ActionExecutionKind::LocalDepFile,
                    timing: Default::default(),
                },
            )))
        } else {
            Ok(None)
        }
    }
}

struct PreparedRunAction {
    expanded: ExpandedCommandLine,
    extra_env: Option<(String, String)>,
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

        for (k, v) in extra_env.into_iter() {
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

    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
        let values = Self::unpack(&self.starlark_values)?;
        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        values.args.visit_artifacts(&mut artifact_visitor)?;
        values.exe.visit_artifacts(&mut artifact_visitor)?;
        if let Some((worker_exe, _)) = values.worker {
            worker_exe.visit_artifacts(&mut artifact_visitor)?;
        }
        for (_, v) in values.env.iter() {
            v.visit_artifacts(&mut artifact_visitor)?;
        }
        Ok(Cow::Owned(artifact_visitor.inputs.into_iter().collect()))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
        Ok(Cow::Borrowed(self.outputs.as_slice()))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Incremental(self)
    }

    fn category(&self) -> &Category {
        &self.inner.category
    }

    fn identifier(&self) -> Option<&str> {
        self.inner.identifier.as_deref()
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
        }
    }
}

#[async_trait]
impl IncrementalActionExecutable for RunAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let knobs = ctx.run_action_knobs();
        let process_dep_files = !self.inner.dep_files.labels.is_empty() || knobs.hash_all_commands;
        let (prepared_run_action, dep_files) = if !process_dep_files {
            (
                self.prepare(&mut SimpleCommandLineArtifactVisitor::new(), ctx)?,
                None,
            )
        } else {
            let mut visitor = DepFilesCommandLineVisitor::new(&self.inner.dep_files);
            let prepared = self.prepare(&mut visitor, ctx)?;
            let dep_files = self.make_dep_file_lookup_key(ctx, visitor, &prepared);
            (prepared, Some(dep_files))
        };

        // Run actions are assumed to be shared
        let host_sharing_requirements = HostSharingRequirements::Shared(self.inner.weight);

        let req = prepared_run_action
            .into_command_execution_request()
            .with_prefetch_lossy_stderr(true)
            .with_executor_preference(self.inner.executor_preference)
            .with_host_sharing_requirements(host_sharing_requirements)
            .with_outputs_cleanup(!self.inner.no_outputs_cleanup)
            .with_allow_cache_upload(self.inner.allow_cache_upload)
            .with_local_environment_inheritance(EnvironmentInheritance::local_command_exclusions())
            .with_force_full_hybrid_if_capable(self.inner.force_full_hybrid_if_capable)
            .with_custom_tmpdir(ctx.target().custom_tmpdir());

        // First prepare the action, check the action cache, check dep_files if needed, and execute the command
        let prepared_action = ctx.prepare_action(&req)?;
        let manager = ctx.command_execution_manager();
        let ((outputs, meta), dep_files) =
            match ctx.action_cache(manager, &req, &prepared_action).await {
                ControlFlow::Break(res) => (res?, dep_files),
                ControlFlow::Continue(manager) => {
                    let dep_files = if let Some(dep_files) = dep_files {
                        match self.check_dep_files(ctx, &dep_files).await? {
                            Some(m) => {
                                // We have a dep_file based match, return early
                                return Ok(m);
                            }
                            None => Some(dep_files),
                        }
                    } else {
                        None
                    };

                    (
                        ctx.exec_cmd(manager, &req, &prepared_action).await?,
                        dep_files,
                    )
                }
            };

        let outputs = ActionOutputs::new(outputs);

        if let Some(dep_files) = dep_files {
            let (dep_files_key, digests, declared_inputs, declared_dep_files) = dep_files;

            populate_dep_files(
                dep_files_key,
                digests,
                declared_inputs,
                declared_dep_files,
                &outputs,
                ctx,
            )
            .await?;
        }

        Ok((outputs, meta))
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::category::Category;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_events::dispatch::span_async;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::request::ActionMetadataBlob;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::OutputType;
use buck2_execute::path::buck_out_path::BuckOutPath;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;
use host_sharing::WeightClass;
use indexmap::indexmap;
use indexmap::IndexSet;
use itertools::Itertools;
use serde_json::json;
use starlark::values::dict::Dict;
use starlark::values::tuple::Tuple;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::execute::action_executor::ActionExecutionKind;
use crate::actions::execute::action_executor::ActionExecutionMetadata;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::impls::run::dep_files::match_or_clear_dep_file;
use crate::actions::impls::run::dep_files::populate_dep_files;
use crate::actions::impls::run::dep_files::DepFilesCommandLineVisitor;
use crate::actions::impls::run::dep_files::DepFilesKey;
use crate::actions::impls::run::dep_files::RunActionDepFiles;
use crate::actions::impls::run::expanded_command_line::ExpandedCommandLine;
use crate::actions::impls::run::metadata::metadata_content;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::IncrementalActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;

pub mod dep_files;
mod expanded_command_line;
pub mod knobs;
mod metadata;

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
}

pub fn new_executor_preference(
    local_only: bool,
    prefer_local: bool,
) -> anyhow::Result<ExecutorPreference> {
    match (local_only, prefer_local) {
        (true, false) => Ok(ExecutorPreference::LocalRequired),
        (false, true) => Ok(ExecutorPreference::LocalPreferred),
        (false, false) => Ok(ExecutorPreference::Default),
        (true, true) => Err(anyhow::anyhow!(
            LocalPreferenceError::LocalOnlyAndPreferLocal
        )),
    }
}

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredRunAction {
    pub category: Category,
    pub identifier: Option<String>,
    pub executor_preference: ExecutorPreference,
    pub always_print_stderr: bool,
    pub weight: usize,
    pub dep_files: RunActionDepFiles,
    pub metadata_param: Option<MetadataParameter>,
    pub no_outputs_cleanup: bool,
    pub allow_cache_upload: bool,
    pub force_full_hybrid_if_capable: bool,
}

impl UnregisteredAction for UnregisteredRunAction {
    fn register(
        self: Box<Self>,
        _: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        let starlark_cli = starlark_data.expect("module data to be present");
        let run_action = RunAction::new(*self, starlark_cli, outputs)?;
        Ok(box run_action)
    }
}

#[derive(Debug, Allocative)]
pub(crate) struct RunAction {
    inner: UnregisteredRunAction,
    starlark_cli: OwnedFrozenValue, // StarlarkCommandLine
    outputs: IndexSet<BuildArtifact>,
}

impl RunAction {
    fn unpack(
        args: &OwnedFrozenValue,
    ) -> Option<(
        &dyn CommandLineArgLike,
        Vec<(&str, &dyn CommandLineArgLike)>,
    )> {
        // We expect (CmdArgs, Option<Dict<String, CmdArgs>>) in the Starlark value
        let (cli, env) = match Tuple::from_value(args.value())?.content() {
            [cli, env] => (*cli, *env),
            _ => return None,
        };
        let cli = cli.as_command_line()?;
        let env = if env.is_none() {
            Vec::new()
        } else {
            let d = Dict::from_value(env)?;
            let mut res = Vec::with_capacity(d.len());
            for (k, v) in d.iter() {
                res.push((k.unpack_str()?, v.as_command_line()?));
            }
            res
        };
        Some((cli, env))
    }

    /// Get the command line expansion for this RunAction.
    fn expand_command_line(
        &self,
        fs: &ExecutorFs,
        artifact_visitor: &mut impl CommandLineArtifactVisitor,
    ) -> anyhow::Result<ExpandedCommandLine> {
        let mut cli_builder = BaseCommandLineBuilder::new(fs);

        let (cli, env) = Self::unpack(&self.starlark_cli).unwrap();
        cli.add_to_command_line(&mut cli_builder)?;
        cli.visit_artifacts(artifact_visitor)?;

        let mut cli_env = HashMap::with_capacity(env.len());
        for (k, v) in env.into_iter() {
            let mut var_builder = BaseCommandLineBuilder::new(fs);
            v.add_to_command_line(&mut var_builder)?;
            v.visit_artifacts(artifact_visitor)?;
            let var = var_builder.build().join(" ");
            cli_env.insert(k.to_owned(), var);
        }

        Ok(ExpandedCommandLine {
            cli: cli_builder.build(),
            env: cli_env,
        })
    }

    pub(crate) fn new(
        inner: UnregisteredRunAction,
        starlark_cli: OwnedFrozenValue,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        if Self::unpack(&starlark_cli).is_none() {
            Err(anyhow::anyhow!(
                RunActionValidationError::ContentsNotCommandLineValue(
                    starlark_cli.value().to_repr()
                )
            ))
        } else {
            Ok(RunAction {
                inner,
                starlark_cli,
                outputs,
            })
        }
    }
}

#[async_trait]
impl Action for RunAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Run
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
        let (cli, env) = Self::unpack(&self.starlark_cli).unwrap();
        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        cli.visit_artifacts(&mut artifact_visitor)?;
        for (_, v) in env.iter() {
            v.visit_artifacts(&mut artifact_visitor)?;
        }
        Ok(Cow::Owned(artifact_visitor.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Borrowed(&self.outputs))
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
        let mut cli_builder = BaseCommandLineBuilder::new(fs);
        let (cli, _env) = Self::unpack(&self.starlark_cli).unwrap();
        cli.add_to_command_line(&mut cli_builder).unwrap();
        let cmd = format!("[{}]", cli_builder.build().iter().join(", "));
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

        let dep_files = if !process_dep_files {
            None
        } else {
            let (matching_result, dep_files) =
                span_async(buck2_data::MatchDepFilesStart {}, async {
                    let res: anyhow::Result<_> = try {
                        let dep_files_key =
                            DepFilesKey::from_command_execution_target(ctx.target());

                        let mut visitor = DepFilesCommandLineVisitor::new(&self.inner.dep_files);
                        let expanded =
                            self.expand_command_line(&ctx.executor_fs(), &mut visitor)?;

                        let DepFilesCommandLineVisitor {
                            inputs: declared_inputs,
                            outputs: declared_dep_files,
                            ..
                        } = visitor;

                        let cli_digest = expanded.fingerprint();

                        let matching_result = match_or_clear_dep_file(
                            &dep_files_key,
                            &cli_digest,
                            &declared_inputs,
                            &declared_dep_files,
                            ctx,
                        )
                        .await?;

                        (
                            matching_result,
                            (
                                dep_files_key,
                                cli_digest,
                                declared_inputs,
                                declared_dep_files,
                            ),
                        )
                    };

                    (res, buck2_data::MatchDepFilesEnd {})
                })
                .await?;

            if let Some(matching_result) = matching_result {
                return Ok((
                    matching_result,
                    ActionExecutionMetadata {
                        execution_kind: ActionExecutionKind::Skipped,
                        timing: Default::default(),
                    },
                ));
            }

            Some(dep_files)
        };

        let fs = ctx.fs();

        let (ExpandedCommandLine { cli, mut env }, inputs) = {
            let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
            let cli = self.expand_command_line(&ctx.executor_fs(), &mut artifact_visitor)?;
            (cli, artifact_visitor.inputs)
        };

        // TODO (@torozco): At this point, might as well just receive the list already. Finding
        // those things in a HashMap is just not very useful.
        let artifact_inputs: Vec<&ArtifactGroupValues> = inputs
            .iter()
            .map(|group| ctx.artifact_values(group))
            .collect();

        let mut inputs: Vec<CommandExecutionInput> =
            artifact_inputs[..].map(|&i| CommandExecutionInput::Artifact(box i.dupe()));

        // Handle case when user requested file with action metadata to be generated.
        // Generate content and output path for the file. It will be either passed
        // to RE as a blob or written to disk in local executor.
        // Path to this file is passed to user in environment variable which is selected by user.
        if let Some(metadata_param) = &self.inner.metadata_param {
            let path = BuckOutPath::new(ctx.target().owner.dupe(), metadata_param.path.clone());
            let resolved_path = fs.buck_out_path_resolver().resolve_gen(&path);
            env.insert(metadata_param.env_var.to_owned(), resolved_path.to_string());
            let (data, digest) = metadata_content(fs, &artifact_inputs)?;
            inputs.push(CommandExecutionInput::ActionMetadata(ActionMetadataBlob {
                data,
                digest,
                path,
            }));
        }

        // Run actions are assumed to be shared
        let host_sharing_requirements =
            HostSharingRequirements::Shared(WeightClass::Permits(self.inner.weight));

        let req = CommandExecutionRequest::new(
            cli,
            inputs,
            self.outputs
                .iter()
                .map(|b| (b.get_path().dupe(), OutputType::FileOrDirectory))
                .collect(),
            env,
        )
        .with_prefetch_lossy_stderr(true)
        .with_executor_preference(self.inner.executor_preference)
        .with_host_sharing_requirements(host_sharing_requirements)
        .with_outputs_cleanup(!self.inner.no_outputs_cleanup)
        .with_allow_cache_upload(self.inner.allow_cache_upload)
        .with_local_environment_inheritance(EnvironmentInheritance::local_command_exclusions())
        .with_force_full_hybrid_if_capable(self.inner.force_full_hybrid_if_capable);

        let (outputs, meta) = ctx.exec_cmd(&req).await?;

        let outputs = outputs
            .into_iter()
            .filter_map(|(o, v)| Some((o.into_build_artifact()?.0, v)))
            .collect();
        let outputs = ActionOutputs::new(outputs);

        if let Some(dep_files) = dep_files {
            let (dep_files_key, cli_digest, declared_inputs, declared_dep_files) = dep_files;

            populate_dep_files(
                dep_files_key,
                cli_digest,
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

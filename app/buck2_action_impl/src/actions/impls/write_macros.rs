/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::IncrementalActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::attrs::resolve::attr_type::arg::value::ResolvedMacro;
use buck2_build_api::attrs::resolve::attr_type::arg::ArgBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineLocation;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_core::category::Category;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use dupe::Dupe;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

#[derive(Allocative)]
pub(crate) struct UnregisteredWriteMacrosToFileAction {
    identifier: String,
}

impl UnregisteredWriteMacrosToFileAction {
    pub(crate) fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

impl UnregisteredAction for UnregisteredWriteMacrosToFileAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("Action data should be present");
        let action = WriteMacrosToFileAction::new(self.identifier, contents, inputs, outputs)?;
        Ok(Box::new(action))
    }
}

#[derive(Debug, Error)]
enum WriteMacrosActionValidationError {
    #[error("At least one output file must be specified for a write macros action")]
    NoOutputsSpecified,
    #[error("Expected command line value, got {0}")]
    ContentsNotCommandLineValue(String),
    #[error(
        "Number of write-to-file macro artifacts during analysis time should be consistent with when those artifacts are created"
    )]
    InconsistentNumberOfMacroArtifacts,
}

#[derive(Debug, Allocative)]
struct WriteMacrosToFileAction {
    identifier: String,
    contents: OwnedFrozenValue, // StarlarkCommandLine
    inputs: Box<[ArtifactGroup]>,
    outputs: Box<[BuildArtifact]>,
}

impl WriteMacrosToFileAction {
    fn new(
        identifier: String,
        contents: OwnedFrozenValue,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        if outputs.is_empty() {
            Err(anyhow::anyhow!(
                WriteMacrosActionValidationError::NoOutputsSpecified
            ))
        } else if contents.value().as_command_line().is_none() {
            Err(anyhow::anyhow!(
                WriteMacrosActionValidationError::ContentsNotCommandLineValue(
                    contents.value().to_repr()
                )
            ))
        } else {
            Ok(Self {
                identifier,
                contents,
                inputs: inputs.into_iter().collect(),
                outputs: outputs.into_iter().collect(),
            })
        }
    }
}

#[async_trait]
impl Action for WriteMacrosToFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::WriteMacrosToFile
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&self.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
        Ok(Cow::Borrowed(&self.outputs))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Incremental(self)
    }

    fn category(&self) -> &Category {
        static WRITE_MACROS_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("write_macros_to_file").unwrap());

        &WRITE_MACROS_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(&self.identifier)
    }
}

#[async_trait]
impl IncrementalActionExecutable for WriteMacrosToFileAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let mut execution_start = None;

        let values = ctx
            .materializer()
            .declare_write(Box::new(|| {
                execution_start = Some(Instant::now());

                let fs = ctx.executor_fs();

                let mut output_contents = Vec::with_capacity(self.outputs.len());
                let mut macro_writer = MacroToFileWriter::new(&fs, &mut output_contents);

                self.contents
                    .value()
                    .as_command_line()
                    .unwrap()
                    .visit_write_to_file_macros(&mut macro_writer)?;

                if self.outputs.len() != output_contents.len() {
                    return Err(
                        WriteMacrosActionValidationError::InconsistentNumberOfMacroArtifacts.into(),
                    );
                }

                Ok(
                    std::iter::zip(self.outputs.iter(), output_contents.into_iter())
                        .map(|(output, content)| WriteRequest {
                            path: fs.fs().resolve_build(output.get_path()),
                            content: content.into_bytes(),
                            is_executable: false,
                        })
                        .collect(),
                )
            }))
            .await?;

        let wall_time = execution_start
            .context("Action did not set execution_start")?
            .elapsed();

        let output_values = std::iter::zip(self.outputs.iter(), values.into_iter())
            .map(|(output, value)| (output.get_path().dupe(), value))
            .collect();

        Ok((
            ActionOutputs::new(output_values),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
            },
        ))
    }
}

struct MacroToFileWriter<'a> {
    fs: &'a ExecutorFs<'a>,
    outputs: &'a mut Vec<String>,
    relative_to_path: Option<RelativePathBuf>,
}

impl<'a> MacroToFileWriter<'a> {
    fn new(fs: &'a ExecutorFs<'a>, outputs: &'a mut Vec<String>) -> Self {
        Self {
            fs,
            outputs,
            relative_to_path: None,
        }
    }
}

impl WriteToFileMacroVisitor for MacroToFileWriter<'_> {
    fn visit_write_to_file_macro(&mut self, resolved_macro: &ResolvedMacro) -> anyhow::Result<()> {
        let content = {
            let mut builder = MacroOutput {
                result: String::new(),
            };
            let mut ctx = MacroContext::new(self.fs, &self.relative_to_path);
            resolved_macro.add_to_arg(&mut builder, &mut ctx)?;
            builder.result
        };

        self.outputs.push(content);
        Ok(())
    }

    fn set_current_relative_to_path(
        &mut self,
        gen: &dyn Fn(&dyn CommandLineContext) -> anyhow::Result<Option<RelativePathBuf>>,
    ) -> anyhow::Result<()> {
        self.relative_to_path = gen(&DefaultCommandLineContext::new(self.fs))?;
        Ok(())
    }
}

struct MacroContext<'a> {
    fs: &'a ExecutorFs<'a>,
    maybe_relative_to_path: &'a Option<RelativePathBuf>,
}

impl<'a> MacroContext<'a> {
    fn new(fs: &'a ExecutorFs, maybe_relative_to_path: &'a Option<RelativePathBuf>) -> Self {
        Self {
            fs,
            maybe_relative_to_path,
        }
    }

    fn relativize_path(&self, path: ProjectRelativePathBuf) -> RelativePathBuf {
        if let Some(relative_to_path) = self.maybe_relative_to_path {
            relative_to_path.relative(path)
        } else {
            path.into()
        }
    }
}

impl CommandLineContext for MacroContext<'_> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        Ok(CommandLineLocation::from_relative_path(
            self.relativize_path(path),
            self.fs.path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs {
        self.fs
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        unreachable!("write-to-file macros could not be nested")
    }
}

// TODO(torozco): Just remove this, and ArgBuilder
struct MacroOutput {
    result: String,
}

impl ArgBuilder for MacroOutput {
    fn push_str(&mut self, s: &str) {
        self.result.push_str(s)
    }
}

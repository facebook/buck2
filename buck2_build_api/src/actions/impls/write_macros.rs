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

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::category::Category;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::materialize::materializer::WriteRequest;
use gazebo::prelude::*;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::ArtifactGroup;
use crate::actions::PristineActionExecutable;
use crate::actions::UnregisteredAction;
use crate::attrs::resolve::attr_type::arg::value::ResolvedMacro;
use crate::attrs::resolve::attr_type::arg::ArgBuilder;
use crate::execute::ActionExecutionKind;
use crate::execute::ActionExecutionMetadata;
use crate::execute::ActionExecutionTimingData;
use crate::execute::ActionOutputs;
use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

pub struct UnregisteredWriteMacrosToFileAction {
    _private: (),
}

impl UnregisteredWriteMacrosToFileAction {
    pub fn new() -> Self {
        Self { _private: () }
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
        let action = WriteMacrosToFileAction::new(contents, inputs, outputs)?;
        Ok(box action)
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

#[derive(Debug)]
struct WriteMacrosToFileAction {
    contents: OwnedFrozenValue, // StarlarkCommandLine
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
}

impl WriteMacrosToFileAction {
    fn new(
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
                contents,
                inputs,
                outputs,
            })
        }
    }
}

#[async_trait]
impl Action for WriteMacrosToFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::WriteMacrosToFile
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
        Ok(Cow::Borrowed(&self.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Borrowed(&self.outputs))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Pristine(self)
    }

    fn category(&self) -> &Category {
        static WRITE_MACROS_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("write_macros_to_file").unwrap());

        &WRITE_MACROS_CATEGORY
    }
}

#[async_trait]
impl PristineActionExecutable for WriteMacrosToFileAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let mut execution_start = None;

        let values = ctx
            .materializer()
            .write(box || {
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
            })
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
            let mut builder = MacroContentBuilder::new(self.fs, &self.relative_to_path);
            resolved_macro.add_to_arg(&mut builder)?;
            builder.result
        };

        self.outputs.push(content);
        Ok(())
    }

    fn set_current_relative_to_path(
        &mut self,
        gen: &dyn Fn(&dyn CommandLineBuilderContext) -> anyhow::Result<Option<RelativePathBuf>>,
    ) -> anyhow::Result<()> {
        self.relative_to_path = gen(&BaseCommandLineBuilder::new(self.fs))?;
        Ok(())
    }
}

struct MacroContentBuilder<'a> {
    fs: &'a ExecutorFs<'a>,
    maybe_relative_to_path: &'a Option<RelativePathBuf>,
    result: String,
}

impl<'a> MacroContentBuilder<'a> {
    fn new(fs: &'a ExecutorFs, maybe_relative_to_path: &'a Option<RelativePathBuf>) -> Self {
        Self {
            fs,
            maybe_relative_to_path,
            result: String::new(),
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

impl CommandLineBuilderContext for MacroContentBuilder<'_> {
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

impl ArgBuilder for MacroContentBuilder<'_> {
    fn push_str(&mut self, s: &str) {
        self.result.push_str(s)
    }
}

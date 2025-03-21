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
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::arg_builder::ArgBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineLocation;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_build_api::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use buck2_core::category::CategoryRef;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::values::OwnedFrozenValue;
use starlark::values::UnpackValue;

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
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("Action data should be present");

        if !inputs.is_empty() {
            return Err(internal_error!(
                "Input artifacts mut be empty for write macros action"
            ));
        }

        let action = WriteMacrosToFileAction::new(self.identifier, contents, outputs)?;

        Ok(Box::new(action))
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
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
    contents: OwnedFrozenValue, // StarlarkCmdArgs
    outputs: Box<[BuildArtifact]>,
}

impl WriteMacrosToFileAction {
    fn new(
        identifier: String,
        contents: OwnedFrozenValue,
        outputs: IndexSet<BuildArtifact>,
    ) -> buck2_error::Result<Self> {
        if outputs.is_empty() {
            Err(WriteMacrosActionValidationError::NoOutputsSpecified.into())
        } else if ValueAsCommandLineLike::unpack_value(contents.value())?.is_none() {
            Err(
                WriteMacrosActionValidationError::ContentsNotCommandLineValue(
                    contents.value().to_repr(),
                )
                .into(),
            )
        } else {
            Ok(Self {
                identifier,
                contents,
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

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(&self.outputs)
    }

    fn first_output(&self) -> &BuildArtifact {
        // Required to have outputs on construction
        &self.outputs[0]
    }

    fn category(&self) -> CategoryRef {
        CategoryRef::unchecked_new("write_macros_to_file")
    }

    fn identifier(&self) -> Option<&str> {
        Some(&self.identifier)
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let mut execution_start = None;

        let values = ctx
            .materializer()
            .declare_write(Box::new(|| {
                execution_start = Some(Instant::now());

                let fs = ctx.executor_fs();

                let mut output_contents = Vec::with_capacity(self.outputs.len());
                let mut macro_writer = MacroToFileWriter::new(&fs, &mut output_contents);

                ValueAsCommandLineLike::unpack_value_err(self.contents.value())?
                    .0
                    .visit_write_to_file_macros(&mut macro_writer)?;

                if self.outputs.len() != output_contents.len() {
                    return Err(buck2_error::Error::from(
                        WriteMacrosActionValidationError::InconsistentNumberOfMacroArtifacts,
                    )
                    .into());
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
            .buck_error_context("Action did not set execution_start")?
            .elapsed();

        let output_values = std::iter::zip(self.outputs.iter(), values.into_iter())
            .map(|(output, value)| (output.get_path().dupe(), value))
            .collect();

        Ok((
            ActionOutputs::new(output_values),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
                input_files_bytes: None,
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
    fn visit_write_to_file_macro(
        &mut self,
        resolved_macro: &ResolvedMacro,
    ) -> buck2_error::Result<()> {
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
        gen: &dyn Fn(&dyn CommandLineContext) -> buck2_error::Result<Option<RelativePathBuf>>,
    ) -> buck2_error::Result<()> {
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
    ) -> buck2_error::Result<CommandLineLocation> {
        Ok(CommandLineLocation::from_relative_path(
            self.relativize_path(path),
            self.fs.path_separator(),
        ))
    }

    fn fs(&self) -> &ExecutorFs {
        self.fs
    }

    fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf> {
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

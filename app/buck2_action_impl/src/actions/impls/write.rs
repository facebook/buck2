/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::slice;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_core::category::CategoryRef;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use dupe::Dupe;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use starlark::values::OwnedFrozenValue;
use starlark::values::UnpackValue;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
enum WriteActionValidationError {
    #[error("WriteAction received inputs")]
    TooManyInputs,
    #[error("WriteAction received no outputs")]
    NoOutputs,
    #[error("WriteAction received more than one output")]
    TooManyOutputs,
    #[error("Expected command line value, got {0}")]
    ContentsNotCommandLineValue(String),
}

#[derive(Allocative, Debug)]
pub(crate) struct UnregisteredWriteAction {
    pub(crate) is_executable: bool,
    pub(crate) absolute: bool,
    pub(crate) macro_files: Option<IndexSet<Artifact>>,
}

impl UnregisteredAction for UnregisteredWriteAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("module data to be present");

        let write_action = WriteAction::new(contents, inputs, outputs, *self)?;
        Ok(Box::new(write_action))
    }
}

#[derive(Debug, Allocative)]
struct WriteAction {
    contents: OwnedFrozenValue, // StarlarkCmdArgs
    output: BuildArtifact,
    inner: UnregisteredWriteAction,
}

impl WriteAction {
    fn new(
        contents: OwnedFrozenValue,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredWriteAction,
    ) -> buck2_error::Result<Self> {
        let mut outputs = outputs.into_iter();

        let output = match (outputs.next(), outputs.next()) {
            (Some(o), None) => o,
            (None, ..) => return Err(WriteActionValidationError::NoOutputs.into()),
            (Some(..), Some(..)) => return Err(WriteActionValidationError::TooManyOutputs.into()),
        };

        if !inputs.is_empty() {
            return Err(WriteActionValidationError::TooManyInputs.into());
        }

        if ValueAsCommandLineLike::unpack_value(contents.value())?.is_none() {
            return Err(WriteActionValidationError::ContentsNotCommandLineValue(
                contents.value().to_repr(),
            )
            .into());
        }

        Ok(WriteAction {
            contents,
            output,
            inner,
        })
    }

    fn get_contents(&self, fs: &ExecutorFs) -> buck2_error::Result<String> {
        let mut cli = Vec::<String>::new();

        let mut ctx = if let Some(macro_files) = &self.inner.macro_files {
            DefaultCommandLineContext::new_with_write_to_file_macros_support(fs, macro_files)
        } else {
            DefaultCommandLineContext::new(fs)
        };

        let mut abs;

        let ctx = if self.inner.absolute {
            abs = AbsCommandLineContext::wrap(ctx);
            &mut abs as _
        } else {
            &mut ctx as _
        };

        ValueAsCommandLineLike::unpack_value_err(self.contents.value())
            .unwrap()
            .0
            .add_to_command_line(&mut cli, ctx)?;

        Ok(cli.join("\n"))
    }
}

#[async_trait]
impl Action for WriteAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Write
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(slice::from_ref(&self.output))
    }

    fn first_output(&self) -> &BuildArtifact {
        &self.output
    }

    fn category(&self) -> CategoryRef {
        CategoryRef::unchecked_new("write")
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output.get_path().path().as_str())
    }

    fn aquery_attributes(&self, fs: &ExecutorFs) -> IndexMap<String, String> {
        // TODO(cjhopman): We should change this api to support returning a Result.
        indexmap! {
            "contents".to_owned() => match self.get_contents(fs) {
                Ok(v) => v,
                Err(e) => format!("ERROR: constructing contents ({})", e)
            },
            "absolute".to_owned() => self.inner.absolute.to_string(),
        }
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let fs = ctx.fs();

        let mut execution_start = None;

        let value = ctx
            .materializer()
            .declare_write(Box::new(|| {
                execution_start = Some(Instant::now());
                let content = self.get_contents(&ctx.executor_fs())?.into_bytes();
                Ok(vec![WriteRequest {
                    path: fs.resolve_build(self.output.get_path()),
                    content,
                    is_executable: self.inner.is_executable,
                }])
            }))
            .await?
            .into_iter()
            .next()
            .buck_error_context("Write did not execute")?;

        let wall_time = execution_start
            .buck_error_context("Action did not set execution_start")?
            .elapsed();

        Ok((
            ActionOutputs::new(indexmap![self.output.get_path().dupe() => value]),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
                input_files_bytes: None,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    // TODO: This needs proper tests, but right now it's kind of a pain to get the
    //       action framework up and running to test actions
    #[test]
    fn writes_file() {}
}

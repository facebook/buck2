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

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::category::Category;
use gazebo::dupe::Dupe;
use indexmap::indexmap;
use indexmap::indexset;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::artifact::fs::ExecutorFs;
use crate::actions::artifact::Artifact;
use crate::actions::artifact::BuildArtifact;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::PristineActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::execute::materializer::WriteRequest;
use crate::execute::ActionExecutionKind;
use crate::execute::ActionExecutionMetadata;
use crate::execute::ActionExecutionTimingData;
use crate::execute::ActionOutputs;
use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;

#[derive(Debug, Error)]
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

pub struct UnregisteredWriteAction {
    is_executable: bool,
    macro_files: Option<IndexSet<Artifact>>,
}

impl UnregisteredWriteAction {
    pub fn new(is_executable: bool, macro_files: Option<IndexSet<Artifact>>) -> Self {
        Self {
            is_executable,
            macro_files,
        }
    }
}

impl UnregisteredAction for UnregisteredWriteAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("module data to be present");

        let write_action = WriteAction::new(
            contents,
            self.is_executable,
            inputs,
            self.macro_files,
            outputs,
        )?;
        Ok(box write_action)
    }
}

#[derive(Debug)]
struct WriteAction {
    contents: OwnedFrozenValue, // StarlarkCommandLine
    is_executable: bool,
    macro_files: Option<IndexSet<Artifact>>,
    output: BuildArtifact,
}

impl WriteAction {
    fn new(
        contents: OwnedFrozenValue,
        is_executable: bool,
        inputs: IndexSet<ArtifactGroup>,
        macro_files: Option<IndexSet<Artifact>>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        let mut outputs = outputs.into_iter();

        let output = match (outputs.next(), outputs.next()) {
            (Some(o), None) => o,
            (None, ..) => return Err(WriteActionValidationError::NoOutputs.into()),
            (Some(..), Some(..)) => return Err(WriteActionValidationError::TooManyOutputs.into()),
        };

        if !inputs.is_empty() {
            return Err(WriteActionValidationError::TooManyInputs.into());
        }

        if contents.value().as_command_line().is_none() {
            return Err(WriteActionValidationError::ContentsNotCommandLineValue(
                contents.value().to_repr(),
            )
            .into());
        }

        Ok(WriteAction {
            contents,
            is_executable,
            macro_files,
            output,
        })
    }

    fn get_contents(&self, fs: &ExecutorFs) -> anyhow::Result<String> {
        let mut cli_builder = if let Some(macro_files) = &self.macro_files {
            BaseCommandLineBuilder::new_with_write_to_file_macros_support(fs, macro_files)
        } else {
            BaseCommandLineBuilder::new(fs)
        };

        self.contents
            .value()
            .as_command_line()
            .unwrap()
            .add_to_command_line(&mut cli_builder)?;

        Ok(cli_builder.build().join("\n"))
    }
}

#[async_trait]
impl Action for WriteAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Write
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
        Ok(Cow::Owned(IndexSet::new()))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Owned(indexset![self.output.dupe()]))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Pristine(self)
    }

    fn category(&self) -> &Category {
        static WRITE_CATEGORY: Lazy<Category> = Lazy::new(|| Category::try_from("write").unwrap());

        &WRITE_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output.get_path().short_path().as_str())
    }

    fn aquery_attributes(&self, fs: &ExecutorFs) -> IndexMap<String, String> {
        // TODO(cjhopman): We should change this api to support returning a Result.
        indexmap! {
            "contents".to_owned() => match self.get_contents(fs) {
                Ok(v) => v,
                Err(e) => format!("ERROR: constructing contents ({})", e)
            }
        }
    }
}

#[async_trait]
impl PristineActionExecutable for WriteAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs();

        let mut execution_start = None;

        let value = ctx
            .materializer()
            .write(box || {
                execution_start = Some(Instant::now());
                let content = self.get_contents(&ctx.executor_fs())?.into_bytes();
                Ok(vec![WriteRequest {
                    path: fs.resolve_build(self.output.get_path()),
                    content,
                    is_executable: self.is_executable,
                }])
            })
            .await?
            .into_iter()
            .next()
            .context("Write did not execute")?;

        let wall_time = execution_start
            .context("Action did not set execution_start")?
            .elapsed();

        Ok((
            ActionOutputs::new(indexmap![self.output.dupe() => value]),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
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

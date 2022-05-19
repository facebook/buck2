/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, convert::TryFrom, time::Instant};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::file_ops::{FileDigest, FileMetadata};
use buck2_core::category::Category;
use gazebo::dupe::Dupe;
use indexmap::{indexmap, IndexMap, IndexSet};
use itertools::Itertools;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::{
    actions::{
        artifact::{Artifact, ArtifactFs, ArtifactValue, BuildArtifact},
        Action, ActionExecutable, ActionExecutionCtx, PristineActionExecutable, UnregisteredAction,
    },
    artifact_groups::ArtifactGroup,
    execute::{
        ActionExecutionKind, ActionExecutionMetadata, ActionExecutionTimingData, ActionOutputs,
    },
    interpreter::rule_defs::cmd_args::{BaseCommandLineBuilder, ValueAsCommandLineLike},
};

#[derive(Debug, Error)]
enum WriteActionValidationError {
    #[error("At least one output file must be specified for a write action")]
    NoOutputsSpecified,
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
    inputs: IndexSet<ArtifactGroup>,
    macro_files: Option<IndexSet<Artifact>>,
    outputs: IndexSet<BuildArtifact>,
    identifier: String,
}

impl WriteAction {
    fn new(
        contents: OwnedFrozenValue,
        is_executable: bool,
        inputs: IndexSet<ArtifactGroup>,
        macro_files: Option<IndexSet<Artifact>>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        if outputs.is_empty() {
            Err(anyhow::anyhow!(
                WriteActionValidationError::NoOutputsSpecified
            ))
        } else if contents.value().as_command_line().is_none() {
            Err(anyhow::anyhow!(
                WriteActionValidationError::ContentsNotCommandLineValue(contents.value().to_repr())
            ))
        } else {
            let identifier = outputs.iter().map(|a| a.get_path().short_path()).join(", ");
            Ok(WriteAction {
                contents,
                is_executable,
                inputs,
                macro_files,
                outputs,
                identifier,
            })
        }
    }

    fn get_contents(&self, fs: &ArtifactFs) -> anyhow::Result<String> {
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
        Ok(Cow::Borrowed(&self.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Borrowed(&self.outputs))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Pristine(self)
    }

    fn category(&self) -> &Category {
        static WRITE_CATEGORY: Lazy<Category> = Lazy::new(|| Category::try_from("write").unwrap());

        &WRITE_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(&self.identifier)
    }

    fn aquery_attributes(&self, fs: &ArtifactFs) -> IndexMap<String, String> {
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
        ctx: &dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs();

        let mut outputs = IndexMap::new();
        let mut execution_time = None;

        // For EdenFS-based buck out, we can do "virtual write" instead, which
        // set the path on the Eden buck-out to a digest and ensure what is about to
        // write exists in CAS.
        // TODO(yipu): We should do the same "virutal write" to write_macro
        if let Some(eden_buck_out) = ctx.materializer().eden_buck_out() {
            let execution_start = Instant::now();
            let full_contents = self.get_contents(fs)?;
            let value = ArtifactValue::file(FileMetadata {
                digest: FileDigest::from_bytes(full_contents.as_bytes()),
                is_executable: self.is_executable,
            });
            eden_buck_out
                .ensure_file_in_cas(&value, full_contents)
                .await?;
            for output in &self.outputs {
                let output_path = fs.resolve_build(output);
                eden_buck_out
                    .set_path_object_id(&output_path, &value)
                    .await?;
                outputs.insert(output.dupe(), value.dupe());
            }
            execution_time = Some(execution_start.elapsed());
        } else {
            ctx.blocking_executor()
                .execute_io_inline(box || {
                    let execution_start = Instant::now();
                    outputs.reserve(self.outputs.len());

                    let full_contents = self.get_contents(fs)?;
                    let value = ArtifactValue::file(FileMetadata {
                        digest: FileDigest::from_bytes(full_contents.as_bytes()),
                        is_executable: self.is_executable,
                    });

                    for output in &self.outputs {
                        fs.write_file(output, &full_contents, self.is_executable)?;
                        outputs.insert(output.dupe(), value.dupe());
                    }

                    execution_time = Some(execution_start.elapsed());

                    Ok(())
                })
                .await?;
        }

        let execution_time = execution_time.context("Action did not set execution_time")?;

        Ok((
            ActionOutputs::new(outputs),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { execution_time },
                stdout: None,
                stderr: None,
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

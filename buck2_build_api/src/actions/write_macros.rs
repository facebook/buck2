/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    borrow::Cow,
    convert::TryFrom,
    time::{Duration, Instant},
};

use async_trait::async_trait;
use buck2_common::file_ops::{FileDigest, FileMetadata};
use buck2_core::{
    category::Category,
    fs::{paths::RelativePathBuf, project::ProjectRelativePathBuf},
};
use gazebo::prelude::*;
use indexmap::{IndexMap, IndexSet};
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::{
    actions::{
        artifact::{ArtifactFs, ArtifactValue, BuildArtifact},
        Action, ActionExecutable, ActionExecutionCtx, ArtifactGroup, PristineActionExecutable,
        UnregisteredAction,
    },
    attrs::attr_type::arg::{value::ResolvedMacro, ArgBuilder},
    execute::{
        ActionExecutionKind, ActionExecutionMetadata, ActionExecutionTimingData, ActionOutputs,
    },
    interpreter::rule_defs::cmd_args::{
        BaseCommandLineBuilder, CommandLineBuilderContext, CommandLineLocation,
        ValueAsCommandLineLike, WriteToFileMacroVisitor,
    },
};

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
        ctx: &dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs();
        let mut output_values = IndexMap::with_capacity(self.outputs.len());
        let mut macro_writer = MacroToFileWriter::new(fs, &self.outputs, &mut output_values);

        let mut execution_time = Duration::ZERO;
        ctx.blocking_executor()
            .execute_io_inline(box || {
                let execution_start = Instant::now();
                let result = self
                    .contents
                    .value()
                    .as_command_line()
                    .unwrap()
                    .visit_write_to_file_macros(&mut macro_writer);
                execution_time = execution_start.elapsed();
                result?;
                Ok(())
            })
            .await?;

        Ok((
            ActionOutputs::new(output_values),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { execution_time },
                stdout: None,
                stderr: None,
            },
        ))
    }
}

struct MacroToFileWriter<'a> {
    fs: &'a ArtifactFs,
    outputs: &'a IndexSet<BuildArtifact>,
    output_values: &'a mut IndexMap<BuildArtifact, ArtifactValue>,
    relative_to_path: Option<RelativePathBuf>,
    output_pos: usize,
}

impl<'a> MacroToFileWriter<'a> {
    fn new(
        fs: &'a ArtifactFs,
        outputs: &'a IndexSet<BuildArtifact>,
        output_values: &'a mut IndexMap<BuildArtifact, ArtifactValue>,
    ) -> Self {
        Self {
            fs,
            outputs,
            output_values,
            output_pos: 0,
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

        let pos = self.output_pos;

        if pos >= self.outputs.len() {
            return Err(anyhow::anyhow!(
                WriteMacrosActionValidationError::InconsistentNumberOfMacroArtifacts
            ));
        }

        let meta = FileMetadata {
            digest: FileDigest::from_bytes(content.as_bytes()),
            is_executable: false,
        };
        let value = ArtifactValue::file(meta);
        self.output_values.insert(self.outputs[pos].dupe(), value);

        self.output_pos += 1;
        self.fs.write_file(&self.outputs[pos], &content, false)
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
    fs: &'a ArtifactFs,
    maybe_relative_to_path: &'a Option<RelativePathBuf>,
    result: String,
}

impl<'a> MacroContentBuilder<'a> {
    fn new(fs: &'a ArtifactFs, maybe_relative_to_path: &'a Option<RelativePathBuf>) -> Self {
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
        project_path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        Ok(CommandLineLocation::from_relative_path(
            self.relativize_path(project_path),
        ))
    }

    fn fs(&self) -> &ArtifactFs {
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

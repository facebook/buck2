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
    io::{sink, Write},
    time::Instant,
};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::file_ops::{FileDigest, FileMetadata};
use buck2_core::category::Category;
use gazebo::prelude::*;
use indexmap::{indexmap, IndexMap, IndexSet};
use itertools::Itertools;
use once_cell::sync::Lazy;
use serde::{Serialize, Serializer};
use starlark::values::{
    dict::Dict, list::List, record::Record, structs::Struct, tuple::Tuple, OwnedFrozenValue, Value,
    ValueLike,
};
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
    interpreter::rule_defs::{
        artifact::{FrozenStarlarkOutputArtifact, StarlarkOutputArtifact, ValueAsArtifactLike},
        cmd_args::{
            builder::BaseCommandLineBuilder, FrozenStarlarkCommandLine, StarlarkCommandLine,
            ValueAsCommandLineLike,
        },
        target_label::StarlarkTargetLabel,
    },
};

#[derive(Debug, Error)]
enum WriteJsonActionValidationError {
    #[error("At least one output file must be specified for a write action")]
    NoOutputsSpecified,
}

/// A wrapper with a Serialize instance so we can pass down the necessary context.
struct SerializeValue<'a, 'v> {
    value: Value<'v>,
    fs: Option<&'a ArtifactFs>,
}

impl<'a, 'v> SerializeValue<'a, 'v> {
    fn with_value(&self, x: Value<'v>) -> Self {
        Self {
            value: x,
            fs: self.fs,
        }
    }
}

fn err<R, E: serde::ser::Error>(res: anyhow::Result<R>) -> Result<R, E> {
    match res {
        Ok(v) => Ok(v),
        Err(e) => Err(serde::ser::Error::custom(format!("{:#}", e))),
    }
}

/// Grab the value as an artifact, if you can.
/// We want to deal with both normal artifacts, and .as_output() artifacts,
/// since otherwise the .as_output ones will fall through as a cmd_args
/// and end up getting wrapped in a list below.
fn get_artifact<'v>(x: Value<'v>) -> Option<Box<dyn FnOnce() -> anyhow::Result<Artifact> + 'v>> {
    if let Some(x) = x.as_artifact() {
        Some(box || Ok(x.get_bound()?.dupe()))
    } else if let Some(x) = x.downcast_ref::<StarlarkOutputArtifact>() {
        Some(box || Ok(((*x.artifact()).dupe().ensure_bound())?.into()))
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkOutputArtifact>() {
        Some(box || Ok(x.artifact().into()))
    } else {
        None
    }
}

impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.value.is_none() {
            serializer.serialize_none()
        } else if let Some(x) = self.value.unpack_str() {
            serializer.serialize_str(x)
        } else if let Some(x) = self.value.unpack_int() {
            serializer.serialize_i32(x)
        } else if let Some(x) = self.value.unpack_bool() {
            serializer.serialize_bool(x)
        } else if let Some(x) = List::from_value(self.value) {
            serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
        } else if let Some(x) = Tuple::from_value(self.value) {
            serializer.collect_seq(x.iter().map(|v| self.with_value(v)))
        } else if let Some(x) = Dict::from_value(self.value) {
            serializer.collect_map(
                x.iter()
                    .map(|(k, v)| (self.with_value(k), self.with_value(v))),
            )
        } else if let Some(x) = Struct::from_value(self.value) {
            serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
        } else if let Some(x) = Record::from_value(self.value) {
            serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
        } else if let Some(x) = StarlarkTargetLabel::from_value(self.value) {
            // Users could do this with `str(ctx.label.raw_target())`, but in some benchmarks that causes
            // a lot of additional memory to be retained for all those strings
            x.serialize(serializer)
        } else if let Some(x) = get_artifact(self.value) {
            match self.fs {
                None => {
                    // Invariant: If fs == None, then the writer = sink().
                    // Therefore, in the None branch, we only care about getting Serde errors,
                    // so pass something of the right type, but don't worry about the value.
                    serializer.serialize_str("")
                }
                Some(fs) => serializer.serialize_str(err(fs.resolve(&err(x())?))?.as_str()),
            }
        } else if let Some(x) = self.value.as_command_line() {
            let singleton = is_singleton_cmdargs(self.value);
            match self.fs {
                None => {
                    // See a few lines up for fs == None details.
                    if singleton {
                        serializer.serialize_str("")
                    } else {
                        serializer.collect_seq(&[""])
                    }
                }
                Some(fs) => {
                    let mut cli_builder = BaseCommandLineBuilder::new(fs);
                    err(x.add_to_command_line(&mut cli_builder))?;
                    let items = cli_builder.build();
                    // We change the type, based on the value - singleton = String, otherwise list.
                    // That's a little annoying (type based on value), but otherwise there would be
                    // no way to produce a cmd_args as a single string.
                    if singleton {
                        serializer.serialize_str(&items.concat())
                    } else {
                        serializer.collect_seq(items)
                    }
                }
            }
        } else {
            Err(serde::ser::Error::custom(format!(
                "Type `{}` is not supported by `write_json`",
                self.value.get_type()
            )))
        }
    }
}

fn is_singleton_cmdargs(x: Value) -> bool {
    if let Some(x) = x.downcast_ref::<StarlarkCommandLine>() {
        x.is_concat()
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkCommandLine>() {
        x.is_concat()
    } else {
        false
    }
}

pub struct UnregisteredWriteJsonAction;

impl UnregisteredWriteJsonAction {
    pub fn new() -> Self {
        Self
    }

    pub fn validate(x: Value) -> anyhow::Result<()> {
        Self::write(x, None, &mut sink())
    }

    fn write(x: Value, fs: Option<&ArtifactFs>, writer: impl Write) -> anyhow::Result<()> {
        serde_json::ser::to_writer(writer, &SerializeValue { value: x, fs })
            .context("When converting to JSON for `write_json`")
    }
}

impl UnregisteredAction for UnregisteredWriteJsonAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        let contents = starlark_data.expect("module data to be present");
        let action = WriteJsonAction::new(contents, inputs, outputs)?;
        Ok(box action)
    }
}

#[derive(Debug)]
struct WriteJsonAction {
    contents: OwnedFrozenValue, // JSON value
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
    identifier: String,
}

impl WriteJsonAction {
    fn new(
        contents: OwnedFrozenValue,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        UnregisteredWriteJsonAction::validate(contents.value())?;
        if outputs.is_empty() {
            Err(WriteJsonActionValidationError::NoOutputsSpecified.into())
        } else {
            let identifier = outputs.iter().map(|a| a.get_path().short_path()).join(", ");
            Ok(WriteJsonAction {
                contents,
                inputs,
                outputs,
                identifier,
            })
        }
    }

    fn write(&self, fs: &ArtifactFs, writer: impl Write) -> anyhow::Result<()> {
        UnregisteredWriteJsonAction::write(self.contents.value(), Some(fs), writer)
    }

    fn get_contents(&self, fs: &ArtifactFs) -> anyhow::Result<Vec<u8>> {
        let mut contents = Vec::new();
        self.write(fs, &mut contents)?;
        Ok(contents)
    }
}

#[async_trait]
impl Action for WriteJsonAction {
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
        static WRITE_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("write_json").unwrap());

        &WRITE_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(&self.identifier)
    }

    fn aquery_attributes(&self, fs: &ArtifactFs) -> IndexMap<String, String> {
        let res: anyhow::Result<String> = try { String::from_utf8(self.get_contents(fs)?)? };
        // TODO(cjhopman): We should change this api to support returning a Result.
        indexmap! {
            "contents".to_owned() => match res {
                Ok(v) => v,
                Err(e) => format!("ERROR: constructing contents ({})", e)
            }
        }
    }
}

#[async_trait]
impl PristineActionExecutable for WriteJsonAction {
    async fn execute(
        &self,
        ctx: &dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs();

        let mut outputs = IndexMap::new();
        let mut execution_time = None;

        ctx.blocking_executor()
            .execute_io_inline(box || {
                let execution_start = Instant::now();
                outputs.reserve(self.outputs.len());

                let full_contents = self.get_contents(fs)?;
                let value = ArtifactValue::file(FileMetadata {
                    digest: FileDigest::from_bytes(&full_contents),
                    is_executable: false,
                });

                for output in &self.outputs {
                    fs.write_file(output, &full_contents, false)?;
                    outputs.insert(output.dupe(), value.dupe());
                }

                execution_time = Some(execution_start.elapsed());

                Ok(())
            })
            .await?;

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

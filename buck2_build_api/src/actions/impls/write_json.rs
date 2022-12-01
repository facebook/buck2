/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;
use std::io::sink;
use std::io::Write;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::category::Category;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::prelude::*;
use indexmap::indexmap;
use indexmap::indexset;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use serde::Serialize;
use serde::Serializer;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::enumeration::EnumValue;
use starlark::values::list::List;
use starlark::values::list::ListRef;
use starlark::values::record::Record;
use starlark::values::structs::Struct;
use starlark::values::tuple::Tuple;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::Artifact;
use crate::actions::execute::action_executor::ActionExecutionKind;
use crate::actions::execute::action_executor::ActionExecutionMetadata;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::IncrementalActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::FrozenStarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::FrozenStarlarkCommandLine;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::provider::ProviderLike;
use crate::interpreter::rule_defs::provider::ValueAsProviderLike;
use crate::interpreter::rule_defs::transitive_set::TransitiveSetJsonProjection;

#[derive(Debug, Error)]
enum WriteJsonActionValidationError {
    #[error("WriteJsonAction received inputs")]
    TooManyInputs,
    #[error("WriteJsonAction received no outputs")]
    NoOutputs,
    #[error("WriteJsonAction received more than one output")]
    TooManyOutputs,
}

/// A wrapper with a Serialize instance so we can pass down the necessary context.
struct SerializeValue<'a, 'v> {
    value: Value<'v>,
    fs: Option<&'a ExecutorFs<'a>>,
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
        Some(box || Ok(x.get_bound_artifact()?.dupe()))
    } else if let Some(x) = x.downcast_ref::<StarlarkOutputArtifact>() {
        Some(box || Ok(((*x.artifact()).dupe().ensure_bound())?.into_artifact()))
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkOutputArtifact>() {
        Some(box || Ok(x.artifact()))
    } else {
        None
    }
}

enum JsonUnpack<'v> {
    None,
    String(&'v str),
    Number(i32),
    Bool(bool),
    List(&'v ListRef<'v>),
    Tuple(&'v Tuple<'v>),
    Dict(DictRef<'v>),
    Struct(&'v Struct<'v>),
    Record(&'v Record<'v>),
    Enum(&'v EnumValue<'v>),
    TransitiveSetJsonProjection(&'v TransitiveSetJsonProjection<'v>),
    TargetLabel(&'v StarlarkTargetLabel),
    Label(&'v Label),
    Artifact(Box<dyn FnOnce() -> anyhow::Result<Artifact> + 'v>),
    CommandLine(&'v dyn CommandLineArgLike),
    Provider(&'v dyn ProviderLike<'v>),
    Unsupported,
}

fn unpack<'v>(value: Value<'v>) -> JsonUnpack<'v> {
    if value.is_none() {
        JsonUnpack::None
    } else if let Some(x) = value.unpack_str() {
        JsonUnpack::String(x)
    } else if let Some(x) = value.unpack_int() {
        JsonUnpack::Number(x)
    } else if let Some(x) = value.unpack_bool() {
        JsonUnpack::Bool(x)
    } else if let Some(x) = List::from_value(value) {
        JsonUnpack::List(x)
    } else if let Some(x) = Tuple::from_value(value) {
        JsonUnpack::Tuple(x)
    } else if let Some(x) = Dict::from_value(value) {
        JsonUnpack::Dict(x)
    } else if let Some(x) = Struct::from_value(value) {
        JsonUnpack::Struct(x)
    } else if let Some(x) = Record::from_value(value) {
        JsonUnpack::Record(x)
    } else if let Some(x) = EnumValue::from_value(value) {
        JsonUnpack::Enum(x)
    } else if let Some(x) = TransitiveSetJsonProjection::from_value(value) {
        JsonUnpack::TransitiveSetJsonProjection(x)
    } else if let Some(x) = StarlarkTargetLabel::from_value(value) {
        JsonUnpack::TargetLabel(x)
    } else if let Some(x) = Label::from_value(value) {
        JsonUnpack::Label(x)
    } else if let Some(x) = get_artifact(value) {
        JsonUnpack::Artifact(x)
    } else if let Some(x) = value.as_command_line() {
        JsonUnpack::CommandLine(x)
    } else if let Some(x) = value.as_provider() {
        JsonUnpack::Provider(x)
    } else {
        JsonUnpack::Unsupported
    }
}

impl<'a, 'v> Serialize for SerializeValue<'a, 'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match unpack(self.value) {
            JsonUnpack::None => serializer.serialize_none(),
            JsonUnpack::String(x) => serializer.serialize_str(x),
            JsonUnpack::Number(x) => serializer.serialize_i32(x),
            JsonUnpack::Bool(x) => serializer.serialize_bool(x),
            JsonUnpack::List(x) => serializer.collect_seq(x.iter().map(|v| self.with_value(v))),
            JsonUnpack::Tuple(x) => serializer.collect_seq(x.iter().map(|v| self.with_value(v))),
            JsonUnpack::Dict(x) => serializer.collect_map(
                x.iter()
                    .map(|(k, v)| (self.with_value(k), self.with_value(v))),
            ),
            JsonUnpack::Struct(x) => {
                serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
            }
            JsonUnpack::Record(x) => {
                serializer.collect_map(x.iter().map(|(k, v)| (k, self.with_value(v))))
            }
            JsonUnpack::Enum(x) => x.serialize(serializer),
            JsonUnpack::TransitiveSetJsonProjection(x) => {
                serializer.collect_seq(err(x.iter_values())?.map(|v| self.with_value(v)))
            }
            JsonUnpack::TargetLabel(x) => {
                // Users could do this with `str(ctx.label.raw_target())`, but in some benchmarks that causes
                // a lot of additional memory to be retained for all those strings
                x.serialize(serializer)
            }
            JsonUnpack::Label(x) => {
                // Users could do this with `str(ctx.label)`, but a bit wasteful
                x.serialize(serializer)
            }
            JsonUnpack::Artifact(x) => {
                match self.fs {
                    None => {
                        // Invariant: If fs == None, then the writer = sink().
                        // Therefore, in the None branch, we only care about getting Serde errors,
                        // so pass something of the right type, but don't worry about the value.
                        serializer.serialize_str("")
                    }
                    Some(fs) => serializer
                        .serialize_str(err(fs.fs().resolve(err(x())?.get_path()))?.as_str()),
                }
            }
            JsonUnpack::CommandLine(x) => {
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
                        // WriteJsonCommandLineArgGen assumes that any args/write-to-file macros are
                        // rejected here and needs to be updated if that changes.
                        let mut items = Vec::<String>::new();
                        let mut ctx = BaseCommandLineBuilder::new(fs);
                        err(x.add_to_command_line(&mut items, &mut ctx))?;
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
            }
            JsonUnpack::Provider(x) => {
                serializer.collect_map(x.items().iter().map(|(k, v)| (k, self.with_value(*v))))
            }
            JsonUnpack::Unsupported => Err(serde::ser::Error::custom(format!(
                "Type `{}` is not supported by `write_json`",
                self.value.get_type()
            ))),
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

#[derive(Allocative)]
pub struct UnregisteredWriteJsonAction;

impl UnregisteredWriteJsonAction {
    pub fn new() -> Self {
        Self
    }

    pub fn validate(x: Value) -> anyhow::Result<()> {
        Self::write(x, None, &mut sink())
    }

    pub fn cli<'v>(
        artifact: Value<'v>,
        content: Value<'v>,
    ) -> anyhow::Result<WriteJsonCommandLineArg<'v>> {
        Ok(WriteJsonCommandLineArg { artifact, content })
    }

    fn write(x: Value, fs: Option<&ExecutorFs>, writer: impl Write) -> anyhow::Result<()> {
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

#[derive(Debug, Allocative)]
struct WriteJsonAction {
    contents: OwnedFrozenValue, // JSON value
    output: BuildArtifact,
}

impl WriteJsonAction {
    fn new(
        contents: OwnedFrozenValue,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        UnregisteredWriteJsonAction::validate(contents.value())?;

        let mut outputs = outputs.into_iter();

        let output = match (outputs.next(), outputs.next()) {
            (Some(o), None) => o,
            (None, ..) => return Err(WriteJsonActionValidationError::NoOutputs.into()),
            (Some(..), Some(..)) => {
                return Err(WriteJsonActionValidationError::TooManyOutputs.into());
            }
        };

        if !inputs.is_empty() {
            return Err(WriteJsonActionValidationError::TooManyInputs.into());
        }

        Ok(WriteJsonAction { contents, output })
    }

    fn write(&self, fs: &ExecutorFs, writer: impl Write) -> anyhow::Result<()> {
        UnregisteredWriteJsonAction::write(self.contents.value(), Some(fs), writer)
    }

    fn get_contents(&self, fs: &ExecutorFs) -> anyhow::Result<Vec<u8>> {
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
        Ok(Cow::Owned(IndexSet::new()))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
        Ok(Cow::Owned(indexset![self.output.dupe()]))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Incremental(self)
    }

    fn category(&self) -> &Category {
        static WRITE_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("write_json").unwrap());

        &WRITE_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output.get_path().short_path().as_str())
    }

    fn aquery_attributes(&self, fs: &ExecutorFs) -> IndexMap<String, String> {
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
impl IncrementalActionExecutable for WriteJsonAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let fs = ctx.fs();

        let mut execution_start = None;

        let value = ctx
            .materializer()
            .declare_write(box || {
                execution_start = Some(Instant::now());
                let content = self.get_contents(&ctx.executor_fs())?;
                Ok(vec![WriteRequest {
                    path: fs.resolve_build(self.output.get_path()),
                    content,
                    is_executable: false,
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
            ActionOutputs::new(indexmap![self.output.get_path().dupe() => value]),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData { wall_time },
            },
        ))
    }
}

/// WriteJsonCommandLineArgGen represents the artifact produced by write_json in a way that it can
/// be added to commandlines while including the artifacts referenced by cmdargs in the content that
/// was written.
#[derive(Debug, Clone, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[derive(NoSerialize)] // TODO we should probably have a serialization for transitive set
#[repr(C)]
pub struct WriteJsonCommandLineArgGen<V> {
    artifact: V,
    // The list of artifacts here could be large and we don't want to hold those explicitly (due to
    // the memory cost) and so we hold the same content value that the write_json action itself will and
    // only traverse it when artifacts are requested.
    content: V,
}

impl<'v, V: ValueLike<'v>> fmt::Display for WriteJsonCommandLineArgGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<write_json_cli_args>")
    }
}

starlark_complex_value!(pub WriteJsonCommandLineArg);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for WriteJsonCommandLineArgGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("write_json_cli_args");
}

pub(crate) fn visit_json_artifacts(
    v: Value,
    visitor: &mut dyn CommandLineArtifactVisitor,
) -> anyhow::Result<()> {
    match unpack(v) {
        JsonUnpack::None
        | JsonUnpack::String(_)
        | JsonUnpack::Number(_)
        | JsonUnpack::Bool(_)
        | JsonUnpack::TargetLabel(_)
        | JsonUnpack::Enum(_)
        | JsonUnpack::Label(_) => {}

        JsonUnpack::List(x) => {
            for x in x.iter() {
                visit_json_artifacts(x, visitor)?;
            }
        }
        JsonUnpack::Tuple(x) => {
            for x in x.iter() {
                visit_json_artifacts(x, visitor)?;
            }
        }
        JsonUnpack::Dict(x) => {
            for (k, v) in x.iter() {
                visit_json_artifacts(k, visitor)?;
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::Struct(x) => {
            for (_k, v) in x.iter() {
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::Record(x) => {
            for (_k, v) in x.iter() {
                visit_json_artifacts(v, visitor)?;
            }
        }
        JsonUnpack::TransitiveSetJsonProjection(x) => visitor.visit_input(
            ArtifactGroup::TransitiveSetProjection(x.to_projection_key()?),
            None,
        ),
        JsonUnpack::Artifact(_x) => {
            // The _x function requires that the artifact is already bound, but we may need to visit artifacts
            // before that happens. Treating it like an opaque command_line works as we want for any artifact
            // type.
            v.as_command_line_err()?.visit_artifacts(visitor)?;
        }
        JsonUnpack::CommandLine(x) => x.visit_artifacts(visitor)?,
        JsonUnpack::Unsupported => {
            return Err(anyhow::anyhow!(
                "Type `{}` is not supported by `write_json` (this should be unreachable)",
                v.get_type()
            ));
        }
        JsonUnpack::Provider(x) => {
            for (_, v) in x.items() {
                visit_json_artifacts(v, visitor)?;
            }
        }
    }
    Ok(())
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for WriteJsonCommandLineArgGen<V> {
    fn add_to_command_line(
        &self,
        builder: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        self.artifact
            .to_value()
            .as_command_line_err()?
            .add_to_command_line(builder, context)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let artifact = self.artifact.to_value();
        let content = self.content.to_value();
        artifact.as_command_line_err()?.visit_artifacts(visitor)?;
        visit_json_artifacts(content, visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        // In the write_json implementation, the commandlinebuilders we use don't support args.
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        // In the write_json implementation, the commandlinebuilders we use don't support args.
        Ok(())
    }
}

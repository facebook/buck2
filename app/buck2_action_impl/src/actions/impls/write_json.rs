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
use std::io::Write;
use std::slice;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::impls::json;
use buck2_build_api::actions::impls::json::validate_json;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::IncrementalActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use buck2_core::category::Category;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::WriteRequest;
use dupe::Dupe;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::starlark_complex_value;
use starlark::starlark_type;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValue;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

#[derive(Debug, Error)]
enum WriteJsonActionValidationError {
    #[error("WriteJsonAction received inputs")]
    TooManyInputs,
    #[error("WriteJsonAction received no outputs")]
    NoOutputs,
    #[error("WriteJsonAction received more than one output")]
    TooManyOutputs,
}

#[derive(Allocative)]
pub(crate) struct UnregisteredWriteJsonAction;

impl UnregisteredWriteJsonAction {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn cli<'v>(
        artifact: Value<'v>,
        content: Value<'v>,
    ) -> anyhow::Result<WriteJsonCommandLineArg<'v>> {
        Ok(WriteJsonCommandLineArg { artifact, content })
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
        Ok(Box::new(action))
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
        validate_json(contents.value())?;

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
        json::write_json(self.contents.value(), Some(fs), writer)
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

    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
        Ok(Cow::Borrowed(slice::from_ref(&self.output)))
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
        Some(self.output.get_path().path().as_str())
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
            .declare_write(Box::new(|| {
                execution_start = Some(Instant::now());
                let content = self.get_contents(&ctx.executor_fs())?;
                Ok(vec![WriteRequest {
                    path: fs.resolve_build(self.output.get_path()),
                    content,
                    is_executable: false,
                }])
            }))
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
pub(crate) struct WriteJsonCommandLineArgGen<V> {
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

starlark_complex_value!(pub(crate) WriteJsonCommandLineArg);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for WriteJsonCommandLineArgGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("write_json_cli_args");

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for WriteJsonCommandLineArgGen<V> {
    fn add_to_command_line(
        &self,
        builder: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
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
        json::visit_json_artifacts(content, visitor)
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::PristineActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_core::category::Category;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::OutputType;
use derivative::Derivative;
use dupe::Dupe;
use indexmap::IndexSet;
use sorted_vector_map::sorted_vector_map;
use starlark::values::OwnedFrozenValue;

/// A simple unregistered action that will eventually be resolved into an action that runs the
/// given cmd as the action execution command. Used for testing
///
/// This action is for testing, and bypasses the need to create starlark values and frozen
/// modules
#[derive(Allocative)]
pub(crate) struct SimpleUnregisteredAction {
    cmd: Vec<String>,
    category: Category,
    identifier: Option<String>,
}

impl SimpleUnregisteredAction {
    pub(crate) fn new(cmd: Vec<String>, category: Category, identifier: Option<String>) -> Self {
        Self {
            cmd,
            category,
            identifier,
        }
    }
}

/// The action created by SimpleUnregisteredAction, or directly.
#[derive(Derivative, Allocative)]
#[derivative(Debug)]
pub(crate) struct SimpleAction {
    inputs: BoxSliceSet<ArtifactGroup>,
    outputs: BoxSliceSet<BuildArtifact>,
    cmd: Vec<String>,
    category: Category,
    identifier: Option<String>,
}

impl SimpleAction {
    pub(crate) fn new(
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        cmd: Vec<String>,
        category: Category,
        identifier: Option<String>,
    ) -> Self {
        Self {
            inputs: BoxSliceSet::from(inputs),
            outputs: BoxSliceSet::from(outputs),
            cmd,
            category,
            identifier,
        }
    }
}

impl UnregisteredAction for SimpleUnregisteredAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(Box::new(SimpleAction {
            inputs: BoxSliceSet::from(inputs),
            outputs: BoxSliceSet::from(outputs),
            cmd: self.cmd,
            category: self.category,
            identifier: self.identifier,
        }))
    }
}

#[async_trait]
impl Action for SimpleAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::NotSet
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(self.inputs.as_slice()))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
        Ok(Cow::Borrowed(self.outputs.as_slice()))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Pristine(self)
    }

    fn category(&self) -> &Category {
        &self.category
    }

    fn identifier(&self) -> Option<&str> {
        self.identifier.as_deref()
    }
}

#[async_trait]
impl PristineActionExecutable for SimpleAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let req = CommandExecutionRequest::new(
            self.cmd.clone(),
            CommandExecutionPaths::new(
                Vec::new(),
                self.outputs
                    .iter()
                    .map(|b| CommandExecutionOutput::BuildArtifact {
                        path: b.get_path().dupe(),
                        output_type: OutputType::File,
                    })
                    .collect(),
                ctx.fs(),
                ctx.digest_config(),
            )?,
            sorted_vector_map![],
        );

        let (outputs, meta) = ctx.exec_cmd(&req).await?;

        let outputs = ActionOutputs::new(outputs);

        Ok((outputs, meta))
    }
}

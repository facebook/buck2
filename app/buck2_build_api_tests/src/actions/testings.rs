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
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_core::category::Category;
use buck2_core::category::CategoryRef;
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
#[derive(Allocative, Clone, PartialEq)]
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
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
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

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(self.inputs.as_slice()))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(self.outputs.as_slice())
    }

    fn first_output(&self) -> &BuildArtifact {
        &self.outputs.as_slice()[0]
    }

    fn category(&self) -> CategoryRef {
        self.category.as_ref()
    }

    fn identifier(&self) -> Option<&str> {
        self.identifier.as_deref()
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let req = CommandExecutionRequest::new(
            vec![],
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

        let prepared_action = ctx.prepare_action(&req)?;
        let manager = ctx.command_execution_manager();
        let result = ctx.exec_cmd(manager, &req, &prepared_action).await;
        let (outputs, meta) = ctx.unpack_command_execution_result(
            req.executor_preference,
            result,
            false,
            false,
            None,
        )?;

        Ok((outputs, meta))
    }
}

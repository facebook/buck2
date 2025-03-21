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
use buck2_build_api::actions::box_slice_set::BoxSliceSet;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_core::category::CategoryRef;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CopiedArtifact;
use dupe::Dupe;
use gazebo::prelude::*;
use indexmap::IndexSet;
use starlark::values::OwnedFrozenValue;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum CopyActionValidationError {
    #[error("Exactly one input file must be specified for a copy action, got {0}")]
    WrongNumberOfInputs(usize),
    #[error("Exactly one output file must be specified for a copy action, got {0}")]
    WrongNumberOfOutputs(usize),
    #[error("Only artifact inputs are supported in copy actions, got {0}")]
    UnsupportedInput(ArtifactGroup),
}

#[derive(Debug, Allocative)]
pub(crate) enum CopyMode {
    Copy,
    Symlink,
}

#[derive(Allocative)]
pub(crate) struct UnregisteredCopyAction {
    copy: CopyMode,
}

impl UnregisteredCopyAction {
    pub(crate) fn new(copy: CopyMode) -> Self {
        Self { copy }
    }
}

impl UnregisteredAction for UnregisteredCopyAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        Ok(Box::new(CopyAction::new(self.copy, inputs, outputs)?))
    }
}

#[derive(Debug, Allocative)]
struct CopyAction {
    copy: CopyMode,
    inputs: BoxSliceSet<ArtifactGroup>,
    outputs: BoxSliceSet<BuildArtifact>,
}

impl CopyAction {
    fn new(
        copy: CopyMode,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
    ) -> buck2_error::Result<Self> {
        // TODO: Exclude other variants once they become available here. For now, this is a noop.
        match inputs.iter().into_singleton() {
            Some(ArtifactGroup::Artifact(..) | ArtifactGroup::Promise(..)) => {}
            Some(other) => {
                return Err(CopyActionValidationError::UnsupportedInput(other.dupe()).into());
            }
            None => return Err(CopyActionValidationError::WrongNumberOfInputs(inputs.len()).into()),
        };

        if outputs.len() != 1 {
            Err(CopyActionValidationError::WrongNumberOfOutputs(outputs.len()).into())
        } else {
            Ok(CopyAction {
                copy,
                inputs: BoxSliceSet::from(inputs),
                outputs: BoxSliceSet::from(outputs),
            })
        }
    }

    fn input(&self) -> &ArtifactGroup {
        self.inputs
            .iter()
            .next()
            .expect("a single input by construction")
    }

    fn output(&self) -> &BuildArtifact {
        self.outputs
            .iter()
            .next()
            .expect("a single artifact by construction")
    }
}

#[async_trait]
impl Action for CopyAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::Copy
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(self.inputs.as_slice()))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(self.outputs.as_slice())
    }

    fn first_output(&self) -> &BuildArtifact {
        self.output()
    }

    fn category(&self) -> CategoryRef {
        CategoryRef::unchecked_new("copy")
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output().get_path().path().as_str())
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        let (input, src_value) = ctx
            .artifact_values(self.input())
            .iter()
            .into_singleton()
            .buck_error_context("Input did not dereference to exactly one artifact")?;

        let artifact_fs = ctx.fs();
        let src = input.resolve_path(artifact_fs)?;
        let dest = artifact_fs.resolve_build(self.output().get_path());

        let value = {
            let fs = artifact_fs.fs();
            let mut builder = ArtifactValueBuilder::new(fs, ctx.digest_config());
            match self.copy {
                CopyMode::Copy => {
                    builder.add_copied(src_value, src.as_ref(), dest.as_ref())?;
                }
                CopyMode::Symlink => {
                    builder.add_symlinked(src_value, src.as_ref(), dest.as_ref())?;
                }
            }

            builder.build(dest.as_ref())?
        };

        ctx.materializer()
            .declare_copy(
                dest.clone(),
                value.dupe(),
                // FIXME(JakobDegen): This is wrong in cases where the input artifact is a source
                // directory with ignored paths, as the materializer will incorrectly assume that
                // the source directory matches the artifact value when it doesn't.
                vec![CopiedArtifact::new(
                    src,
                    dest,
                    value.entry().dupe().map_dir(|d| d.as_immutable()),
                )],
                ctx.cancellation_context(),
            )
            .await?;

        Ok((
            ActionOutputs::from_single(self.output().get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
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
    fn copies_file() {}
}

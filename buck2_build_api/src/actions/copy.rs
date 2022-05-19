/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, convert::TryFrom};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::category::Category;
use gazebo::prelude::*;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::{
    actions::{
        artifact::BuildArtifact, artifact_utils::ArtifactValueBuilder, Action, ActionExecutable,
        ActionExecutionCtx, PristineActionExecutable, UnregisteredAction,
    },
    artifact_groups::ArtifactGroup,
    execute::{
        materializer::CopiedArtifact, ActionExecutionKind, ActionExecutionMetadata,
        ActionExecutionTimingData, ActionOutputs,
    },
};

#[derive(Debug, Error)]
enum CopyActionValidationError {
    #[error("Exactly one input file must be specified for a copy action, got {0}")]
    WrongNumberOfInputs(usize),
    #[error("Exactly one output file must be specified for a copy action, got {0}")]
    WrongNumberOfOutputs(usize),
    #[error("Only artifact inputs are supported in copy actions, got {0}")]
    UnsupportedInput(ArtifactGroup),
}

pub struct UnregisteredCopyAction {
    copy: bool,
}

impl UnregisteredCopyAction {
    pub fn new(copy: bool) -> Self {
        Self { copy }
    }
}

impl UnregisteredAction for UnregisteredCopyAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(box CopyAction::new(self.copy, inputs, outputs)?)
    }
}

#[derive(Debug)]
struct CopyAction {
    copy: bool,
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
}

impl CopyAction {
    fn new(
        copy: bool,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
    ) -> anyhow::Result<Self> {
        // TODO: Exclude other variants once they become available here. For now, this is a noop.
        match inputs.iter().into_singleton() {
            Some(ArtifactGroup::Artifact(..)) => {}
            Some(other) => {
                return Err(CopyActionValidationError::UnsupportedInput(other.dupe()).into());
            }
            None => return Err(CopyActionValidationError::WrongNumberOfInputs(inputs.len()).into()),
        };

        if outputs.len() != 1 {
            Err(anyhow::anyhow!(
                CopyActionValidationError::WrongNumberOfOutputs(outputs.len())
            ))
        } else {
            Ok(CopyAction {
                copy,
                inputs,
                outputs,
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
        static COPY_CATEGORY: Lazy<Category> = Lazy::new(|| Category::try_from("copy").unwrap());

        &COPY_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output().get_path().short_path().as_str())
    }
}

#[async_trait]
impl PristineActionExecutable for CopyAction {
    async fn execute(
        &self,
        ctx: &dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let (input, src_value) = ctx
            .artifact_values(self.input())
            .iter()
            .into_singleton()
            .context("Input did not dereference to exactly one artifact")?;

        let artifact_fs = ctx.fs();
        let src = artifact_fs.resolve(input)?;
        let dest = artifact_fs.resolve_build(self.output());

        let value = {
            let fs = artifact_fs.fs();
            let mut builder = ArtifactValueBuilder::new(fs);
            if self.copy {
                builder.add_copied(src_value, src.as_ref(), dest.as_ref())?;
            } else {
                builder.add_symlinked(src_value, src.as_ref(), dest.as_ref())?;
            }

            builder.build(dest.as_ref())?
        };

        ctx.materializer()
            .declare_copy(
                dest.as_ref(),
                value.dupe(),
                vec![CopiedArtifact::new(
                    src,
                    dest.clone(),
                    value.entry().dupe().map_dir(|d| d.as_immutable()),
                )],
            )
            .await?;

        Ok((
            ActionOutputs::from_single(self.output().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
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
    fn copies_file() {}
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::category::Category;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use chrono::DateTime;
use chrono::Utc;
use gazebo::prelude::*;
use indexmap::indexset;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::execute::action_executor::ActionExecutionKind;
use crate::actions::execute::action_executor::ActionExecutionMetadata;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::Action;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::IncrementalActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;

#[derive(Debug, Error)]
enum CasArtifactActionDeclarationError {
    #[error("CAS artifact action should not have inputs, got {0}")]
    WrongNumberOfInputs(usize),
    #[error("CAS artifact action should have exactly 1 output, got {0}")]
    WrongNumberOfOutputs(usize),
}

#[derive(Debug, Error)]
enum CasArtifactActionExecutionError {
    #[error("Error accessing digest expiration for: `{0}`")]
    GetDigestExpirationError(FileDigest),

    #[error(
        "The digest `{digest}` was declated to expire after `{declared_expiration}`, but it expires at `{effective_expiration}`"
    )]
    InvalidExpiration {
        digest: FileDigest,
        declared_expiration: DateTime<Utc>,
        effective_expiration: DateTime<Utc>,
    },
}

/// This is an action that lets you reference a CAS artifact. Notionally it's a bit like
/// download_file. When the action executes it'll just verify that the content exists. You have to
/// provide an minimum expiration timestamp when you add this to force users to think about the TTL
/// of the artifacts they are referencing (though admittedly this was also an issue in
/// download_file).
#[derive(Debug)]
pub struct UnregisteredCasArtifactAction {
    digest: FileDigest,
    re_use_case: RemoteExecutorUseCase,
    /// We require the caller to declare when this digest will expire. The intention is to force
    /// callers to pay some modicum of attention to when their digests expire.
    expires_after: DateTime<Utc>,
    executable: bool,
}

impl UnregisteredCasArtifactAction {
    pub fn new(
        digest: FileDigest,
        re_use_case: RemoteExecutorUseCase,
        expires_after: DateTime<Utc>,
        executable: bool,
    ) -> Self {
        Self {
            digest,
            re_use_case,
            expires_after,
            executable,
        }
    }
}

impl UnregisteredAction for UnregisteredCasArtifactAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(box CasArtifactAction::new(inputs, outputs, *self)?)
    }
}

#[derive(Debug)]
struct CasArtifactAction {
    output: BuildArtifact,
    inner: UnregisteredCasArtifactAction,
}

impl CasArtifactAction {
    fn new(
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredCasArtifactAction,
    ) -> anyhow::Result<Self> {
        if !inputs.is_empty() {
            return Err(anyhow::anyhow!(
                CasArtifactActionDeclarationError::WrongNumberOfInputs(inputs.len())
            ));
        }

        let outputs_len = outputs.len();
        let mut outputs = outputs.into_iter();

        let output = match (outputs.next(), outputs.next()) {
            (Some(output), None) => output,
            _ => {
                return Err(anyhow::anyhow!(
                    CasArtifactActionDeclarationError::WrongNumberOfOutputs(outputs_len)
                ));
            }
        };

        Ok(Self { output, inner })
    }
}

#[async_trait]
impl Action for CasArtifactAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::CasArtifact
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
        static CAS_ARTIFACT_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("cas_artifact").unwrap());
        &CAS_ARTIFACT_CATEGORY
    }
}

#[async_trait]
impl IncrementalActionExecutable for CasArtifactAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let expiration = ctx
            .re_client()
            .get_digest_expiration(self.inner.digest.to_re(), self.inner.re_use_case)
            .await
            .with_context(|| {
                CasArtifactActionExecutionError::GetDigestExpirationError(self.inner.digest.dupe())
            })?;

        if expiration < self.inner.expires_after {
            return Err(CasArtifactActionExecutionError::InvalidExpiration {
                digest: self.inner.digest.dupe(),
                declared_expiration: self.inner.expires_after,
                effective_expiration: expiration,
            }
            .into());
        }

        let digest = TrackedFileDigest::new_expires(self.inner.digest.dupe(), expiration);

        let metadata = FileMetadata {
            digest,
            is_executable: self.inner.executable,
        };
        let value = ArtifactValue::file(metadata);

        let path = ctx.fs().resolve_build(self.output.get_path());
        ctx.materializer()
            .declare_cas_many(
                Arc::new(CasDownloadInfo::new_declared(self.inner.re_use_case)),
                vec![(path, value.dupe())],
            )
            .await?;

        Ok((
            ActionOutputs::from_single(self.output.get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Deferred,
                timing: ActionExecutionTimingData::default(),
            },
        ))
    }
}

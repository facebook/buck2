/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::slice;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutable;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::IncrementalActionExecutable;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::category::Category;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::directory::re_directory_to_re_tree;
use buck2_execute::directory::re_tree_to_directory;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::INTERNER;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use dupe::Dupe;
use indexmap::IndexSet;
use more_futures::cancellation::CancellationContext;
use once_cell::sync::Lazy;
use remote_execution as RE;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

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
        "The digest `{digest}` was declared to expire after `{declared_expiration}`, but it expires at `{effective_expiration}`"
    )]
    InvalidExpiration {
        digest: FileDigest,
        declared_expiration: DateTime<Utc>,
        effective_expiration: DateTime<Utc>,
    },
}

#[derive(Debug, Allocative, Clone, Dupe, Copy)]
pub(crate) enum DirectoryKind {
    Directory,
    Tree,
}

#[derive(Debug, Allocative)]
pub(crate) enum ArtifactKind {
    Directory(DirectoryKind),
    File,
}

/// This is an action that lets you reference a CAS artifact. Notionally it's a bit like
/// download_file. When the action executes it'll just verify that the content exists. You have to
/// provide an minimum expiration timestamp when you add this to force users to think about the TTL
/// of the artifacts they are referencing (though admittedly this was also an issue in
/// download_file).
#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredCasArtifactAction {
    pub(crate) digest: FileDigest,
    pub(crate) re_use_case: RemoteExecutorUseCase,
    /// We require the caller to declare when this digest will expire. The intention is to force
    /// callers to pay some modicum of attention to when their digests expire.
    #[allocative(skip)]
    pub(crate) expires_after: DateTime<Utc>,
    pub(crate) executable: bool,
    pub(crate) kind: ArtifactKind,
}

impl UnregisteredAction for UnregisteredCasArtifactAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(Box::new(CasArtifactAction::new(inputs, outputs, *self)?))
    }
}

#[derive(Debug, Allocative)]
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
        static CAS_ARTIFACT_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("cas_artifact").unwrap());
        &CAS_ARTIFACT_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output.get_path().path().as_str())
    }
}

#[async_trait]
impl IncrementalActionExecutable for CasArtifactAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        cancellation: &CancellationContext,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let expiration = ctx
            .re_client()
            .get_digest_expirations(vec![self.inner.digest.to_re()], self.inner.re_use_case)
            .await
            .with_context(|| {
                CasArtifactActionExecutionError::GetDigestExpirationError(self.inner.digest.dupe())
            })?
            .into_iter()
            .next()
            .context("get_digest_expirations did not return anything")?
            .1;

        if expiration < self.inner.expires_after {
            return Err(CasArtifactActionExecutionError::InvalidExpiration {
                digest: self.inner.digest.dupe(),
                declared_expiration: self.inner.expires_after,
                effective_expiration: expiration,
            }
            .into());
        }

        let value = match self.inner.kind {
            ArtifactKind::Directory(directory_kind) => {
                let tree = match directory_kind {
                    DirectoryKind::Tree => ctx
                        .re_client()
                        .download_typed_blobs::<RE::Tree>(
                            vec![self.inner.digest.to_re()],
                            self.inner.re_use_case,
                        )
                        .await
                        .map_err(anyhow::Error::from)
                        .and_then(|trees| trees.into_iter().next().context("RE response was empty"))
                        .with_context(|| {
                            format!("Error downloading tree: {}", self.inner.digest)
                        })?,
                    DirectoryKind::Directory => {
                        let re_client = ctx.re_client();
                        let root_directory = re_client
                            .download_typed_blobs::<RE::Directory>(
                                vec![self.inner.digest.to_re()],
                                self.inner.re_use_case,
                            )
                            .await
                            .map_err(anyhow::Error::from)
                            .and_then(|dirs| {
                                dirs.into_iter().next().context("RE response was empty")
                            })
                            .with_context(|| {
                                format!("Error downloading dir: {}", self.inner.digest)
                            })?;
                        re_directory_to_re_tree(root_directory, &re_client, self.inner.re_use_case)
                            .await?
                    }
                };

                // NOTE: We assign a zero timestamp here because we didn't check the nodes in the tree,
                // just the tree itself. Perhaps we should, but some of the prospective users for this
                // have very large trees so that might not be wise.
                let dir = re_tree_to_directory(
                    &tree,
                    &Utc.timestamp_opt(0, 0).unwrap(),
                    ctx.digest_config(),
                )
                .context("Invalid directory")?;

                ArtifactValue::new(
                    ActionDirectoryEntry::Dir(
                        dir.fingerprint(ctx.digest_config().as_directory_serializer())
                            .shared(&*INTERNER),
                    ),
                    None,
                )
            }
            ArtifactKind::File => {
                let digest = TrackedFileDigest::new_expires(
                    self.inner.digest.dupe(),
                    expiration,
                    ctx.digest_config().cas_digest_config(),
                );
                let metadata = FileMetadata {
                    digest,
                    is_executable: self.inner.executable,
                };
                ArtifactValue::file(metadata)
            }
        };

        let path = ctx.fs().resolve_build(self.output.get_path());
        ctx.materializer()
            .declare_cas_many(
                Arc::new(CasDownloadInfo::new_declared(self.inner.re_use_case)),
                vec![(path, value.dupe())],
                cancellation,
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::slice;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_build_api::actions::UnregisteredAction;
use buck2_build_api::actions::execute::action_executor::ActionExecutionKind;
use buck2_build_api::actions::execute::action_executor::ActionExecutionMetadata;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::execute::error::ExecuteError;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_common::io::trace::TracingIoProvider;
use buck2_core::category::CategoryRef;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::INTERNER;
use buck2_execute::directory::re_directory_to_re_tree;
use buck2_execute::directory::re_tree_to_directory;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use chrono::DateTime;
use chrono::TimeZone;
use chrono::Utc;
use dupe::Dupe;
use indexmap::IndexSet;
use remote_execution as RE;
use starlark::values::OwnedFrozenValue;

use crate::actions::impls::offline;

#[derive(Debug, buck2_error::Error)]
enum CasArtifactActionDeclarationError {
    #[error("CAS artifact action should have exactly 1 output, got {0}")]
    #[buck2(tag = ReCasArtifactWrongNumberOfOutputs)]
    WrongNumberOfOutputs(usize),
}

#[derive(Debug, buck2_error::Error)]
enum CasArtifactActionExecutionError {
    #[error(
        "The digest `{digest}` was declared to expire after `{declared_expiration}`, but it expires at `{effective_expiration}`"
    )]
    #[buck2(tag = ReCasArtifactInvalidExpiration)]
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
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        Ok(Box::new(CasArtifactAction::new(outputs, *self)?))
    }
}

#[derive(Debug, Allocative)]
struct CasArtifactAction {
    output: BuildArtifact,
    inner: UnregisteredCasArtifactAction,
}

impl CasArtifactAction {
    fn new(
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredCasArtifactAction,
    ) -> buck2_error::Result<Self> {
        let outputs_len = outputs.len();
        let mut outputs = outputs.into_iter();

        let output = match (outputs.next(), outputs.next()) {
            (Some(output), None) => output,
            _ => {
                return Err(
                    CasArtifactActionDeclarationError::WrongNumberOfOutputs(outputs_len).into(),
                );
            }
        };

        Ok(Self { output, inner })
    }

    async fn execute_for_offline(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> buck2_error::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let outputs = offline::declare_copy_from_offline_cache(ctx, &[&self.output]).await?;

        Ok((
            outputs,
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Deferred,
                timing: ActionExecutionTimingData::default(),
                input_files_bytes: None,
            },
        ))
    }
}

#[async_trait]
impl Action for CasArtifactAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::CasArtifact
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(slice::from_ref(&self.output))
    }

    fn first_output(&self) -> &BuildArtifact {
        &self.output
    }

    fn category(&self) -> CategoryRef<'_> {
        CategoryRef::unchecked_new("cas_artifact")
    }

    fn identifier(&self) -> Option<&str> {
        Some(self.output.get_path().path().as_str())
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        // If running in offline environment, try to restore from cached outputs.
        if ctx.run_action_knobs().use_network_action_output_cache {
            return self.execute_for_offline(ctx).await.map_err(Into::into);
        }

        let re_client = ctx.re_client().with_use_case(self.inner.re_use_case);
        let expiration = re_client
            .get_digest_expirations(vec![self.inner.digest.to_re()])
            .await
            .with_buck_error_context(|| {
                format!(
                    "Error accessing digest expiration for: `{}`",
                    self.inner.digest,
                )
            })?
            .into_iter()
            .next()
            .buck_error_context("get_digest_expirations did not return anything")
            .tag(buck2_error::ErrorTag::ReCasArtifactGetDigestExpirationError)?
            .1;

        if expiration < self.inner.expires_after {
            return Err(buck2_error::Error::from(
                CasArtifactActionExecutionError::InvalidExpiration {
                    digest: self.inner.digest.dupe(),
                    declared_expiration: self.inner.expires_after,
                    effective_expiration: expiration,
                },
            )
            .into());
        }

        let value = match self.inner.kind {
            ArtifactKind::Directory(directory_kind) => {
                // TODO: should honor the semaphore here from OutputTreesDownloadConfig.

                let tree = match directory_kind {
                    DirectoryKind::Tree => re_client
                        .download_typed_blobs::<RE::Tree>(None, vec![self.inner.digest.to_re()])
                        .await
                        .map_err(buck2_error::Error::from)
                        .and_then(|trees| {
                            trees
                                .into_iter()
                                .next()
                                .buck_error_context("RE response was empty")
                        })
                        .with_buck_error_context(|| {
                            format!("Error downloading tree: {}", self.inner.digest)
                        })?,
                    DirectoryKind::Directory => {
                        let root_directory = re_client
                            .download_typed_blobs::<RE::Directory>(
                                None,
                                vec![self.inner.digest.to_re()],
                            )
                            .await
                            .map_err(buck2_error::Error::from)
                            .and_then(|dirs| {
                                dirs.into_iter()
                                    .next()
                                    .buck_error_context("RE response was empty")
                            })
                            .with_buck_error_context(|| {
                                format!("Error downloading dir: {}", self.inner.digest)
                            })?;
                        re_directory_to_re_tree(root_directory, &re_client).await?
                    }
                };

                // NOTE: We assign a zero timestamp here because we didn't check the nodes in the tree,
                // just the tree itself. Perhaps we should, but some of the prospective users for this
                // have very large trees so that might not be wise.
                let dir = re_tree_to_directory(
                    &tree,
                    &Utc.timestamp_opt(0, 0).unwrap(),
                    ctx.digest_config(),
                    ctx.output_trees_download_config()
                        .fingerprint_re_output_trees_eagerly(),
                )
                .buck_error_context("Invalid directory")?;

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

        let path = ctx.fs().resolve_build(
            self.output.get_path(),
            if self.output.get_path().is_content_based_path() {
                Some(value.content_based_path_hash())
            } else {
                None
            }
            .as_ref(),
        )?;
        ctx.materializer()
            .declare_cas_many(
                Arc::new(CasDownloadInfo::new_declared(self.inner.re_use_case)),
                vec![DeclareArtifactPayload {
                    path,
                    artifact: value.dupe(),
                    persist_full_directory_structure: false,
                }],
            )
            .await?;

        let io_provider = ctx.io_provider();
        if let Some(tracer) = TracingIoProvider::from_io(&*io_provider) {
            let offline_cache_path =
                offline::declare_copy_to_offline_output_cache(ctx, &self.output, value.dupe())
                    .await?;
            tracer.add_buck_out_entry(offline_cache_path);
        }

        Ok((
            ActionOutputs::from_single(self.output.get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Deferred,
                timing: ActionExecutionTimingData::default(),
                input_files_bytes: None,
            },
        ))
    }
}

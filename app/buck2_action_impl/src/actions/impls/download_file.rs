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
use buck2_common::cas_digest::RawDigest;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::io::trace::TracingIoProvider;
use buck2_core::category::Category;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::http::http_client;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::http::http_head;
use buck2_execute::materialize::http::Checksum;
use buck2_execute::materialize::http::HttpError;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use dupe::Dupe;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::impls::offline;

#[derive(Debug, Error)]
enum DownloadFileActionError {
    #[error("download file action should not have inputs, got {0}")]
    WrongNumberOfInputs(usize),
    #[error("Exactly one output file must be specified for a download file action, got {0}")]
    WrongNumberOfOutputs(usize),
    #[error(transparent)]
    Http(#[from] HttpError),
}

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredDownloadFileAction {
    checksum: Checksum,
    url: Arc<str>,
    is_executable: bool,
    is_deferrable: bool,
}

impl UnregisteredDownloadFileAction {
    pub(crate) fn new(
        checksum: Checksum,
        url: Arc<str>,
        is_executable: bool,
        is_deferrable: bool,
    ) -> Self {
        Self {
            checksum,
            url,
            is_executable,
            is_deferrable,
        }
    }
}

impl UnregisteredAction for UnregisteredDownloadFileAction {
    fn register(
        self: Box<Self>,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
    ) -> anyhow::Result<Box<dyn Action>> {
        Ok(Box::new(DownloadFileAction::new(inputs, outputs, *self)?))
    }
}

#[derive(Debug, Allocative)]
struct DownloadFileAction {
    inputs: Box<[ArtifactGroup]>,
    outputs: Box<[BuildArtifact]>,
    inner: UnregisteredDownloadFileAction,
}

impl DownloadFileAction {
    fn new(
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredDownloadFileAction,
    ) -> anyhow::Result<Self> {
        if !inputs.is_empty() {
            Err(anyhow::anyhow!(
                DownloadFileActionError::WrongNumberOfInputs(inputs.len())
            ))
        } else if outputs.len() != 1 {
            Err(anyhow::anyhow!(
                DownloadFileActionError::WrongNumberOfOutputs(outputs.len())
            ))
        } else {
            Ok(Self {
                inputs: inputs.into_iter().collect(),
                outputs: outputs.into_iter().collect(),
                inner,
            })
        }
    }

    fn output(&self) -> &BuildArtifact {
        self.outputs
            .iter()
            .next()
            .expect("a single artifact by construction")
    }

    /// Try to produce a FileMetadata without downloading the file.
    async fn declared_metadata(
        &self,
        client: &reqwest::Client,
        digest_config: DigestConfig,
    ) -> anyhow::Result<Option<FileMetadata>> {
        if !self.inner.is_deferrable {
            return Ok(None);
        }

        if !digest_config.cas_digest_config().allows_sha1() {
            return Ok(None);
        }

        let sha1 = match self.inner.checksum.sha1() {
            Some(sha1) => sha1,
            _ => return Ok(None),
        };

        let sha1 = match RawDigest::parse_sha1(sha1.as_bytes()) {
            Ok(sha1) => sha1,
            Err(_) => return Ok(None),
        };

        let head = http_head(client, &self.inner.url).await?;

        // NOTE: Don't use reqwest's content_length() method here, that always returns zero!
        // https://github.com/seanmonstar/reqwest/issues/843
        let content_length = head
            .headers()
            .get(http::header::CONTENT_LENGTH)
            .map(|content_length| {
                let content_length = content_length
                    .to_str()
                    .context("Header is not valid utf-8")?;
                let content_length_number = content_length
                    .parse()
                    .with_context(|| format!("Header is not a number: `{}`", content_length))?;
                anyhow::Ok(content_length_number)
            })
            .transpose()
            .with_context(|| {
                format!(
                    "Request to `{}` returned an invalid `{}` header",
                    self.inner.url,
                    http::header::CONTENT_LENGTH
                )
            })?;

        match content_length {
            Some(length) => {
                let digest = TrackedFileDigest::new(
                    FileDigest::new(sha1, length),
                    digest_config.cas_digest_config(),
                );
                Ok(Some(FileMetadata {
                    digest,
                    is_executable: self.inner.is_executable,
                }))
            }
            None => Ok(None),
        }
    }

    /// Execute this action for offline builds (e.g. no network).
    async fn execute_for_offline(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let outputs = offline::declare_copy_from_offline_cache(ctx, self.output()).await?;

        Ok((
            outputs,
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
            },
        ))
    }
}

#[async_trait]
impl Action for DownloadFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::DownloadFile
    }

    fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&self.inputs))
    }

    fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
        Ok(Cow::Borrowed(&self.outputs))
    }

    fn as_executable(&self) -> ActionExecutable<'_> {
        ActionExecutable::Incremental(self)
    }

    fn category(&self) -> &Category {
        static DOWNLOAD_FILE_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("download_file").unwrap());

        &DOWNLOAD_FILE_CATEGORY
    }

    fn identifier(&self) -> Option<&str> {
        self.outputs
            .iter()
            .next()
            .map(|o| o.get_path().path().as_str())
    }
}

#[async_trait]
impl IncrementalActionExecutable for DownloadFileAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        // Early return - if this path exists, it's because we're running in a
        // special offline mode where the HEAD request below will likely fail.
        // Shortcut and just return this path as the action output.
        //
        // This mostly looks like a "copy" action.
        if ctx.run_action_knobs().use_network_action_output_cache {
            return self.execute_for_offline(ctx).await;
        }

        let client = http_client()?;

        let (value, execution_kind) = {
            match self.declared_metadata(&client, ctx.digest_config()).await? {
                Some(metadata) => {
                    let artifact_fs = ctx.fs();
                    let rel_path = artifact_fs.resolve_build(self.output().get_path());

                    // Fast path: download later via the materializer.
                    ctx.materializer()
                        .declare_http(
                            rel_path,
                            HttpDownloadInfo {
                                url: self.inner.url.dupe(),
                                checksum: self.inner.checksum.dupe(),
                                metadata: metadata.dupe(),
                                owner: ctx.target().owner().dupe().into_dyn(),
                            },
                            ctx.cancellation_context(),
                        )
                        .await?;

                    (ArtifactValue::file(metadata), ActionExecutionKind::Deferred)
                }
                None => {
                    ctx.cleanup_outputs().await?;

                    let artifact_fs = ctx.fs();
                    let project_fs = artifact_fs.fs();
                    let rel_path = artifact_fs.resolve_build(self.output().get_path());

                    // Slow path: download now.
                    let digest = http_download(
                        &client,
                        project_fs,
                        ctx.digest_config(),
                        &rel_path,
                        &self.inner.url,
                        &self.inner.checksum,
                        self.inner.is_executable,
                    )
                    .await?;

                    let metadata = FileMetadata {
                        digest,
                        is_executable: self.inner.is_executable,
                    };
                    ctx.materializer()
                        .declare_existing(vec![(rel_path, ArtifactValue::file(metadata.dupe()))])
                        .await?;

                    (ArtifactValue::file(metadata), ActionExecutionKind::Simple)
                }
            }
        };

        // If we're tracing I/O, get the materializer to copy to the offline cache
        // so we can include it in the offline archive manifest later.
        let io_provider = ctx.io_provider();
        let maybe_io_tracer = io_provider.as_any().downcast_ref::<TracingIoProvider>();
        if let Some(tracer) = maybe_io_tracer {
            let offline_cache_path =
                offline::declare_copy_to_offline_output_cache(ctx, self.output(), value.dupe())
                    .await?;
            tracer.add_buck_out_entry(offline_cache_path);
        }

        Ok((
            ActionOutputs::from_single(self.output().get_path().dupe(), value),
            ActionExecutionMetadata {
                execution_kind,
                timing: ActionExecutionTimingData::default(),
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    // TODO: This needs proper tests, but right now it's kind of a pain to get the
    //       action framework up and running to test actions
    #[test]
    fn downloads_file() {}
}

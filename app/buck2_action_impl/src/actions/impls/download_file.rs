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
use buck2_build_signals::env::WaitingData;
use buck2_common::cas_digest::RawDigest;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileMetadata;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_common::io::trace::TracingIoProvider;
use buck2_core::category::CategoryRef;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::http::Checksum;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::http::http_head;
use buck2_execute::materialize::materializer::DeclareArtifactPayload;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_http::HttpClient;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::values::OwnedFrozenValue;

use crate::actions::impls::offline;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DownloadFileActionError {
    #[error("Exactly one output file must be specified for a download file action, got {0}")]
    WrongNumberOfOutputs(usize),
    #[error(
        "Downloads using content-based path {0} must supply metadata (usually in the form of a sha1)!"
    )]
    ContentBasedPathWithoutMetadata(BuildArtifactPath),
}

#[derive(Debug, Allocative)]
pub(crate) struct UnregisteredDownloadFileAction {
    checksum: Checksum,
    size_bytes: Option<u64>,
    url: Arc<str>,
    vpnless_url: Option<Arc<str>>,
    is_executable: bool,
}

impl UnregisteredDownloadFileAction {
    pub(crate) fn new(
        checksum: Checksum,
        size_bytes: Option<u64>,
        url: Arc<str>,
        vpnless_url: Option<Arc<str>>,
        is_executable: bool,
    ) -> Self {
        Self {
            checksum,
            url,
            size_bytes,
            vpnless_url,
            is_executable,
        }
    }
}

impl UnregisteredAction for UnregisteredDownloadFileAction {
    fn register(
        self: Box<Self>,
        outputs: IndexSet<BuildArtifact>,
        _starlark_data: Option<OwnedFrozenValue>,
        _error_handler: Option<OwnedFrozenValue>,
    ) -> buck2_error::Result<Box<dyn Action>> {
        Ok(Box::new(DownloadFileAction::new(outputs, *self)?))
    }
}

#[derive(Debug, Allocative)]
struct DownloadFileAction {
    outputs: Box<[BuildArtifact]>,
    inner: UnregisteredDownloadFileAction,
}

impl DownloadFileAction {
    fn new(
        outputs: IndexSet<BuildArtifact>,
        inner: UnregisteredDownloadFileAction,
    ) -> buck2_error::Result<Self> {
        if outputs.len() != 1 {
            Err(DownloadFileActionError::WrongNumberOfOutputs(outputs.len()).into())
        } else {
            Ok(Self {
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

    fn url(&self, client: &HttpClient) -> &Arc<str> {
        if client.supports_vpnless() {
            self.inner.vpnless_url.as_ref().unwrap_or(&self.inner.url)
        } else {
            &self.inner.url
        }
    }

    /// Try to produce a FileMetadata without downloading the file.
    async fn declared_metadata(
        &self,
        client: &HttpClient,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Option<FileMetadata>> {
        let digest = if digest_config.cas_digest_config().allows_sha1() {
            self.inner
                .checksum
                .sha1()
                .and_then(|sha1| RawDigest::parse_sha1(sha1.as_bytes()).ok())
        } else if digest_config.cas_digest_config().allows_sha256() {
            self.inner
                .checksum
                .sha256()
                .and_then(|sha256| RawDigest::parse_sha256(sha256.as_bytes()).ok())
        } else {
            None
        };

        let digest = match digest {
            Some(digest) => digest,
            None => return Ok(None),
        };

        let size = match self.inner.size_bytes {
            Some(s) => Some(s),
            None => {
                let url = self.url(client);
                let head = http_head(client, url).await.map_err(|e| {
                    buck2_error::Error::from(e).tag([ErrorTag::DownloadFileHeadRequest])
                })?;

                head.headers()
                    .get(http::header::CONTENT_LENGTH)
                    .map(|content_length| {
                        let content_length = content_length
                            .to_str()
                            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Http))
                            .buck_error_context("Header is not valid utf-8")?;
                        let content_length_number =
                            content_length.parse().with_buck_error_context(|| {
                                format!("Header is not a number: `{content_length}`")
                            })?;
                        buck2_error::Ok(content_length_number)
                    })
                    .transpose()
                    .with_buck_error_context(|| {
                        format!(
                            "Request to `{}` returned an invalid `{}` header",
                            url,
                            http::header::CONTENT_LENGTH
                        )
                    })?
            }
        };

        match size {
            Some(size) => {
                let digest = TrackedFileDigest::new(
                    FileDigest::new(digest, size),
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
    ) -> buck2_error::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let outputs = offline::declare_copy_from_offline_cache(ctx, &[self.output()]).await?;

        Ok((
            outputs,
            ActionExecutionMetadata {
                execution_kind: ActionExecutionKind::Simple,
                timing: ActionExecutionTimingData::default(),
                input_files_bytes: None,
                waiting_data: WaitingData::new(),
            },
        ))
    }
}

#[async_trait]
impl Action for DownloadFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::DownloadFile
    }

    fn inputs(&self) -> buck2_error::Result<Cow<'_, [ArtifactGroup]>> {
        Ok(Cow::Borrowed(&[]))
    }

    fn outputs(&self) -> Cow<'_, [BuildArtifact]> {
        Cow::Borrowed(&self.outputs)
    }

    fn first_output(&self) -> &BuildArtifact {
        self.output()
    }

    fn category(&self) -> CategoryRef<'_> {
        CategoryRef::unchecked_new("download_file")
    }

    fn identifier(&self) -> Option<&str> {
        self.outputs
            .iter()
            .next()
            .map(|o| o.get_path().path().as_str())
    }

    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
        waiting_data: WaitingData,
    ) -> Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError> {
        // Early return - if this path exists, it's because we're running in a
        // special offline mode where the HEAD request below will likely fail.
        // Shortcut and just return this path as the action output.
        //
        // This mostly looks like a "copy" action.
        if ctx.run_action_knobs().use_network_action_output_cache {
            return self.execute_for_offline(ctx).await.map_err(Into::into);
        }

        let client = ctx.http_client();
        let url = self.url(&client);

        let (value, execution_kind) = {
            match self.declared_metadata(&client, ctx.digest_config()).await? {
                Some(metadata) => {
                    let artifact_fs = ctx.fs();
                    let value = ArtifactValue::file(metadata.dupe());
                    let rel_path = artifact_fs.resolve_build(
                        self.output().get_path(),
                        if self.output().get_path().is_content_based_path() {
                            Some(value.content_based_path_hash())
                        } else {
                            None
                        }
                        .as_ref(),
                    )?;

                    // Fast path: download later via the materializer.
                    ctx.materializer()
                        .declare_http(
                            rel_path,
                            HttpDownloadInfo {
                                url: url.dupe(),
                                checksum: self.inner.checksum.dupe(),
                                metadata,
                                owner: ctx.target().owner().dupe(),
                            },
                        )
                        .await?;

                    (value, ActionExecutionKind::Deferred)
                }
                None => {
                    if self.output().get_path().is_content_based_path() {
                        return Err(ExecuteError::Error {
                            error: DownloadFileActionError::ContentBasedPathWithoutMetadata(
                                self.output().get_path().dupe(),
                            )
                            .into(),
                        });
                    }

                    ctx.cleanup_outputs().await?;

                    let artifact_fs = ctx.fs();
                    let project_fs = artifact_fs.fs();

                    let rel_path = artifact_fs.resolve_build(self.output().get_path(), None)?;

                    // Slow path: download now.
                    let digest = http_download(
                        &client,
                        project_fs,
                        ctx.digest_config(),
                        &rel_path,
                        url,
                        &self.inner.checksum,
                        self.inner.is_executable,
                    )
                    .await?;

                    let metadata = FileMetadata {
                        digest,
                        is_executable: self.inner.is_executable,
                    };
                    ctx.materializer()
                        .declare_existing(vec![DeclareArtifactPayload {
                            path: rel_path,
                            artifact: ArtifactValue::file(metadata.dupe()),
                            persist_full_directory_structure: false,
                        }])
                        .await?;

                    (ArtifactValue::file(metadata), ActionExecutionKind::Simple)
                }
            }
        };

        // If we're tracing I/O, get the materializer to copy to the offline cache
        // so we can include it in the offline archive manifest later.
        let io_provider = ctx.io_provider();
        if let Some(tracer) = TracingIoProvider::from_io(&*io_provider) {
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
                input_files_bytes: None,
                waiting_data,
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

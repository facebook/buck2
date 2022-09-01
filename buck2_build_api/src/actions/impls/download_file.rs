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
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::category::Category;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::materialize::http::http_client;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::http::http_head;
use buck2_execute::materialize::http::Checksum;
use buck2_execute::materialize::http::HttpDownloadError;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use gazebo::prelude::*;
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
use crate::actions::PristineActionExecutable;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::ArtifactGroup;

#[derive(Debug, Error)]
enum DownloadFileActionError {
    #[error("download file action should not have inputs, got {0}")]
    WrongNumberOfInputs(usize),
    #[error("Exactly one output file must be specified for a download file action, got {0}")]
    WrongNumberOfOutputs(usize),
    #[error(transparent)]
    Http(#[from] HttpDownloadError),
}

#[derive(Debug)]
pub struct UnregisteredDownloadFileAction {
    checksum: Checksum,
    url: Arc<str>,
    is_executable: bool,
    is_deferrable: bool,
}

impl UnregisteredDownloadFileAction {
    pub fn new(
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
        Ok(box DownloadFileAction::new(inputs, outputs, *self)?)
    }
}

#[derive(Debug)]
struct DownloadFileAction {
    inputs: IndexSet<ArtifactGroup>,
    outputs: IndexSet<BuildArtifact>,
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
                inputs,
                outputs,
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
    ) -> anyhow::Result<Option<FileMetadata>> {
        if !self.inner.is_deferrable {
            return Ok(None);
        }

        let sha1 = match self.inner.checksum.sha1() {
            Some(sha1) => sha1,
            _ => return Ok(None),
        };

        // NOTE: We should probably fail earlier here, but since historically we didn't, we'll let
        // that proceed to download and flag the wrong digest.
        let sha1 = match FileDigest::parse_digest(sha1.as_bytes()) {
            Some(sha1) => sha1,
            None => return Ok(None),
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
                let digest = TrackedFileDigest::new(FileDigest { sha1, size: length });
                Ok(Some(FileMetadata {
                    digest,
                    is_executable: self.inner.is_executable,
                }))
            }
            None => Ok(None),
        }
    }
}

#[async_trait]
impl Action for DownloadFileAction {
    fn kind(&self) -> buck2_data::ActionKind {
        buck2_data::ActionKind::DownloadFile
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
        static DOWNLOAD_FILE_CATEGORY: Lazy<Category> =
            Lazy::new(|| Category::try_from("download_file").unwrap());

        &DOWNLOAD_FILE_CATEGORY
    }
}

#[async_trait]
impl PristineActionExecutable for DownloadFileAction {
    async fn execute(
        &self,
        ctx: &mut dyn ActionExecutionCtx,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let artifact_fs = ctx.fs();
        let project_fs = artifact_fs.fs();
        let rel_path = artifact_fs.resolve_build(self.output().get_path());

        let client = http_client()?;

        let (metadata, execution_kind) = match self.declared_metadata(&client).await? {
            Some(metadata) => {
                // Fast path: download later via the materializer.
                ctx.materializer()
                    .declare_http(
                        rel_path,
                        HttpDownloadInfo {
                            url: self.inner.url.dupe(),
                            checksum: self.inner.checksum.dupe(),
                            metadata: metadata.dupe(),
                            owner: ctx.target().owner.dupe(),
                        },
                    )
                    .await?;

                (metadata, ActionExecutionKind::Deferred)
            }
            None => {
                // Slow path: download now.
                let digest = http_download(
                    &client,
                    project_fs,
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

                (metadata, ActionExecutionKind::Simple)
            }
        };

        let value = ArtifactValue::file(metadata);

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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_events::BuckEvent;
use dupe::Dupe;
use futures::Future;
use futures::FutureExt;
use tokio::process::Child;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::subscribers::should_upload_log;
use crate::subscribers::subscriber::EventSubscriber;

#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq)]
enum LogMode {
    Json,
    Protobuf,
}
pub(crate) struct ReLog {
    re_session_id: Option<String>,
    isolation_dir: FileNameBuf,
    async_cleanup_context: AsyncCleanupContext,
}

impl ReLog {
    pub(crate) fn new(
        isolation_dir: FileNameBuf,
        async_cleanup_context: AsyncCleanupContext,
    ) -> Self {
        Self {
            re_session_id: None,
            isolation_dir,
            async_cleanup_context,
        }
    }

    fn log_upload(&mut self) -> impl Future<Output = anyhow::Result<()>> + 'static + Send + Sync {
        // We put `None` in place of re_session_id which means we will only attempt to upload
        // the logs once no matter how many times this function is called
        let session_id = self.re_session_id.take();
        let isolation_dir = self.isolation_dir.clone();
        async move {
            if let Some(s_id) = session_id {
                log_upload_impl(s_id, isolation_dir).await?;
            }
            Ok(())
        }
    }
}

#[async_trait]
impl EventSubscriber for ReLog {
    async fn exit(&mut self) -> anyhow::Result<()> {
        self.log_upload().await
    }

    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        for event in events {
            match event.data() {
                buck2_data::buck_event::Data::Instant(ref instant) => {
                    match instant.data.as_ref().context("Missing `data`")? {
                        buck2_data::instant_event::Data::ReSession(session) => {
                            self.re_session_id = Some(session.session_id.clone());
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl Drop for ReLog {
    fn drop(&mut self) {
        let upload = self.log_upload();
        self.async_cleanup_context.register(
            "RE log upload",
            async move {
                if let Err(e) = upload.await {
                    tracing::warn!("Failed to cleanup ReLog: {:#}", e);
                }
            }
            .boxed(),
        );
    }
}

async fn log_upload_impl(session_id: String, isolation_dir: FileNameBuf) -> anyhow::Result<()> {
    if !should_upload_log()? {
        return Ok(());
    }

    let mut buck = buck2_util::process::async_background_command(std::env::current_exe()?);
    let command = buck
        .arg("--isolation-dir")
        .arg(isolation_dir.as_str())
        .arg("debug")
        .arg("upload-re-logs")
        .arg("--session-id")
        .arg(session_id)
        .stdin(Stdio::null())
        .stdout(Stdio::null());

    let block_on_upload = std::env::var_os("SANDCASTLE").is_some();
    if block_on_upload {
        let child = command.stderr(Stdio::piped()).spawn()?;
        match blocking_upload(child).await {
            Ok(_) => {}
            Err(e) => {
                tracing::warn!("Error uploading RE logs: {:#}", e);
            }
        }
    } else {
        command.stderr(Stdio::null()).spawn()?;
    };

    Ok(())
}

async fn blocking_upload(child: Child) -> anyhow::Result<()> {
    let res = child.wait_with_output();
    let timeout = tokio::time::timeout(Duration::from_secs(20), res)
        .await
        .context("Timed out waiting for RE log upload to manifold")??;
    if !timeout.status.success() {
        let stderr = String::from_utf8_lossy(&timeout.stderr);
        return Err(anyhow::anyhow!(
            "RE log upload exited with {}. Stderr: `{}`",
            timeout.status,
            stderr.trim(),
        ));
    };
    Ok(())
}

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

use async_trait::async_trait;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_event_log::should_block_on_log_upload;
use buck2_event_log::should_upload_log;
use buck2_event_log::wait_for_child_and_log;
use buck2_event_log::FutureChildOutput;
use buck2_event_observer::unpack_event::unpack_event;
use buck2_event_observer::unpack_event::UnpackedBuckEvent;
use buck2_events::BuckEvent;
use buck2_util::cleanup_ctx::AsyncCleanupContext;
use futures::Future;
use futures::FutureExt;

use crate::subscribers::subscriber::EventSubscriber;

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

    fn log_upload(
        &mut self,
    ) -> impl Future<Output = buck2_error::Result<()>> + 'static + Send + Sync {
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
    async fn exit(&mut self) -> buck2_error::Result<()> {
        self.log_upload().await
    }

    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> buck2_error::Result<()> {
        for event in events {
            match unpack_event(event)? {
                UnpackedBuckEvent::Instant(
                    _,
                    _,
                    buck2_data::instant_event::Data::ReSession(session),
                ) => {
                    self.re_session_id = Some(session.session_id.clone());
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

async fn log_upload_impl(
    session_id: String,
    isolation_dir: FileNameBuf,
) -> buck2_error::Result<()> {
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
    if should_block_on_log_upload()? {
        let child = command.stderr(Stdio::piped()).spawn()?;
        wait_for_child_and_log(FutureChildOutput::new(child), "RE Log").await;
    } else {
        command.stderr(Stdio::null()).spawn()?;
    };

    Ok(())
}

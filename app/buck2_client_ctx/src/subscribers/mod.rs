/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::process;
use std::time::Duration;

use anyhow::Context as _;
use buck2_core::env_helper::EnvHelper;
use tokio::process::Child;
use tokio::task::JoinHandle;

pub(crate) mod build_graph_stats;
pub(crate) mod build_id_writer;
pub mod event_log;
pub mod get;
pub(crate) mod observer;
pub mod re_log;
pub mod recorder;
pub(crate) mod simpleconsole;
pub mod stdout_stderr_forwarder;
pub mod subscriber;
pub mod subscriber_unpack;
pub mod superconsole;

pub fn should_upload_log() -> anyhow::Result<bool> {
    if buck2_core::is_open_source() {
        return Ok(false);
    }
    static DISABLE_LOG_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_DISABLE_LOG_UPLOAD");
    Ok(!DISABLE_LOG_UPLOAD.get()?.copied().unwrap_or_default())
}

pub fn should_block_on_log_upload() -> anyhow::Result<bool> {
    static SANDCASTLE: EnvHelper<String> = EnvHelper::new("SANDCASTLE");

    // Used by our tests.
    static TEST_BLOCK_ON_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_BLOCK_ON_UPLOAD");

    Ok(SANDCASTLE.get()?.is_some() || TEST_BLOCK_ON_UPLOAD.get_copied()?.unwrap_or_default())
}

/// Wait for the child to finish. Assume its stderr was piped.
pub async fn wait_for_child_and_log(child: FutureChildOutput, reason: &str) {
    async fn inner(child: FutureChildOutput) -> anyhow::Result<()> {
        let res = tokio::time::timeout(Duration::from_secs(20), child.task)
            .await
            .context("Timed out")?
            .context("Task failed")?
            .context("Process failed")?;

        if !res.status.success() {
            let stderr = String::from_utf8_lossy(&res.stderr);
            return Err(anyhow::anyhow!(
                "Upload exited with status `{}`. Stderr: `{}`",
                res.status,
                stderr.trim(),
            ));
        };
        Ok(())
    }

    match inner(child).await {
        Ok(_) => {}
        Err(e) => {
            tracing::warn!("Error uploading {}: {:#}", reason, e);
        }
    }
}

/// Ensure that if we spawn children, we don't block their stderr.
pub struct FutureChildOutput {
    task: JoinHandle<io::Result<process::Output>>,
}

impl FutureChildOutput {
    pub fn new(child: Child) -> Self {
        Self {
            task: tokio::task::spawn(async move { child.wait_with_output().await }),
        }
    }
}

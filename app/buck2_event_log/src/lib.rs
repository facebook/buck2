/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(used_with_arg)]
#![feature(error_generic_member_access)]

use std::io;
use std::process;
use std::time::Duration;

use buck2_core::buck2_env;
use buck2_core::ci::is_ci;
use buck2_error::BuckErrorContext;
use tokio::process::Child;
use tokio::task::JoinHandle;

pub mod file_names;
pub mod read;
pub mod stream_value;
pub mod ttl;
pub mod user_event_types;
pub mod utils;
pub mod write;
pub mod writer;

pub fn should_upload_log() -> buck2_error::Result<bool> {
    if buck2_core::is_open_source() {
        return Ok(false);
    }
    Ok(!buck2_env!(
        "BUCK2_TEST_DISABLE_LOG_UPLOAD",
        bool,
        applicability = testing
    )?)
}

pub fn should_block_on_log_upload() -> buck2_error::Result<bool> {
    // `BUCK2_TEST_BLOCK_ON_UPLOAD` is used by our tests.
    Ok(is_ci()? || buck2_env!("BUCK2_TEST_BLOCK_ON_UPLOAD", bool, applicability = internal)?)
}

/// Wait for the child to finish. Assume its stderr was piped.
pub async fn wait_for_child_and_log(child: FutureChildOutput, reason: &str) {
    async fn inner(child: FutureChildOutput) -> buck2_error::Result<()> {
        let res = tokio::time::timeout(Duration::from_secs(20), child.task)
            .await
            .buck_error_context("Timed out")?
            .buck_error_context("Task failed")?
            .buck_error_context("Process failed")?;

        if !res.status.success() {
            let stderr = String::from_utf8_lossy(&res.stderr);
            return Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
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

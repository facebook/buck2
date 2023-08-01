/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
use buck2_client_ctx::manifold::ManifoldClient;
use buck2_common::result::SharedResult;
use buck2_util::process::async_background_command;

use crate::commands::rage::manifold::buf_to_manifold;

pub async fn upload_thread_dump(
    buckd: &SharedResult<BuckdProcessInfo<'_>>,
    manifold: &ManifoldClient,
    manifold_id: &String,
) -> anyhow::Result<String> {
    let buckd_pid = buckd.as_ref().map_err(|e| e.clone())?.pid();
    let command = async_background_command("lldb")
        .arg("-p")
        .arg(buckd_pid.to_string())
        .arg("--batch")
        .arg("-o")
        .arg("thread backtrace all")
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .context("Failed to spawn lldb command")?
        .wait_with_output()
        .await?;

    if command.status.success() {
        let manifold_filename = format!("flat/{}_thread_dump", manifold_id);
        buf_to_manifold(manifold, &command.stdout, manifold_filename).await
    } else {
        let stderr = &command.stderr;
        Ok(String::from_utf8_lossy(stderr).to_string())
    }
}

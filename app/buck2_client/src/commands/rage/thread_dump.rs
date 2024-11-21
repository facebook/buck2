/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::daemon::client::connect::BuckdProcessInfo;
use buck2_common::manifold::ManifoldClient;
use buck2_error::BuckErrorContext;
use buck2_util::process::async_background_command;

use crate::commands::rage::manifold::buf_to_manifold;

pub(crate) fn thread_dump_command(
    buckd: &BuckdProcessInfo<'_>,
) -> buck2_error::Result<tokio::process::Command> {
    let pid = buckd.pid()?;
    let mut cmd = async_background_command("lldb");
    cmd.arg("-p")
        .arg(pid.to_string())
        .arg("--batch")
        .arg("-o")
        .arg("thread backtrace all")
        .stdin(std::process::Stdio::null());
    Ok(cmd)
}

pub(crate) async fn upload_thread_dump(
    buckd: &buck2_error::Result<BuckdProcessInfo<'_>>,
    manifold: &ManifoldClient,
    manifold_id: &String,
) -> buck2_error::Result<String> {
    let buckd = buckd.as_ref().map_err(|e| e.clone())?;
    let command = thread_dump_command(buckd)?
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .buck_error_context("Failed to spawn lldb command")?
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

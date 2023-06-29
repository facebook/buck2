/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Cursor;

use anyhow::Context;
use buck2_client_ctx::daemon::client::connect::BootstrapBuckdClient;
use buck2_client_ctx::manifold;
use buck2_common::result::SharedResult;
use buck2_util::process::async_background_command;

pub async fn upload_thread_dump(
    buckd: &SharedResult<BootstrapBuckdClient>,
    manifold_id: &String,
) -> anyhow::Result<String> {
    let buckd_pid = buckd.clone()?.pid();
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
        let manifold_bucket = manifold::Bucket::RAGE_DUMPS;
        let manifold_filename = format!("{}_thread_dump", manifold_id);
        let mut stdout = command.stdout;
        manifold::Upload::new(manifold_bucket, &manifold_filename)
            .with_default_ttl()
            .from_async_read(&mut Cursor::new(&mut stdout))?
            .spawn()
            .await?;
        Ok(format!(
            "{}/flat/{}",
            manifold_bucket.name, manifold_filename
        ))
    } else {
        let stderr = &command.stderr;
        Ok(String::from_utf8_lossy(stderr).to_string())
    }
}

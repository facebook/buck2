/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_util::process::async_background_command;

use crate::daemon::client::connect::BuckdProcessInfo;

pub fn thread_dump_command(
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

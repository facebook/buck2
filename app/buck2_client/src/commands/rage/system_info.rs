/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) async fn get() -> anyhow::Result<String> {
    let info = buck2_events::metadata::system_info();
    let output = format!(
        "username: {}
hostname: {}
os: {}
os_version: {}
",
        info.username.unwrap_or_else(|| "".to_owned()),
        info.hostname.unwrap_or_else(|| "".to_owned()),
        info.os,
        info.os_version.unwrap_or_else(|| "".to_owned()),
    );
    Ok(output)
}

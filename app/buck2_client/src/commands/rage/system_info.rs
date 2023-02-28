/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

pub(crate) struct SystemInfo {
    username: Option<String>,
    hostname: Option<String>,
    os: String,
    os_version: Option<String>,
}

impl fmt::Display for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "username: {}
hostname: {}
os: {}
os_version: {}
",
            self.username.as_deref().unwrap_or(""),
            self.hostname.as_deref().unwrap_or(""),
            self.os,
            self.os_version.as_deref().unwrap_or(""),
        )
    }
}

pub(crate) async fn get() -> anyhow::Result<SystemInfo> {
    let info = buck2_events::metadata::system_info();
    let output = SystemInfo {
        username: info.username,
        hostname: info.hostname,
        os: info.os,
        os_version: info.os_version,
    };
    Ok(output)
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;

use tokio::process::Command;

pub fn cli_upload_command(cli_path: OsString, bucket_path: &String, bucket_key: &str) -> Command {
    let mut upload = buck2_core::process::async_background_command(cli_path);

    tracing::debug!(
        "Uploading event log to {} using manifold CLI with command {:?}",
        bucket_path,
        upload
    );

    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        if hostcaps::is_corp() {
            upload.arg("-vip");
        }
    }
    upload.args([
        "--apikey",
        bucket_key,
        "--timeout-ms",
        "20000",
        "put",
        bucket_path,
    ]);
    upload
}

pub fn get_cli_path() -> Option<OsString> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    {
        match which::which("manifold") {
            Ok(path) => Some(path.as_os_str().to_owned()),
            Err(_) => None,
        }
    }
    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        None
    }
}

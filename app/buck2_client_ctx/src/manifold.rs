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

use crate::find_certs::find_tls_cert;

pub fn upload_command(
    manifold_bucket_name: &str,
    manifold_bucket_path: &str,
    bucket_key: &str,
) -> anyhow::Result<Option<Command>> {
    // we use manifold CLI as it works cross-platform
    let manifold_cli_path = get_cli_path();

    match manifold_cli_path {
        None => curl_upload_command(manifold_bucket_name, manifold_bucket_path, bucket_key),
        Some(cli_path) => Ok(Some(cli_upload_command(
            cli_path,
            &format!("{}/{}", manifold_bucket_name, manifold_bucket_path),
            bucket_key,
        ))),
    }
}

fn curl_upload_command(
    manifold_bucket_name: &str,
    manifold_bucket_path: &str,
    bucket_key: &str,
) -> anyhow::Result<Option<Command>> {
    if cfg!(windows) {
        // We do not have `curl` on Windows.
        return Ok(None);
    }

    let manifold_url = match log_upload_url() {
        None => return Ok(None),
        Some(x) => x,
    };
    let cert = find_tls_cert()?;

    let url = format!(
        "{}/v0/write/{}?bucketName={}&apiKey={}&timeoutMsec=20000",
        manifold_url, manifold_bucket_path, manifold_bucket_name, bucket_key
    );

    tracing::debug!(
        "Uploading event log to `{}` using certificate `{}`",
        url,
        cert.to_string_lossy(),
    );

    let mut upload = buck2_core::process::async_background_command("curl");
    upload.args([
        "--silent",
        "--show-error",
        "--fail",
        "-X",
        "PUT",
        "-H",
        "X-Manifold-Obj-Predicate:NoPredicate", // Do not check existance
        "--data-binary",
        "@-", // stdin
        &url,
        "-E",
    ]);
    upload.arg(cert);
    Ok(Some(upload))
}

fn cli_upload_command(
    cli_path: OsString,
    manifold_bucket_path: &String,
    bucket_key: &str,
) -> Command {
    let mut upload = buck2_core::process::async_background_command(cli_path);

    tracing::debug!(
        "Uploading event log to {} using manifold CLI with command {:?}",
        manifold_bucket_path,
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
        manifold_bucket_path,
        "--ignoreExisting",
    ]);
    upload
}

fn get_cli_path() -> Option<OsString> {
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

/// Return the place to upload logs, or None to not upload logs at all
pub fn log_upload_url() -> Option<&'static str> {
    #[cfg(any(fbcode_build, cargo_internal_build))]
    if hostcaps::is_prod() {
        Some("https://manifold.facebook.net")
    } else {
        Some("https://manifold.c2p.facebook.net")
    }
    #[cfg(not(any(fbcode_build, cargo_internal_build)))]
    {
        #[cfg(fbcode_build)]
        compile_error!("this code is not meant to be compiled in fbcode");

        None
    }
}

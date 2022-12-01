/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;

use anyhow::Context as _;

pub fn find_tls_cert() -> anyhow::Result<OsString> {
    let cert;

    #[cfg(fbcode_build)]
    {
        cert = ::find_certs::find_tls_cert();
    }

    #[cfg(not(fbcode_build))]
    {
        cert = Err(anyhow::anyhow!("Disabled in Cargo builds"));
    }

    cert.context("Error finding a cert")
}

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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use anyhow::Context;
use gazebo::prelude::VecExt;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;

/// Load the system root certificates into rustls cert store.
fn load_system_root_certs() -> anyhow::Result<RootCertStore> {
    let mut roots = rustls::RootCertStore::empty();
    let native_certs: Vec<_> = rustls_native_certs::load_native_certs()
        .context("Error loading system root certificates")?
        .into_map(|cert| cert.0);

    // According to [`rustls` documentation](https://docs.rs/rustls/latest/rustls/struct.RootCertStore.html#method.add_parsable_certificates),
    // it's better to only add parseable certs when loading system certs because
    // there are typically many system certs and not all of them can be valid. This
    // is pertinent for e.g. macOS which may have a lot of old certificates that may
    // not parse correctly.
    let (valid, invalid) = roots.add_parsable_certificates(native_certs.as_slice());

    // But make sure we get at least _one_ valid cert, otherwise we legitimately won't be
    // able to make any connections via https.
    anyhow::ensure!(
        valid > 0,
        "Error loading system certs: unable to find any valid system certs"
    );
    tracing::debug!("Loaded {} valid system root certs", valid);
    tracing::debug!("Loaded {} invalid system root certs", invalid);
    Ok(roots)
}

/// Deserialize certificate pair at `cert` and `key` into structures that can
/// be inserted into rustls CertStore.
fn load_cert_pair<P: AsRef<Path>>(
    cert: P,
    key: P,
) -> anyhow::Result<(Vec<Certificate>, PrivateKey)> {
    let cert = cert.as_ref();
    let key = key.as_ref();
    let cert_file = File::open(cert)
        .with_context(|| format!("Error opening certificate file `{}`", cert.display()))?;
    let key_file =
        File::open(key).with_context(|| format!("Error opening key file `{}`", key.display()))?;
    let mut cert_reader = BufReader::new(&cert_file);
    let mut key_reader = BufReader::new(&key_file);

    let certs = rustls_pemfile::certs(&mut cert_reader)
        .with_context(|| format!("Error parsing certificate file `{}`", cert.display()))?
        .into_map(Certificate);

    let private_key = rustls_pemfile::pkcs8_private_keys(&mut key_reader)
        .with_context(|| format!("Error parsing key file `{}`", key.display()))?
        .pop()
        .with_context(|| format!("Found no private key in key file `{}`", key.display()))?;
    let key = PrivateKey(private_key);

    Ok((certs, key))
}

pub fn tls_config_with_system_roots() -> anyhow::Result<ClientConfig> {
    let system_roots = load_system_root_certs()?;
    Ok(ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_no_client_auth())
}

pub fn tls_config_with_single_cert<P: AsRef<Path>>(
    cert_path: P,
    key_path: P,
) -> anyhow::Result<ClientConfig> {
    let system_roots = load_system_root_certs()?;
    let (cert, key) =
        load_cert_pair(cert_path, key_path).context("Error loading certificate pair")?;
    // TODO: replace with_single_cert with with_client_auth_cert
    //       once rustls get upgraded to >0.21.4
    #[allow(deprecated)]
    ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_single_cert(cert, key)
        .context("Error creating TLS config with cert and key path")
}

/// Find TLS certs.
///
/// Return `None` in Cargo or open source builds.
/// Return `Err` if certificates cannot be found in internal buck2 builds.
pub fn find_internal_cert() -> anyhow::Result<Option<OsString>> {
    #[cfg(fbcode_build)]
    return find_certs::find_tls_cert().map(Some);

    #[cfg(not(fbcode_build))]
    return Ok(None);
}

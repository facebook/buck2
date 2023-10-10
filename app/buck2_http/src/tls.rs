/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
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

/// Load the system root certificates using native frameworks.
fn load_system_root_certs_native() -> anyhow::Result<Vec<Vec<u8>>> {
    let native_certs: Vec<_> = rustls_native_certs::load_native_certs()
        .context("Error loading system root certificates")?
        .into_map(|cert| cert.0);
    Ok(native_certs)
}

/// Load system root certifcates from disk.
fn load_system_root_certs_disk(path: &OsStr) -> anyhow::Result<Vec<Vec<u8>>> {
    let file = File::open(path)
        .with_context(|| format!("Opening root certs at: {}", path.to_string_lossy()))?;
    let mut reader = BufReader::new(file);
    let certs = rustls_pemfile::certs(&mut reader)
        .with_context(|| format!("Loading root certs at: {}", path.to_string_lossy()))?
        .into_iter()
        .collect();

    Ok(certs)
}

/// Load system root certs, trying a few different methods to get a valid root
/// certificate store.
fn load_system_root_certs() -> anyhow::Result<RootCertStore> {
    let mut roots = RootCertStore::empty();
    let root_certs = if let Ok(certs) = load_system_root_certs_native() {
        certs
    } else if let Some(path) = find_root_ca_certs() {
        tracing::debug!(
            "Failed loading certs from native OS, falling back to disk at: {}",
            path.to_string_lossy(),
        );
        load_system_root_certs_disk(&path)
            .with_context(|| format!("Loading root certs from: {}", path.to_string_lossy()))?
    } else {
        anyhow::bail!("Unable to load system root certificates");
    };

    // According to [`rustls` documentation](https://docs.rs/rustls/latest/rustls/struct.RootCertStore.html#method.add_parsable_certificates),
    // it's better to only add parseable certs when loading system certs because
    // there are typically many system certs and not all of them can be valid. This
    // is pertinent for e.g. macOS which may have a lot of old certificates that may
    // not parse correctly.
    let (valid, invalid) = roots.add_parsable_certificates(root_certs.as_slice());

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
/// Return `None` in Cargo or open source builds; we do not support internal certs
/// in these builds.
pub fn find_internal_cert() -> Option<OsString> {
    #[cfg(fbcode_build)]
    return find_certs::find_tls_cert();

    #[cfg(not(fbcode_build))]
    return None;
}

/// Find root CA certs.
///
/// In OSS or non-fbcode builds, returns None; we do not support hardcoded root
/// certificates in non-fbcode builds and rely solely on rustls-native-certs.
pub fn find_root_ca_certs() -> Option<OsString> {
    #[cfg(fbcode_build)]
    return find_certs::find_root_ca_certs();

    #[cfg(not(fbcode_build))]
    return None;
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsString;
use std::path::Path;

use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use rustls::ClientConfig;
use rustls::RootCertStore;
use rustls_pki_types::CertificateDer;
use rustls_pki_types::PrivateKeyDer;
use rustls_pki_types::pem::PemObject;

pub fn maybe_setup_cryptography() {
    setup_cryptography().ok();
}

pub fn setup_cryptography_or_fail() {
    setup_cryptography().unwrap();
}

fn setup_cryptography() -> std::result::Result<(), std::sync::Arc<rustls::crypto::CryptoProvider>> {
    // https://fb.workplace.com/groups/rust.language/permalink/29117966747825230/
    // Note that all but the first call will fail, so we callers should only use
    // this function as early as possible in their lifetime
    // Note that the use of 'ring' here is arbitrary and should not be
    // taken as an intentional choice of cryptographic provider
    rustls::crypto::ring::default_provider().install_default()
}

/// Load system root certs, trying a few different methods to get a valid root
/// certificate store.
async fn load_system_root_certs() -> buck2_error::Result<RootCertStore> {
    let root_certs = if let Some(path) = find_root_ca_certs() {
        load_certs(&path).await.with_buck_error_context(|| {
            format!("Loading root certs from: {}", path.to_string_lossy())
        })
    } else {
        let mut native_certs_results = rustls_native_certs::load_native_certs();

        if !native_certs_results.certs.is_empty() {
            Ok(native_certs_results.certs)
        } else {
            // Consider the last error to be indicative of the overall problem
            let native_certs_error = native_certs_results
                .errors
                .pop()
                .map(buck2_error::Error::from)
                .unwrap_or(buck2_error!(
                    buck2_error::ErrorTag::NoValidCerts,
                    "No certs or cert errors"
                ));

            // Annotate the error with our context, but note that we do not return
            // the error here because we may recover through find_root_ca_certs()/load_certs() below
            if cfg!(fbcode_build) {
                let windows_message = if cfg!(target_os = "windows") {
                    " on an admin PowerShell"
                } else {
                    ""
                };
                let context = format!(
                    "Error loading system root certificates native frameworks.
                    This is usually due to Chef not installed or working properly.
                    Please try `getchef -reason 'chef broken'`{windows_message}, `Fix My <OS>` via the f-menu, then `buck2 killall`.
                    If that doesn't resolve it, please visit HelpDesk to get Chef back to a healthy state."
                );
                Err(native_certs_error.context(context))
            } else {
                Err(native_certs_error
                    .context("Error loading system root certificates native frameworks."))
            }
        }
    }?;

    // According to [`rustls` documentation](https://docs.rs/rustls/latest/rustls/struct.RootCertStore.html#method.add_parsable_certificates),
    // it's better to only add parseable certs when loading system certs because
    // there are typically many system certs and not all of them can be valid. This
    // is pertinent for e.g. macOS which may have a lot of old certificates that may
    // not parse correctly.
    let mut roots = RootCertStore::empty();
    let (valid, invalid) = roots.add_parsable_certificates(root_certs);

    // But make sure we get at least _one_ valid cert, otherwise we legitimately won't be
    // able to make any connections via https.
    if valid == 0 {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Environment,
            "Error loading system certs: unable to find any valid system certs"
        ));
    }
    tracing::debug!("Loaded {} valid system root certs", valid);
    tracing::debug!("Loaded {} invalid system root certs", invalid);
    Ok(roots)
}

// Load private key from the given path
async fn load_key<P: AsRef<Path>>(key: P) -> buck2_error::Result<PrivateKeyDer<'static>> {
    let key = key.as_ref();

    let private_key = PrivateKeyDer::from_pem_file(key)
        .with_buck_error_context(|| format!("Error opening key file `{}`", key.display()))?;

    Ok(private_key)
}

/// Deserialize certificate pair at `cert` and `key` into structures that can
/// be inserted into rustls CertStore.
async fn load_cert_pair<P: AsRef<Path>>(
    cert: P,
    key: P,
) -> buck2_error::Result<(Vec<CertificateDer<'static>>, PrivateKeyDer<'static>)> {
    let certs = load_certs(cert).await?;
    let key = load_key(key).await?;

    Ok((certs, key))
}

pub async fn tls_config_with_system_roots() -> buck2_error::Result<ClientConfig> {
    let system_roots = load_system_root_certs().await?;
    Ok(ClientConfig::builder()
        .with_root_certificates(system_roots)
        .with_no_client_auth())
}

pub async fn tls_config_with_single_cert<P: AsRef<Path>>(
    cert_path: P,
    key_path: P,
) -> buck2_error::Result<ClientConfig> {
    let system_roots = load_system_root_certs().await?;
    let (cert, key) = load_cert_pair(cert_path, key_path)
        .await
        .buck_error_context("Error loading certificate pair")?;
    ClientConfig::builder()
        .with_root_certificates(system_roots)
        .with_client_auth_cert(cert, key)
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Certs))
        .buck_error_context("Error creating TLS config with cert and key path")
}

// Load certs from the given path
pub(crate) async fn load_certs<P: AsRef<Path>>(
    cert_path: P,
) -> buck2_error::Result<Vec<CertificateDer<'static>>> {
    let cert_path = cert_path.as_ref();

    let cert_data = tokio::fs::read(cert_path)
        .await
        .with_buck_error_context(|| {
            format!("Error reading certificate file `{}`", cert_path.display())
        })?;

    let cert_results: Vec<Result<CertificateDer, rustls_pki_types::pem::Error>> =
        CertificateDer::pem_reader_iter(&mut cert_data.as_slice()).collect();

    let certs: Result<Vec<CertificateDer<'static>>, rustls_pki_types::pem::Error> =
        cert_results.into_iter().collect();

    certs.with_buck_error_context(|| {
        format!("Error reading certificate file `{}`", cert_path.display())
    })
}

/// Find root CA certs.
///
/// In OSS or non-fbcode builds, returns None; we do not support hardcoded root
/// certificates in non-fbcode builds and rely solely on rustls-native-certs.
pub(crate) fn find_root_ca_certs() -> Option<OsString> {
    #[cfg(fbcode_build)]
    return find_certs::find_root_ca_certs();

    #[cfg(not(fbcode_build))]
    match std::env::var_os("ROOT_CA_CERT_PATH") {
        Some(path) if Path::new(&path).exists() => Some(path),
        _ => None,
    }
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

/// Whether the machine buck is running on supports vpnless operation.
pub fn supports_vpnless() -> bool {
    #[cfg(fbcode_build)]
    return cpe::x2p::supports_vpnless();

    #[cfg(not(fbcode_build))]
    return false;
}

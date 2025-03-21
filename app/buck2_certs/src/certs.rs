/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsString;
use std::path::Path;

use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use gazebo::prelude::VecExt;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;

/// Load system root certs, trying a few different methods to get a valid root
/// certificate store.
async fn load_system_root_certs() -> buck2_error::Result<RootCertStore> {
    let native_certs =
        rustls_native_certs::load_native_certs().tag(buck2_error::ErrorTag::Environment);
    let native_certs = if cfg!(fbcode_build) {
        native_certs.buck_error_context(
            "Error loading system root certificates native frameworks.
            This is usually due to Chef not installed or working properly.
            Please try `getchef -reason 'chef broken'`, `Fix My <OS>` via the f-menu, then `buck2 killall`.
            If that doesn't resolve it, please visit HelpDesk to get Chef back to a healthy state.",
        )
    } else {
        native_certs.buck_error_context("Error loading system root certificates native frameworks.")
    };

    let root_certs =
          // Load the system root certificates using native frameworks.
          if let Ok(certs) = native_certs {
              certs.into_map(|cert| cert.0)
          }
          else if let Some(path) = find_root_ca_certs() {
              tracing::debug!(
                  "Failed loading certs from native OS, falling back to disk at: {}",
                  path.to_string_lossy(),
              );
              load_certs(&path)
                  .await
                  .with_buck_error_context(|| format!("Loading root certs from: {}", path.to_string_lossy()))?
          } else {
              native_certs?;
              return Err(buck2_error!(buck2_error::ErrorTag::Environment, "Unable to load system root certificates"));
          };

    // According to [`rustls` documentation](https://docs.rs/rustls/latest/rustls/struct.RootCertStore.html#method.add_parsable_certificates),
    // it's better to only add parseable certs when loading system certs because
    // there are typically many system certs and not all of them can be valid. This
    // is pertinent for e.g. macOS which may have a lot of old certificates that may
    // not parse correctly.
    let mut roots = RootCertStore::empty();
    let (valid, invalid) = roots.add_parsable_certificates(root_certs.as_slice());

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
async fn load_key<P: AsRef<Path>>(key: P) -> buck2_error::Result<PrivateKey> {
    let key = key.as_ref();

    let key_data = tokio::fs::read(key)
        .await
        .with_buck_error_context(|| format!("Error opening key file `{}`", key.display()))?;

    let private_key = rustls_pemfile::pkcs8_private_keys(&mut key_data.as_slice())
        .with_buck_error_context(|| format!("Error parsing key file `{}`", key.display()))?
        .pop()
        .with_buck_error_context(|| {
            format!("Found no private key in key file `{}`", key.display())
        })?;
    let key = PrivateKey(private_key);

    Ok(key)
}

/// Deserialize certificate pair at `cert` and `key` into structures that can
/// be inserted into rustls CertStore.
async fn load_cert_pair<P: AsRef<Path>>(
    cert: P,
    key: P,
) -> buck2_error::Result<(Vec<Certificate>, PrivateKey)> {
    let certs = load_certs(cert).await?.into_map(Certificate);
    let key = load_key(key).await?;

    Ok((certs, key))
}

pub async fn tls_config_with_system_roots() -> buck2_error::Result<ClientConfig> {
    let system_roots = load_system_root_certs().await?;
    Ok(ClientConfig::builder()
        .with_safe_defaults()
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
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_client_auth_cert(cert, key)
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
        .buck_error_context("Error creating TLS config with cert and key path")
}

// Load certs from the given path, returns the bytes of the certs so caller can decide what to do with it
pub(crate) async fn load_certs<P: AsRef<Path>>(cert_path: P) -> buck2_error::Result<Vec<Vec<u8>>> {
    let cert_path = cert_path.as_ref();

    let cert_data = tokio::fs::read(cert_path)
        .await
        .with_buck_error_context(|| {
            format!("Error reading certificate file `{}`", cert_path.display())
        })?;

    let certs = rustls_pemfile::certs(&mut cert_data.as_slice()).with_buck_error_context(|| {
        format!("Error parsing certificate file `{}`", cert_path.display())
    })?;

    Ok(certs)
}

/// Find root CA certs.
///
/// In OSS or non-fbcode builds, returns None; we do not support hardcoded root
/// certificates in non-fbcode builds and rely solely on rustls-native-certs.
pub(crate) fn find_root_ca_certs() -> Option<OsString> {
    #[cfg(fbcode_build)]
    return find_certs::find_root_ca_certs();

    #[cfg(not(fbcode_build))]
    return None;
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

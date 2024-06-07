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
use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use anyhow::Context;
use buck2_util::process::background_command;
use gazebo::prelude::VecExt;
use rustls::Certificate;
use rustls::ClientConfig;
use rustls::PrivateKey;
use rustls::RootCertStore;

#[derive(Debug, buck2_error::Error)]
#[error("ERROR - COULD NOT FIND VALID CERTS")]
struct InvalidCertsError;

/// Find root CA certs.
///
/// In OSS or non-fbcode builds, returns None; we do not support hardcoded root
/// certificates in non-fbcode builds and rely solely on rustls-native-certs.
fn find_root_ca_certs() -> Option<OsString> {
    #[cfg(fbcode_build)]
    return find_certs::find_root_ca_certs();

    #[cfg(not(fbcode_build))]
    return None;
}

/// Load system root certificates from disk.
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
    let native_certs = rustls_native_certs::load_native_certs()
        .context("Error loading system root certificates native frameworks");

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
            load_system_root_certs_disk(&path)
                .with_context(|| format!("Loading root certs from: {}", path.to_string_lossy()))?
        } else {
            if let Err(e) = native_certs {
                return Err(e);
            }

            return Err(anyhow::anyhow!("Unable to load system root certificates"));
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
        return Err(anyhow::anyhow!(
            "Error loading system certs: unable to find any valid system certs"
        ));
    }
    tracing::debug!("Loaded {} valid system root certs", valid);
    tracing::debug!("Loaded {} invalid system root certs", invalid);
    Ok(roots)
}

// Load private key from the given path
fn load_key<P: AsRef<Path>>(key: P) -> anyhow::Result<PrivateKey> {
    let key = key.as_ref();

    let key_data =
        fs::read(key).with_context(|| format!("Error opening key file `{}`", key.display()))?;

    let private_key = rustls_pemfile::pkcs8_private_keys(&mut key_data.as_slice())
        .with_context(|| format!("Error parsing key file `{}`", key.display()))?
        .pop()
        .with_context(|| format!("Found no private key in key file `{}`", key.display()))?;
    let key = PrivateKey(private_key);

    Ok(key)
}

/// Deserialize certificate pair at `cert` and `key` into structures that can
/// be inserted into rustls CertStore.
fn load_cert_pair<P: AsRef<Path>>(
    cert: P,
    key: P,
) -> anyhow::Result<(Vec<Certificate>, PrivateKey)> {
    let certs = load_certs(cert)?.into_map(Certificate);
    let key = load_key(key)?;

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
    ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(system_roots)
        .with_client_auth_cert(cert, key)
        .context("Error creating TLS config with cert and key path")
}

// Load certs from the given path, returns the bytes of the certs so caller can decide what to do with it
pub fn load_certs<P: AsRef<Path>>(cert: P) -> anyhow::Result<Vec<Vec<u8>>> {
    let cert = cert.as_ref();

    let cert_data = fs::read(cert)
        .with_context(|| format!("Error reading certificate file `{}`", cert.display()))?;

    let certs = rustls_pemfile::certs(&mut cert_data.as_slice())
        .with_context(|| format!("Error parsing certificate file `{}`", cert.display()))?;

    Ok(certs)
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

/// Use SKS Agent to check the status of the VPNless cert in the scenario that VPNless is supported.
/// SKS Agent is different in Windows so we need to use the appropriate command for the OS.
///
/// TODO(minglunli): Maybe this code should be moved to new crate like `buck2_certs`` or something?
pub fn is_vpnless_cert_valid() -> bool {
    if !crate::x2p::supports_vpnless() {
        return false;
    }

    let sks_agent = if cfg!(target_os = "windows") {
        "sks-agent"
    } else {
        "fb-sks-agent"
    };

    // Post suggests using the following for VPN-less scenario
    // https://fb.workplace.com/groups/382932749004606/permalink/1473311023300101/
    let cmd_result = background_command(sks_agent)
        .args(["renew", "--status", "--corp-x509"])
        .output();

    match cmd_result {
        Ok(cmd_output) => String::from_utf8_lossy(&cmd_output.stdout).starts_with("true"),
        Err(_) => false,
    }
}

/// Check if the provided certs exists and if it is still valid at the current time.
pub fn is_cert_valid(certs: Vec<Vec<u8>>) -> bool {
    certs.iter().any(|bytes| {
        let x509_cert = match x509_parser::parse_x509_certificate(bytes) {
            Ok((_, x509_cert)) => x509_cert,
            Err(_) => return false,
        };

        x509_cert.validity().is_valid()
    })
}

pub fn validate_certs() -> anyhow::Result<()> {
    if cfg!(fbcode_build) {
        let tls_certs_valid = match find_internal_cert() {
            Some(cert_path) => {
                let certs = load_certs(cert_path)?;
                is_cert_valid(certs)
            }
            None => false,
        };

        if !tls_certs_valid && !is_vpnless_cert_valid() {
            return Err(InvalidCertsError.into());
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use x509_parser::pem::parse_x509_pem;

    use crate::tls::is_cert_valid;

    fn x509_str_to_der(pem: &str) -> Vec<u8> {
        let der = parse_x509_pem(pem.as_bytes()).unwrap();
        der.1.contents
    }

    #[test]
    fn invalid_certs_test() {
        let empty_certs = vec![vec![]];
        let invalid_certs = vec![vec![4, 9, 7, 0, 8, 42]];

        // Self-signed cert for testing. Expired 05/31/2024
        let expired_cert = "-----BEGIN CERTIFICATE-----
MIICRjCCAa+gAwIBAgIBADANBgkqhkiG9w0BAQ0FADBAMQswCQYDVQQGEwJ1czET
MBEGA1UECAwKV2FzaGluZ3RvbjENMAsGA1UECgwETWV0YTENMAsGA1UEAwwEQnVj
azAeFw0yNDA1MzExODMzMjFaFw0yNDA2MDExODMzMjFaMEAxCzAJBgNVBAYTAnVz
MRMwEQYDVQQIDApXYXNoaW5ndG9uMQ0wCwYDVQQKDARNZXRhMQ0wCwYDVQQDDARC
dWNrMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHgu9ZYLZCd6MdI3mLTwCD
La8u8Qqt10rlyUZ7PxivRHVKKa7MtFD9GniYq3KxeSmUG2MCZaqlMRWsef+4tXXy
6jXalPZKEQEqupc9QCBcAeQvWL+wpzRPG4eYambnhMbI+I7qUwb0LKZssV9kxTzm
ulA+OPR78NBOuP2a7HECVwIDAQABo1AwTjAdBgNVHQ4EFgQUvH8Of9v7NJPpufEf
MYigdGf4QCowHwYDVR0jBBgwFoAUvH8Of9v7NJPpufEfMYigdGf4QCowDAYDVR0T
BAUwAwEB/zANBgkqhkiG9w0BAQ0FAAOBgQCCovywmK/CpX/a6Uy/p0NoVBk/Mv7S
rZDz0fxhm4ae0KLTXZVKQb/gHCbbZwfurv1wu2gcYrxSlHOPAC9EhWBq7BSOowZ6
lXKnDGs/z5T+p7fuwjNj2qqBc3Ap/v430KvLQo5NH3nX0ur3R7J4zFOO2a/uwtpw
Bx4/wCWapqMUyw==
-----END CERTIFICATE-----";

        assert_eq!(false, is_cert_valid(empty_certs));
        assert_eq!(false, is_cert_valid(invalid_certs));
        assert_eq!(false, is_cert_valid(vec![x509_str_to_der(expired_cert)]));
    }

    #[test]
    fn valid_cert_test() {
        // Self-signed cert for testing. Should expire in 100 years if this is around for that long!
        // Generated using:
        // 1. openssl genrsa -out mykey.pem 2048
        // 2. openssl req -new -key mykey.pem -out mycsr.csr
        // 3. openssl x509 -req -in mycsr.csr -signkey mykey.pem -out x509.crt -days 36500
        // Copy content in x509.crt
        let valid_cert = "-----BEGIN CERTIFICATE-----
MIICSDCCAbGgAwIBAgIBADANBgkqhkiG9w0BAQ0FADBAMQswCQYDVQQGEwJ1czET
MBEGA1UECAwKV2FzaGluZ3RvbjENMAsGA1UECgwETWV0YTENMAsGA1UEAwwEQnVj
azAgFw0yNDA1MzExODM2NTFaGA8yMTI0MDUwNzE4MzY1MVowQDELMAkGA1UEBhMC
dXMxEzARBgNVBAgMCldhc2hpbmd0b24xDTALBgNVBAoMBE1ldGExDTALBgNVBAMM
BEJ1Y2swgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAL3zwyj19w2+Q3WR7S0Y
oZiHp+Yv6YIj824PPyVV/vFQr43BAScCic1nSZynHLmQEQA8EDrdNdQt/XvSW1hk
/IAV+h/9tnt5IlJ4f+GtNDVvYm749N45vnbeIghGqi9a2O5Rq8UbODQxN1dp6/JA
0M2RGIWFuC7J0XyugmZYQ0s1AgMBAAGjUDBOMB0GA1UdDgQWBBSUKFZzjdxaHECE
INHhx66lztPozTAfBgNVHSMEGDAWgBSUKFZzjdxaHECEINHhx66lztPozTAMBgNV
HRMEBTADAQH/MA0GCSqGSIb3DQEBDQUAA4GBAJUtNrGWSCe2B3oh0xTN7ovieFXw
tw4vIDXD37nIRxw3hJEUOy6+/IsyvMK8zKSG1gDfFWsFtFtI1F/g3gqUornjvpHA
E4miAiU9J+PZbNobBKzhYcb6DppuNFr0Q1mNq0oxmodDCR4+pSCZJJETorhtF96z
nzcrwb6QVFOKt510
-----END CERTIFICATE-----";

        assert_eq!(true, is_cert_valid(vec![x509_str_to_der(valid_cert)]));
    }
}

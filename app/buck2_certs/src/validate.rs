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
use std::sync::Arc;

use buck2_error::buck2_error;
use buck2_util::process::async_background_command;
use dupe::Dupe;
use tokio::sync::Mutex;

use crate::certs;
use crate::certs::load_certs;

#[derive(Debug, buck2_error::Error)]
#[buck2(environment, tag = NoValidCerts)]
enum InvalidCertsError {
    #[error(
        "Could not find valid root certs. Please check your machine certificate settings. Instructions: https://fburl.com/devcert\nFailure Reason: {0}"
    )]
    SystemCerts(String),
    #[error(
        "Could not find valid client certs. Try refreshing your internal certs or re-login and try again. Instructions: https://fburl.com/devcert\nFailure Reason: {0}"
    )]
    ClientCerts(String),
    #[error(
        "Could not find valid certs for VPNless. Please refresh/renew certs with SKS agent and try again. Instructions: https://fburl.com/devcert"
    )]
    VPNlessCerts,
}

/// Use SKS Agent to check the status of the VPNless cert in the scenario that VPNless is supported.
/// SKS Agent is different in Windows so we need to use the appropriate command for the OS.
async fn is_vpnless_cert_valid() -> bool {
    let sks_agent = if cfg!(target_os = "windows") {
        "sks-agent"
    } else {
        "fb-sks-agent"
    };

    // Post suggests using the following for VPN-less scenario
    // https://fb.workplace.com/groups/382932749004606/permalink/1473311023300101/
    let cmd_result = async_background_command(sks_agent)
        .args(["renew", "--status", "--corp-x509"])
        .output()
        .await;

    match cmd_result {
        Ok(cmd_output) => String::from_utf8_lossy(&cmd_output.stdout).starts_with("true"),
        Err(_) => false,
    }
}

/// Check if the provided certs exists and if it is still valid at the current time.
async fn verify(path: &OsString) -> buck2_error::Result<()> {
    let certs = load_certs(path).await?;
    if certs.is_empty() {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Environment,
            "Could not find any certs to validate at '{0}'",
            path.to_string_lossy()
        ));
    }

    let valid = certs.iter().any(|bytes| {
        let x509_cert = match x509_parser::parse_x509_certificate(bytes) {
            Ok((_, x509_cert)) => x509_cert,
            Err(_) => return false,
        };

        x509_cert.validity().is_valid()
    });

    if !valid {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Environment,
            "Certificate Expired: expired certs found at '{0}'",
            path.to_string_lossy()
        ));
    }

    Ok(())
}

pub async fn validate_certs() -> buck2_error::Result<()> {
    if cfg!(not(fbcode_build)) {
        return Ok(());
    }

    if certs::supports_vpnless() {
        if is_vpnless_cert_valid().await {
            return Ok(());
        }

        return Err(InvalidCertsError::VPNlessCerts.into());
    } else {
        let err_msg = "Could not find any files that may contain certificates";
        // System certs are unlikely to be invalid, but if it is, it's a bigger issue than invalid client certs so we check it first
        match certs::find_root_ca_certs() {
            Some(root_certs) => {
                if let Err(e) = verify(&root_certs).await {
                    return Err(InvalidCertsError::SystemCerts(e.to_string()).into());
                }
            }
            None => return Err(InvalidCertsError::SystemCerts(err_msg.to_owned()).into()),
        }

        match certs::find_internal_cert() {
            Some(client_certs) => {
                if let Err(e) = verify(&client_certs).await {
                    return Err(InvalidCertsError::ClientCerts(e.to_string()).into());
                }
            }
            None => return Err(InvalidCertsError::ClientCerts(err_msg.to_owned()).into()),
        }
    }

    Ok(())
}

#[derive(Clone, Dupe)]
pub struct CertState {
    pub state: Arc<Mutex<bool>>,
}

impl CertState {
    pub async fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(validate_certs().await.is_ok())),
        }
    }
}

pub async fn check_cert_state(cert_state: CertState) -> Option<buck2_error::Error> {
    let mut valid = cert_state.state.lock().await;

    // If previous state is error, then we need to check regardless of the current state
    // since we are expecting users to actively fix the issue and retry
    if !*valid {
        match validate_certs().await {
            Ok(_) => *valid = true,
            Err(e) => return Some(e),
        }
    }

    None
}

#[cfg(fbcode_build)]
#[cfg(test)]
mod tests {
    use std::env;
    use std::ffi::OsString;

    use crate::validate::verify;

    #[tokio::test]
    async fn invalid_certs_test() {
        let base_path = env::var("TEST_CERT_LOCATIONS").unwrap();

        let empty_path = format!("{base_path}/test_empty.pem");
        let empty_res = verify(&OsString::from(empty_path)).await;
        assert!(empty_res.is_err());
        let err_msg = empty_res.unwrap_err().to_string();
        assert!(
            err_msg.starts_with("Could not find any certs to validate"),
            "{}",
            format!("Actual: {err_msg}")
        );

        let invalid_path = format!("{base_path}/test_invalid.pem");
        let invalid_res = verify(&OsString::from(invalid_path)).await;
        assert!(invalid_res.is_err());
        let err_msg = invalid_res.unwrap_err().to_string();
        assert!(
            err_msg.starts_with("Could not find any certs to validate"),
            "{}",
            format!("Actual: {err_msg}")
        );

        // Self-signed cert for testing. Expired 05/31/2024
        let expired_path = format!("{base_path}/test_expired.pem");
        let expired_res = verify(&OsString::from(expired_path)).await;
        assert!(expired_res.is_err());
        let err_msg = expired_res.unwrap_err().to_string();
        assert!(
            err_msg.starts_with("Certificate Expired"),
            "{}",
            format!("Actual: {err_msg}")
        );
    }

    #[tokio::test]
    async fn valid_cert_test() {
        // Self-signed cert for testing. Should expire in 100 years if this is around for that long!
        // Generated using:
        // 1. openssl genrsa -out mykey.pem 2048
        // 2. openssl req -new -key mykey.pem -out mycsr.csr
        // 3. openssl x509 -req -in mycsr.csr -signkey mykey.pem -out x509.crt -days 36500
        // Copy content in x509.crt
        let base_path = env::var("TEST_CERT_LOCATIONS").unwrap();
        let valid_path = format!("{base_path}/test_valid.pem");
        assert!(verify(&OsString::from(valid_path)).await.is_ok());
    }
}

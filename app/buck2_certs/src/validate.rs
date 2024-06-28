/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::process::async_background_command;

use crate::certs::find_internal_cert;
use crate::certs::load_certs;
use crate::certs::supports_vpnless;

#[derive(Debug, buck2_error::Error)]
#[buck2(input, tag = NoValidCerts)]
#[error("ERROR - COULD NOT FIND VALID CERTS")]
struct InvalidCertsError;

/// Use SKS Agent to check the status of the VPNless cert in the scenario that VPNless is supported.
/// SKS Agent is different in Windows so we need to use the appropriate command for the OS.
pub async fn is_vpnless_cert_valid() -> bool {
    if !supports_vpnless() {
        return false;
    }

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
pub fn is_cert_valid(certs: Vec<Vec<u8>>) -> bool {
    certs.iter().any(|bytes| {
        let x509_cert = match x509_parser::parse_x509_certificate(bytes) {
            Ok((_, x509_cert)) => x509_cert,
            Err(_) => return false,
        };

        x509_cert.validity().is_valid()
    })
}

pub async fn validate_certs() -> anyhow::Result<()> {
    if cfg!(fbcode_build) {
        let tls_certs_valid = match find_internal_cert() {
            Some(cert_path) => {
                let certs = load_certs(cert_path).await?;
                is_cert_valid(certs)
            }
            None => false,
        };

        if !tls_certs_valid && !is_vpnless_cert_valid().await {
            return Err(InvalidCertsError.into());
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use x509_parser::pem::parse_x509_pem;

    use crate::validate::is_cert_valid;

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

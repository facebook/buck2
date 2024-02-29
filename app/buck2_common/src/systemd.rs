/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::ffi::OsStr;
use std::io::ErrorKind;
use std::num::ParseIntError;
use std::process::Command;
use std::sync::OnceLock;

use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_util::process;

const SYSTEMD_MIN_VERSION: u32 = 253;
static AVAILABILITY: OnceLock<Option<SystemdNotAvailableReason>> = OnceLock::new();

#[derive(Debug, buck2_error::Error)]
enum SystemdNotAvailableReason {
    #[error("Unexpected `systemctl --version` output format: {0}")]
    UnexpectedVersionOutputFormat(String),
    #[error("Failed to parse systemd version number into u32: {:#}", .0)]
    VersionNumberParseError(ParseIntError),
    #[error("Detected systemd version {detected}. Minimum requirement is {min_required}.")]
    TooOldSystemdVersion { detected: u32, min_required: u32 },
    #[error("Systemctl command returned non-zero: {0}")]
    SystemctlCommandReturnedNonZero(String),
    #[error("Systemctl command failed to launch: {:#}", .0)]
    SystemctlCommandLaunchFailed(std::io::Error),
    #[error("Systemctl command not found in PATH.")]
    SystemctlCommandNotFound,
    #[error("Resource control with systemd is only supported on Linux.")]
    UnsupportedPlatform,
}

pub struct SystemdRunner {
    unit_name: String,
    working_directory: AbsNormPathBuf,
    collect: bool,
    properties: HashMap<String, String>,
}

impl SystemdRunner {
    pub fn new(
        unit_name: String,
        working_directory: AbsNormPathBuf,
        collect: bool,
        properties: HashMap<String, String>,
    ) -> Self {
        Self {
            unit_name,
            working_directory,
            collect,
            properties,
        }
    }

    pub fn background_command_linux<S: AsRef<OsStr>>(&self, program: S) -> std::process::Command {
        let mut cmd = process::background_command("systemd-run");
        cmd.arg("--user")
            .arg("--scope")
            .arg("--quiet")
            .arg(format!("--working-directory={}", self.working_directory))
            .arg(format!("--unit={}", self.unit_name));
        if self.collect {
            cmd.arg("--collect");
        }
        for (key, value) in self.properties.iter() {
            cmd.arg(format!("--property={}={}", key, value));
        }

        cmd.arg(program);
        cmd
    }
}

fn validate_systemd_version(raw_stdout: &[u8]) -> Result<(), SystemdNotAvailableReason> {
    let stdout = String::from_utf8_lossy(raw_stdout);
    let version = stdout
        .split(' ')
        .nth(1)
        .ok_or_else(|| {
            SystemdNotAvailableReason::UnexpectedVersionOutputFormat(stdout.to_string())
        })?
        .parse::<u32>()
        .map_err(SystemdNotAvailableReason::VersionNumberParseError)?;

    if version < SYSTEMD_MIN_VERSION {
        Err(SystemdNotAvailableReason::TooOldSystemdVersion {
            detected: version,
            min_required: SYSTEMD_MIN_VERSION,
        })
    } else {
        Ok(())
    }
}

pub fn is_available() -> anyhow::Result<()> {
    if !cfg!(target_os = "linux") {
        return Err(SystemdNotAvailableReason::UnsupportedPlatform.into());
    }

    let unavailable_reason = AVAILABILITY.get_or_init(|| -> Option<SystemdNotAvailableReason> {
        // patternlint-disable-next-line buck2-no-command-new
        match Command::new("systemctl").arg("--version").output() {
            Ok(output) => {
                if output.status.success() {
                    match validate_systemd_version(&output.stdout) {
                        Ok(_) => None,
                        Err(e) => Some(e),
                    }
                } else {
                    Some(SystemdNotAvailableReason::SystemctlCommandReturnedNonZero(
                        String::from_utf8_lossy(&output.stderr).to_string(),
                    ))
                }
            }
            Err(e) => match e.kind() {
                ErrorKind::NotFound => Some(SystemdNotAvailableReason::SystemctlCommandNotFound),
                _ => Some(SystemdNotAvailableReason::SystemctlCommandLaunchFailed(e)),
            },
        }
    });

    match unavailable_reason {
        None => Ok(()),
        Some(r) => Err(r.into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_systemd_version_normal() {
        let raw_output = "systemd 253 (v253.7-1.9.hs+fb.el9)".as_bytes();
        assert!(validate_systemd_version(raw_output).is_ok());
    }

    #[test]
    fn test_validate_systemd_version_unexpected_format() {
        let raw_output = "abc".as_bytes();
        assert!(matches!(
            validate_systemd_version(raw_output).unwrap_err(),
            SystemdNotAvailableReason::UnexpectedVersionOutputFormat(..)
        ));
    }

    #[test]
    fn test_validate_systemd_version_empty() {
        let raw_output = "".as_bytes();
        assert!(matches!(
            validate_systemd_version(raw_output).unwrap_err(),
            SystemdNotAvailableReason::UnexpectedVersionOutputFormat(..)
        ));
    }

    #[test]
    fn test_validate_systemd_version_unexpected_version() {
        let raw_output = "systemd v253.7-1.9.hs+fb.el9".as_bytes();
        assert!(matches!(
            validate_systemd_version(raw_output).unwrap_err(),
            SystemdNotAvailableReason::VersionNumberParseError(..)
        ));
    }

    #[test]
    fn test_validate_systemd_version_old_version() {
        let raw_output = "systemd 111 (v253.7-1.9.hs+fb.el9)".as_bytes();
        assert!(matches!(
            validate_systemd_version(raw_output).unwrap_err(),
            SystemdNotAvailableReason::TooOldSystemdVersion { .. }
        ));
    }

    #[cfg(not(target_os = "linux"))]
    #[test]
    fn test_always_unavailable_on_nonlinux() {
        assert!(matches!(
            is_available()
                .unwrap_err()
                .downcast::<SystemdNotAvailableReason>()
                .unwrap(),
            SystemdNotAvailableReason::UnsupportedPlatform
        ));
    }

    #[test]
    fn test_systemd_runner_background_command_normal() {
        let prefix = if cfg!(windows) { "C:" } else { "" };
        let mut properties = HashMap::new();
        properties.insert("test_k0".to_owned(), "test_v0".to_owned());
        properties.insert("test_k1".to_owned(), "test_v1".to_owned());
        let runner = SystemdRunner::new(
            "test_unit".to_owned(),
            AbsNormPathBuf::from(format!("{}/test/path", prefix)).unwrap(),
            true,
            properties,
        );
        let cmd = runner.background_command_linux("test_prog");

        assert_eq!(cmd.get_program(), "systemd-run");
        assert!(
            *cmd.get_args().collect::<Vec<&OsStr>>()
                == [
                    "--user",
                    "--scope",
                    "--quiet",
                    format!("--working-directory={}/test/path", prefix).as_str(),
                    "--unit=test_unit",
                    "--collect",
                    "--property=test_k0=test_v0",
                    "--property=test_k1=test_v1",
                    "test_prog"
                ]
                || *cmd.get_args().collect::<Vec<&OsStr>>()
                    == [
                        "--user",
                        "--scope",
                        "--quiet",
                        format!("--working-directory={}/test/path", prefix).as_str(),
                        "--unit=test_unit",
                        "--collect",
                        "--property=test_k1=test_v1",
                        "--property=test_k0=test_v0",
                        "test_prog"
                    ]
        );
    }

    #[test]
    fn test_systemd_runner_background_command_no_property_scope() {
        let prefix = if cfg!(windows) { "C:" } else { "" };
        let runner = SystemdRunner::new(
            "test_unit".to_owned(),
            AbsNormPathBuf::from(format!("{}/test/path", prefix)).unwrap(),
            false,
            HashMap::new(),
        );
        let cmd = runner.background_command_linux("test_prog");

        assert_eq!(cmd.get_program(), "systemd-run");
        assert!(
            *cmd.get_args().collect::<Vec<&OsStr>>()
                == [
                    "--user",
                    "--scope",
                    "--quiet",
                    format!("--working-directory={}/test/path", prefix).as_str(),
                    "--unit=test_unit",
                    "test_prog"
                ]
        );
    }
}

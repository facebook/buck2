/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ffi::OsStr;
use std::io::ErrorKind;
use std::num::ParseIntError;
use std::sync::OnceLock;

use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_util::process;

use crate::init::ResourceControlConfig;
use crate::init::ResourceControlStatus;

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

pub enum SystemdPropertySetType {
    Daemon,
    Worker,
}

pub struct SystemdRunner {
    fixed_systemd_args: Vec<String>,
    parent_slice: String,
}

impl SystemdRunner {
    fn create(
        property_set_type: SystemdPropertySetType,
        config: &ResourceControlConfig,
        parent_slice: &str,
        slice_inherit: bool,
    ) -> Self {
        // Common settings
        let mut args = vec![
            "--user".to_owned(),
            "--scope".to_owned(),
            "--quiet".to_owned(),
        ];
        if let Some(memory_max) = &config.memory_max {
            args.push(format!("--property=MemoryMax={}", memory_max.to_owned()));
            // Without setting `MemorySwapMax`, the process starts using swap until it's
            // filled when the total memory usage reaches to `MemoryMax`. This may seem
            // counterintuitive for mostly expected use cases. Setting `MemorySwapMax`
            // to zero makes `MemoryMax` to be a 'hard limit' at which the process is
            // stopped by OOM killer
            args.push("--property=MemorySwapMax=0".to_owned());
            // Set `OOMPolicy=kill` explicitly since otherwise (`OOMPolicy=continue`)
            // some workers can keep alive even after buck2 daemon has gone due to OOM.
            args.push("--property=OOMPolicy=kill".to_owned());
        }

        // Type-specific settings
        match property_set_type {
            SystemdPropertySetType::Daemon => {
                // Set `--collect` because this is the outermost scope in buck2's context
                // and we don't assume the upper-layer unit collects the garbage of this
                // scope after being killed.
                args.push("--collect".to_owned());
            }
            SystemdPropertySetType::Worker => { // TODO
            }
        }
        if slice_inherit {
            args.push("--slice-inherit".to_owned());
        }
        Self {
            fixed_systemd_args: args,
            parent_slice: parent_slice.to_owned(),
        }
    }

    pub fn create_if_enabled(
        property_set_type: SystemdPropertySetType,
        config: &ResourceControlConfig,
        parent_slice: &str,
        slice_inherit: bool,
    ) -> anyhow::Result<Option<Self>> {
        match config.status {
            ResourceControlStatus::Off => Ok(None),
            ResourceControlStatus::IfAvailable | ResourceControlStatus::Required => {
                if let Err(e) = is_available() {
                    if config.status == ResourceControlStatus::Required {
                        return Err(e.context("Systemd is unavailable but required by buckconfig"));
                    }
                    tracing::warn!(
                        "Systemd is not available on this system. Continuing without resource control: {:#}",
                        e
                    );
                    Ok(None)
                } else {
                    Ok(Some(Self::create(
                        property_set_type,
                        config,
                        parent_slice,
                        slice_inherit,
                    )))
                }
            }
        }
    }

    /// Creates `std::process::Command` to run `program` under a systemd scope unit. `unit_name` is
    /// an arbitrary string that you name the unit so it can be identified by the name later.
    pub fn background_command_linux<S: AsRef<OsStr>>(
        &self,
        program: S,
        unit_name: &str,
        working_directory: &AbsNormPath,
    ) -> std::process::Command {
        let mut cmd = process::background_command("systemd-run");
        cmd.args(&self.fixed_systemd_args);
        cmd.arg(format!("--slice={}-{}", self.parent_slice, unit_name));
        cmd.arg(format!("--working-directory={}", working_directory))
            .arg(format!("--unit={}", unit_name));
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

fn is_available() -> anyhow::Result<()> {
    if !cfg!(target_os = "linux") {
        return Err(SystemdNotAvailableReason::UnsupportedPlatform.into());
    }

    let unavailable_reason = AVAILABILITY.get_or_init(|| -> Option<SystemdNotAvailableReason> {
        match process::background_command("systemctl")
            .arg("--version")
            .output()
        {
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
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp::PartialEq;
use std::ffi::OsStr;
use std::io::ErrorKind;
use std::num::ParseIntError;
use std::sync::OnceLock;

use buck2_common::init::ResourceControlConfig;
use buck2_common::init::ResourceControlStatus;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_util::process;

const SYSTEMD_MIN_VERSION: u32 = 253;
static AVAILABILITY: OnceLock<Option<buck2_error::Error>> = OnceLock::new();

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum SystemdNotAvailableReason {
    #[error("Unexpected `systemctl --version` output format: {0}")]
    UnexpectedVersionOutputFormat(String),
    #[error("Failed to parse systemd version number into u32: {0:#}")]
    VersionNumberParseError(ParseIntError),
    #[error("Detected systemd version {detected}. Minimum requirement is {min_required}.")]
    TooOldSystemdVersion { detected: u32, min_required: u32 },
    #[error("Systemctl command returned non-zero: {0}")]
    SystemctlCommandReturnedNonZero(String),
    #[error("Systemctl command failed to launch: {0:#}")]
    SystemctlCommandLaunchFailed(std::io::Error),
    #[error("Systemctl command not found in PATH.")]
    SystemctlCommandNotFound,
    #[error("Resource control with systemd is only supported on Linux.")]
    UnsupportedPlatform,
}

pub enum SystemdCreationDecision {
    SkipNotNeeded,
    SkipPreferredButNotRequired { e: buck2_error::Error },
    SkipRequiredButUnavailable { e: buck2_error::Error },
    Create,
}

pub enum ParentSlice {
    /// Makes a new unit part of the specified slice inherited from root slice
    Root(String),
}

/// Turns on delegation of further resource control partitioning to processes of the unit.
/// Units where this is enabled may create and manage their own private subhierarchy
/// of control groups below the control group of the unit itself.
#[derive(PartialEq, Clone, Copy)]
pub enum CgroupDelegation {
    Enabled,
    Disabled,
}

pub struct ResourceControlRunner {
    fixed_systemd_args: Vec<String>,
}

impl ResourceControlRunner {
    fn create(parent_slice: ParentSlice) -> buck2_error::Result<Self> {
        // Common settings
        let mut args = vec![
            "--user".to_owned(),
            "--scope".to_owned(),
            "--quiet".to_owned(),
            "--collect".to_owned(),
            "--property=Delegate=yes".to_owned(),
        ];

        match &parent_slice {
            ParentSlice::Root(slice) => {
                args.push(format!("--slice={slice}"));
            }
        }

        Ok(Self {
            fixed_systemd_args: args,
        })
    }

    fn creation_decision(status: &ResourceControlStatus) -> SystemdCreationDecision {
        if status == &ResourceControlStatus::Off {
            return SystemdCreationDecision::SkipNotNeeded;
        }
        match (status, is_available()) {
            (ResourceControlStatus::Off, _) => unreachable!("Checked earlier"),
            (ResourceControlStatus::IfAvailable | ResourceControlStatus::Required, Ok(_)) => {
                SystemdCreationDecision::Create
            }
            (ResourceControlStatus::IfAvailable, Err(e)) => {
                SystemdCreationDecision::SkipPreferredButNotRequired { e }
            }
            (ResourceControlStatus::Required, Err(e)) => {
                SystemdCreationDecision::SkipRequiredButUnavailable { e }
            }
        }
    }

    fn is_enabled(config: &ResourceControlStatus) -> buck2_error::Result<bool> {
        let decision = Self::creation_decision(config);
        match decision {
            SystemdCreationDecision::SkipNotNeeded => Ok(false),
            SystemdCreationDecision::SkipPreferredButNotRequired { e } => {
                tracing::warn!(
                    "Systemd is not available on this system. Continuing without resource control: {:#}",
                    e
                );
                Ok(false)
            }
            SystemdCreationDecision::SkipRequiredButUnavailable { e } => {
                Err(e.context("Systemd is unavailable but required by buckconfig"))
            }
            SystemdCreationDecision::Create => Ok(true),
        }
    }

    pub fn create_if_enabled(
        config: &ResourceControlConfig,
        parent_slice: ParentSlice,
    ) -> buck2_error::Result<Option<Self>> {
        if Self::is_enabled(&config.status)? {
            Ok(Some(Self::create(parent_slice)?))
        } else {
            Ok(None)
        }
    }

    /// Creates `std::process::Command` to run `program` under a systemd scope unit (cgroup). `unit_name` is
    /// an arbitrary string that you name the unit so it can be identified by the name later.
    pub fn cgroup_scoped_command<S: AsRef<OsStr>>(
        &self,
        program: S,
        unit_name: &str,
        working_directory: &AbsNormPath,
    ) -> std::process::Command {
        let mut cmd = process::background_command("systemd-run");
        cmd.args(&self.fixed_systemd_args);
        cmd.arg(format!("--working-directory={working_directory}"))
            .arg(format!("--unit={unit_name}"));
        cmd.arg(program);
        cmd
    }
}

// Helper function to replace a special characters in a cgroup unit name
pub fn replace_unit_delimiter(unit: &str) -> String {
    unit.replace("-", "_").replace(":", "_")
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

fn is_available() -> buck2_error::Result<()> {
    if !cfg!(target_os = "linux") {
        return Err(SystemdNotAvailableReason::UnsupportedPlatform.into());
    }

    let unavailable_reason = AVAILABILITY.get_or_init(|| -> Option<buck2_error::Error> {
        match process::background_command("systemctl")
            .arg("--version")
            .output()
        {
            Ok(output) => {
                if output.status.success() {
                    match validate_systemd_version(&output.stdout) {
                        Ok(_) => None,
                        Err(e) => Some(e.into()),
                    }
                } else {
                    Some(
                        SystemdNotAvailableReason::SystemctlCommandReturnedNonZero(
                            String::from_utf8_lossy(&output.stderr).to_string(),
                        )
                        .into(),
                    )
                }
            }
            Err(e) => match e.kind() {
                ErrorKind::NotFound => {
                    Some(SystemdNotAvailableReason::SystemctlCommandNotFound.into())
                }
                _ => Some(SystemdNotAvailableReason::SystemctlCommandLaunchFailed(e).into()),
            },
        }
    });

    match unavailable_reason {
        None => Ok(()),
        Some(r) => Err(r.clone()),
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
        let _error = buck2_error::Error::from(SystemdNotAvailableReason::UnsupportedPlatform);
        assert!(matches!(is_available().unwrap_err(), _error));
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::PartialEq;
use std::ffi::OsStr;
use std::io::ErrorKind;
use std::num::ParseIntError;
use std::sync::OnceLock;

use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_util::process;

use crate::init::ResourceControlConfig;
use crate::init::ResourceControlStatus;

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
    /// Makes a new unit part of the specified slice inherited from current process slice
    Inherit(String),
    /// Makes a new unit part of the specified slice inherited from root slice
    Root(String),
}

/// Turns on delegation of further resource control partitioning to processes of the unit.
/// Units where this is enabled may create and manage their own private subhierarchy
/// of control groups below the control group of the unit itself.
#[derive(PartialEq)]
pub enum CgroupDelegation {
    Enabled,
    Disabled,
}

pub struct SystemdRunnerConfig {
    /// A config to determine if systemd is available.
    pub status: ResourceControlStatus,
    /// A memory threshold. Semantics (whether it is memory of a daemon or an action process) depend on the context.
    pub memory_max: Option<String>,
    /// Parent slice behaviour
    pub parent_slice: ParentSlice,
    /// Delegation of further resource control partitioning of cgroup unit
    pub delegation: CgroupDelegation,
}

impl SystemdRunnerConfig {
    pub fn daemon_runner_config(config: &ResourceControlConfig, parent_slice: ParentSlice) -> Self {
        Self {
            status: config.status.clone(),
            memory_max: config.memory_max.clone(),
            parent_slice,
            delegation: CgroupDelegation::Disabled,
        }
    }

    pub fn action_runner_config(config: &ResourceControlConfig, parent_slice: ParentSlice) -> Self {
        Self {
            status: config.status.clone(),
            memory_max: config.memory_max_per_action.clone(),
            parent_slice,
            delegation: CgroupDelegation::Enabled,
        }
    }
}

pub struct SystemdRunner {
    fixed_systemd_args: Vec<String>,
    memory_limit: Option<String>,
}

impl SystemdRunner {
    fn create(config: &SystemdRunnerConfig) -> Self {
        // Common settings
        let mut args = vec![
            "--user".to_owned(),
            "--scope".to_owned(),
            "--quiet".to_owned(),
            "--collect".to_owned(),
            #[cfg(fbcode_build)]
            "--setenv=CHGDISABLE=1".to_owned(),
        ];

        if let Some(memory_max) = &config.memory_max {
            args.push(format!("--property=MemoryMax={}", memory_max));
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

        if config.delegation == CgroupDelegation::Enabled {
            args.push("--property=Delegate=yes".to_owned());
        }

        match &config.parent_slice {
            ParentSlice::Inherit(slice) => {
                args.push(format!("--slice={}", slice));
                args.push("--slice-inherit".to_owned());
            }
            ParentSlice::Root(slice) => {
                args.push(format!("--slice={}", slice));
            }
        }
        Self {
            fixed_systemd_args: args,
            memory_limit: config.memory_max.clone(),
        }
    }

    pub fn creation_decision(status: &ResourceControlStatus) -> SystemdCreationDecision {
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

    pub fn create_if_enabled(config: &SystemdRunnerConfig) -> buck2_error::Result<Option<Self>> {
        let decision = Self::creation_decision(&config.status);
        match decision {
            SystemdCreationDecision::SkipNotNeeded => Ok(None),
            SystemdCreationDecision::SkipPreferredButNotRequired { e } => {
                tracing::warn!(
                    "Systemd is not available on this system. Continuing without resource control: {:#}",
                    e
                );
                Ok(None)
            }
            SystemdCreationDecision::SkipRequiredButUnavailable { e } => {
                Err(e.context("Systemd is unavailable but required by buckconfig"))
            }
            SystemdCreationDecision::Create => Ok(Some(Self::create(config))),
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
        cmd.arg(format!("--working-directory={}", working_directory))
            .arg(format!("--unit={}", unit_name));
        cmd.arg(program);
        cmd
    }

    pub async fn set_slice_memory_limit(&self, slice: &str) -> buck2_error::Result<()> {
        let Some(memory_limit) = &self.memory_limit else {
            return Ok(());
        };
        let mut cmd = process::async_background_command("systemctl");
        cmd.arg("--user");
        cmd.arg("set-property");
        cmd.arg(slice);
        cmd.arg(format!("MemoryMax={}", memory_limit));
        cmd.arg("MemorySwapMax=0");
        cmd.arg("ManagedOOMMemoryPressure=kill");
        let result = cmd.output().await?;
        if !result.status.success() {
            let stderr = String::from_utf8(result.stderr)?;
            return Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Failed to set memory limits for {}: {}",
                slice,
                stderr
            ));
        }
        Ok(())
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

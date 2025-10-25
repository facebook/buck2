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
use std::path::Path;
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

#[derive(Debug, Clone, Copy)]
pub enum CgroupMemoryFile {
    MemoryHigh,
    MemoryMax,
    MemorySwapMax,
    MemorySwapHigh,
}

impl std::fmt::Display for CgroupMemoryFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CgroupMemoryFile::MemoryHigh => write!(f, "memory.high"),
            CgroupMemoryFile::MemoryMax => write!(f, "memory.max"),
            CgroupMemoryFile::MemorySwapMax => write!(f, "memory.swap.max"),
            CgroupMemoryFile::MemorySwapHigh => write!(f, "memory.swap.high"),
        }
    }
}

impl CgroupMemoryFile {
    pub fn read(&self, path: impl AsRef<Path>) -> buck2_error::Result<String> {
        let file_path = path.as_ref().join(self.to_string());
        Ok(std::fs::read_to_string(file_path)?.trim().to_owned())
    }

    pub async fn read_async(&self, path: impl AsRef<Path>) -> buck2_error::Result<String> {
        let file_path = path.as_ref().join(self.to_string());
        let content = tokio::fs::read_to_string(&file_path)
            .await?
            .trim()
            .to_owned();
        Ok(content)
    }

    pub fn set(&self, path: impl AsRef<Path>, value: &str) -> buck2_error::Result<()> {
        let file_path = path.as_ref().join(self.to_string());
        std::fs::write(file_path, value)?;
        Ok(())
    }

    /// Helper function to generate the sequence of paths to check when traversing up the cgroup hierarchy.
    fn get_ancestor_paths(path: &Path) -> impl Iterator<Item = std::path::PathBuf> {
        let cgroup_root = Path::new("/sys/fs/cgroup");
        let current_path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            cgroup_root.join(path)
        };

        // Start from the slice parent
        let start_path = current_path.parent().map(|p| p.to_path_buf());
        let cgroup_root_parent = cgroup_root
            .parent()
            .expect("The cgroup root has a parent")
            .to_path_buf();

        std::iter::successors(start_path, move |current| {
            current.parent().and_then(|parent| {
                // Stop if we've reached the cgroup root's parent
                if parent == cgroup_root_parent {
                    return None;
                }
                Some(parent.to_path_buf())
            })
        })
    }

    /// Finds the nearest ancestor memory limit by traversing up the cgroup hierarchy (async version).
    ///
    /// This function walks up the cgroup tree starting from the given path until it finds
    /// a concrete memory limit (non-"max" value) or reaches the root. This is useful for
    /// percentage-based memory calculations that need to know the parent limit.
    ///
    /// If the path is relative, it will be treated as relative to `/sys/fs/cgroup`.
    pub async fn find_ancestor_memory_limit_async(
        &self,
        path: impl AsRef<Path>,
    ) -> buck2_error::Result<Option<String>> {
        for current_path in Self::get_ancestor_paths(path.as_ref()) {
            match self.read_async(&current_path).await {
                Ok(content) => {
                    let trimmed = content.trim();
                    if trimmed != "max" {
                        return Ok(Some(trimmed.to_owned()));
                    }
                }
                Err(_) => {
                    // File doesn't exist, continue to parent
                }
            }
        }

        Ok(None)
    }
}

pub struct ResourceControlRunner {
    fixed_systemd_args: Vec<String>,
    memory_limit: Option<String>,
    memory_high: Option<String>,
}

impl ResourceControlRunner {
    fn create(
        config: &ResourceControlConfig,
        parent_slice: ParentSlice,
    ) -> buck2_error::Result<Self> {
        // Common settings
        let mut args = vec![
            "--user".to_owned(),
            "--scope".to_owned(),
            "--quiet".to_owned(),
            "--collect".to_owned(),
            #[cfg(fbcode_build)]
            "--setenv=CHGDISABLE=1".to_owned(),
            "--property=Delegate=yes".to_owned(),
        ];

        match &parent_slice {
            ParentSlice::Root(slice) => {
                args.push(format!("--slice={slice}"));
            }
        }

        Ok(Self {
            fixed_systemd_args: args,
            memory_limit: config.memory_max.clone(),
            memory_high: config.memory_high.clone(),
        })
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
            Ok(Some(Self::create(config, parent_slice)?))
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

    pub async fn set_slice_memory_info(&self, slice: &str) -> buck2_error::Result<()> {
        if self.memory_limit.is_none() && self.memory_high.is_none() {
            return Ok(());
        }

        let mut cmd = process::async_background_command("systemctl");
        cmd.arg("--user");
        cmd.arg("set-property");
        cmd.arg(slice);

        // Resolves memory restrictions by converting percentage values to absolute bytes
        // based on parent slice limits, or returns the original value if not a percentage
        async fn resolve_memory_restriction_value(
            memory_restriction: &str,
            slice: &str,
            memory_file_type: CgroupMemoryFile,
        ) -> buck2_error::Result<String> {
            if memory_restriction.ends_with("%") {
                let memory_percentage = memory_restriction
                    .strip_suffix("%")
                    .unwrap()
                    .parse::<u64>()
                    .expect(
                        "if setting the percentage of the memory, the value should be an integer",
                    );
                let cgroup_path = get_cgroup_path_async(slice).await?;
                if let Some(parent_memory_restrictions) = memory_file_type
                    .find_ancestor_memory_limit_async(&cgroup_path)
                    .await?
                {
                    // The value from the memory file is either "max" or a integer value representing bytes
                    let parent_memory_bytes = parent_memory_restrictions.parse::<u64>()?;
                    let calculated_memory_bytes = (parent_memory_bytes * memory_percentage) / 100;
                    Ok(calculated_memory_bytes.to_string())
                } else {
                    Ok(memory_restriction.to_owned())
                }
            } else {
                Ok(memory_restriction.to_owned())
            }
        }

        if let Some(memory_limit) = &self.memory_limit {
            let memory_limit =
                resolve_memory_restriction_value(&memory_limit, slice, CgroupMemoryFile::MemoryMax)
                    .await?;

            cmd.arg(format!("MemoryMax={memory_limit}"));
            cmd.arg("MemorySwapMax=0");
            cmd.arg("ManagedOOMMemoryPressure=kill");
        }

        if let Some(memory_high) = &self.memory_high {
            let memory_high =
                resolve_memory_restriction_value(&memory_high, slice, CgroupMemoryFile::MemoryHigh)
                    .await?;

            cmd.arg(format!("MemoryHigh={memory_high}"));
        }

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

/// Gets the cgroup path from systemctl asynchronously
async fn get_cgroup_path_async(slice_name: &str) -> buck2_error::Result<String> {
    let mut cmd = process::async_background_command("systemctl");
    cmd.args(["--user", "show", "-p", "ControlGroup", slice_name]);

    let output = cmd.output().await?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Failed to get ControlGroup for slice {}: {}",
            slice_name,
            stderr
        ));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let cgroup_relative_path = stdout
        .strip_prefix("ControlGroup=")
        .and_then(|s| s.trim().strip_prefix('/'))
        .ok_or_else(|| {
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Unexpected ControlGroup output format: {}",
                stdout
            )
        })?;

    Ok(cgroup_relative_path.to_owned())
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

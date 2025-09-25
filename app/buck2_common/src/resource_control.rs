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
use tracing::info;

#[cfg(unix)]
use crate::cgroup_pool::CgroupPool;
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
#[derive(PartialEq, Clone, Copy)]
pub enum CgroupDelegation {
    Enabled,
    Disabled,
}

#[derive(PartialEq, Clone)]
pub enum ActionCgroupPoolConfig {
    Disabled,
    Enabled {
        pool_memory_high: Option<String>,
        /// Size of cgroup pool for action processes
        pool_size: Option<u64>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum CgroupMemoryFile {
    MemoryHigh,
    MemoryMax,
}

impl std::fmt::Display for CgroupMemoryFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CgroupMemoryFile::MemoryHigh => write!(f, "memory.high"),
            CgroupMemoryFile::MemoryMax => write!(f, "memory.max"),
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
}

pub struct ResourceControlRunnerConfig {
    /// A config to determine if systemd is available.
    pub status: ResourceControlStatus,
    /// Parent slice behaviour
    pub parent_slice: ParentSlice,
    /// Delegation of further resource control partitioning of cgroup unit
    pub delegation: CgroupDelegation,
    /// Configuration variant
    pub config: ResourceControlRunnerConfigVariant,
}

pub enum ResourceControlRunnerConfigVariant {
    /// Configuration for buck daemon
    BuckDaemon {
        /// A memory threshold for daemon, forkserver and worker processes
        memory_max: Option<String>,
        /// The memory limit before tasks start to be throttled for daemon, forkserver and worker processes
        memory_high: Option<String>,
    },
    /// Configuration for action runner
    ActionRunner {
        /// A memory threshold for each action processes
        memory_max_per_action: Option<String>,
        /// The memory limit before tasks start to be throttled for each action processes
        memory_high_per_action: Option<String>,
        /// Cgroup pool configuration for action processes
        action_cgroup_pool_config: ActionCgroupPoolConfig,
    },
    /// Configuration for forkserver
    Forkserver,
}

impl ResourceControlRunnerConfigVariant {
    pub fn memory_max(&self) -> Option<&str> {
        match self {
            ResourceControlRunnerConfigVariant::BuckDaemon { memory_max, .. } => {
                memory_max.as_deref()
            }
            ResourceControlRunnerConfigVariant::ActionRunner {
                memory_max_per_action,
                ..
            } => memory_max_per_action.as_deref(),
            ResourceControlRunnerConfigVariant::Forkserver => None,
        }
    }

    pub fn memory_high(&self) -> Option<&str> {
        match self {
            ResourceControlRunnerConfigVariant::BuckDaemon { memory_high, .. } => {
                memory_high.as_deref()
            }
            ResourceControlRunnerConfigVariant::ActionRunner {
                memory_high_per_action,
                ..
            } => memory_high_per_action.as_deref(),
            ResourceControlRunnerConfigVariant::Forkserver => None,
        }
    }

    pub fn action_cgroup_pool_config(&self) -> &ActionCgroupPoolConfig {
        match self {
            ResourceControlRunnerConfigVariant::BuckDaemon { .. } => {
                &ActionCgroupPoolConfig::Disabled
            }
            ResourceControlRunnerConfigVariant::ActionRunner {
                action_cgroup_pool_config,
                ..
            } => action_cgroup_pool_config,
            ResourceControlRunnerConfigVariant::Forkserver => &ActionCgroupPoolConfig::Disabled,
        }
    }
}

impl ResourceControlRunnerConfig {
    pub fn daemon_runner_config(config: &ResourceControlConfig, parent_slice: ParentSlice) -> Self {
        Self {
            status: config.status.clone(),
            parent_slice,
            delegation: CgroupDelegation::Disabled,
            config: ResourceControlRunnerConfigVariant::BuckDaemon {
                memory_max: config.memory_max.clone(),
                memory_high: config.memory_high.clone(),
            },
        }
    }

    pub fn action_runner_config(config: &ResourceControlConfig, parent_slice: ParentSlice) -> Self {
        Self {
            status: config.status.clone(),
            parent_slice,
            delegation: CgroupDelegation::Enabled,
            config: ResourceControlRunnerConfigVariant::ActionRunner {
                memory_max_per_action: config.memory_max_per_action.clone(),
                memory_high_per_action: config.memory_high_per_action.clone(),
                action_cgroup_pool_config: if config.enable_action_cgroup_pool.unwrap_or(false) {
                    ActionCgroupPoolConfig::Enabled {
                        pool_memory_high: config.memory_high_action_cgroup_pool.clone(),
                        pool_size: config.cgroup_pool_size,
                    }
                } else {
                    ActionCgroupPoolConfig::Disabled
                },
            },
        }
    }

    pub fn forkserver_config(config: &ResourceControlConfig, parent_slice: ParentSlice) -> Self {
        Self {
            status: config.status.clone(),
            parent_slice,
            delegation: CgroupDelegation::Enabled,
            config: ResourceControlRunnerConfigVariant::Forkserver,
        }
    }
}

pub struct ResourceControlRunner {
    fixed_systemd_args: Vec<String>,
    memory_limit: Option<String>,
    memory_high: Option<String>,
    #[cfg(unix)]
    cgroup_pool: Option<CgroupPool>,
}

impl ResourceControlRunner {
    fn create(
        config_variant: &ResourceControlRunnerConfigVariant,
        parent_slice: &ParentSlice,
        delegation: CgroupDelegation,
    ) -> buck2_error::Result<Self> {
        // Common settings
        let mut args = vec![
            "--user".to_owned(),
            "--scope".to_owned(),
            "--quiet".to_owned(),
            "--collect".to_owned(),
            #[cfg(fbcode_build)]
            "--setenv=CHGDISABLE=1".to_owned(),
        ];

        // Only action runner needs to set MemorySwapMax and MemoryHigh
        match config_variant {
            ResourceControlRunnerConfigVariant::ActionRunner {
                memory_max_per_action,
                memory_high_per_action,
                action_cgroup_pool_config: _,
            } => {
                if let Some(memory_max) = &memory_max_per_action {
                    args.push(format!("--property=MemoryMax={memory_max}"));
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

                if let Some(memory_high) = &memory_high_per_action {
                    args.push(format!("--property=MemoryHigh={memory_high}"));
                }
            }
            _ => {}
        }

        if delegation == CgroupDelegation::Enabled {
            args.push("--property=Delegate=yes".to_owned());
        }

        match &parent_slice {
            ParentSlice::Inherit(slice) => {
                args.push(format!("--slice={slice}"));
                args.push("--slice-inherit".to_owned());
            }
            ParentSlice::Root(slice) => {
                args.push(format!("--slice={slice}"));
            }
        }

        Ok(Self {
            fixed_systemd_args: args,
            memory_limit: config_variant.memory_max().map(|x| x.to_owned()),
            memory_high: config_variant.memory_high().map(|x| x.to_owned()),

            #[cfg(unix)]
            cgroup_pool: match config_variant.action_cgroup_pool_config() {
                ActionCgroupPoolConfig::Enabled {
                    pool_memory_high,
                    pool_size,
                } => {
                    // Use num_cpus to set the capacity of the cgroup pool.
                    use buck2_error::BuckErrorContext;
                    // if cgroup pool size is not set, use the number of available cpus
                    let capacity = pool_size
                        .map(|x| x as usize)
                        .unwrap_or(buck2_util::threads::available_parallelism_fresh());
                    let cgroup_pool = CgroupPool::new(
                        capacity,
                        config_variant.memory_high(),
                        pool_memory_high.as_deref(),
                    )
                    .buck_error_context("Failed to create cgroup pool")?;
                    Some(cgroup_pool)
                }
                ActionCgroupPoolConfig::Disabled => None,
            },
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

    pub fn create_if_enabled(
        config: &ResourceControlRunnerConfig,
    ) -> buck2_error::Result<Option<Self>> {
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
            SystemdCreationDecision::Create => Ok(Some(Self::create(
                &config.config,
                &config.parent_slice,
                config.delegation,
            )?)),
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
    /// Finds the nearest ancestor memory limit by traversing up the cgroup hierarchy.
    ///
    /// This function walks up the cgroup tree starting from the given slice until it finds
    /// a concrete memory limit (non-"max" value) or reaches the root. This is useful for
    /// percentage-based memory calculations that need to know the parent limit.
    async fn find_ancestor_memory_limit(
        slice: &str,
        memory_file_type: CgroupMemoryFile,
    ) -> buck2_error::Result<Option<String>> {
        // Get the cgroup path from systemctl
        let mut cmd = process::async_background_command("systemctl");
        cmd.args(["--user", "show", "-p", "ControlGroup", slice]);

        let output = cmd.output().await?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Failed to get ControlGroup for slice {}: {}",
                slice,
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

        // Construct absolute path
        let cgroup_root = Path::new("/sys/fs/cgroup");
        let mut current_path = cgroup_root.join(cgroup_relative_path);
        // Start from the slice parent
        current_path = match current_path.parent() {
            Some(parent) => parent.to_path_buf(),
            None => return Ok(None), // No parent to traverse from
        };

        // Traverse up the hierarchy
        loop {
            // Try to read the memory file
            match memory_file_type.read_async(&current_path).await {
                Ok(content) => {
                    let trimmed = content.trim();
                    // If we found a non-"max" value, return it
                    if trimmed != "max" {
                        return Ok(Some(trimmed.to_owned()));
                    }
                }
                Err(_) => {
                    // File doesn't exist, continue to parent
                }
            }

            // Move to parent directory
            if let Some(parent) = current_path.parent() {
                // Stop if we've reached the cgroup root's parent
                if parent == cgroup_root.parent().unwrap() {
                    break;
                }
                current_path = parent.to_path_buf();
            } else {
                break;
            }
        }

        Ok(None)
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
                if let Some(parent_memory_restrictions) =
                    ResourceControlRunner::find_ancestor_memory_limit(slice, memory_file_type)
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

    pub async fn ensure_scope_stopped(&self, scope: &str) -> buck2_error::Result<()> {
        let mut cmd = process::async_background_command("systemctl");
        cmd.arg("is-active").arg("--quiet").arg("--user").arg(scope);
        // systemctl returns no error if scope is active
        let is_active = cmd.status().await?.success();
        if is_active {
            info!(
                "Transient scope unit {} is already active. Stopping before start a new one.",
                scope
            );
            self.stop_scope(scope).await?;
        }
        Ok(())
    }

    async fn stop_scope(&self, scope: &str) -> buck2_error::Result<()> {
        let mut cmd = process::async_background_command("systemctl");
        cmd.arg("stop").arg("--user").arg(scope);
        cmd.spawn()?.wait().await?;
        Ok(())
    }

    #[cfg(unix)]
    pub fn cgroup_pool(&self) -> Option<&CgroupPool> {
        self.cgroup_pool.as_ref()
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

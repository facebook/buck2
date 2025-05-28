/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::str::FromStr;
use std::time::Duration;

use allocative::Allocative;
use buck2_core::buck2_env;
use buck2_error::BuckErrorContext;
use serde::Deserialize;
use serde::Serialize;

use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::manifold::BucketsConfig;

pub const DEFAULT_RETAINED_EVENT_LOGS: usize = 12;

/// Helper enum to categorize the kind of timeout we get from the startup config.
#[derive(Clone, Debug)]
pub enum Timeout {
    /// Timeout value is set in the config, use that.
    Value(Duration),
    /// Timeout value was not set in config, apply the default.
    Default,
    /// Timeout value was explicitly set to 0, meaning we shouldn't use a timeout.
    NoTimeout,
}

impl Timeout {
    pub fn new(value: Option<Duration>) -> Self {
        match value {
            Some(Duration::ZERO) => Self::NoTimeout,
            Some(value) => Self::Value(value),
            None => Self::Default,
        }
    }
}

#[derive(
    Allocative,
    Clone,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq
)]
pub struct HttpConfig {
    connect_timeout_ms: Option<u64>,
    read_timeout_ms: Option<u64>,
    write_timeout_ms: Option<u64>,
    pub http2: bool,
    pub max_redirects: Option<usize>,
    pub max_concurrent_requests: Option<usize>,
}

impl HttpConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> buck2_error::Result<Self> {
        let connect_timeout_ms = config.parse(BuckconfigKeyRef {
            section: "http",
            property: "connect_timeout_ms",
        })?;
        let read_timeout_ms = config.parse(BuckconfigKeyRef {
            section: "http",
            property: "read_timeout_ms",
        })?;
        let write_timeout_ms = config.parse(BuckconfigKeyRef {
            section: "http",
            property: "write_timeout_ms",
        })?;
        let max_redirects = config.parse(BuckconfigKeyRef {
            section: "http",
            property: "max_redirects",
        })?;
        let http2 = config
            .parse(BuckconfigKeyRef {
                section: "http",
                property: "http2",
            })?
            .unwrap_or(true);
        let max_concurrent_requests = config.parse(BuckconfigKeyRef {
            section: "http",
            property: "max_concurrent_requests",
        })?;

        Ok(Self {
            connect_timeout_ms,
            read_timeout_ms,
            write_timeout_ms,
            max_redirects,
            http2,
            max_concurrent_requests,
        })
    }

    pub fn connect_timeout(&self) -> Timeout {
        match self.connect_timeout_ms.map(Duration::from_millis) {
            Some(Duration::ZERO) => Timeout::NoTimeout,
            Some(value) => Timeout::Value(value),
            None => Timeout::Default,
        }
    }

    pub fn read_timeout(&self) -> Timeout {
        match self.read_timeout_ms.map(Duration::from_millis) {
            Some(Duration::ZERO) => Timeout::NoTimeout,
            Some(value) => Timeout::Value(value),
            None => Timeout::Default,
        }
    }

    pub fn write_timeout(&self) -> Timeout {
        match self.write_timeout_ms.map(Duration::from_millis) {
            Some(Duration::ZERO) => Timeout::NoTimeout,
            Some(value) => Timeout::Value(value),
            None => Timeout::Default,
        }
    }
}

#[derive(
    Allocative,
    Clone,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq
)]
pub struct SystemWarningConfig {
    /// A threshold that is used to determine the percent of memory buck2 uses to display memory pressure warnings.
    /// If None, we don't warn the user.
    /// The corresponding buckconfig is `buck2_system_warning.memory_pressure_threshold_percent`.
    pub memory_pressure_threshold_percent: Option<u64>,
    /// A threshold that is used to determine remaining disk space buck2 uses to display disk space warnings.
    /// If None, we don't warn the user.
    /// The corresponding buckconfig is `buck2_system_warning.remaining_disk_space_threshold`.
    pub remaining_disk_space_threshold_gb: Option<u64>,
    /// Minimum number of bytes downloaded to measure average download speed.
    /// If None, we don't warn the user.
    /// The corresponding buckconfig is `buck2_system_warning.min_re_download_bytes_threshold`.
    pub min_re_download_bytes_threshold: Option<u64>,
    /// A threshold that is used to determine if download speed is too low and display a warning.
    /// If None, we don't warn the user.
    /// The corresponding buckconfig is `buck2_system_warning.avg_re_download_bytes_per_sec_threshold`.
    pub avg_re_download_bytes_per_sec_threshold: Option<u64>,
    /// A regex that controls which targets are opted into the vpn check.
    /// The corresponding buckconfig is `buck2_health_check.optin_vpn_check_targets_regex`.
    pub optin_vpn_check_targets_regex: Option<String>,
    /// Whether to enable the stable revision check.
    pub enable_stable_revision_check: Option<bool>,
    /// Run the health checks in a separate process.
    pub enable_health_check_process_isolation: Option<bool>,
}

impl SystemWarningConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> buck2_error::Result<Self> {
        let memory_pressure_threshold_percent = config.parse(BuckconfigKeyRef {
            section: "buck2_system_warning",
            property: "memory_pressure_threshold_percent",
        })?;
        let remaining_disk_space_threshold_gb = config.parse(BuckconfigKeyRef {
            section: "buck2_system_warning",
            property: "remaining_disk_space_threshold_gb",
        })?;
        let min_re_download_bytes_threshold = config.parse(BuckconfigKeyRef {
            section: "buck2_system_warning",
            property: "min_re_download_bytes_threshold",
        })?;
        let avg_re_download_bytes_per_sec_threshold = config.parse(BuckconfigKeyRef {
            section: "buck2_system_warning",
            property: "avg_re_download_bytes_per_sec_threshold",
        })?;
        let optin_vpn_check_targets_regex = config.parse(BuckconfigKeyRef {
            section: "buck2_health_check",
            property: "optin_vpn_check_targets_regex",
        })?;
        let enable_stable_revision_check = config.parse(BuckconfigKeyRef {
            section: "buck2_health_check",
            property: "enable_stable_revision_check",
        })?;
        let enable_health_check_process_isolation = config.parse(BuckconfigKeyRef {
            section: "buck2_health_check",
            property: "enable_health_check_process_isolation",
        })?;
        Ok(Self {
            memory_pressure_threshold_percent,
            remaining_disk_space_threshold_gb,
            min_re_download_bytes_threshold,
            avg_re_download_bytes_per_sec_threshold,
            optin_vpn_check_targets_regex,
            enable_stable_revision_check,
            enable_health_check_process_isolation,
        })
    }

    pub fn serialize(&self) -> buck2_error::Result<String> {
        serde_json::to_string(&self).buck_error_context("Error serializing SystemWarningConfig")
    }

    pub fn deserialize(s: &str) -> buck2_error::Result<Self> {
        serde_json::from_str::<Self>(s)
            .buck_error_context("Error deserializing SystemWarningConfig")
    }
}

#[derive(Allocative, Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResourceControlConfig {
    /// A config to determine if the resource control should be activated or not.
    /// The corresponding buckconfig is `buck2_resource_control.status` that can take
    /// one of `{off | if_available | required}`.
    pub status: ResourceControlStatus,
    /// Maximum allowed memory usage for all work buck2 manages.
    ///
    /// Accepts either a number of bytes or a percentage of the available resources.
    ///
    /// The corresponding buckconfig is `buck2_resource_control.memory_max`.
    pub memory_max: Option<String>,
    /// Like `memory_max`, but controls cgroupv2's `memory.high`
    ///
    /// The corresponding buckconfig is `buck2_resource_control.memory_high`.
    pub memory_high: Option<String>,
    /// A memory threshold that any action is allowed to allocate.
    pub memory_max_per_action: Option<String>,
    /// A memory threshold that any action is allowed to reach before being throttled.
    pub memory_high_per_action: Option<String>,
    /// Memory high limit for action cgroup pool. Used when enable_action_cgroup_pool is true.
    /// The corresponding buckconfig is `buck2_resource_control.memory_high_action_cgroup_pool`.
    /// Mainly for testing purpose.
    pub memory_high_action_cgroup_pool: Option<String>,
    /// If provided and above the threshold, the cgroups will enforce this memory pressure and will freeze/kill actions
    /// to stay under this pressure limit. (Currently only used for logging purposes and doesn't actually do the above)
    pub memory_pressure_threshold_percent: u64,
    /// Enable suspension when memory pressure is high.
    pub enable_suspension: bool,
    pub preferred_action_suspend_strategy: ActionSuspendStrategy,
}

impl ResourceControlConfig {
    pub fn testing_default() -> Self {
        Self::from_config(&LegacyBuckConfig::empty()).unwrap()
    }
}

#[derive(Allocative, Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum ActionSuspendStrategy {
    CgroupFreeze,
    KillAndRetry,
}

impl FromStr for ActionSuspendStrategy {
    type Err = buck2_error::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "kill_and_retry" => Ok(Self::KillAndRetry),
            "cgroup_freeze" => Ok(Self::CgroupFreeze),
            _ => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid suspend strategy: `{}`",
                s
            )),
        }
    }
}

#[derive(
    Allocative,
    Clone,
    Copy,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq
)]
pub enum ResourceControlStatus {
    #[default]
    /// The resource is not controlled or limited.
    Off,
    /// The resource is controlled by `systemd` if it's available on the system, otherwise off.
    IfAvailable,
    /// The resource is controlled by `systemd`. If it is not available on the system,
    /// buck2 errors it out and the command returns with an error exit code.
    Required,
}

impl FromStr for ResourceControlStatus {
    type Err = buck2_error::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "off" => Ok(Self::Off),
            "if_available" => Ok(Self::IfAvailable),
            "required" => Ok(Self::Required),
            _ => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid resource control status: `{}`",
                s
            )),
        }
    }
}

/// The current version of the resource control algorithm. Say you have some important change to the
/// algo that fixes a bug. Incrementing this to `N + 1` and setting the
/// `buck2_resource_control.enable_suspension_if_min_algo_version` buckconfig to `N + 1` enables
/// suspension only if your bug fix is actually included in the version of buck in use
const RESOURCE_CONTROL_ALGO_VERSION: u32 = 3;

impl ResourceControlConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> buck2_error::Result<Self> {
        if let Some(env_conf) = buck2_env!(
            "BUCK2_TEST_RESOURCE_CONTROL_CONFIG",
            applicability = testing,
        )? {
            Self::deserialize(env_conf)
        } else {
            let status = config
                .parse(BuckconfigKeyRef {
                    section: "buck2_resource_control",
                    property: "status",
                })?
                .unwrap_or(ResourceControlStatus::Off);
            let memory_max = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "memory_max",
            })?;
            let memory_high = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "memory_high",
            })?;
            let memory_max_per_action = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "memory_max_per_action",
            })?;
            let memory_high_per_action = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "memory_high_per_action",
            })?;
            let memory_high_action_cgroup_pool = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "memory_high_action_cgroup_pool",
            })?;
            let memory_pressure_threshold_percent = config
                .parse(BuckconfigKeyRef {
                    section: "buck2_resource_control",
                    property: "memory_pressure_threshold_percent",
                })?
                .unwrap_or(10);
            let enable_suspension = config.parse(BuckconfigKeyRef {
                section: "buck2_resource_control",
                property: "enable_suspension",
            })?;
            let enable_suspension_if_min_algo_version: Option<u32> =
                config.parse(BuckconfigKeyRef {
                    section: "buck2_resource_control",
                    property: "enable_suspension_if_min_algo_version",
                })?;
            let enable_suspension = enable_suspension.unwrap_or(false)
                || enable_suspension_if_min_algo_version
                    .is_some_and(|min_version| RESOURCE_CONTROL_ALGO_VERSION >= min_version);
            let preferred_action_suspend_strategy = config
                .parse(BuckconfigKeyRef {
                    section: "buck2_resource_control",
                    property: "preferred_action_suspend_strategy",
                })?
                .unwrap_or(ActionSuspendStrategy::KillAndRetry);
            Ok(Self {
                status,
                memory_max,
                memory_high,
                memory_max_per_action,
                memory_high_per_action,
                memory_high_action_cgroup_pool,
                memory_pressure_threshold_percent,
                enable_suspension,
                preferred_action_suspend_strategy,
            })
        }
    }

    pub fn serialize(&self) -> buck2_error::Result<String> {
        serde_json::to_string(&self).buck_error_context("Error serializing ResourceControlConfig")
    }

    pub fn deserialize(s: &str) -> buck2_error::Result<Self> {
        serde_json::from_str::<Self>(s)
            .buck_error_context("Error deserializing ResourceControlConfig")
    }
}

#[derive(Allocative, Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum LogDownloadMethod {
    Manifold,
    Curl(String),
    None,
}

#[derive(
    Allocative,
    Clone,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq
)]
pub struct HealthCheckConfig {
    pub enable_health_checks: bool,
    pub disabled_health_check_names: Option<String>,
}

impl HealthCheckConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> buck2_error::Result<Self> {
        let enable_health_checks = config.parse(BuckconfigKeyRef {
            section: "buck2_health_check",
            property: "enable_health_checks",
        })?;
        let disabled_health_check_names = config.parse(BuckconfigKeyRef {
            section: "buck2_health_check",
            property: "disabled_health_check_names",
        })?;

        Ok(Self {
            // TODO(rajneeshl): When the rollout is successful, change this to default to true.
            enable_health_checks: enable_health_checks.unwrap_or(false),
            disabled_health_check_names,
        })
    }
}

/// Configurations that are used at startup by the daemon. Those are actually read by the client,
/// and passed on to the daemon.
///
/// The fields here are often raw String we get from the buckconfig, the daemon will do
/// deserialization once it receives them. That said, this is not a requirement.
///
/// Backwards compatibility on Serialize / Deserialize is not required: if the client cannot read
/// the DaemonStartupConfig provided by the daemon when it tries to connect, it will reject that
/// daemon and restart (and in fact it will probably not get that far since a version check is done
/// before parsing DaemonStartupConfig).
#[derive(Allocative, Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct DaemonStartupConfig {
    pub num_tokio_workers: Option<usize>,
    pub daemon_buster: Option<String>,
    pub digest_algorithms: Option<String>,
    pub source_digest_algorithm: Option<String>,
    pub paranoid: bool,
    pub materializations: Option<String>,
    pub http: HttpConfig,
    pub resource_control: ResourceControlConfig,
    pub log_download_method: LogDownloadMethod,
    pub buckets_config: Option<BucketsConfig>,
    pub health_check_config: HealthCheckConfig,
    pub retained_event_logs: usize,
}

impl DaemonStartupConfig {
    pub fn new(config: &LegacyBuckConfig) -> buck2_error::Result<Self> {
        // Intepreted client side because we need the value here.

        let log_download_method = {
            // Determine the log download method to use. Only default to
            // manifold in fbcode contexts, or when specifically asked.
            let use_manifold_default = cfg!(fbcode_build);
            let use_manifold = config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "log_use_manifold",
                })?
                .unwrap_or(use_manifold_default);

            if use_manifold {
                Ok(LogDownloadMethod::Manifold)
            } else {
                let log_url = config.get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "log_url",
                });
                if let Some(log_url) = log_url {
                    if log_url.is_empty() {
                        Err(buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "log_url is empty, but log_use_manifold is false"
                        ))
                    } else {
                        Ok(LogDownloadMethod::Curl(log_url.to_owned()))
                    }
                } else {
                    Ok(LogDownloadMethod::None)
                }
            }
        }?;

        Ok(Self {
            num_tokio_workers: config
                .parse(BuckconfigKeyRef {
                    section: "build",
                    property: "num_tokio_workers",
                })
                .unwrap_or(Some(0)),
            daemon_buster: config
                .get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "daemon_buster",
                })
                .map(ToOwned::to_owned),
            digest_algorithms: config
                .get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "digest_algorithms",
                })
                .map(ToOwned::to_owned),
            source_digest_algorithm: config
                .get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "source_digest_algorithm",
                })
                .map(ToOwned::to_owned),
            paranoid: false, // Setup later in ImmediateConfig
            materializations: config
                .get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "materializations",
                })
                .map(ToOwned::to_owned),
            http: HttpConfig::from_config(config)?,
            resource_control: ResourceControlConfig::from_config(config)?,
            buckets_config: BucketsConfig::from_config(config)?,
            log_download_method,
            health_check_config: HealthCheckConfig::from_config(config)?,
            retained_event_logs: config
                .get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "retained_event_logs",
                })
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(DEFAULT_RETAINED_EVENT_LOGS),
        })
    }

    pub fn serialize(&self) -> buck2_error::Result<String> {
        serde_json::to_string(&self).buck_error_context("Error serializing DaemonStartupConfig")
    }

    pub fn deserialize(s: &str) -> buck2_error::Result<Self> {
        serde_json::from_str::<Self>(s)
            .buck_error_context("Error deserializing DaemonStartupConfig")
    }

    pub fn testing_empty() -> Self {
        Self {
            num_tokio_workers: None,
            daemon_buster: None,
            digest_algorithms: None,
            source_digest_algorithm: None,
            paranoid: false,
            materializations: None,
            http: HttpConfig::default(),
            resource_control: ResourceControlConfig::testing_default(),
            log_download_method: if cfg!(fbcode_build) {
                LogDownloadMethod::Manifold
            } else {
                LogDownloadMethod::None
            },
            // TODO(jadel): is this a regression in test vs before?
            buckets_config: None,
            health_check_config: HealthCheckConfig::default(),
            retained_event_logs: DEFAULT_RETAINED_EVENT_LOGS,
        }
    }
}

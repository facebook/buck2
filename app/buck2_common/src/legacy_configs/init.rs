/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use allocative::Allocative;
use anyhow::Context;
use serde::Deserialize;
use serde::Serialize;

use crate::legacy_configs::LegacyBuckConfig;

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
    pub max_redirects: Option<usize>,
}

impl HttpConfig {
    pub fn from_config(config: &LegacyBuckConfig) -> anyhow::Result<Self> {
        let connect_timeout_ms = config.parse("http", "connect_timeout_ms")?;
        let read_timeout_ms = config.parse("http", "read_timeout_ms")?;
        let write_timeout_ms = config.parse("http", "write_timeout_ms")?;
        let max_redirects = config.parse("http", "max_redirects")?;

        Ok(Self {
            connect_timeout_ms,
            read_timeout_ms,
            write_timeout_ms,
            max_redirects,
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
    pub daemon_buster: Option<String>,
    pub digest_algorithms: Option<String>,
    pub source_digest_algorithm: Option<String>,
    pub allow_vpnless: bool,
    pub allow_vpnless_for_logging: bool,
    pub paranoid: bool,
    pub materializations: Option<String>,
    pub http: HttpConfig,
}

impl DaemonStartupConfig {
    pub fn new(config: &LegacyBuckConfig) -> anyhow::Result<Self> {
        // Intepreted client side because we need the value here.
        let allow_vpnless = config.parse("buck2", "allow_vpnless")?.unwrap_or_default();
        let allow_vpnless_for_logging = config
            .parse("buck2", "allow_vpnless_for_logging")?
            .unwrap_or(allow_vpnless);

        Ok(Self {
            daemon_buster: config.get("buck2", "daemon_buster").map(ToOwned::to_owned),
            digest_algorithms: config
                .get("buck2", "digest_algorithms")
                .map(ToOwned::to_owned),
            source_digest_algorithm: config
                .get("buck2", "source_digest_algorithm")
                .map(ToOwned::to_owned),
            allow_vpnless,
            allow_vpnless_for_logging,
            paranoid: false, // Setup later in ImmediateConfig
            materializations: config
                .get("buck2", "materializations")
                .map(ToOwned::to_owned),
            http: HttpConfig::from_config(config)?,
        })
    }

    pub fn serialize(&self) -> anyhow::Result<String> {
        serde_json::to_string(&self).context("Error serializing DaemonStartupConfig")
    }

    pub fn deserialize(s: &str) -> anyhow::Result<Self> {
        serde_json::from_str::<Self>(s).context("Error deserializing DaemonStartupConfig")
    }

    pub fn testing_empty() -> Self {
        Self {
            daemon_buster: None,
            digest_algorithms: None,
            source_digest_algorithm: None,
            allow_vpnless: false,
            allow_vpnless_for_logging: false,
            paranoid: false,
            materializations: None,
            http: HttpConfig::default(),
        }
    }
}

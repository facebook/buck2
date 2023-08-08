/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use serde::Deserialize;
use serde::Serialize;

use crate::legacy_configs::LegacyBuckConfig;

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
    pub use_tonic_rt: Option<String>,
    pub materializations: Option<String>,
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
            use_tonic_rt: config.get("buck2", "use_tonic_rt").map(ToOwned::to_owned),
            materializations: config
                .get("buck2", "materializations")
                .map(ToOwned::to_owned),
        })
    }

    pub fn serialize(&self) -> String {
        // This only contains String, so it'll successfully serialize to JSON.
        serde_json::to_string(&self).unwrap()
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
            use_tonic_rt: None,
            materializations: None,
        }
    }
}

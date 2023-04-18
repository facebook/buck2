/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use allocative::Allocative;
use buck2_common::legacy_configs::LegacyBuckConfig;

static BUCK2_RE_CLIENT_CFG_SECTION: &str = "buck2_re_client";

/// We put functions here that both things need to implement for code that isn't gated behind a
/// fbcode_build or not(fbcode_build)
pub trait RemoteExecutionStaticMetadataImpl: Sized {
    fn from_legacy_config(legacy_config: &LegacyBuckConfig) -> anyhow::Result<Self>;
    fn cas_semaphore_size(&self) -> usize;
}

#[allow(unused)]
mod fbcode {
    use super::*;

    /// Metadata that doesn't change between executions
    #[derive(Clone, Debug, Default, Allocative)]
    pub struct RemoteExecutionStaticMetadata {
        pub cas_address: Option<String>,
        pub cas_connection_count: i32,
        pub action_cache_address: Option<String>,
        pub action_cache_connection_count: i32,
        pub engine_address: Option<String>,
        pub engine_connection_count: i32,

        pub verbose_logging: bool,

        pub use_manifold_rich_client: bool,
        pub use_zippy_rich_client: bool,
        pub use_p2p: bool,

        pub cas_thread_count: i32,

        pub rich_client_channels_per_blob: Option<i32>,
        pub rich_client_attempt_timeout_ms: Option<i32>,
        pub rich_client_retries_count: Option<i32>,
        pub force_enable_deduplicate_find_missing: Option<bool>,

        pub features_config_path: Option<String>,
    }

    impl RemoteExecutionStaticMetadataImpl for RemoteExecutionStaticMetadata {
        fn from_legacy_config(legacy_config: &LegacyBuckConfig) -> anyhow::Result<Self> {
            Ok(Self {
                cas_address: legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_address")?,
                cas_connection_count: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_connection_count")?
                    .unwrap_or(16),
                action_cache_address: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "action_cache_address")?,
                action_cache_connection_count: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "action_cache_connection_count")?
                    .unwrap_or(4),
                engine_address: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "engine_address")?,
                engine_connection_count: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "engine_connection_count")?
                    .unwrap_or(4),
                verbose_logging: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "verbose_logging")?
                    .unwrap_or(false),
                cas_thread_count: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_thread_count")?
                    .unwrap_or(4),
                use_manifold_rich_client: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_manifold_rich_client_new")?
                    .unwrap_or(true),
                use_zippy_rich_client: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_zippy_rich_client")?
                    .unwrap_or(false),
                use_p2p: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_p2p")?
                    .unwrap_or(false),
                rich_client_channels_per_blob: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "rich_client_channels_per_blob")?,
                rich_client_attempt_timeout_ms: legacy_config.parse(
                    BUCK2_RE_CLIENT_CFG_SECTION,
                    "rich_client_attempt_timeout_ms",
                )?,
                rich_client_retries_count: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "rich_client_retries_count")?,
                force_enable_deduplicate_find_missing: legacy_config.parse(
                    BUCK2_RE_CLIENT_CFG_SECTION,
                    "force_enable_deduplicate_find_missing",
                )?,
                features_config_path: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "features_config_path")?,
            })
        }

        fn cas_semaphore_size(&self) -> usize {
            self.cas_connection_count as usize * 30
        }
    }
}

#[allow(unused)]
mod not_fbcode {
    use super::*;

    /// Metadata that doesn't change between executions
    #[derive(Clone, Debug, Default, Allocative)]
    pub struct RemoteExecutionStaticMetadata(pub Buck2OssReConfiguration);

    impl RemoteExecutionStaticMetadataImpl for RemoteExecutionStaticMetadata {
        fn from_legacy_config(legacy_config: &LegacyBuckConfig) -> anyhow::Result<Self> {
            Ok(Self(Buck2OssReConfiguration::from_legacy_config(
                legacy_config,
            )?))
        }

        fn cas_semaphore_size(&self) -> usize {
            // FIXME: make this configurable?
            1024
        }
    }
}

/// A configuration used only in our OSS builds. We still compile this always, which lets us
/// gate less code behind fbcode_build.
#[derive(Clone, Debug, Default, Allocative)]
pub struct Buck2OssReConfiguration {
    pub cas_address: Option<String>,
    pub engine_address: Option<String>,
    pub action_cache_address: Option<String>,
    /// Path to a CA certificates bundle. This must be PEM-encoded. If none is set, a default
    /// bundle will be used.
    ///
    /// This can contain environment variables using shell interpolation syntax (i.e. $VAR). They
    /// will be substituted before using the value.
    pub tls_ca_certs: Option<String>,
    /// Path to a client certificate (and intermediate chain), as well as its associated private
    /// key. This must be PEM-encoded.
    ///
    /// This can contain environment variables using shell interpolation syntax (i.e. $VAR). They
    /// will be substituted before using the value.
    pub tls_client_cert: Option<String>,
    /// HTTP headers to inject in all requests to RE. This is a comma-separated list of `Header:
    /// Value` pairs. Minimal validation of those headers is done here.
    ///
    /// This can contain environment variables using shell interpolation syntax (i.e. $VAR). They
    /// will be substituted before using the value.
    pub http_headers: Vec<HttpHeader>,
}

#[derive(Clone, Debug, Default, Allocative)]
pub struct HttpHeader {
    pub key: String,
    pub value: String,
}

impl FromStr for HttpHeader {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(':');
        match (iter.next(), iter.next(), iter.next()) {
            (Some(key), Some(value), None) => Ok(Self {
                key: key.trim().to_owned(),
                value: value.trim().to_owned(),
            }),
            _ => Err(anyhow::anyhow!(
                "Invalid header (expect exactly one `:`): `{}`",
                s
            )),
        }
    }
}

impl Buck2OssReConfiguration {
    pub fn from_legacy_config(legacy_config: &LegacyBuckConfig) -> anyhow::Result<Self> {
        let tls_ca_certs = legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "tls_ca_certs")?;
        let tls_client_cert =
            legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "tls_client_cert")?;
        let http_headers = legacy_config
            .parse_list(BUCK2_RE_CLIENT_CFG_SECTION, "http_headers")?
            .unwrap_or_default(); // Empty list is as good None.

        // if the set the 'address' field, just use that for everything
        let default_address: Option<String> =
            legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "address")?;

        match default_address {
            None => Ok(Self {
                cas_address: legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_address")?,
                engine_address: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "engine_address")?,
                action_cache_address: legacy_config
                    .parse(BUCK2_RE_CLIENT_CFG_SECTION, "action_cache_address")?,
                tls_ca_certs,
                tls_client_cert,
                http_headers,
            }),
            Some(address) => Ok(Self {
                cas_address: Some(address.clone()),
                engine_address: Some(address.clone()),
                action_cache_address: Some(address.clone()),
                tls_ca_certs,
                tls_client_cert,
                http_headers,
            }),
        }
    }
}

#[cfg(fbcode_build)]
pub use fbcode::RemoteExecutionStaticMetadata;
#[cfg(not(fbcode_build))]
pub use not_fbcode::RemoteExecutionStaticMetadata;

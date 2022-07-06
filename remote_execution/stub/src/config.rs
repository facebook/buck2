/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub enum CASDaemonClientCfg {
    #[allow(non_camel_case_types)]
    embedded_config(EmbeddedCASDaemonClientCfg),
}

impl Default for CASDaemonClientCfg {
    fn default() -> Self {
        Self::embedded_config(Default::default())
    }
}

#[derive(Default)]
pub struct RichClientMode(pub i32);

impl RichClientMode {
    pub const DISABLED: Self = RichClientMode(1i32);
    pub const HYBRID: Self = RichClientMode(2i32);
    pub const ENABLED: Self = RichClientMode(3i32);
}

#[derive(Default)]
pub struct CASRichClientCfg {
    pub attempt_timeout_ms: i32,
    pub disable_p2p: bool,
    pub enable_rich_client: bool,
    pub enable_zippy_rich_client: bool,
    pub get_tree_cache_size: i64,
    pub number_of_parallel_channels: i32,
    pub number_of_retries: i32,
    pub zdb_client_mode: RichClientMode,
}

#[derive(Default)]
pub struct CopyPolicy(pub i32);

impl CopyPolicy {
    pub const FULL_COPY: Self = CopyPolicy(1i32);
}

#[derive(Default)]
pub struct CASClientCacheCfg {
    pub digest_cache_size: i64,
    pub find_missing_cache_size_byte: i64,
}

#[derive(Default)]
pub struct EmbeddedCASDaemonClientCfg {
    pub address: Option<String>,
    pub cache_config: CASClientCacheCfg,
    pub connection_count: i32,
    pub copy_policy: CopyPolicy,
    pub name: String,
    pub rich_client_config: CASRichClientCfg,
    pub thread_count: i32,
    pub writable_outputs: bool,
    pub force_enable_deduplicate_find_missing: Option<bool>,
}

#[derive(Default)]
pub struct GRPCClientCfg {
    pub address: Option<String>,
    pub connection_count: i32,
}

#[derive(Default)]
pub struct ClientCfg {
    pub action_cache_client_config: GRPCClientCfg,
    pub cas_client_config: CASDaemonClientCfg,
    pub execution_client_config: GRPCClientCfg,
    pub quiet_mode: bool,
    pub log_max_file_size: i64,
    pub log_rollup_window_size: i32,
    pub log_file_location: Option<String>,
}

pub fn create_default_config() -> ClientCfg {
    ClientCfg::default()
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::target_cfg::TargetCfgWithUniverseOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "configured-graph-size",
    about = "utility to measure performance of configured graph size computation"
)]
pub struct ConfiguredGraphSizeCommand {
    #[clap(name = "TARGET_PATTERNS", help = "Target patterns to audit")]
    pub patterns: Vec<String>,

    #[clap(long)]
    pub json: bool,

    #[clap(long, help = "whether to compute sketch or not")]
    pub sketch: bool,

    #[clap(flatten)]
    pub target_cfg: TargetCfgWithUniverseOptions,

    #[clap(flatten)]
    pub common_opts: CommonCommandOptions,
}

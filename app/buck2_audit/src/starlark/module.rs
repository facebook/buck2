/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::common::target_cfg::TargetCfgUnusedOptions;
use buck2_client_ctx::common::CommonCommandOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "module",
    about = "Inspect Starlark module by fully qualified import string like foo//bar:baz.bzl"
)]
pub struct StarlarkModuleCommand {
    #[clap(name = "IMPORT_PATH", help = "Module import path")]
    pub import_path: String,

    /// Command doesn't need these flags, but they are used in mode files, so we need to keep them.
    #[clap(flatten)]
    _target_cfg: TargetCfgUnusedOptions,

    #[clap(flatten)]
    pub(crate) common_opts: CommonCommandOptions,
}

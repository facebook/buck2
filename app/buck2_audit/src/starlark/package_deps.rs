/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::common::CommonCommandOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(
    name = "module",
    about = "Inspect Starlark package file all bzl dependencies by package name like foo//bar/baz"
)]
pub struct StarlarkPackageDepsCommand {
    #[clap(name = "PACKAGE", help = "Package")]
    pub package: String,

    #[clap(flatten)]
    pub(crate) common_opts: CommonCommandOptions,
}

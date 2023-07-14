/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::common::CommonCommandOptions;

use crate::AuditSubcommand;

/// Inspect package values.
///
/// Package values is the thing set with `write_package_value`
/// function from `PACKAGE` files.
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "package-values")]
pub struct PackageValuesCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    /// Package names to inspect (like `//foo/bar`, no trailing colon).
    pub packages: Vec<String>,
}

impl AuditSubcommand for PackageValuesCommand {
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}

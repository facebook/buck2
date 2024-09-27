/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::path_arg::PathArg;

use crate::StarlarkClientSubcommand;
use crate::StarlarkCommandCommonOptions;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "starlark-lint", about = "Run the Starlark linter.")]
pub struct StarlarkLintCommand {
    #[clap(flatten)]
    pub common_opts: StarlarkCommandCommonOptions,

    #[clap(value_name = "PATH", required = true)]
    pub paths: Vec<PathArg>,
}

impl StarlarkClientSubcommand for StarlarkLintCommand {
    fn common_opts(&self) -> &StarlarkCommandCommonOptions {
        &self.common_opts
    }
}

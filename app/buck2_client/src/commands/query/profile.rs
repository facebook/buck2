/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::path_arg::PathArg;

use crate::commands::profile::profile_mode_to_profile;
use crate::commands::profile::BuckProfileMode;

/// Starlark profiling options
#[derive(Debug, Clone, clap::Parser)]
#[clap(next_help_heading = "Starlark Profiling Options")]
pub(crate) struct QueryProfileOptions {
    /// Profile target loading.
    ///
    /// When this option is enabled, Buck will profile every `BUCK` file loaded during the query
    /// and merge the results into a single profile.
    /// The command may return cached profile data if `BUCK` files were not invalidated.
    #[clap(long, requires("profile_output"))]
    pub(crate) profile_mode: Option<BuckProfileMode>,

    /// Where to write profile output.
    #[clap(long)]
    pub(crate) profile_output: Option<PathArg>,
}

impl QueryProfileOptions {
    pub(crate) fn profile_mode_proto(&self) -> Option<buck2_cli_proto::ProfileMode> {
        self.profile_mode.map(profile_mode_to_profile)
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::BuckErrorContext as _;

/// Defines common options for options with timeouts
#[derive(Debug, clap::Parser)]
pub struct CommonTimeoutOptions {
    // NOTE: the field below is given a different name from the test runner's
    // `timeout` to avoid confusion between the two parameters.
    /// How long to run the command for. If the timeout is exceeded, Buck2 will
    /// exit as quickly as possible and not run further builds or tests.
    ///
    /// In-flight builds and tests will be cancelled. The test orchestrator, if
    /// there is one, will be allowed to shut down gracefully.
    ///
    /// The exit code for builds will be a user error, for tests it is
    /// controlled by the test orchestrator (which normally should report zero
    /// for this).
    ///
    /// The format is a concatenation of time spans (separated by spaces). Each time span is an
    /// integer number and a suffix.
    ///
    /// Relevant supported suffixes: seconds, second, sec, s, minutes, minute, min, m, hours, hour,
    /// hr, h
    ///
    /// For example: `5m 10s`, `500s`.
    #[clap(long = "overall-timeout")]
    timeout: Option<humantime::Duration>,
}

impl CommonTimeoutOptions {
    pub fn overall_timeout(&self) -> buck2_error::Result<Option<::prost_types::Duration>> {
        self.timeout
            .map(|t| {
                let t: std::time::Duration = t.into();
                t.try_into()
            })
            .transpose()
            .buck_error_context("Invalid `timeout`")
    }
}

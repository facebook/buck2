/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::buck2_env;

/// Are we running in CI?
pub fn is_ci() -> anyhow::Result<bool> {
    // The CI environment variable is consistently set by CI providers.
    //
    // - GitHub Actions: https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables
    // - GitLab CI/CD: https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
    // - CircleCI: https://circleci.com/docs/variables/#built-in-environment-variables
    // - many others
    //
    // Internally, CI should be setting SANDCASTLE env var.
    Ok(
        buck2_env!("SANDCASTLE", applicability = internal)?.is_some()
            || buck2_env!("CI", type = bool, default = false)?,
    )
}

pub fn sandcastle_id() -> anyhow::Result<Option<&'static str>> {
    buck2_env!("SANDCASTLE_ID")
}

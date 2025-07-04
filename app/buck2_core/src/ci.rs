/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::buck2_env;

/// Are we running in CI?
pub fn is_ci() -> buck2_error::Result<bool> {
    // The CI environment variable is consistently set by CI providers.
    //
    // - GitHub Actions: https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables
    // - GitLab CI/CD: https://docs.gitlab.com/ee/ci/variables/predefined_variables.html
    // - CircleCI: https://circleci.com/docs/variables/#built-in-environment-variables
    // - many others
    //
    // Internally, CI should be setting SANDCASTLE env var.
    Ok(buck2_env!("SANDCASTLE", applicability = internal)?.is_some() || buck2_env!("CI", bool)?)
}

/// Returns a list of possible identifiers for the currently running CI job, in `(name, value)` form
///
/// Earlier items in the list are better identifiers
pub fn ci_identifiers()
-> buck2_error::Result<impl Iterator<Item = (&'static str, Option<&'static str>)>> {
    Ok([
        (
            "sandcastle_job_info",
            buck2_env!("SANDCASTLE_JOB_INFO", applicability = internal)?,
        ),
        (
            "skycastle_workflow_run_id",
            buck2_env!("SKYCASTLE_WORKFLOW_RUN_ID", applicability = internal)?,
        ),
        (
            "sandcastle_alias",
            buck2_env!("SANDCASTLE_ALIAS", applicability = internal)?,
        ),
        (
            "skycastle_workflow_alias",
            buck2_env!("SKYCASTLE_WORKFLOW_ALIAS", applicability = internal)?,
        ),
        (
            "sandcastle_type",
            buck2_env!("SANDCASTLE_TYPE", applicability = internal)?,
        ),
    ]
    .into_iter())
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Reclassifies a failed action command as an infra error when its stderr shows the failure came
//! from shared infrastructure: the kernel, the filesystem, RE, dotslash. A match adds
//! `ActionCommandInfraFailure` and the specific tag instead of `ActionCommandFailure`.
//!
//! Only add patterns for shared infrastructure failures that are not practical to tag directly at
//! their source, and only match stable text that infrastructure produces, never text a user's own
//! tool or script could print. Anything a rule author can recognize belongs in the action's
//! Starlark `error_handler` instead.

use buck2_error::ErrorTag;

/// A fragment of infrastructure-produced stderr and the tag a failed command earns for it.
struct InfraErrorPattern {
    /// Matched case-insensitively as a substring of the command's stderr.
    pattern: &'static str,
    tag: ErrorTag,
}

impl InfraErrorPattern {
    fn matches(&self, stderr_lower: &str) -> bool {
        stderr_lower.contains(self.pattern)
    }
}

const INFRA_PATTERNS: &[InfraErrorPattern] = &[
    InfraErrorPattern {
        pattern: "transport endpoint is not connected",
        tag: ErrorTag::IoNotConnected,
    },
    InfraErrorPattern {
        pattern: "out of memory",
        tag: ErrorTag::ActionOom,
    },
    InfraErrorPattern {
        pattern: "input/output error",
        tag: ErrorTag::IoInputOutputError,
    },
];

/// Tags for an action whose command exited non-zero. If the command's stderr matches an infra
/// pattern this is `ActionCommandInfraFailure` plus the specific tag, which sets the error's tier;
/// otherwise it is `ActionCommandFailure`, which marks the error as an input (user) error.
pub(crate) fn command_failure_tags(
    last_command: Option<&buck2_data::CommandExecution>,
) -> Vec<ErrorTag> {
    match check_infra_error_patterns(last_command) {
        Some(infra_tag) => vec![ErrorTag::ActionCommandInfraFailure, infra_tag],
        None => vec![ErrorTag::ActionCommandFailure],
    }
}

fn check_infra_error_patterns(
    last_command: Option<&buck2_data::CommandExecution>,
) -> Option<ErrorTag> {
    let stderr = last_command
        .and_then(|c| c.details.as_ref())
        .map_or("", |d| d.cmd_stderr.as_str());

    let stderr_lower = stderr.to_lowercase();
    INFRA_PATTERNS
        .iter()
        .find(|p| p.matches(&stderr_lower))
        .map(|p| p.tag)
}

#[cfg(test)]
mod tests {
    use buck2_error::ErrorTag;

    use crate::actions::errors::infra_error_handler::check_infra_error_patterns;
    use crate::actions::errors::infra_error_handler::command_failure_tags;

    #[test]
    fn test_command_failure_tags() {
        assert_eq!(
            command_failure_tags(Some(&command_with_stderr("fatal error: Out of memory"))),
            vec![ErrorTag::ActionCommandInfraFailure, ErrorTag::ActionOom],
        );
        assert_eq!(
            command_failure_tags(Some(&command_with_stderr(
                "main.cpp:1:1: error: expected `;`"
            ))),
            vec![ErrorTag::ActionCommandFailure],
        );
    }

    fn command_with_stderr(stderr: &str) -> buck2_data::CommandExecution {
        buck2_data::CommandExecution {
            details: Some(buck2_data::CommandExecutionDetails {
                cmd_stderr: stderr.to_owned(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    #[test]
    fn test_host_read_fault_is_infra() {
        let stderr = "Exception in thread \"main\" java.lang.RuntimeException: zipalign failed to process apk file:\n\
             W zip     : fread 32768 bytes failed, Input/output error\n\
             W zip     : copy of 'resources.arsc' failed\n";

        assert_eq!(
            check_infra_error_patterns(Some(&command_with_stderr(stderr))),
            Some(ErrorTag::IoInputOutputError),
            "EIO raised inside the action should be tagged as an environment error"
        );
    }

    #[test]
    fn test_malformed_input_is_not_infra() {
        let stderr = "Exception in thread \"main\" java.lang.RuntimeException: zipalign failed to process apk file:\n\
             Unable to open 'intermediate.apk' as zip archive\n";

        assert_eq!(
            check_infra_error_patterns(Some(&command_with_stderr(stderr))),
            None,
            "A malformed input is a user error, not an infra failure"
        );
    }

    #[test]
    fn test_existing_patterns_still_match() {
        assert_eq!(
            check_infra_error_patterns(Some(&command_with_stderr(
                "error: Transport endpoint is not connected"
            ))),
            Some(ErrorTag::IoNotConnected),
        );
        assert_eq!(
            check_infra_error_patterns(Some(&command_with_stderr("fatal error: Out of memory"))),
            Some(ErrorTag::ActionOom),
        );
    }

    #[test]
    fn test_compile_failure_is_not_infra() {
        assert_eq!(
            check_infra_error_patterns(Some(&command_with_stderr(
                "main.cpp:12:5: error: use of undeclared identifier `foo`"
            ))),
            None,
            "A compile error should stay attributed to the user"
        );
    }

    #[test]
    fn test_no_command_is_not_infra() {
        assert_eq!(check_infra_error_patterns(None), None);
    }
}

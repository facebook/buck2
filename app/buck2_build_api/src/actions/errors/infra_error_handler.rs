/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_error::ErrorTag;

pub(crate) fn check_infra_error_patterns(
    last_command: Option<&buck2_data::CommandExecution>,
) -> Option<ErrorTag> {
    let stderr = last_command
        .and_then(|c| c.details.as_ref())
        .map_or("", |d| d.cmd_stderr.as_str());

    const INFRA_PATTERNS: &[(&str, ErrorTag)] = &[
        (
            "transport endpoint is not connected",
            ErrorTag::IoNotConnected,
        ),
        ("out of memory", ErrorTag::ActionOom),
        ("input/output error", ErrorTag::IoInputOutputError),
    ];

    let stderr_lower = stderr.to_lowercase();
    INFRA_PATTERNS
        .iter()
        .find(|(pattern, _)| stderr_lower.contains(pattern))
        .map(|(_, tag)| *tag)
}

#[cfg(test)]
mod tests {
    use buck2_error::ErrorTag;

    use crate::actions::errors::infra_error_handler::check_infra_error_patterns;

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

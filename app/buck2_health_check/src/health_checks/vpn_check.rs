/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // The code here will be used in future diffs.

use regex::Regex;

use crate::health_check_context::HealthCheckContext;

pub(crate) fn can_run(health_check_context: &HealthCheckContext) -> bool {
    if let Some(optin_target_regex) = &health_check_context
        .experiment_configurations
        .optin_vpn_check_targets_regex
    {
        if let Ok(optin_pattern) = Regex::new(optin_target_regex) {
            return health_check_context
                .parsed_target_patterns
                .as_ref()
                .is_some_and(|f| {
                    f.target_patterns
                        .iter()
                        .any(|pattern| optin_pattern.is_match(&pattern.value))
                });
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    const MATCHING_TARGET: &str = "//foo/bar:baz";
    const MATCHING_REGEX: &str = "bar";

    fn health_check_context(target: Option<String>, regex: Option<String>) -> HealthCheckContext {
        HealthCheckContext {
            experiment_configurations: buck2_data::SystemInfo {
                optin_vpn_check_targets_regex: regex,
                ..Default::default()
            },
            parsed_target_patterns: target.map(|t| buck2_data::ParsedTargetPatterns {
                target_patterns: vec![buck2_data::TargetPattern {
                    value: t.to_owned(),
                }],
            }),
            ..Default::default()
        }
    }

    #[test]
    fn test_can_run_with_matching_target() {
        assert!(can_run(&health_check_context(
            Some(MATCHING_TARGET.to_owned()),
            Some(MATCHING_REGEX.to_owned())
        )));
    }

    #[test]
    fn test_can_run_with_no_target_patterns() {
        assert!(!can_run(&health_check_context(
            None,
            Some(MATCHING_REGEX.to_owned())
        )));
    }

    #[test]
    fn test_can_run_with_no_optin_target_regex() {
        assert!(!can_run(&health_check_context(
            Some(MATCHING_TARGET.to_owned()),
            None
        )));
    }

    #[test]
    fn test_can_run_with_no_matching_target() {
        assert!(!can_run(&health_check_context(
            Some(MATCHING_TARGET.to_owned()),
            Some("buck".to_owned())
        )));
    }

    #[test]
    fn test_regex_matching_multiple_targets() {
        assert!(can_run(&health_check_context(
            Some(MATCHING_TARGET.to_owned()),
            Some("(buck|bar)".to_owned())
        )));
        assert!(can_run(&health_check_context(
            Some("//foo/buck:baz".to_owned()),
            Some("(buck|bar)".to_owned())
        )));
    }
}

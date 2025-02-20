/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // The code here will be used in future diffs.

use buck2_core::soft_error;
use regex::Regex;

use crate::health_check_context::HealthCheckContext;

/// Check if the user is on VPN.
pub(crate) struct VpnCheck {
    // TODO(rajneeshl): When the server is ready, this cache will need to be maintained per command/traceid.
    can_run: Option<bool>,
}

impl VpnCheck {
    pub(crate) fn new() -> Self {
        Self { can_run: None }
    }

    pub(crate) fn can_run(&self) -> bool {
        self.can_run.unwrap_or(false)
    }

    pub fn try_update_can_run(&mut self, context: &HealthCheckContext) {
        if self.can_run.is_some() {
            return;
        }
        let (Some(configs), Some(parsed_target_patterns)) = (
            &context.experiment_configurations,
            &context.parsed_target_patterns,
        ) else {
            return;
        };
        let Some(optin_target_regex) = &configs.optin_vpn_check_targets_regex else {
            return;
        };

        self.can_run = match Regex::new(optin_target_regex) {
            Ok(regex) => Some(
                parsed_target_patterns
                    .target_patterns
                    .iter()
                    .any(|pattern| regex.is_match(&pattern.value)),
            ),
            Err(e) => {
                let _unused = soft_error!(
                    "vpn_check_regex_error",
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "invalid optin target pattern regex: {}, error: {:#}",
                        optin_target_regex,
                        e
                    )
                    .into()
                );
                Some(false)
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MATCHING_TARGET: &str = "//foo/bar:baz";
    const MATCHING_REGEX: &str = "bar";

    fn health_check_context(target: Option<String>, regex: Option<String>) -> HealthCheckContext {
        HealthCheckContext {
            experiment_configurations: Some(buck2_data::SystemInfo {
                optin_vpn_check_targets_regex: regex,
                ..Default::default()
            }),
            parsed_target_patterns: target.map(|t| buck2_data::ParsedTargetPatterns {
                target_patterns: vec![buck2_data::TargetPattern {
                    value: t.to_owned(),
                }],
            }),
            ..Default::default()
        }
    }

    fn can_run_check(target: Option<String>, regex: Option<String>) -> bool {
        let mut vpn_check = VpnCheck::new();
        let health_check_context = health_check_context(target, regex);
        vpn_check.try_update_can_run(&health_check_context);
        vpn_check.can_run()
    }

    #[test]
    fn test_can_run_with_matching_target() {
        assert!(can_run_check(
            Some(MATCHING_TARGET.to_owned()),
            Some(MATCHING_REGEX.to_owned())
        ));
    }

    #[test]
    fn test_can_run_with_no_target_patterns() {
        assert!(!can_run_check(None, Some(MATCHING_REGEX.to_owned())));
    }

    #[test]
    fn test_can_run_with_no_optin_target_regex() {
        assert!(!can_run_check(Some(MATCHING_TARGET.to_owned()), None));
    }

    #[test]
    fn test_can_run_with_no_matching_target() {
        assert!(!can_run_check(
            Some(MATCHING_TARGET.to_owned()),
            Some("buck".to_owned())
        ));
    }

    #[test]
    fn test_regex_matching_multiple_targets() {
        assert!(can_run_check(
            Some(MATCHING_TARGET.to_owned()),
            Some("(buck|bar)".to_owned())
        ));
        assert!(can_run_check(
            Some("//foo/buck:baz".to_owned()),
            Some("(buck|bar)".to_owned())
        ));
    }
}

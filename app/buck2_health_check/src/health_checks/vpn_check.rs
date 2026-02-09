/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)] // The code here will be used in future diffs.

use buck2_core::soft_error;
use regex::Regex;

use crate::interface::HealthCheck;
use crate::interface::HealthCheckContext;
use crate::interface::HealthCheckSnapshotData;
use crate::report::DisplayReport;
use crate::report::HealthIssue;
use crate::report::Message;
use crate::report::Remediation;
use crate::report::Report;
use crate::report::Severity;

pub const REMEDIATION_LINK: &str = "https://fburl.com/buck2_vpn_enabled";
pub const TAG: &str = "vpn_enabled";

/// Check if the user is on VPN.
pub(crate) struct VpnCheck {
    // TODO(rajneeshl): When the server is ready, this cache will need to be maintained per command/traceid.
    can_run: Option<bool>,
}

impl VpnCheck {
    pub(crate) fn new() -> Self {
        Self { can_run: None }
    }

    pub(crate) fn run(&self) -> Option<Report> {
        let is_vpn_enabled = Self::cisco_iface_connected().unwrap_or(false);

        Some(Report {
            display_report: self.generate_display_report(is_vpn_enabled),
            tag: is_vpn_enabled.then(|| TAG.to_owned()),
        })
    }

    pub(crate) fn can_run(&self) -> bool {
        self.can_run.unwrap_or(false)
    }

    pub(crate) fn try_update_can_run(&mut self, context: &HealthCheckContext) {
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
                );
                Some(false)
            }
        };
    }

    fn generate_display_report(&self, is_vpn_enabled: bool) -> Option<DisplayReport> {
        self.can_run().then(|| DisplayReport {
            health_check_type: crate::interface::HealthCheckType::VpnEnabled,
            health_issue: self.generate_warning(is_vpn_enabled),
        })
    }

    fn generate_warning(&self, is_vpn_enabled: bool) -> Option<HealthIssue> {
        is_vpn_enabled.then(|| HealthIssue {
            severity: Severity::Warning,
            message: Message::Simple(
                "For optimal build speed, consider disconnecting from VPN".to_owned(),
            ),
            remediation: Some(Remediation::Link(REMEDIATION_LINK.to_owned())),
        })
    }

    #[cfg(any(target_os = "macos", target_os = "linux"))]
    fn cisco_iface_connected() -> buck2_error::Result<bool> {
        // Brittle check based on Cisco client's current behaviour.
        // Small section copied from https://fburl.com/code/g7ttsdz3
        Ok(std::path::Path::new("/opt/cisco/secureclient/vpn/ac_pf.token").exists())
    }

    #[cfg(target_os = "windows")]
    fn cisco_iface_connected() -> buck2_error::Result<bool> {
        let table = buck2_util::os::win::network_interface_table::NetworkInterfaceTable::new()?;

        for interface in table {
            if interface
                .description()
                .contains("Cisco AnyConnect Virtual Miniport Adapter")
            {
                // This is a hack for VPN check: The presence of a connected interface with the Cisco name.
                return Ok(interface.is_connected());
            }
        }
        Ok(false)
    }
}

#[async_trait::async_trait]
impl HealthCheck for VpnCheck {
    fn run_check(
        &mut self,
        _snapshot: HealthCheckSnapshotData,
    ) -> buck2_error::Result<Option<Report>> {
        Ok(self.run())
    }

    async fn handle_context_update(&mut self, context: &HealthCheckContext) {
        self.try_update_can_run(context)
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

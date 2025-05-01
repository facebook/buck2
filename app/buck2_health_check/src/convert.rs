/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// gRPC to rust converters

use buck2_error::BuckErrorContext;

use crate::interface::HealthCheckContextEvent;
use crate::interface::HealthCheckType;
use crate::report::DisplayReport;
use crate::report::HealthIssue;
use crate::report::Remediation;
use crate::report::Report;
use crate::report::Severity;

impl TryFrom<i32> for Severity {
    type Error = buck2_error::Error;
    fn try_from(s: i32) -> buck2_error::Result<Self> {
        let severity = buck2_health_check_proto::Severity::try_from(s)
            .buck_error_context("Invalid `severity`")?;
        Ok(match severity {
            buck2_health_check_proto::Severity::Info => Severity::Info,
            buck2_health_check_proto::Severity::Warning => Severity::Warning,
        })
    }
}

impl TryInto<i32> for Severity {
    type Error = buck2_error::Error;
    fn try_into(self) -> buck2_error::Result<i32> {
        Ok(match self {
            Severity::Info => buck2_health_check_proto::Severity::Info,
            Severity::Warning => buck2_health_check_proto::Severity::Warning,
        } as i32)
    }
}

impl TryFrom<buck2_health_check_proto::Remediation> for Remediation {
    type Error = buck2_error::Error;

    fn try_from(value: buck2_health_check_proto::Remediation) -> buck2_error::Result<Self> {
        Ok(
            match value.data.buck_error_context("Invalid `remediation`")? {
                buck2_health_check_proto::remediation::Data::Message(message) => {
                    Remediation::Message(message)
                }
                buck2_health_check_proto::remediation::Data::Link(link) => Remediation::Link(link),
            },
        )
    }
}

impl TryInto<buck2_health_check_proto::Remediation> for Remediation {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<buck2_health_check_proto::Remediation> {
        let value = match self {
            Remediation::Message(message) => {
                buck2_health_check_proto::remediation::Data::Message(message)
            }
            Remediation::Link(link) => buck2_health_check_proto::remediation::Data::Link(link),
        };
        Ok(buck2_health_check_proto::Remediation { data: Some(value) })
    }
}

impl TryFrom<i32> for HealthCheckType {
    type Error = buck2_error::Error;

    fn try_from(value: i32) -> buck2_error::Result<Self> {
        let value = buck2_health_check_proto::HealthCheckType::try_from(value)
            .buck_error_context("Invalid `health_check_type`")?;
        Ok(match value {
            buck2_health_check_proto::HealthCheckType::MemoryPressure => {
                HealthCheckType::MemoryPressure
            }
            buck2_health_check_proto::HealthCheckType::LowDiskSpace => {
                HealthCheckType::LowDiskSpace
            }
            buck2_health_check_proto::HealthCheckType::SlowDownloadSpeed => {
                HealthCheckType::SlowDownloadSpeed
            }
            buck2_health_check_proto::HealthCheckType::VpnEnabled => HealthCheckType::VpnEnabled,
            buck2_health_check_proto::HealthCheckType::StableRevision => {
                HealthCheckType::StableRevision
            }
        })
    }
}

impl TryInto<i32> for HealthCheckType {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<i32> {
        Ok(match self {
            HealthCheckType::MemoryPressure => {
                buck2_health_check_proto::HealthCheckType::MemoryPressure
            }
            HealthCheckType::LowDiskSpace => {
                buck2_health_check_proto::HealthCheckType::LowDiskSpace
            }
            HealthCheckType::SlowDownloadSpeed => {
                buck2_health_check_proto::HealthCheckType::SlowDownloadSpeed
            }
            HealthCheckType::VpnEnabled => buck2_health_check_proto::HealthCheckType::VpnEnabled,
            HealthCheckType::StableRevision => {
                buck2_health_check_proto::HealthCheckType::StableRevision
            }
        } as i32)
    }
}

impl TryFrom<buck2_health_check_proto::HealthIssue> for HealthIssue {
    type Error = buck2_error::Error;

    fn try_from(value: buck2_health_check_proto::HealthIssue) -> buck2_error::Result<Self> {
        Ok(HealthIssue {
            severity: value.severity.try_into()?,
            message: value.message,
            remediation: value.remediation.map(|r| r.try_into()).transpose()?,
        })
    }
}

impl TryInto<buck2_health_check_proto::HealthIssue> for HealthIssue {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<buck2_health_check_proto::HealthIssue> {
        Ok(buck2_health_check_proto::HealthIssue {
            severity: self.severity.try_into()?,
            message: self.message,
            remediation: self.remediation.map(|r| r.try_into()).transpose()?,
        })
    }
}

impl TryFrom<buck2_health_check_proto::DisplayReport> for DisplayReport {
    type Error = buck2_error::Error;

    fn try_from(value: buck2_health_check_proto::DisplayReport) -> buck2_error::Result<Self> {
        Ok(DisplayReport {
            health_check_type: value.health_check_type.try_into()?,
            health_issue: value.health_issue.map(|i| i.try_into()).transpose()?,
        })
    }
}
impl TryInto<buck2_health_check_proto::DisplayReport> for DisplayReport {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<buck2_health_check_proto::DisplayReport> {
        Ok(buck2_health_check_proto::DisplayReport {
            health_check_type: self.health_check_type.try_into()?,
            health_issue: self.health_issue.map(|i| i.try_into()).transpose()?,
        })
    }
}

impl TryFrom<buck2_health_check_proto::Report> for Report {
    type Error = buck2_error::Error;

    fn try_from(value: buck2_health_check_proto::Report) -> buck2_error::Result<Self> {
        Ok(Report {
            display_report: value.display_report.map(|d| d.try_into()).transpose()?,
            tag: value.tag,
        })
    }
}

impl TryInto<buck2_health_check_proto::Report> for Report {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<buck2_health_check_proto::Report> {
        Ok(buck2_health_check_proto::Report {
            display_report: self.display_report.map(|d| d.try_into()).transpose()?,
            tag: self.tag,
        })
    }
}

impl TryInto<buck2_health_check_proto::HealthCheckContextEvent> for HealthCheckContextEvent {
    type Error = buck2_error::Error;

    fn try_into(self) -> buck2_error::Result<buck2_health_check_proto::HealthCheckContextEvent> {
        Ok(match self {
            HealthCheckContextEvent::BranchedFromRevision(rev) => {
                buck2_health_check_proto::HealthCheckContextEvent {
                    data: Some(buck2_health_check_proto::health_check_context_event::Data::BranchedFromRevision(rev)),
                }
            }
            HealthCheckContextEvent::CommandStart(cmd) => {
                buck2_health_check_proto::HealthCheckContextEvent {
                    data: Some(buck2_health_check_proto::health_check_context_event::Data::CommandStart(cmd.clone())),
                }
            }
            HealthCheckContextEvent::ParsedTargetPatterns(patterns) => {
                buck2_health_check_proto::HealthCheckContextEvent {
                    data: Some(buck2_health_check_proto::health_check_context_event::Data::ParsedTargetPatterns(patterns.clone())),
                }
            }
            HealthCheckContextEvent::HasExcessCacheMisses() => {
                buck2_health_check_proto::HealthCheckContextEvent {
                    data: Some(buck2_health_check_proto::health_check_context_event::Data::HasExcessCacheMisses(true)),
                }
            }
            HealthCheckContextEvent::ExperimentConfigurations(system_info) => {
                buck2_health_check_proto::HealthCheckContextEvent {
                    data: Some(buck2_health_check_proto::health_check_context_event::Data::ExperimentConfigurations(system_info.clone())),
                }
            }
        })
    }
}

impl TryFrom<buck2_health_check_proto::HealthCheckContextEvent> for HealthCheckContextEvent {
    type Error = buck2_error::Error;
    fn try_from(
        value: buck2_health_check_proto::HealthCheckContextEvent,
    ) -> buck2_error::Result<Self> {
        Ok( match value.data.buck_error_context("Invalid `health_check_context_event`")? {
            buck2_health_check_proto::health_check_context_event::Data::BranchedFromRevision(rev) => {
                HealthCheckContextEvent::BranchedFromRevision(rev)
            }
            buck2_health_check_proto::health_check_context_event::Data::CommandStart(cmd) => {
                HealthCheckContextEvent::CommandStart(cmd)
            }
            buck2_health_check_proto::health_check_context_event::Data::ParsedTargetPatterns(patterns) => {
                HealthCheckContextEvent::ParsedTargetPatterns(patterns)
            }
            buck2_health_check_proto::health_check_context_event::Data::HasExcessCacheMisses(_) => {
                HealthCheckContextEvent::HasExcessCacheMisses()
            }
            buck2_health_check_proto::health_check_context_event::Data::ExperimentConfigurations(system_info) => {
                HealthCheckContextEvent::ExperimentConfigurations(system_info)
            }
        }
    )
    }
}

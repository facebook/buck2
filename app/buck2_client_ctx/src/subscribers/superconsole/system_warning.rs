/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::conversion::from_any_with_tag;
use buck2_health_check::health_check_client::HealthCheckClient;
use buck2_health_check::report::DisplayReport;
use crossterm::style::Color;
use crossterm::style::Stylize;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;

use crate::subscribers::system_warning::check_memory_pressure_snapshot;
use crate::subscribers::system_warning::check_remaining_disk_space_snapshot;
use crate::subscribers::system_warning::is_vpn_enabled;
use crate::subscribers::system_warning::low_disk_space_msg;
use crate::subscribers::system_warning::system_memory_exceeded_msg;
use crate::subscribers::system_warning::vpn_enabled_msg;

/// This component is used to display system warnings for a command e.g. memory pressure, low disk space etc.
pub(crate) struct SystemWarningComponent<'a> {
    pub(crate) last_snapshot: Option<&'a buck2_data::Snapshot>,
    pub(crate) system_info: &'a buck2_data::SystemInfo,
    // TODO(rajneeshl) : Deprecate this reference of health_check_client and use the reports instead.
    pub(crate) health_check_client: Option<&'a HealthCheckClient>,
    pub(crate) health_check_reports: Option<&'a Vec<DisplayReport>>,
}

fn warning_styled(text: &str) -> buck2_error::Result<Line> {
    // cross term doesn't directly define orange as a color
    let orange = Color::Rgb {
        r: (244),
        g: (140),
        b: (40),
    };
    Ok(Line::from_iter([Span::new_styled(
        text.to_owned().with(orange),
    )
    .map_err(|e| {
        from_any_with_tag(e, buck2_error::ErrorTag::Tier0)
    })?]))
}

impl<'a> Component for SystemWarningComponent<'a> {
    fn draw_unchecked(&self, _dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
        let mut lines = Vec::new();

        if let Some(memory_pressure) =
            check_memory_pressure_snapshot(self.last_snapshot, self.system_info)
        {
            lines.push(warning_styled(&system_memory_exceeded_msg(
                &memory_pressure,
            ))?);
        }
        if let Some(low_disk_space) =
            check_remaining_disk_space_snapshot(self.last_snapshot, self.system_info)
        {
            lines.push(warning_styled(&low_disk_space_msg(&low_disk_space))?);
        }
        if let Some(health_check_client) = self.health_check_client {
            if health_check_client.is_vpn_check_enabled() && is_vpn_enabled() {
                lines.push(warning_styled(&vpn_enabled_msg())?);
            }
        }
        if let Some(reports) = self.health_check_reports {
            for report in reports {
                if let Some(warning) = &report.warning {
                    lines.push(warning_styled(&warning.to_string())?);
                }
            }
        }
        Ok(Lines(lines))
    }
}

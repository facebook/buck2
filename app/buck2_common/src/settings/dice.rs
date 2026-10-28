/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;

use crate::settings::DaemonBuckSettings;

/// Reads the daemon's Buck settings from DICE global data. Unlike buckconfig
/// reads this is a plain lookup, not a tracked computation.
/// Only daemon-scoped settings may be consumed from this startup snapshot.
pub trait HasBuckSettings {
    /// The settings the daemon started with; defaults when none were installed
    /// (in-process tests).
    fn get_buck_settings(&self) -> DaemonBuckSettings;
}

pub trait SetBuckSettings {
    fn set_buck_settings(&mut self, settings: DaemonBuckSettings);
}

impl HasBuckSettings for DiceData {
    fn get_buck_settings(&self) -> DaemonBuckSettings {
        match self.get::<DaemonBuckSettings>() {
            Ok(settings) => settings.dupe(),
            Err(_) => DaemonBuckSettings::default(),
        }
    }
}

impl SetBuckSettings for DiceDataBuilder {
    fn set_buck_settings(&mut self, settings: DaemonBuckSettings) {
        self.set(settings)
    }
}

impl SetBuckSettings for DiceData {
    fn set_buck_settings(&mut self, settings: DaemonBuckSettings) {
        self.set(settings)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::BuckSettings;

    #[test]
    fn test_dice_exposes_only_daemon_settings() -> buck2_error::Result<()> {
        let mut data = DiceData::new();
        assert!(
            data.get_buck_settings()
                .analysis
                .record_requested_anon_targets()
        );
        let settings: BuckSettings = serde_json::from_str(
            r#"{"analysis":{"record_requested_anon_targets":false},"agent_materialization":{"enabled":true}}"#,
        )?;
        data.set_buck_settings(settings.daemon);
        assert!(
            !data
                .get_buck_settings()
                .analysis
                .record_requested_anon_targets()
        );
        assert!(data.get::<BuckSettings>().is_err());
        Ok(())
    }
}

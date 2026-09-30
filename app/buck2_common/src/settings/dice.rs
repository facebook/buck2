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

use crate::settings::BuckSettings;

/// Reads the daemon's Buck settings from DICE global data. Unlike buckconfig
/// reads this is a plain lookup, not a tracked computation.
pub trait HasBuckSettings {
    /// The settings the daemon started with; defaults when none were installed
    /// (in-process tests).
    fn get_buck_settings(&self) -> BuckSettings;
}

pub trait SetBuckSettings {
    fn set_buck_settings(&mut self, settings: BuckSettings);
}

impl HasBuckSettings for DiceData {
    fn get_buck_settings(&self) -> BuckSettings {
        match self.get::<BuckSettings>() {
            Ok(settings) => settings.dupe(),
            Err(_) => BuckSettings::default(),
        }
    }
}

impl SetBuckSettings for DiceDataBuilder {
    fn set_buck_settings(&mut self, settings: BuckSettings) {
        self.set(settings)
    }
}

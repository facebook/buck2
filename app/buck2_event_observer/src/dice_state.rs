/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;

use buck2_data::DiceKeyState;
use buck2_data::DiceStateSnapshot;

pub struct DiceState {
    key_states: BTreeMap<String, DiceKeyState>,
}

impl DiceState {
    pub fn new() -> Self {
        Self {
            key_states: BTreeMap::new(),
        }
    }

    pub fn update(&mut self, update: &DiceStateSnapshot) {
        for (k, v) in &update.key_states {
            self.key_states.insert(k.clone(), *v);
        }
    }

    pub fn key_states(&self) -> &BTreeMap<String, DiceKeyState> {
        &self.key_states
    }
}

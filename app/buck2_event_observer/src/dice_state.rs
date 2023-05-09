/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
            self.key_states.insert(k.clone(), v.clone());
        }
    }

    pub fn key_states(&self) -> &BTreeMap<String, DiceKeyState> {
        &self.key_states
    }
}

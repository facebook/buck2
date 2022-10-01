/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::Dice;

/// Dice metrics.
#[derive(Debug)]
pub struct Metrics {
    pub key_count: usize,
    pub currently_running_key_count: usize,
}

impl Metrics {
    pub fn collect(dice: &Dice) -> Metrics {
        let dice_map = dice.map.read().unwrap();
        Metrics {
            key_count: dice_map.key_count(),
            currently_running_key_count: dice_map.currently_running_key_count(),
        }
    }
}

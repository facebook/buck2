/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::str::FromStr;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::VariantName;

/// which dice impl to use
#[derive(Clone, Dupe, Copy, Debug, VariantName, Allocative, PartialEq)]
pub enum WhichDice {
    Legacy,
    Modern,
}

impl FromStr for WhichDice {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "MODERN" => Ok(WhichDice::Modern),
            "LEGACY" => Ok(WhichDice::Legacy),
            _ => Err(format!("Invalid type of WhichDice: `{s}`")),
        }
    }
}

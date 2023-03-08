/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::VariantName;
use thiserror::Error;

/// which dice impl to use
#[derive(Clone, Dupe, Copy, Debug, VariantName, Allocative)]
pub enum WhichDice {
    Legacy,
    Modern,
}

#[derive(Error, Debug)]
#[error("Invalid type of WhichDice: `{0}`")]
pub struct InvalidType(String);

impl FromStr for WhichDice {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "MODERN" => Ok(WhichDice::Modern),
            "LEGACY" => Ok(WhichDice::Legacy),
            _ => Err(InvalidType(s.to_owned())),
        }
    }
}

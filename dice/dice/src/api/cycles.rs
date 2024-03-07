/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cycle detection in DICE

use std::fmt::Debug;
use std::str::FromStr;

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::VariantName;
use thiserror::Error;

#[derive(Clone, Dupe, Copy, Debug, VariantName, Allocative)]
pub enum DetectCycles {
    Enabled,
    Disabled,
}

#[derive(Error, Debug)]
#[error("Invalid type of DetectCycles: `{0}`")]
pub struct InvalidType(String);

impl FromStr for DetectCycles {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("ENABLED") {
            Ok(DetectCycles::Enabled)
        } else if s.eq_ignore_ascii_case("DISABLED") {
            Ok(DetectCycles::Disabled)
        } else {
            Err(InvalidType(s.to_owned()))
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn parse() {
        assert_matches!("enabled".parse::<DetectCycles>(), Ok(DetectCycles::Enabled));
        assert_matches!("ENABLED".parse::<DetectCycles>(), Ok(DetectCycles::Enabled));
        assert_matches!(
            "disabled".parse::<DetectCycles>(),
            Ok(DetectCycles::Disabled)
        );
        assert_matches!(
            "DISABLED".parse::<DetectCycles>(),
            Ok(DetectCycles::Disabled)
        );
        assert_matches!(
            "foo".parse::<DetectCycles>(),
            Err(InvalidType(x)) if x == "foo"
        );
    }
}

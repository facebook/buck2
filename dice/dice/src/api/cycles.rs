/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Cycle detection in DICE

use std::fmt::Debug;
use std::str::FromStr;

use allocative::Allocative;
pub use dice_error::cycles::DetectCyclesParseError;
use dupe::Dupe;
use gazebo::variants::VariantName;

#[derive(Clone, Dupe, Copy, Debug, VariantName, Allocative)]
pub enum DetectCycles {
    Enabled,
    Disabled,
}

impl FromStr for DetectCycles {
    type Err = DetectCyclesParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("ENABLED") {
            Ok(DetectCycles::Enabled)
        } else if s.eq_ignore_ascii_case("DISABLED") {
            Ok(DetectCycles::Disabled)
        } else {
            Err(DetectCyclesParseError {
                value: s.to_owned(),
            })
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

        let invalid = "foo".parse::<DetectCycles>();
        assert!(invalid.is_err());
        assert_eq!(invalid.unwrap_err().value, "foo");
    }
}

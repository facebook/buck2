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
use variant_name::VariantName;

/// Whether DICE looks for dependency cycles among running computations.
///
/// A computation that depends, through others, on itself never finishes, and neither does
/// anything that depends on it. With detection enabled, DICE periodically checks whether a
/// version's computations are stuck on each other and, when they are, fails the requests that
/// make up the cycle with a `DiceError`, which the computations involved then see from
/// `DiceComputations::compute`. Detection is periodic, and deferred while other computations are
/// still completing, so a cycle is reported within about two check intervals of the computation
/// as a whole coming to a halt rather than immediately; see `DiceDataBuilder` for the interval.
///
/// A computation that races a dependency against something DICE knows nothing about looks stuck
/// on that dependency all the same, so such a computation can be failed for a cycle it would
/// have gotten out of by itself.
///
/// The detector is a task spawned through the `Spawner` of the transaction that starts a
/// version's first computation, and it sleeps on tokio timers, so the runtime it ends up on
/// must have those enabled.
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::dice_state::DiceState;
use crate::span_tracker::Roots;
use crate::span_tracker::SpanTrackable;

/// Estimate how many things are still left to do in a build. This is an approximation since our
/// roots and DICE state are not necessarily entirely in sync.
pub fn pending_estimate<T: SpanTrackable>(roots: &Roots<T>, dice: &DiceState) -> u64 {
    let mut total = 0;
    for k in &["BuildKey", "AnalysisKey"] {
        let from_dice = dice.key_states().get(*k).map_or(0, |v| {
            v.started + v.check_deps_started - v.finished - v.check_deps_finished
        });

        let from_roots = roots.dice_counts().get(k).copied().unwrap_or(0);

        total += u64::from(from_dice).saturating_sub(from_roots);
    }

    total
}

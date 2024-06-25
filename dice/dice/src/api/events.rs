/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

#[derive(Allocative, PartialEq, Eq, Debug)]
pub enum DiceEvent {
    /// Key evaluation started.
    Started { key_type: &'static str },

    /// Key evaluation finished.
    Finished { key_type: &'static str },

    /// Checking dependencies has started.
    CheckDepsStarted { key_type: &'static str },

    /// Checking dependencies has finished.
    CheckDepsFinished { key_type: &'static str },

    /// Compute has started.
    ComputeStarted { key_type: &'static str },

    /// Compute has finished.
    ComputeFinished { key_type: &'static str },
}

pub trait DiceEventListener: Allocative + Send + Sync + 'static {
    fn event(&self, ev: DiceEvent);
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

use crate::components::Dimensions;
use crate::components::DrawMode;
use crate::Component;
use crate::Lines;
use crate::State;

/// Component that repeats whatever lines are put into it.
/// Used in tests.
pub(crate) struct Echo(pub(crate) Lines);

impl Component for Echo {
    fn draw_unchecked(
        &self,
        _state: &State,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        Ok(self.0.clone())
    }
}

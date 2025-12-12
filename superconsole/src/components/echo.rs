/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![cfg(test)]

use std::convert::Infallible;

use crate::Component;
use crate::Lines;
use crate::components::Dimensions;
use crate::components::DrawMode;

/// Component that repeats whatever lines are put into it.
/// Used in tests.
pub(crate) struct Echo(pub(crate) Lines);

impl Component for Echo {
    type Error = Infallible;

    fn draw_unchecked(
        &self,
        _dimensions: Dimensions,
        _mode: DrawMode,
    ) -> Result<Lines, Infallible> {
        Ok(self.0.clone())
    }
}

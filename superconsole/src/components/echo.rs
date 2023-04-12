/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

use std::fmt::Debug;
use std::marker::PhantomData;

use crate::components::Dimensions;
use crate::components::DrawMode;
use crate::Component;
use crate::Lines;
use crate::State;

/// Component that repeats whatever lines are put into it.
/// Used in tests.
#[derive(Debug)]
pub(crate) struct Echo<Msg> {
    collapse: bool,
    _state: PhantomData<Msg>,
}

impl<Msg> Echo<Msg> {
    pub fn new(collapse: bool) -> Self {
        Self {
            collapse,
            _state: PhantomData,
        }
    }
}

impl<Msg: AsRef<Lines> + Send + 'static + Debug> Component for Echo<Msg> {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        match mode {
            DrawMode::Final if self.collapse => Ok(Lines::new()),
            _ => state.get::<Msg>().map(|msg| msg.as_ref().clone()),
        }
    }
}

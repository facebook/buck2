/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{fmt::Debug, marker::PhantomData};

use crate::{
    components::{Dimensions, DrawMode},
    Component, Line, State,
};

/// Component that repeats whatever lines are put into it.
/// Mostly useful for testing purposes.
#[derive(Debug)]
pub struct Echo<Msg> {
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

impl<Msg: AsRef<Vec<Line>> + Send + 'static + Debug> Component for Echo<Msg> {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Vec<Line>> {
        match mode {
            DrawMode::Final if self.collapse => Ok(vec![]),
            _ => state.get::<Msg>().map(|msg| msg.as_ref().clone()),
        }
    }
}

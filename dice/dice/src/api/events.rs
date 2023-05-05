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
    Started { key_type: &'static str },
    Finished { key_type: &'static str },
}

pub trait DiceEventListener: Allocative + Send + Sync + 'static {
    fn event(&self, ev: DiceEvent);
}

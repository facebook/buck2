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
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use cmp_any::PartialEqAny;

/// A `Key` that has been requested within Dice.
pub trait RequestedKey: Allocative + Display + Debug + Send + Sync {
    fn get_key_equality(&self) -> PartialEqAny;
    fn hash(&self, state: &mut dyn Hasher);
}

impl<T> RequestedKey for T
where
    T: Allocative + Display + Debug + Hash + Eq + Send + Sync + 'static,
{
    fn get_key_equality(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state)
    }
}

impl Hash for dyn RequestedKey {
    fn hash<H: Hasher>(&self, mut state: &mut H) {
        self.hash(&mut state)
    }
}

impl PartialEq for dyn RequestedKey {
    fn eq(&self, other: &Self) -> bool {
        self.get_key_equality() == other.get_key_equality()
    }
}

impl Eq for dyn RequestedKey {}

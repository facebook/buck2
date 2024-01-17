/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Used later in the stack.

use std::fmt;
use std::fmt::Debug;
use std::mem;

/// Map `u32` to `T`.
pub(crate) struct VecAsMap<T> {
    pub(crate) vec: Vec<Option<T>>,
}

impl<T: Debug> Debug for VecAsMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.entries()).finish()
    }
}

impl<T> Default for VecAsMap<T> {
    fn default() -> Self {
        VecAsMap { vec: Vec::new() }
    }
}

impl<T> VecAsMap<T> {
    pub(crate) fn get(&self, index: u32) -> Option<&T> {
        self.vec.get(index as usize).and_then(|e| e.as_ref())
    }

    pub(crate) fn get_mut(&mut self, index: u32) -> Option<&mut T> {
        self.vec.get_mut(index as usize).and_then(|e| e.as_mut())
    }

    pub(crate) fn contains_key(&self, index: u32) -> bool {
        self.get(index).is_some()
    }

    fn entries(&self) -> impl Iterator<Item = (u32, &T)> {
        self.vec
            .iter()
            .enumerate()
            .filter_map(|(i, e)| e.as_ref().map(|e| (i as u32, e)))
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = u32> + '_ {
        self.entries().map(|(k, _)| k)
    }

    pub(crate) fn insert(&mut self, index: u32, value: T) -> Option<T> {
        if self.vec.len() <= index as usize {
            self.vec.resize_with(index as usize + 1, || None);
        }
        mem::replace(&mut self.vec[index as usize], Some(value))
    }
}

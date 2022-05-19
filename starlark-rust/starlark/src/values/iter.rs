/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use gazebo::{cast, cell::ARef};

use crate::values::Value;

/// A relatively safe way of implementing an iterator over an ARef.
/// If this is properly safe, why doesn't exist for Ref?
pub(crate) struct ARefIterator<'a, 'v, T: ?Sized, I: Iterator<Item = Value<'v>>> {
    // OK to be dead, since the main thing is the Drop implementation
    #[allow(dead_code)]
    aref: ARef<'a, T>,
    iter: I,
}

impl<'a, 'v, T: ?Sized, I: Iterator<Item = Value<'v>>> ARefIterator<'a, 'v, T, I> {
    pub fn new(aref: ARef<'a, T>, f: impl FnOnce(&'a T) -> I) -> Self {
        // This is safe because we never unpack the ARefIterator
        let aref_ptr = unsafe { cast::ptr_lifetime(&*aref) };
        let iter = f(aref_ptr);
        Self { aref, iter }
    }
}

impl<'a, 'v, T: ?Sized, I: Iterator<Item = Value<'v>>> Iterator for ARefIterator<'a, 'v, T, I> {
    type Item = Value<'v>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

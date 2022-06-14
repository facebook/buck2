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

//! An optimised string HashMap which goes even faster when the keys can
//! be pre-hashed or otherwise precomputed.
//!
//! The two bottlenecks in our use of these hash tables are computing the hashes and comparing
//! the resulting keys for equality. We precompute the hashes. We also use `[u64]` to do faster
//! comparison when possible. We use the Starlark SmallHash hashes, promoted by IdHasher,
//! so we can reuse a SmallMap hash.
//!
//! Benchmarks on which the `[u64]` choice was made (mac/linux, all in ns):
//!                           8 bytes       32 bytes      64 bytes
//! slice equality (memcmp)   3.5/3.8       3.5/ 3.0      4.5/ 4.7
//! u64 equality loop         1.0/1.4       2.7/ 3.5      3.5/ 6.0
//! u8 equality loop          3.4/5.7      13.7/19.7     22.6/44.8
//!
//! Measuring some sample strings, the P50 = 21 bytes, P75 = 27, P95 = 35,
//! so we can reasonably expect to hit the smaller cases most often.

use std::{
    fmt::{self, Debug},
    intrinsics::copy_nonoverlapping,
    mem, slice, str,
};

use gazebo::coerce::Coerce;
use hashbrown::raw::RawTable;

use crate as starlark;
use crate::{
    collections::{Hashed, StarlarkHashValue},
    values::{StringValue, Trace},
};

// We use a RawTable (the thing that underlies HashMap) so we can look up efficiently
// and easily by Symbol and str, without being limited by `Borrow` traits.
#[derive(Clone, Trace)]
pub(crate) struct SymbolMap<T>(RawTable<(Symbol, T)>);

impl<T: Debug> Debug for SymbolMap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|x| (&x.0, &x.1)))
            .finish()
    }
}

/// A pre-hashed string used for efficient dictionary lookup.
#[derive(Clone, Trace)]
pub(crate) struct Symbol {
    hash: u64,
    len: u32,
    payload: Box<[u64]>,
    small_hash: StarlarkHashValue,
}

unsafe impl Coerce<Symbol> for Symbol {}

impl Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }

        let p1 = &*self.payload;
        let p2 = &*other.payload;
        // Important to use the payload len, which is in u64 units, rather than len which is in u8
        for i in 0..self.payload.len() {
            // Safe because we checked the lengths at the start
            if unsafe { p1.get_unchecked(i) != p2.get_unchecked(i) } {
                return false;
            }
        }
        true
    }
}

impl Eq for Symbol {}

impl Symbol {
    pub fn new(x: &str) -> Self {
        Self::new_hashed(Hashed::new(x))
    }

    pub fn new_hashed(x: Hashed<&str>) -> Self {
        let small_hash = x.hash();
        let hash = small_hash.promote();
        let len = x.key().len();
        let len8 = (len + 7) / 8;
        let mut payload = vec![0; len8]; // 0 pad it at the end
        unsafe {
            copy_nonoverlapping(x.key().as_ptr(), payload.as_mut_ptr() as *mut u8, len);
        }
        Self {
            hash,
            len: len.try_into().unwrap(),
            payload: payload.into_boxed_slice(),
            small_hash,
        }
    }

    pub fn as_str(&self) -> &str {
        // All safe because we promise we started out with a str
        unsafe {
            let s = slice::from_raw_parts(
                self.payload.as_ptr() as *const u64 as *const u8,
                self.len as usize,
            );
            str::from_utf8_unchecked(s)
        }
    }

    pub(crate) fn as_str_hashed(&self) -> Hashed<&str> {
        Hashed::new_unchecked(self.small_hash, self.as_str())
    }

    pub fn small_hash(&self) -> StarlarkHashValue {
        self.small_hash
    }
}

impl<T> SymbolMap<T> {
    pub fn new() -> Self {
        Self(RawTable::new())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(RawTable::with_capacity(capacity))
    }

    pub fn insert(&mut self, key: &str, value: T) -> Option<T> {
        let s = Symbol::new(key);
        if let Some((_, item)) = self.0.get_mut(s.hash, |x| s == x.0) {
            Some(mem::replace(item, value))
        } else {
            // This insert doesn't remove old values, so do that manually first
            self.0.insert(s.hash, (s, value), |x| x.0.hash);
            None
        }
    }

    pub fn get(&self, key: &Symbol) -> Option<&T> {
        self.0.get(key.hash, |x| key == &x.0).map(|x| &x.1)
    }

    pub fn get_str(&self, key: &str) -> Option<&T> {
        self.get_hashed_str(Hashed::new(key))
    }

    pub fn get_hashed_str(&self, key: Hashed<&str>) -> Option<&T> {
        self.0
            .get(key.hash().promote(), |x| x.0.as_str() == *key.key())
            .map(|x| &x.1)
    }

    pub(crate) fn get_hashed_string_value(&self, key: Hashed<StringValue>) -> Option<&T> {
        self.0
            .get(key.hash().promote(), |x| x.0.as_str() == key.key().as_str())
            .map(|x| &x.1)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = &'a (Symbol, T)> + 'a {
        // Unsafe because it doesn't have a lifetime, but we added one in the type signature
        unsafe { self.0.iter().map(|x| x.as_ref()) }
    }

    pub fn keys<'a>(&'a self) -> impl ExactSizeIterator<Item = &'a Symbol> + 'a {
        self.iter().map(|x| &x.0)
    }

    pub fn values<'a>(&'a self) -> impl ExactSizeIterator<Item = &'a T> + 'a {
        self.iter().map(|x| &x.1)
    }
}

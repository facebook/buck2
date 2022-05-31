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

//! Stack pointer.

use std::ops::Add;

use gazebo::dupe::Dupe;

/// Index of the slot in the function frame.
/// This can be both a local variable or a temporary.
/// When reading local variable, it must be definitely initialized (e.g. function parameter).
#[derive(
    Copy,
    Clone,
    Dupe,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display
)]
#[display(fmt = "&{}", _0)]
pub(crate) struct BcSlot(pub(crate) u32);

impl Add<u32> for BcSlot {
    type Output = BcSlot;

    #[inline]
    fn add(self, rhs: u32) -> BcSlot {
        BcSlot(self.0 + rhs)
    }
}

/// `N` slots starting with given number.
#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BcSlotsN<const N: usize> {
    /// `N` slots starting with given slot.
    pub(crate) start: BcSlot,
}

impl<const N: usize> BcSlotsN<N> {
    #[inline]
    pub(crate) fn get<const I: u32>(self) -> BcSlot {
        assert!((I as usize) < N);
        self.start + I
    }

    pub(crate) fn from_range(range: BcSlotRange) -> BcSlotsN<N> {
        assert_eq!(N, range.len() as usize);
        BcSlotsN { start: range.start }
    }

    pub(crate) fn start(self) -> BcSlot {
        self.start
    }

    pub(crate) fn end(self) -> BcSlot {
        self.start + N as u32
    }
}

#[derive(Copy, Clone, Dupe, Debug, derive_more::Display)]
#[display(fmt = "{}..{}", start, end)]
pub(crate) struct BcSlotRange {
    pub(crate) start: BcSlot,
    pub(crate) end: BcSlot,
}

impl BcSlotRange {
    #[inline]
    pub(crate) fn len(self) -> u32 {
        self.end.0 - self.start.0
    }

    pub(crate) fn to_range_from(self) -> BcSlotRangeFrom {
        BcSlotRangeFrom(self.start)
    }

    pub(crate) fn iter(self) -> impl Iterator<Item = BcSlot> {
        (self.start.0..self.end.0).map(BcSlot)
    }
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BcSlotRangeFrom(pub(crate) BcSlot);

impl BcSlotRangeFrom {
    #[inline]
    pub(crate) fn to_range(self, len: u32) -> BcSlotRange {
        BcSlotRange {
            start: self.0,
            end: self.0 + len,
        }
    }
}

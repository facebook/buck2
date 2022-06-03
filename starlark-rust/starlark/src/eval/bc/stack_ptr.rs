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

impl BcSlot {
    pub(crate) fn to_in(self) -> BcSlotIn {
        BcSlotIn(self)
    }

    pub(crate) fn to_out(self) -> BcSlotOut {
        BcSlotOut(self)
    }
}

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

    pub(crate) fn to_in(self) -> BcSlotsInN<N> {
        BcSlotsInN {
            start: self.start.to_in(),
        }
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

    pub(crate) fn iter(self) -> impl Iterator<Item = BcSlot> {
        (self.start.0..self.end.0).map(BcSlot)
    }

    pub(crate) fn to_in(self) -> BcSlotInRange {
        BcSlotInRange {
            start: self.start.to_in(),
            end: self.end.to_in(),
        }
    }
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BcSlotRangeFrom(pub(crate) BcSlot);

/// Slot containing a value.
///
/// The slot may be a local variable, so this slot cannot be used to store a temporary value.
#[derive(Debug, Copy, Clone, Dupe, derive_more::Display)]
#[display(fmt = "{}", _0)]
pub(crate) struct BcSlotIn(BcSlot);

impl Add<u32> for BcSlotIn {
    type Output = BcSlotIn;

    #[inline]
    fn add(self, rhs: u32) -> BcSlotIn {
        BcSlotIn(self.0 + rhs)
    }
}

impl BcSlotIn {
    /// Take the slot.
    ///
    /// This operation need to be used carefully: this slot cannot be used for writing.
    #[inline]
    pub(crate) fn get(self) -> BcSlot {
        self.0
    }
}

/// `N` slots starting with given number, which can be read, but cannot be written to.
#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BcSlotsInN<const N: usize> {
    /// `N` slots starting with given slot.
    start: BcSlotIn,
}

impl<const N: usize> BcSlotsInN<N> {
    #[inline]
    pub(crate) fn get<const I: u32>(self) -> BcSlotIn {
        assert!((I as usize) < N);
        self.start + I
    }

    pub(crate) fn from_range(range: BcSlotInRange) -> BcSlotsInN<N> {
        assert_eq!(N, range.len() as usize);
        BcSlotsInN { start: range.start }
    }

    pub(crate) fn start(self) -> BcSlotIn {
        self.start
    }

    pub(crate) fn end(self) -> BcSlotIn {
        self.start + N as u32
    }
}

#[derive(Copy, Clone, Dupe, Debug, derive_more::Display)]
#[display(fmt = "{}..{}", start, end)]
pub(crate) struct BcSlotInRange {
    pub(crate) start: BcSlotIn,
    pub(crate) end: BcSlotIn,
}

impl BcSlotInRange {
    #[inline]
    pub(crate) fn len(self) -> u32 {
        self.end.0.0 - self.start.0.0
    }

    pub(crate) fn to_range_from(self) -> BcSlotInRangeFrom {
        BcSlotInRangeFrom(self.start)
    }
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BcSlotInRangeFrom(pub(crate) BcSlotIn);

impl BcSlotInRangeFrom {
    #[inline]
    pub(crate) fn to_range(self, len: u32) -> BcSlotInRange {
        BcSlotInRange {
            start: self.0,
            end: self.0 + len,
        }
    }
}

/// Slot where the value should be stored.
///
/// The slot may be a local variable, so this slot cannot be used to store a temporary value.
#[derive(Debug, Copy, Clone, Dupe, derive_more::Display)]
#[display(fmt = "{}", _0)]
pub(crate) struct BcSlotOut(BcSlot);

impl BcSlotOut {
    #[inline]
    pub(crate) fn get(self) -> BcSlot {
        self.0
    }
}

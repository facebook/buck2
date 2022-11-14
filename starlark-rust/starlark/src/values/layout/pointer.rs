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

// We use pointer tagging on the bottom three bits:
// ?00 => frozen pointer
// ?01 => mutable pointer
// ?10 => int (32 bit)
// third bit is a tag set by the user (get_user_tag)

// We group our bytes based on the tag info, not traditional alignment.
// This lint is fairly new, so have to also enable unknown-clippy-lints.
#![allow(clippy::unusual_byte_groupings)]

use std::marker::PhantomData;
use std::mem;
use std::num::NonZeroUsize;

use either::Either;
use gazebo::cast;
use gazebo::phantom::PhantomDataInvariant;
use gazebo::prelude::*;
use static_assertions::assert_eq_size;

use crate::values::int::PointerI32;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueOrForward;

/// Tagged pointer logically equivalent to `*mut AValueHeader`.
#[derive(Clone, Copy, Dupe, PartialEq, Eq, Hash)]
pub(crate) struct RawPointer(pub(crate) NonZeroUsize);

impl RawPointer {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: usize) -> RawPointer {
        debug_assert!(ptr != 0);
        RawPointer(NonZeroUsize::new_unchecked(ptr))
    }

    #[inline]
    pub(crate) fn ptr_value(self) -> usize {
        self.0.get()
    }

    #[inline]
    pub(crate) fn is_str(self) -> bool {
        (self.0.get() & TAG_STR) != 0
    }

    #[inline]
    pub(crate) fn is_unfrozen(self) -> bool {
        (self.0.get() & TAG_UNFROZEN) != 0
    }

    #[inline]
    pub(crate) fn unpack_int(self) -> Option<i32> {
        let p = self.0.get();
        if p & TAG_INT == 0 {
            None
        } else {
            Some(untag_int(p))
        }
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> i32 {
        let p = self.0.get();
        debug_assert!(p & TAG_BITS == TAG_INT);
        untag_int(p)
    }
}

// A structure that is morally a `PointerUnpack`, but gets encoded in one
// pointer sized lump. The two types P1 and P2 are arbitrary pointers (which we
// instantiate to FrozenValueMem and ValueMem)
#[derive(Clone, Copy, Dupe)]
pub(crate) struct Pointer<'p> {
    pointer: RawPointer,
    // Make sure we are invariant in all the types/lifetimes.
    // See https://stackoverflow.com/questions/62659221/why-does-a-program-compile-despite-an-apparent-lifetime-mismatch
    phantom: PhantomDataInvariant<&'p AValueHeader>,
}

// Similar to `Pointer` but allows widening lifetime, which is valid operation for frozen pointers.
#[derive(Clone, Copy, Dupe)]
pub(crate) struct FrozenPointer<'p> {
    pointer: RawPointer,
    phantom: PhantomData<&'p AValueHeader>,
}

fn _test_lifetime_covariant<'a>(p: FrozenPointer<'static>) -> FrozenPointer<'a> {
    p
}

assert_eq_size!(Pointer<'static>, usize);
assert_eq_size!(Option<Pointer<'static>>, usize);
assert_eq_size!(FrozenPointer<'static>, usize);
assert_eq_size!(Option<FrozenPointer<'static>>, usize);

const TAG_BITS: usize = 0b111;

const TAG_INT: usize = 0b010;
const TAG_STR: usize = 0b100;
// Pointer to an object, which is not frozen.
// Note, an object can be changed from unfrozen to frozen, not vice versa.
const TAG_UNFROZEN: usize = 0b001;

#[inline]
unsafe fn untag_pointer<'a>(x: usize) -> &'a AValueOrForward {
    cast::usize_to_ptr(x & !TAG_BITS)
}

#[allow(clippy::unused_unit)]
const _: () = if mem::size_of::<usize>() > mem::size_of::<i32>() {
    ()
} else {
    panic!("starlark-rust requires 64 bit usize")
};

#[inline]
fn tag_int(x: i32) -> usize {
    ((x as u32 as usize) << 3) | TAG_INT
}

#[inline]
fn untag_int(x: usize) -> i32 {
    const INT_DATA_MASK: usize = 0xffffffff << 3;
    debug_assert!(x & !INT_DATA_MASK == TAG_INT);

    ((x as isize) >> 3) as i32
}

impl<'p> Pointer<'p> {
    #[inline]
    fn new(pointer: usize) -> Self {
        let phantom = PhantomDataInvariant::new();
        let pointer = unsafe { RawPointer::new_unchecked(pointer) };
        Self { pointer, phantom }
    }

    #[inline]
    pub fn new_unfrozen_usize(x: usize, is_string: bool) -> Self {
        debug_assert!((x & TAG_BITS) == 0);
        let x = if is_string { x | TAG_STR } else { x };
        Self::new(x | TAG_UNFROZEN)
    }

    #[inline]
    pub fn new_unfrozen_usize_with_str_tag(x: usize) -> Self {
        debug_assert!((x & TAG_BITS & !TAG_STR) == 0);
        Self::new(x | TAG_UNFROZEN)
    }

    #[inline]
    pub fn new_unfrozen(x: &'p AValueHeader, is_string: bool) -> Self {
        Self::new_unfrozen_usize(cast::ptr_to_usize(x), is_string)
    }

    #[inline]
    pub(crate) fn is_str(self) -> bool {
        self.pointer.is_str()
    }

    #[inline]
    pub fn is_unfrozen(self) -> bool {
        self.pointer.is_unfrozen()
    }

    #[inline]
    pub fn unpack(self) -> Either<&'p AValueOrForward, &'static PointerI32> {
        let p = self.pointer.0.get();
        if p & TAG_INT == 0 {
            Either::Left(unsafe { untag_pointer(p) })
        } else {
            Either::Right(unsafe { cast::usize_to_ptr(p) })
        }
    }

    #[inline]
    pub fn unpack_int(self) -> Option<i32> {
        self.pointer.unpack_int()
    }

    #[inline]
    pub fn unpack_ptr(self) -> Option<&'p AValueOrForward> {
        let p = self.pointer.0.get();
        if p & TAG_INT == 0 {
            Some(unsafe { untag_pointer(p) })
        } else {
            None
        }
    }

    /// Unpack pointer when it is known to be not an integer.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_unchecked(self) -> &'p AValueOrForward {
        let p = self.pointer.0.get();
        debug_assert!(p & TAG_INT == 0);
        untag_pointer(p)
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> i32 {
        self.pointer.unpack_int_unchecked()
    }

    #[inline]
    pub fn ptr_eq(self, other: Pointer<'_>) -> bool {
        self.pointer == other.pointer
    }

    #[inline]
    pub fn raw(self) -> RawPointer {
        self.pointer
    }

    #[inline]
    pub unsafe fn cast_lifetime<'p2>(self) -> Pointer<'p2> {
        Pointer {
            pointer: self.pointer,
            phantom: PhantomDataInvariant::new(),
        }
    }

    #[inline]
    pub(crate) unsafe fn to_frozen_pointer(self) -> FrozenPointer<'p> {
        debug_assert!(!self.is_unfrozen());
        FrozenPointer {
            pointer: self.pointer,
            phantom: PhantomData,
        }
    }
}

impl<'p> FrozenPointer<'p> {
    #[inline]
    pub(crate) unsafe fn new(pointer: usize) -> Self {
        // Never zero because the only TAG which is zero is P1, and that must be a pointer
        debug_assert!(pointer != 0);
        debug_assert!((pointer & TAG_UNFROZEN) == 0);
        let pointer = RawPointer::new_unchecked(pointer);
        Self {
            pointer,
            phantom: PhantomData,
        }
    }

    #[inline]
    pub fn new_frozen_usize(x: usize, is_string: bool) -> Self {
        debug_assert!((x & TAG_BITS) == 0);
        let x = if is_string { x | TAG_STR } else { x };
        unsafe { Self::new(x) }
    }

    #[inline]
    pub fn new_frozen_usize_with_str_tag(x: usize) -> Self {
        debug_assert!((x & TAG_BITS & !TAG_STR) == 0);
        unsafe { Self::new(x) }
    }

    #[inline]
    pub(crate) fn new_frozen(x: &'p AValueHeader, is_str: bool) -> Self {
        Self::new_frozen_usize(cast::ptr_to_usize(x), is_str)
    }

    #[inline]
    pub(crate) fn new_int(x: i32) -> Self {
        unsafe { Self::new(tag_int(x)) }
    }

    /// It is safe to bitcast `FrozenPointer` to `Pointer`
    /// but not vice versa.
    #[inline]
    pub(crate) fn to_pointer(self) -> Pointer<'p> {
        Pointer {
            pointer: self.pointer,
            phantom: PhantomDataInvariant::new(),
        }
    }

    #[inline]
    pub(crate) fn raw(self) -> RawPointer {
        self.pointer
    }

    #[inline]
    pub(crate) fn unpack_int(self) -> Option<i32> {
        self.to_pointer().unpack_int()
    }

    /// Unpack pointer when it is known to be not an integer.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_unchecked(self) -> &'p AValueOrForward {
        let p = self.pointer.0.get();
        debug_assert!(p & TAG_INT == 0);
        untag_pointer(p)
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> i32 {
        self.pointer.unpack_int_unchecked()
    }

    /// Unpack pointer when it is known to be not an integer, not a string, and not frozen.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_no_str_unchecked(self) -> &'p AValueOrForward {
        let p = self.pointer.0.get();
        debug_assert!(p & TAG_BITS == 0);
        cast::usize_to_ptr(p)
    }
}

#[cfg(test)]
#[test]
fn test_int_tag() {
    fn check(x: i32) {
        assert_eq!(x, untag_int(tag_int(x)))
    }

    for x in -10..10 {
        check(x)
    }
    check(i32::MAX);
    check(i32::MIN);
}

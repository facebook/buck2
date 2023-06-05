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
#![allow(clippy::unusual_byte_groupings)]

use std::cell::Cell;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;
use std::num::NonZeroUsize;

use allocative::Allocative;
use dupe::Dupe;
use either::Either;
use static_assertions::assert_eq_size;

use crate::cast;
use crate::values::int::PointerI32;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueOrForward;
use crate::values::types::inline_int::InlineInt;

/// Tagged pointer logically equivalent to `*mut AValueHeader`.
#[derive(Clone, Copy, Dupe, PartialEq, Eq, Hash, Allocative)]
pub(crate) struct RawPointer(pub(crate) NonZeroUsize);

impl Debug for RawPointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("RawPointer")
            .field(&format_args!("0x{:x}", self.ptr_value()))
            .finish()
    }
}

impl RawPointer {
    #[inline]
    pub(crate) unsafe fn new_unchecked(ptr: usize) -> RawPointer {
        assert_eq!(
            mem::size_of::<u64>(),
            mem::size_of_val(&ptr),
            "32 bit starlark is not supported yet"
        );
        debug_assert!(ptr != 0);
        RawPointer(NonZeroUsize::new_unchecked(ptr))
    }

    #[inline]
    pub(crate) fn new_int(i: InlineInt) -> RawPointer {
        let ptr = ((i.to_i32() as u32 as usize) << TAG_BITS) | TAG_INT;
        unsafe { Self::new_unchecked(ptr) }
    }

    #[inline]
    pub(crate) fn new_unfrozen(ptr: &AValueHeader, is_string: bool) -> RawPointer {
        let ptr = cast::ptr_to_usize(ptr);
        let ptr = if is_string { ptr | TAG_STR } else { ptr };
        let ptr = ptr | TAG_UNFROZEN;
        unsafe { Self::new_unchecked(ptr) }
    }

    #[inline]
    pub(crate) fn new_frozen(ptr: &AValueHeader, is_string: bool) -> RawPointer {
        let ptr = cast::ptr_to_usize(ptr);
        let ptr = if is_string { ptr | TAG_STR } else { ptr };
        unsafe { Self::new_unchecked(ptr) }
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
    pub(crate) fn is_int(self) -> bool {
        (self.0.get() & TAG_INT) != 0
    }

    #[inline]
    pub(crate) fn is_unfrozen(self) -> bool {
        (self.0.get() & TAG_UNFROZEN) != 0
    }

    #[inline]
    pub(crate) fn unpack_int(self) -> Option<InlineInt> {
        if !self.is_int() {
            None
        } else {
            unsafe { Some(self.unpack_int_unchecked()) }
        }
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> InlineInt {
        debug_assert!(self.is_int());

        const INT_DATA_MASK: usize = 0xffffffff << TAG_BITS;
        debug_assert!(self.0.get() & !INT_DATA_MASK == TAG_INT);

        InlineInt::new_unchecked(((self.0.get() as isize) >> TAG_BITS) as i32)
    }

    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_unchecked<'v>(self) -> &'v AValueOrForward {
        debug_assert!(!self.is_int());
        let ptr = self.0.get() & !(TAG_STR | TAG_UNFROZEN);
        cast::usize_to_ptr(ptr)
    }
}

// A structure that is morally a `PointerUnpack`, but gets encoded in one
// pointer sized lump. The two types P1 and P2 are arbitrary pointers (which we
// instantiate to FrozenValueMem and ValueMem)
#[derive(Clone, Copy, Dupe)]
pub(crate) struct Pointer<'p> {
    ptr: RawPointer,
    // Make sure we are invariant in all the types/lifetimes.
    // See https://stackoverflow.com/questions/62659221/why-does-a-program-compile-despite-an-apparent-lifetime-mismatch
    _phantom: PhantomData<Cell<&'p AValueHeader>>,
}

// Similar to `Pointer` but allows widening lifetime, which is valid operation for frozen pointers.
#[derive(Clone, Copy, Dupe)]
pub(crate) struct FrozenPointer<'p> {
    ptr: RawPointer,
    phantom: PhantomData<&'p AValueHeader>,
}

fn _test_lifetime_covariant<'a>(p: FrozenPointer<'static>) -> FrozenPointer<'a> {
    p
}

assert_eq_size!(Pointer<'static>, usize);
assert_eq_size!(Option<Pointer<'static>>, usize);
assert_eq_size!(FrozenPointer<'static>, usize);
assert_eq_size!(Option<FrozenPointer<'static>>, usize);

const TAG_BITS: usize = 3;
const TAG_MASK: usize = 0b111;
#[allow(clippy::assertions_on_constants)]
const _: () = assert!(TAG_MASK == (1 << TAG_BITS) - 1);

const TAG_INT: usize = 0b010;
const TAG_STR: usize = 0b100;
// Pointer to an object, which is not frozen.
// Note, an object can be changed from unfrozen to frozen, not vice versa.
const TAG_UNFROZEN: usize = 0b001;

#[inline]
unsafe fn untag_pointer<'a>(x: usize) -> &'a AValueOrForward {
    cast::usize_to_ptr(x & !TAG_MASK)
}

impl<'p> Pointer<'p> {
    #[inline]
    unsafe fn new(ptr: RawPointer) -> Pointer<'p> {
        Pointer {
            ptr,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub(crate) unsafe fn new_unfrozen_usize_with_str_tag(x: usize) -> Self {
        debug_assert!((x & TAG_MASK & !TAG_STR) == 0);
        Self::new(RawPointer::new_unchecked(x | TAG_UNFROZEN))
    }

    #[inline]
    pub fn new_unfrozen(x: &'p AValueHeader, is_string: bool) -> Self {
        unsafe { Self::new(RawPointer::new_unfrozen(x, is_string)) }
    }

    #[inline]
    pub(crate) fn is_str(self) -> bool {
        self.ptr.is_str()
    }

    #[inline]
    pub fn is_unfrozen(self) -> bool {
        self.ptr.is_unfrozen()
    }

    #[inline]
    pub fn unpack(self) -> Either<&'p AValueOrForward, &'static PointerI32> {
        if !self.ptr.is_int() {
            Either::Left(unsafe { self.ptr.unpack_ptr_no_int_unchecked() })
        } else {
            Either::Right(unsafe { PointerI32::from_raw_pointer_unchecked(self.ptr) })
        }
    }

    #[inline]
    pub fn unpack_int(self) -> Option<InlineInt> {
        self.ptr.unpack_int()
    }

    #[inline]
    pub fn unpack_ptr(self) -> Option<&'p AValueOrForward> {
        let p = self.ptr.0.get();
        if p & TAG_INT == 0 {
            Some(unsafe { untag_pointer(p) })
        } else {
            None
        }
    }

    /// Unpack pointer when it is known to be not an integer.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_unchecked(self) -> &'p AValueOrForward {
        let p = self.ptr.0.get();
        debug_assert!(p & TAG_INT == 0);
        untag_pointer(p)
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> InlineInt {
        self.ptr.unpack_int_unchecked()
    }

    #[inline]
    pub fn ptr_eq(self, other: Pointer<'_>) -> bool {
        self.ptr == other.ptr
    }

    #[inline]
    pub fn raw(self) -> RawPointer {
        self.ptr
    }

    #[inline]
    pub unsafe fn cast_lifetime<'p2>(self) -> Pointer<'p2> {
        Pointer {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub(crate) unsafe fn to_frozen_pointer_unchecked(self) -> FrozenPointer<'p> {
        FrozenPointer::new(self.ptr)
    }
}

impl<'p> FrozenPointer<'p> {
    #[inline]
    pub(crate) unsafe fn new(ptr: RawPointer) -> FrozenPointer<'p> {
        debug_assert!(!ptr.is_unfrozen());
        FrozenPointer {
            ptr,
            phantom: PhantomData,
        }
    }

    #[inline]
    pub fn new_frozen_usize_with_str_tag(x: usize) -> Self {
        debug_assert!((x & TAG_MASK & !TAG_STR) == 0);
        unsafe { Self::new(RawPointer::new_unchecked(x)) }
    }

    #[inline]
    pub(crate) fn new_frozen(x: &'p AValueHeader, is_str: bool) -> Self {
        unsafe { Self::new(RawPointer::new_frozen(x, is_str)) }
    }

    #[inline]
    pub(crate) fn new_int(x: InlineInt) -> Self {
        FrozenPointer {
            ptr: RawPointer::new_int(x),
            phantom: PhantomData,
        }
    }

    /// It is safe to bitcast `FrozenPointer` to `Pointer`
    /// but not vice versa.
    #[inline]
    pub(crate) fn to_pointer(self) -> Pointer<'p> {
        Pointer {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn raw(self) -> RawPointer {
        self.ptr
    }

    /// Unpack pointer when it is known to be not an integer.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_unchecked(self) -> &'p AValueOrForward {
        debug_assert!(!self.ptr.is_int());
        self.ptr.unpack_ptr_no_int_unchecked()
    }

    /// Unpack integer when it is known to be not a pointer.
    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> InlineInt {
        self.ptr.unpack_int_unchecked()
    }

    /// Unpack pointer when it is known to be not an integer, not a string, and not frozen.
    #[inline]
    pub(crate) unsafe fn unpack_ptr_no_int_no_str_unchecked(self) -> &'p AValueOrForward {
        let p = self.ptr.0.get();
        debug_assert!(p & TAG_MASK == 0);
        cast::usize_to_ptr(p)
    }
}

#[cfg(test)]
#[test]
fn test_int_tag() {
    fn check(x: InlineInt) {
        assert_eq!(x, RawPointer::new_int(x).unpack_int().unwrap());
    }

    for x in -10..10 {
        check(InlineInt::try_from(x).ok().unwrap());
    }
    check(InlineInt::MAX);
    check(InlineInt::MIN);
}

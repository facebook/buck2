/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Describes the types we can store in `LockFreeRawTable`.

use std::num::NonZeroU32;
use std::num::NonZeroU64;
use std::ptr;
use std::ptr::NonNull;
use std::sync::Arc;

use bytemuck::NoUninit;

/// Generalized non-null pointer.
///
/// Anything which can be stored in `LockFreeRawTable`.
pub trait AtomicValue {
    /// The value stored in the table.
    ///
    /// If the value is larger than max support atomic width on the platform,
    /// atomic operations will be spinlocked.
    type Raw: NoUninit;
    /// Dereferenced value.
    type Ref<'a>: Copy
    where
        Self: 'a;

    /// Null value. This is stored in `LockFreeRawTable` when the entry is missing.
    fn null() -> Self::Raw;
    /// Check if the value is null.
    fn is_null(this: Self::Raw) -> bool;
    /// Obtain the raw pointer. It must be non-null.
    fn into_raw(this: Self) -> Self::Raw;
    /// Construct the value from the raw pointer.
    unsafe fn from_raw(raw: Self::Raw) -> Self;
    /// Dereference the raw pointer.
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a>;
}

impl<T> AtomicValue for Box<T> {
    type Raw = usize; // *mut T
    type Ref<'a>
        = &'a T
    where
        Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        0
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this == 0
    }

    #[inline]
    fn into_raw(this: Self) -> Self::Raw {
        Box::into_raw(this).expose_provenance()
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> Self {
        unsafe { Box::from_raw(ptr::with_exposed_provenance_mut(raw)) }
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        unsafe { &*ptr::with_exposed_provenance(raw) }
    }
}

impl<T> AtomicValue for Arc<T> {
    type Raw = usize; // *const T
    type Ref<'a>
        = &'a T
    where
        Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        0
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this == 0
    }

    #[inline]
    fn into_raw(this: Self) -> Self::Raw {
        Arc::into_raw(this).expose_provenance()
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> Self {
        unsafe { Arc::from_raw(ptr::with_exposed_provenance(raw)) }
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        unsafe { &*ptr::with_exposed_provenance(raw) }
    }
}

impl AtomicValue for NonZeroU64 {
    type Raw = u64;
    type Ref<'a> = NonZeroU64;

    #[inline]
    fn null() -> u64 {
        0
    }

    #[inline]
    fn is_null(this: u64) -> bool {
        this == 0
    }

    #[inline]
    fn into_raw(this: NonZeroU64) -> u64 {
        this.get()
    }

    #[inline]
    unsafe fn from_raw(raw: u64) -> NonZeroU64 {
        unsafe { NonZeroU64::new_unchecked(raw) }
    }

    #[inline]
    unsafe fn deref<'a>(raw: u64) -> Self::Ref<'a> {
        unsafe { NonZeroU64::new_unchecked(raw) }
    }
}

impl AtomicValue for NonZeroU32 {
    type Raw = u32;
    type Ref<'a> = NonZeroU32;

    #[inline]
    fn null() -> u32 {
        0
    }

    #[inline]
    fn is_null(this: u32) -> bool {
        this == 0
    }

    #[inline]
    fn into_raw(this: NonZeroU32) -> u32 {
        this.get()
    }

    #[inline]
    unsafe fn from_raw(raw: u32) -> NonZeroU32 {
        unsafe { NonZeroU32::new_unchecked(raw) }
    }

    #[inline]
    unsafe fn deref<'a>(raw: u32) -> Self::Ref<'a> {
        unsafe { NonZeroU32::new_unchecked(raw) }
    }
}

/// Raw pointers stored in the table.
#[derive(Copy, Clone)]
pub struct RawPtr<T>(pub NonNull<T>);

impl<T> AtomicValue for RawPtr<T> {
    type Raw = usize; // *mut T
    type Ref<'a>
        = NonNull<T>
    where
        Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        0
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this == 0
    }

    #[inline]
    fn into_raw(this: RawPtr<T>) -> Self::Raw {
        this.0.as_ptr().expose_provenance()
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> RawPtr<T> {
        unsafe {
            RawPtr(NonNull::new_unchecked(ptr::with_exposed_provenance_mut(
                raw,
            )))
        }
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a>
    where
        Self: 'a,
    {
        unsafe { NonNull::new_unchecked(ptr::with_exposed_provenance_mut(raw)) }
    }
}

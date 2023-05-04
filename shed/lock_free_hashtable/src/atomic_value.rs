/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Describes the types we can store in `LockFreeRawTable`.

use std::num::NonZeroU32;
use std::num::NonZeroU64;
use std::ptr;
use std::ptr::NonNull;
use std::sync::Arc;

/// Generalized non-null pointer.
///
/// Anything which can be stored in `LockFreeRawTable`.
pub trait AtomicValue {
    /// The value stored in the table.
    ///
    /// If the value is larger than max support atomic width on the platform,
    /// atomic operations will be spinlocked.
    type Raw: Copy;
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
    type Raw = *mut T;
    type Ref<'a> = &'a T where Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        ptr::null_mut()
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this.is_null()
    }

    #[inline]
    fn into_raw(this: Self) -> Self::Raw {
        Box::into_raw(this)
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> Self {
        unsafe { Box::from_raw(raw) }
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        unsafe { &*raw }
    }
}

impl<T> AtomicValue for Arc<T> {
    type Raw = *const T;
    type Ref<'a> = &'a T where Self: 'a;

    #[inline]
    fn null() -> Self::Raw {
        ptr::null()
    }

    #[inline]
    fn is_null(this: Self::Raw) -> bool {
        this.is_null()
    }

    #[inline]
    fn into_raw(this: Self) -> Self::Raw {
        Arc::into_raw(this)
    }

    #[inline]
    unsafe fn from_raw(raw: Self::Raw) -> Self {
        Arc::from_raw(raw)
    }

    #[inline]
    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        unsafe { &*raw }
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
        NonZeroU64::new_unchecked(raw)
    }

    #[inline]
    unsafe fn deref<'a>(raw: u64) -> Self::Ref<'a> {
        NonZeroU64::new_unchecked(raw)
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
        NonZeroU32::new_unchecked(raw)
    }

    #[inline]
    unsafe fn deref<'a>(raw: u32) -> Self::Ref<'a> {
        NonZeroU32::new_unchecked(raw)
    }
}

/// Raw pointers stored in the table.
#[derive(Copy, Clone)]
pub struct RawPtr<T>(pub NonNull<T>);

impl<T> AtomicValue for RawPtr<T> {
    type Raw = *mut T;
    type Ref<'a> = NonNull<T> where Self: 'a;

    #[inline]
    fn null() -> *mut T {
        ptr::null_mut()
    }

    #[inline]
    fn is_null(this: *mut T) -> bool {
        this.is_null()
    }

    #[inline]
    fn into_raw(this: RawPtr<T>) -> *mut T {
        this.0.as_ptr()
    }

    #[inline]
    unsafe fn from_raw(raw: *mut T) -> RawPtr<T> {
        RawPtr(NonNull::new_unchecked(raw))
    }

    #[inline]
    unsafe fn deref<'a>(raw: *mut T) -> Self::Ref<'a>
    where
        Self: 'a,
    {
        NonNull::new_unchecked(raw)
    }
}

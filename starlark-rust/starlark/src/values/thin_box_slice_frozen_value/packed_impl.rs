/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ptr::NonNull;

use either::Either;

use crate::values::thin_box_slice_frozen_value::thin_box::AllocatedThinBoxSlice;
use crate::values::FrozenValue;

/// Wrapper to handle the packing and most of the unsafety.
///
/// The representation is as follows:
///  - In the case of a length 1 slice, the `FrozenValue` is stored in the `NonNull` pointer.
///  - In all other cases, the `NonNull` is a `AllocatedThinBoxSlice<FrozenValue>` with the bottom
///    bit set to 1.
struct PackedImpl(NonNull<()>);

impl PackedImpl {
    const fn new_allocated(allocated: AllocatedThinBoxSlice<FrozenValue>) -> Self {
        let allocated = unsafe { allocated.into_inner().byte_add(1) };
        Self(allocated.cast::<()>())
    }

    fn new(iter: impl IntoIterator<Item = FrozenValue>) -> Self {
        let mut iter = iter.into_iter();
        let Some(first) = iter.next() else {
            return Self::new_allocated(AllocatedThinBoxSlice::empty());
        };
        let Some(second) = iter.next() else {
            return Self(unsafe { mem::transmute::<FrozenValue, NonNull<()>>(first) });
        };
        Self::new_allocated(AllocatedThinBoxSlice::from_iter(
            [first, second].into_iter().chain(iter),
        ))
    }

    fn unpack(&self) -> Either<&FrozenValue, AllocatedThinBoxSlice<FrozenValue>> {
        let ptr = self.0.as_ptr();
        if (ptr as usize) & 1 == 1 {
            let allocated = (ptr as usize & !1) as *mut FrozenValue;
            let allocated = unsafe {
                AllocatedThinBoxSlice::<FrozenValue>::from_inner(
                    NonNull::<FrozenValue>::new_unchecked(allocated),
                )
            };
            Either::Right(allocated)
        } else {
            let val = unsafe { &*(self as *const PackedImpl as *const FrozenValue) };
            Either::Left(val)
        }
    }

    fn as_slice(&self) -> &[FrozenValue] {
        match self.unpack() {
            Either::Left(val) => std::slice::from_ref(val),
            Either::Right(allocated) => {
                let slice: &[FrozenValue] = &allocated;
                let slice: &[FrozenValue] = unsafe { mem::transmute(slice) };
                slice
            }
        }
    }
}

impl Drop for PackedImpl {
    fn drop(&mut self) {
        match self.unpack() {
            Either::Left(_) => {}
            Either::Right(allocated) => allocated.run_drop(),
        }
    }
}

impl allocative::Allocative for PackedImpl {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        // Intentionally don't `enter_self_sized()`, and instead just report the
        // `ThinBoxSliceFrozenValue` itself
        match self.unpack() {
            Either::Left(value) => {
                visitor.visit_simple(allocative::Key::new("inline"), std::mem::size_of_val(value));
            }
            Either::Right(allocated) => {
                allocative::Allocative::visit(&allocated, visitor);
            }
        }
    }
}

unsafe impl Send for PackedImpl where FrozenValue: Send {}
unsafe impl Sync for PackedImpl where FrozenValue: Sync {}

/// Optimized version of a `Box<[FrozenValue]>`.
///
/// Specifically, this type uses bit packing and other tricks so that it is only
/// 8 bytes in size, while being allocation free for lengths zero and one. It
/// depends on the lower bit of a FrozenPointer always being unset.
pub struct ThinBoxSliceFrozenValue<'v>(PackedImpl, PhantomData<&'v ()>);

impl<'v> ThinBoxSliceFrozenValue<'v> {
    /// Produces an empty list
    pub const fn empty() -> Self {
        Self(
            PackedImpl::new_allocated(AllocatedThinBoxSlice::empty()),
            PhantomData,
        )
    }
}

impl<'v> Deref for ThinBoxSliceFrozenValue<'v> {
    type Target = [FrozenValue];

    #[inline]
    fn deref<'a>(&'a self) -> &'a Self::Target {
        self.0.as_slice()
    }
}

impl<'v> FromIterator<FrozenValue> for ThinBoxSliceFrozenValue<'v> {
    fn from_iter<I: IntoIterator<Item = FrozenValue>>(iter: I) -> Self {
        Self(PackedImpl::new(iter), PhantomData)
    }
}

impl<'v> allocative::Allocative for ThinBoxSliceFrozenValue<'v> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        allocative::Allocative::visit(&self.0, &mut visitor);
        visitor.exit();
    }
}

impl<'v> Default for ThinBoxSliceFrozenValue<'v> {
    #[inline]
    fn default() -> Self {
        ThinBoxSliceFrozenValue::from_iter(std::iter::empty())
    }
}

impl<'v> std::fmt::Debug for ThinBoxSliceFrozenValue<'v> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[_] as std::fmt::Debug>::fmt(&self, f)
    }
}

impl<'v> PartialEq for ThinBoxSliceFrozenValue<'v> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        <[_] as PartialEq>::eq(&**self, &**other)
    }
}

impl<'v> Eq for ThinBoxSliceFrozenValue<'v> {}

#[cfg(test)]
mod tests {
    use super::ThinBoxSliceFrozenValue;
    use crate::values::int::inline_int::InlineInt;
    use crate::values::FrozenHeap;
    use crate::values::FrozenValue;

    fn across_lengths(a: [FrozenValue; 16]) {
        for len in 0..=16 {
            let val = ThinBoxSliceFrozenValue::from_iter(a.into_iter().take(len));
            assert_eq!(val.len(), len);
            assert_eq!(&*val, &a[..len]);
        }
    }

    #[test]
    fn test_strings() {
        let h = FrozenHeap::new();
        let s: [_; 16] = ["", "abc", "def", "ghijkl"].repeat(4).try_into().unwrap();
        let s = s.map(|s| h.alloc_str(s).to_frozen_value());
        across_lengths(s);
    }

    #[test]
    fn test_ints() {
        let i: [_; 16] = [0, 1, 2, 3, 4, 5, 1000, 1 << 20]
            .repeat(2)
            .try_into()
            .unwrap();
        let i = i.map(|i| FrozenValue::new_int(InlineInt::testing_new(i)));
        across_lengths(i);
    }

    #[test]
    fn test_mixed_types() {
        let a: [_; 16] = [
            FrozenValue::new_none(),
            FrozenValue::new_int(InlineInt::testing_new(0)),
            FrozenValue::new_empty_list(),
            FrozenValue::new_bool(true),
        ]
        .repeat(4)
        .try_into()
        .unwrap();

        across_lengths(a);
    }

    #[test]
    fn test_default() {
        let val = ThinBoxSliceFrozenValue::default();
        assert_eq!(val.len(), 0);
    }
}

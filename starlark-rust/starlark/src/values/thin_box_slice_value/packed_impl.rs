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
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::ptr;

use either::Either;
use static_assertions::assert_eq_align;
use static_assertions::assert_eq_size;

use crate::pagable::StarlarkDeserialize;
use crate::pagable::StarlarkSerialize;
use crate::values::Value;
use crate::values::thin_box_slice_value::thin_box::AllocatedThinBoxSlice;

/// Wrapper to handle the packing and most of the unsafety.
///
/// The word is one of:
///  - a `Value<'v>`, the sole element of a length one slice;
///  - an `AllocatedThinBoxSlice<Value<'v>>`, for every other length.
///
/// They are told apart by the tag in the word's low bits, which an `AllocatedThinBoxSlice`
/// always has and a `Value` never does.
#[repr(transparent)]
struct PackedImpl<'v>(NonZeroUsize, PhantomData<Value<'v>>);

assert_eq_size!(PackedImpl<'static>, Value<'static>);
assert_eq_align!(PackedImpl<'static>, Value<'static>);
assert_eq_size!(PackedImpl<'static>, AllocatedThinBoxSlice<Value<'static>>);
assert_eq_align!(PackedImpl<'static>, AllocatedThinBoxSlice<Value<'static>>);

impl<'v> PackedImpl<'v> {
    const fn new_allocated(allocated: AllocatedThinBoxSlice<Value<'v>>) -> Self {
        let Some(word) = NonZeroUsize::new(allocated.into_inner()) else {
            panic!("an `AllocatedThinBoxSlice`'s word carries a nonzero tag")
        };
        Self(word, PhantomData)
    }

    fn new_inline(value: Value<'v>) -> Self {
        Self(value.ptr_value().0, PhantomData)
    }

    fn new(iter: impl IntoIterator<Item = Value<'v>>) -> Self {
        let mut iter = iter.into_iter();
        let Some(first) = iter.next() else {
            return Self::new_allocated(AllocatedThinBoxSlice::empty());
        };
        let Some(second) = iter.next() else {
            return Self::new_inline(first);
        };
        Self::new_allocated(AllocatedThinBoxSlice::from_iter(
            [first, second].into_iter().chain(iter),
        ))
    }

    fn unpack<'a>(&'a self) -> Either<&'a Value<'v>, &'a AllocatedThinBoxSlice<Value<'v>>> {
        let word = ptr::from_ref(self);
        if AllocatedThinBoxSlice::<Value<'v>>::is_word(self.0.get()) {
            // SAFETY: No `Value` carries this tag, so the word was built by `new_allocated`, from
            // a value of the target type; the types are one word of the same layout.
            Either::Right(unsafe { &*word.cast::<AllocatedThinBoxSlice<Value<'v>>>() })
        } else {
            // SAFETY: Every other word was built by `new_inline`, from the `Value<'v>`'s own
            // word; `Value` is that single word (its size and alignment are asserted above).
            Either::Left(unsafe { &*word.cast::<Value<'v>>() })
        }
    }

    fn as_slice(&self) -> &[Value<'v>] {
        match self.unpack() {
            Either::Left(value) => std::slice::from_ref(value),
            Either::Right(allocated) => allocated,
        }
    }
}

impl<'v> From<Either<Value<'v>, AllocatedThinBoxSlice<Value<'v>>>> for PackedImpl<'v> {
    fn from(value: Either<Value<'v>, AllocatedThinBoxSlice<Value<'v>>>) -> Self {
        match value {
            Either::Left(value) => Self::new_inline(value),
            Either::Right(allocated) => Self::new_allocated(allocated),
        }
    }
}

impl<'v> Drop for PackedImpl<'v> {
    fn drop(&mut self) {
        if let Either::Right(allocated) = self.unpack() {
            // SAFETY: The handle is copied out of a word that is never read again, so its
            // allocation is freed exactly once.
            unsafe { ptr::read(allocated) }.run_drop();
        }
    }
}

impl<'v> allocative::Allocative for PackedImpl<'v> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        // Intentionally don't `enter_self_sized()`, and instead just report the
        // `ThinBoxSliceValue` itself
        match self.unpack() {
            Either::Left(value) => {
                visitor.visit_simple(allocative::Key::new("inline"), std::mem::size_of_val(value));
            }
            Either::Right(allocated) => {
                allocative::Allocative::visit(allocated, visitor);
            }
        }
    }
}

/// A `Box<[Value<'v>]>` in one word.
///
/// Bit packing keeps this pointer-sized and allocation free for lengths zero and one.
pub struct ThinBoxSliceValue<'v>(PackedImpl<'v>);

assert_eq_size!(Option<ThinBoxSliceValue<'static>>, usize);

impl<'v> StarlarkSerialize for ThinBoxSliceValue<'v> {
    fn starlark_serialize(
        &self,
        ctx: &mut dyn crate::pagable::StarlarkSerializeContext,
    ) -> crate::Result<()> {
        self.0.unpack().starlark_serialize(ctx)
    }
}

impl<'v> StarlarkDeserialize for ThinBoxSliceValue<'v> {
    fn starlark_deserialize(
        ctx: &mut dyn crate::pagable::StarlarkDeserializeContext<'_>,
    ) -> crate::Result<Self> {
        let packed =
            <Either<Value<'v>, AllocatedThinBoxSlice<Value<'v>>>>::starlark_deserialize(ctx)?;
        Ok(Self(packed.into()))
    }
}

impl<'v> ThinBoxSliceValue<'v> {
    /// Produces an empty list
    pub const fn empty() -> Self {
        Self(PackedImpl::new_allocated(AllocatedThinBoxSlice::empty()))
    }
}

impl<'v> Deref for ThinBoxSliceValue<'v> {
    type Target = [Value<'v>];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl<'v> FromIterator<Value<'v>> for ThinBoxSliceValue<'v> {
    fn from_iter<I: IntoIterator<Item = Value<'v>>>(iter: I) -> Self {
        Self(PackedImpl::new(iter))
    }
}

impl<'v> allocative::Allocative for ThinBoxSliceValue<'v> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        allocative::Allocative::visit(&self.0, &mut visitor);
        visitor.exit();
    }
}

impl<'v> Default for ThinBoxSliceValue<'v> {
    #[inline]
    fn default() -> Self {
        ThinBoxSliceValue::empty()
    }
}

impl<'v> std::fmt::Debug for ThinBoxSliceValue<'v> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[_] as std::fmt::Debug>::fmt(self, f)
    }
}

impl<'v> PartialEq for ThinBoxSliceValue<'v> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        <[_] as PartialEq>::eq(&**self, &**other)
    }
}

impl<'v> Eq for ThinBoxSliceValue<'v> {}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::AllocatedThinBoxSlice;
    use super::PackedImpl;
    use super::ThinBoxSliceValue;
    use crate::values::FrozenHeap;
    use crate::values::Heap;
    use crate::values::Value;
    use crate::values::layout::pointer::TAG_MASK;

    /// One value for each of the five tags a `Value` can carry.
    ///
    /// The frozen values are brought over as `FrozenValue`s: only their tags matter here.
    fn one_of_each_tag<'v>(heap: Heap<'v>, frozen_heap: FrozenHeap<'_>) -> [Value<'v>; 5] {
        [
            frozen_heap
                .alloc_list(&[])
                .unpack_frozen()
                .unwrap()
                .to_value(),
            heap.alloc_list(&[]),
            Value::testing_new_int(17),
            frozen_heap
                .alloc_str_intern("frozen")
                .to_frozen_value()
                .to_value(),
            heap.alloc_str("unfrozen").to_value(),
        ]
    }

    /// The slice must hand back the very words it was given, so compare identities rather
    /// than `PartialEq`, which is starlark equality.
    fn assert_same_values(actual: &[Value], expected: &[Value]) {
        assert_eq!(actual.len(), expected.len());
        for (a, e) in actual.iter().zip(expected) {
            assert!(a.ptr_eq(*e), "{a:?} != {e:?}");
        }
    }

    fn across_lengths(a: [Value; 16]) {
        for len in 0..=16 {
            let val = ThinBoxSliceValue::from_iter(a.into_iter().take(len));
            assert_same_values(&val, &a[..len]);
        }
    }

    #[test]
    fn test_no_value_is_an_allocated_word() {
        FrozenHeap::temp(|frozen_heap| {
            Heap::temp(|heap| {
                for value in one_of_each_tag(heap, frozen_heap) {
                    assert!(
                        !AllocatedThinBoxSlice::<Value>::is_word(value.ptr_value().0.get()),
                        "{value:?}"
                    );
                }
            });
        });
    }

    #[test]
    fn test_one_of_each_tag() {
        FrozenHeap::temp(|frozen_heap| {
            Heap::temp(|heap| {
                let a: [_; 16] = one_of_each_tag(heap, frozen_heap)
                    .into_iter()
                    .cycle()
                    .take(16)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                across_lengths(a);
            });
        });
    }

    #[test]
    fn test_strings() {
        FrozenHeap::temp(|frozen_heap| {
            Heap::temp(|heap| {
                let s: [_; 16] = ["", "abc", "def", "ghijkl"].repeat(4).try_into().unwrap();
                let s = s.map(|s| heap.alloc_str(s).to_value());
                across_lengths(s);
                let s = s.map(|s| {
                    frozen_heap
                        .alloc_str_intern(s.unpack_str().unwrap())
                        .to_value()
                });
                across_lengths(s);
            });
        });
    }

    #[test]
    fn test_ints() {
        let i: [_; 16] = [0, 1, 2, 3, 4, 5, 1000, 1 << 20]
            .repeat(2)
            .try_into()
            .unwrap();
        let i = i.map(Value::testing_new_int);
        across_lengths(i);
    }

    #[test]
    fn test_mixed_types() {
        let a: [_; 16] = [
            Value::new_none(),
            Value::testing_new_int(0),
            Value::new_empty_list(),
            Value::new_bool(true),
        ]
        .repeat(4)
        .try_into()
        .unwrap();

        across_lengths(a);
    }

    #[test]
    fn test_default() {
        let val = ThinBoxSliceValue::default();
        assert_eq!(val.len(), 0);
    }

    #[test]
    fn test_empty() {
        let val_a = ThinBoxSliceValue::empty();
        let val_b = ThinBoxSliceValue::empty();
        // Check that the empty value is the same for all empty values so that we're not doing extra allocations
        assert_eq!(val_a.0.0, val_b.0.0);

        // Since this and PackedImpl are closely tied together, provide some
        // low-level checks that the representations are what we expect.
        let val_c = PackedImpl::new(std::iter::empty());
        assert_eq!(val_a.0.0, val_c.0);
        assert_eq!(mem::size_of_val(&val_c), mem::size_of::<usize>());
        // The empty slice is the null address under an allocated tag.
        assert_eq!(val_c.0.get() & !TAG_MASK, 0);
        assert!(AllocatedThinBoxSlice::<Value>::is_word(val_c.0.get()));
        assert_eq!(
            val_c.0.get(),
            AllocatedThinBoxSlice::<Value>::empty().into_inner()
        );
    }
}

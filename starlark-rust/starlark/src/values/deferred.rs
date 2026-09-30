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

//! Explicit read boundaries for resident Starlark fields.
//!
//! Constructors and deserializers produce resident values. The one-word
//! representation reserves a deferred tag, and reads use an acquire load and
//! tag check. The wrapper does not own a heap; use `DeferredOwnedFrozen` when
//! the field must retain its owner.

use std::fmt;
use std::marker::PhantomData;
use std::mem;
use std::ptr;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use allocative::Allocative;

use crate::pagable::StarlarkDeserialize;
use crate::pagable::StarlarkDeserializeContext;
use crate::pagable::StarlarkSerialize;
use crate::pagable::StarlarkSerializeContext;
use crate::values::Freeze;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::StarlarkValue;
use crate::values::ThinBoxSliceValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::pointer::TAG_DEFERRED;
use crate::values::layout::pointer::TAG_MASK;

mod private {
    pub trait Sealed {}
}

/// The one-word types a [`Deferred`] can hold.
///
/// # Safety
///
/// `Self` has the size and alignment of one word and never carries
/// `TAG_DEFERRED` in its low bits. A word containing valid `Self` bits
/// therefore supports a reference to `Self`.
pub unsafe trait DeferredWord: Sized + private::Sealed {}

impl<'v> private::Sealed for Value<'v> {}
// SAFETY: `Value` is a transparent non-null tagged pointer, and
// `TAG_DEFERRED` is in `TAGS_NEVER_VALUE`.
unsafe impl<'v> DeferredWord for Value<'v> {}

impl<'v> private::Sealed for Option<Value<'v>> {}
// SAFETY: `Value` is transparent over a non-null word, so its `Option` uses
// zero for `None` and otherwise preserves the representation and reserved tags.
unsafe impl<'v> DeferredWord for Option<Value<'v>> {}

impl<'v, T: StarlarkValue<'v>> private::Sealed for ValueTyped<'v, T> {}
// SAFETY: `ValueTyped` is transparent over `Value`.
unsafe impl<'v, T: StarlarkValue<'v>> DeferredWord for ValueTyped<'v, T> {}

impl<'v> private::Sealed for ThinBoxSliceValue<'v> {}
// SAFETY: the transparent word holds a `Value` or a slice allocation with one
// of the two tags owned by `thin_box`; `TAG_DEFERRED` is the third tag.
unsafe impl<'v> DeferredWord for ThinBoxSliceValue<'v> {}

const _: () = assert!(mem::size_of::<Value<'static>>() == mem::size_of::<usize>());
const _: () = assert!(mem::size_of::<ThinBoxSliceValue<'static>>() == mem::size_of::<usize>());

/// A one-word field accessed through an explicit read boundary.
///
/// All constructors and deserializers produce resident values. `peek` observes
/// availability without materializing, and `read` returns the value. A read
/// fails when a field's stored form cannot be read; no resident field can fail.
/// The owning heap, not this wrapper, keeps the value's pointers alive.
pub struct Deferred<T: DeferredWord> {
    /// Valid `T` bits; zero represents Rust `None` for an optional value.
    ///
    /// Atomic so that shared readers and any future writer meet through the
    /// atomic memory model; a word holding a value is never written again, so a
    /// reference into it taken after an acquire load reads settled bits.
    word: AtomicUsize,
    _marker: PhantomData<T>,
}

// SAFETY: the word stores a valid `T` and is only read through it once it holds
// one; the corresponding auto-trait bound is required of `T`.
unsafe impl<T: DeferredWord + Send> Send for Deferred<T> {}
unsafe impl<T: DeferredWord + Sync> Sync for Deferred<T> {}

impl<T: DeferredWord> Deferred<T> {
    /// Constructs a field with an already available value.
    pub fn new(value: T) -> Self {
        // SAFETY: `DeferredWord` promises a one-word representation.
        let word = unsafe { ptr::from_ref(&value).cast::<usize>().read() };
        mem::forget(value);
        debug_assert!(
            word & TAG_MASK != TAG_DEFERRED,
            "a `DeferredWord` is never tagged deferred"
        );
        Self {
            word: AtomicUsize::new(word),
            _marker: PhantomData,
        }
    }

    /// Returns an available value without materializing it.
    pub fn peek(&self) -> Option<&T> {
        if self.word.load(Ordering::Acquire) & TAG_MASK == TAG_DEFERRED {
            return None;
        }
        // SAFETY: the load observed valid `T` bits, which nothing writes again,
        // so the reference does not race with any other access to the word.
        Some(unsafe { &*self.word.as_ptr().cast::<T>() })
    }

    fn peek_mut(&mut self) -> &mut T {
        // SAFETY: the word holds a valid `T`, and `&mut self` excludes other access.
        unsafe { &mut *ptr::from_mut(self.word.get_mut()).cast::<T>() }
    }

    /// Returns the value. Fails when the field's stored form cannot be read.
    pub fn read(&self) -> crate::Result<&T> {
        Ok(self.resident())
    }

    fn resident(&self) -> &T {
        self.peek()
            .expect("only resident fields can be constructed")
    }

    fn into_inner(self) -> T {
        // SAFETY: the word holds a valid `T`; forgetting `self` transfers its
        // ownership to the returned value without running this wrapper's drop.
        let value = unsafe { self.word.as_ptr().cast::<T>().read() };
        mem::forget(self);
        value
    }
}

impl<'v> Deferred<ThinBoxSliceValue<'v>> {
    /// The number of elements.
    pub fn len(&self) -> usize {
        self.resident().len()
    }

    /// Whether the slice has no elements.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T: DeferredWord + Clone> Clone for Deferred<T> {
    fn clone(&self) -> Self {
        Deferred::new(self.resident().clone())
    }
}

impl<T: DeferredWord> Drop for Deferred<T> {
    fn drop(&mut self) {
        // SAFETY: the word stores the valid `T` this wrapper owns, and `&mut self`
        // excludes other access.
        unsafe { ptr::drop_in_place(ptr::from_mut(self.word.get_mut()).cast::<T>()) }
    }
}

impl<T: DeferredWord + fmt::Debug> fmt::Debug for Deferred<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Deliberately never reads: a `{:?}` reaching an unread field is the accident this
        // type exists to avoid.
        match self.peek() {
            Some(value) => fmt::Debug::fmt(value, f),
            None => f.write_str("<unread>"),
        }
    }
}

impl<T: DeferredWord + Allocative> Allocative for Deferred<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if let Some(value) = self.peek() {
            visitor.visit_field(allocative::Key::new("value"), value);
        }
        visitor.exit();
    }
}

// SAFETY: every field is resident, so tracing observes every value it contains.
unsafe impl<'v, T: DeferredWord + Trace<'v>> Trace<'v> for Deferred<T> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.peek_mut().trace(tracer);
    }
}

impl<'v, T> Freeze<'v> for Deferred<T>
where
    T: DeferredWord + Freeze<'v>,
    for<'fv> T::Frozen<'fv>: DeferredWord,
{
    type Frozen<'fv> = Deferred<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(Deferred::new(self.into_inner().freeze(freezer)?))
    }
}

impl<T: DeferredWord + StarlarkSerialize> StarlarkSerialize for Deferred<T> {
    fn starlark_serialize(&self, ctx: &mut dyn StarlarkSerializeContext) -> crate::Result<()> {
        self.read()?.starlark_serialize(ctx)
    }
}

impl<'fv, T: DeferredWord + StarlarkDeserialize<'fv>> StarlarkDeserialize<'fv> for Deferred<T> {
    fn starlark_deserialize(
        ctx: &mut dyn StarlarkDeserializeContext<'_, 'fv>,
    ) -> crate::Result<Self> {
        Ok(Deferred::new(T::starlark_deserialize(ctx)?))
    }
}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::*;
    use crate::values::FrozenHeap;
    use crate::values::Heap;

    #[test]
    fn test_size() {
        assert_eq!(mem::size_of::<Deferred<Value>>(), mem::size_of::<usize>());
        assert_eq!(
            mem::size_of::<Deferred<ThinBoxSliceValue>>(),
            mem::size_of::<usize>()
        );
        assert_eq!(
            mem::size_of::<Deferred<Option<Value>>>(),
            mem::size_of::<usize>()
        );
        assert_eq!(
            mem::size_of::<Option<Deferred<Value>>>(),
            2 * mem::size_of::<usize>()
        );
    }

    #[test]
    fn test_resident_value() {
        Heap::temp(|heap| {
            let value = heap.alloc_str("resident").to_value();
            let deferred = Deferred::new(value);
            assert!(deferred.peek().unwrap().ptr_eq(value));
            assert!(deferred.read().unwrap().ptr_eq(value));
            assert_eq!(format!("{deferred:?}"), format!("{value:?}"));
        });
    }

    #[test]
    fn test_optional_value_preserves_starlark_none() {
        let absent = Deferred::new(None::<Value>);
        let present = Deferred::new(Some(Value::new_none()));
        assert!(absent.read().unwrap().is_none());
        assert!(
            present
                .read()
                .unwrap()
                .expect("present Starlark None")
                .is_none()
        );
        assert!(absent.clone().read().unwrap().is_none());
        assert!(present.clone().read().unwrap().is_some());
    }

    #[test]
    fn test_resident_slice() {
        Heap::temp(|heap| {
            let values = [
                heap.alloc_str("a").to_value(),
                Value::testing_new_int(1),
                heap.alloc_list(&[]),
            ];
            let deferred = Deferred::new(ThinBoxSliceValue::from_iter(values));
            assert_eq!(deferred.len(), 3);
            for (got, want) in deferred.read().unwrap().iter().zip(values) {
                assert!(got.ptr_eq(want));
            }
        });
    }

    #[test]
    fn test_resident_typed_is_one_word() {
        FrozenHeap::temp(|heap| {
            let s = heap.alloc_str("typed");
            let typed: ValueTyped<crate::values::string::str_type::StarlarkStr> =
                ValueTyped::new(s.to_value()).unwrap();
            let deferred = Deferred::new(typed);
            assert_eq!(mem::size_of_val(&deferred), mem::size_of::<usize>());
            assert!(deferred.read().unwrap().to_value().ptr_eq(s.to_value()));
        });
    }

    #[test]
    fn test_resident_drops_its_value() {
        // A resident slice owns its allocation; dropping the field frees it exactly once
        // (under ASAN a double free or a leak fails the test).
        Heap::temp(|heap| {
            let values: Vec<_> = (0..4)
                .map(|i| heap.alloc_str(&i.to_string()).to_value())
                .collect();
            let deferred = Deferred::new(ThinBoxSliceValue::from_iter(values));
            assert_eq!(deferred.len(), 4);
            drop(deferred);
        });
    }
}

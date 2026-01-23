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

use std::cell::Cell;
use std::cell::RefCell;
use std::cell::RefMut;
use std::cmp;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::mem;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr;
use std::sync::Arc;

use allocative::Allocative;
use bumpalo::Bump;
use dupe::Dupe;
use dupe::IterDupedExt;
use starlark_map::small_set::SmallSet;

use crate::cast;
use crate::cast::transmute;
use crate::collections::StarlarkHashValue;
use crate::eval::runtime::profile::instant::ProfilerInstant;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenStringValue;
use crate::values::FrozenValueOfUnchecked;
use crate::values::FrozenValueTyped;
use crate::values::HeapSendable;
use crate::values::OwnedFrozenValue;
use crate::values::OwnedFrozenValueTyped;
use crate::values::StarlarkValue;
use crate::values::StringValue;
use crate::values::Trace;
use crate::values::UnpackValue;
use crate::values::ValueOf;
use crate::values::ValueOfUnchecked;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::allocator::alloc::allocator::ChunkAllocator;
use crate::values::layout::heap::arena::Arena;
use crate::values::layout::heap::arena::ArenaVisitor;
use crate::values::layout::heap::arena::Reservation;
use crate::values::layout::heap::call_enter_exit::CallEnter;
use crate::values::layout::heap::call_enter_exit::CallExit;
use crate::values::layout::heap::call_enter_exit::NeedsDrop;
use crate::values::layout::heap::call_enter_exit::NoDrop;
use crate::values::layout::heap::fast_cell::FastCell;
use crate::values::layout::heap::profile::by_type::HeapSummary;
use crate::values::layout::heap::repr::AValueOrForwardUnpack;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value::FrozenValue;
use crate::values::layout::value::Value;
use crate::values::string::intern::interner::FrozenStringValueInterner;
use crate::values::string::intern::interner::StringValueInterner;

#[derive(Copy, Clone, Dupe)]
pub(crate) enum HeapKind {
    Unfrozen,
    Frozen,
}

/// An owned heap on which [`Value`]s can be allocated.
///
/// This type owns a [`Heap`] and derefs to it for convenience. Use [`OwnedHeap::new`]
/// to create a new heap.
pub struct OwnedHeap {
    /// Peak memory seen when a garbage collection takes place (may be lower than currently allocated)
    peak_allocated: Cell<usize>,
    arena: FastCell<Arena<Bump>>,
    str_interner: RefCell<StringValueInterner<'static>>,
    /// Memory I depend on.
    refs: RefCell<SmallSet<FrozenHeapRef>>,
}

impl Debug for OwnedHeap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.as_ref(), f)
    }
}

impl Default for OwnedHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl OwnedHeap {
    /// Create a new [`OwnedHeap`].
    pub fn new() -> Self {
        Self {
            peak_allocated: Default::default(),
            arena: Default::default(),
            str_interner: Default::default(),
            refs: Default::default(),
        }
    }

    /// Get access to the underlying [`Heap`].
    pub fn as_ref<'v>(&'v self) -> Heap<'v> {
        Heap(self, PhantomData)
    }
}

/// A heap on which [`Value`]s can be allocated. The values will be annotated with the heap lifetime.
#[derive(Copy, Clone, Dupe)]
// `PhantomData` is needed to make the type invariant in `'v` - without that, branding doesn't mean
// anything.
pub struct Heap<'v>(&'v OwnedHeap, PhantomData<fn(&'v ()) -> &'v ()>);

impl<'v> Debug for Heap<'v> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut x = f.debug_struct("Heap");
        x.field(
            "bytes",
            &self.0.arena.try_borrow().map(|x| x.allocated_bytes()),
        );
        x.finish()
    }
}

impl<'v> Heap<'v> {
    /// Create a heap and use it within the closure
    ///
    /// Heap is discarded at the end of the closure.
    pub fn temp<F, R>(f: F) -> R
    where
        F: for<'v2> FnOnce(Heap<'v2>) -> R,
    {
        let heap = OwnedHeap::new();
        f(heap.as_ref())
    }

    pub(in crate::values::layout) fn string_interner(self) -> RefMut<'v, StringValueInterner<'v>> {
        // SAFETY: The lifetime of the interner is the lifetime of the heap.
        unsafe {
            transmute!(
                RefMut<'v, StringValueInterner<'static>>,
                RefMut<'v, StringValueInterner<'v>>,
                self.0.str_interner.borrow_mut()
            )
        }
    }

    pub(crate) fn trace_interner(self, tracer: &Tracer<'v>) {
        self.string_interner().trace(tracer);
    }

    pub(crate) fn referenced_heaps(self) -> Vec<FrozenHeapRef> {
        self.0.refs.borrow().iter().duped().collect()
    }

    /// Get access to the underlying value within the context of this heap.
    ///
    /// Adds the frozen value's heap as a dependency of this heap.
    ///
    /// See the `branding` module for more details.
    pub fn access_owned_frozen_value(self, v: &OwnedFrozenValue) -> Value<'v> {
        let fh = v.owner();
        let mut refs = self.0.refs.borrow_mut();
        if !refs.contains(fh) {
            refs.insert(fh.dupe());
        }

        // SAFETY: We just added a reference to this heap
        unsafe { v.unchecked_frozen_value().to_value() }
    }

    /// Similar to `access_owned_frozen_value`, but typed.
    pub fn access_owned_frozen_value_typed<T: for<'a> StarlarkValue<'a>>(
        self,
        v: &OwnedFrozenValueTyped<T>,
    ) -> ValueTyped<'v, T> {
        let fh = v.owner();
        let mut refs = self.0.refs.borrow_mut();
        if !refs.contains(fh) {
            refs.insert(fh.dupe());
        }

        // SAFETY: We just added a reference to this heap
        unsafe { v.value_typed().to_value_typed() }
    }
}

/// A heap on which [`FrozenValue`]s can be allocated.
/// Can be kept alive by a [`FrozenHeapRef`].
#[derive(Default)]
pub struct FrozenHeap {
    /// My memory.
    arena: Arena<ChunkAllocator>,
    /// Memory I depend on.
    refs: RefCell<SmallSet<FrozenHeapRef>>,
    /// String interner.
    str_interner: RefCell<FrozenStringValueInterner>,
}

/// `FrozenHeap` when it is no longer modified and can be share between threads.
/// Although, `arena` is not safe to share between threads, but at least `refs` is.
#[derive(Default, Allocative)]
#[allow(clippy::non_send_fields_in_send_ty)]
struct FrozenFrozenHeap {
    arena: Arena<ChunkAllocator>,
    refs: Box<[FrozenHeapRef]>,
}

// Safe because we never mutate the Arena other than with &mut
unsafe impl Sync for FrozenFrozenHeap {}
unsafe impl Send for FrozenFrozenHeap {}

impl Debug for FrozenHeap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut x = f.debug_struct("FrozenHeap");
        x.field("bytes", &self.arena.allocated_bytes());
        x.field("refs", &self.refs.try_borrow().map(|x| x.len()));
        x.finish()
    }
}

impl Debug for FrozenFrozenHeap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut x = f.debug_struct("FrozenHeap");
        x.field("bytes", &self.arena.allocated_bytes());
        x.field("refs", &self.refs.len());
        x.finish()
    }
}

/// A reference to a [`FrozenHeap`] that keeps alive all values on the underlying heap.
/// Note that the [`Hash`] is consistent for a single [`FrozenHeapRef`], but non-deterministic
/// across executions and distinct but observably identical [`FrozenHeapRef`] values.
#[derive(Default, Clone, Dupe, Debug, Allocative)]
// The Eq/Hash are by pointer rather than value, since we produce unique values
// given an underlying FrozenHeap.
pub struct FrozenHeapRef(Option<Arc<FrozenFrozenHeap>>);

fn _test_frozen_heap_ref_send_sync()
where
    FrozenHeapRef: Send + Sync,
{
}

impl Hash for FrozenHeapRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(arc) = &self.0 {
            let x: &FrozenFrozenHeap = Deref::deref(&arc);
            ptr::hash(x, state);
        }
    }
}

impl PartialEq<FrozenHeapRef> for FrozenHeapRef {
    fn eq(&self, other: &FrozenHeapRef) -> bool {
        match (&self.0, &other.0) {
            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
            (None, None) => true,
            (Some(_), None) | (None, Some(_)) => false,
        }
    }
}

impl Eq for FrozenHeapRef {}

impl FrozenHeapRef {
    /// Number of bytes allocated on this heap, not including any memory
    /// allocated outside of the starlark heap.
    pub fn allocated_bytes(&self) -> usize {
        self.0.as_ref().map_or(0, |a| a.arena.allocated_bytes())
    }

    /// Number of bytes allocated by the heap but not filled.
    /// Note that these bytes will _never_ be filled as no further allocations can
    /// be made on this heap (it has been sealed).
    pub fn available_bytes(&self) -> usize {
        self.0.as_ref().map_or(0, |a| a.arena.available_bytes())
    }

    /// Obtain a summary of how much memory is currently allocated by this heap.
    /// Doesn't include the heaps it keeps alive by reference.
    pub fn allocated_summary(&self) -> HeapSummary {
        self.0
            .as_ref()
            .map_or_else(HeapSummary::default, |a| a.arena.allocated_summary())
    }
}

impl FrozenHeap {
    /// Create a new [`FrozenHeap`].
    pub fn new() -> Self {
        Self::default()
    }

    /// After all values have been allocated, convert the [`FrozenHeap`] into a
    /// [`FrozenHeapRef`] which can be [`clone`](Clone::clone)d, shared between threads,
    /// and ensures the underlying values allocated on the [`FrozenHeap`] remain valid.
    pub fn into_ref(self) -> FrozenHeapRef {
        let FrozenHeap {
            mut arena, refs, ..
        } = self;
        arena.finish();
        let refs = refs.into_inner();
        if arena.is_empty() && refs.is_empty() {
            FrozenHeapRef::default()
        } else {
            FrozenHeapRef(Some(Arc::new(FrozenFrozenHeap {
                arena,
                refs: refs.into_iter().collect(),
            })))
        }
    }

    /// Keep the argument [`FrozenHeapRef`] alive as long as this [`FrozenHeap`]
    /// is kept alive. Used if a [`FrozenValue`] in this heap points at values in another
    /// [`FrozenHeap`].
    pub fn add_reference(&self, heap: &FrozenHeapRef) {
        if heap.0.is_none() {
            return;
        }

        let mut refs = self.refs.borrow_mut();
        if !refs.contains(heap) {
            refs.insert(heap.dupe());
        }
    }

    pub(in crate::values::layout) fn string_interner(
        &self,
    ) -> RefMut<'_, FrozenStringValueInterner> {
        self.str_interner.borrow_mut()
    }

    pub(in crate::values::layout) fn alloc_raw<'fv, T>(
        &'fv self,
        x: AValueImpl<'fv, T>,
    ) -> FrozenValueTyped<'fv, T::StarlarkValue>
    where
        T: AValue<'fv, ExtraElem = ()>,
        T::StarlarkValue: HeapSendable<'fv>,
        T::StarlarkValue: HeapSyncable<'fv>,
    {
        let v: &AValueRepr<AValueImpl<T>> = self.arena.alloc(x);
        FrozenValueTyped::new_repr(v)
    }

    pub(in crate::values::layout) fn alloc_raw_extra<'fv, T>(
        &'fv self,
        x: AValueImpl<'fv, T>,
    ) -> (
        FrozenValueTyped<'fv, T::StarlarkValue>,
        *mut [MaybeUninit<T::ExtraElem>],
    )
    where
        T: AValue<'fv>,
        T::StarlarkValue: HeapSendable<'fv>,
        T::StarlarkValue: HeapSyncable<'fv>,
    {
        let (v, extra) = self.arena.alloc_extra(x);
        let v = unsafe { FrozenValueTyped::new_repr(&*v) };
        (v, extra)
    }

    #[inline]
    pub(in crate::values::layout) fn alloc_str_init(
        &self,
        len: usize,
        hash: StarlarkHashValue,
        init: impl FnOnce(*mut u8),
    ) -> FrozenStringValue {
        let v = self.arena.alloc_str_init(len, hash, init);

        unsafe {
            let value = FrozenValue::new_ptr(&*v, true);
            FrozenStringValue::new_unchecked(value)
        }
    }

    /// Allocate a new value on a [`FrozenHeap`].
    pub fn alloc<T: AllocFrozenValue>(&self, val: T) -> FrozenValue {
        val.alloc_frozen_value(self)
    }

    /// Allocate a value and return [`ValueOfUnchecked`] of it.
    pub fn alloc_typed_unchecked<T: AllocFrozenValue>(
        &self,
        val: T,
    ) -> FrozenValueOfUnchecked<'static, T> {
        FrozenValueOfUnchecked::new(val.alloc_frozen_value(self))
    }

    /// Number of bytes allocated on this heap, not including any memory
    /// allocated outside of the starlark heap.
    pub fn allocated_bytes(&self) -> usize {
        self.arena.allocated_bytes()
    }

    /// Number of bytes allocated by the heap but not yet filled.
    pub fn available_bytes(&self) -> usize {
        self.arena.available_bytes()
    }

    /// Obtain a summary of how much memory is currently allocated by this heap.
    pub fn allocated_summary(&self) -> HeapSummary {
        self.arena.allocated_summary()
    }

    pub(crate) fn reserve_with_extra<'v, 'v2, T: AValue<'v2>>(
        &'v self,
        extra_len: usize,
    ) -> (
        FrozenValue,
        Reservation<'v2, T>,
        *mut [MaybeUninit<T::ExtraElem>],
    ) {
        let (r, extra) = self.arena.reserve_with_extra::<T>(extra_len);
        let fv = FrozenValue::new_ptr(unsafe { cast::ptr_lifetime(r.ptr()) }, false);
        (fv, r, extra)
    }
}

impl<'v> Heap<'v> {
    /// Number of bytes allocated on this heap, not including any memory
    /// allocated outside of the starlark heap.
    pub fn allocated_bytes(self) -> usize {
        self.0.arena.borrow().allocated_bytes()
    }

    /// Peak memory allocated to this heap, even if the value is now lower
    /// as a result of a subsequent garbage collection.
    pub fn peak_allocated_bytes(&self) -> usize {
        cmp::max(self.allocated_bytes(), self.0.peak_allocated.get())
    }

    /// Number of bytes allocated by the heap but not yet filled.
    pub fn available_bytes(&self) -> usize {
        self.0.arena.borrow().available_bytes()
    }

    pub(in crate::values::layout) fn alloc_raw<A>(
        self,
        x: AValueImpl<'v, A>,
    ) -> ValueTyped<'v, A::StarlarkValue>
    where
        A: AValue<'v, ExtraElem = ()>,
        A::StarlarkValue: HeapSendable<'v>,
    {
        let arena = self.0.arena.borrow();
        let v: &AValueRepr<_> = arena.alloc(x);
        ValueTyped::new_repr(v)
    }

    pub(in crate::values::layout) fn alloc_raw_extra<A>(
        self,
        x: AValueImpl<'v, A>,
    ) -> (
        ValueTyped<'v, A::StarlarkValue>,
        *mut [MaybeUninit<A::ExtraElem>],
    )
    where
        A: AValue<'v>,
        A::StarlarkValue: HeapSendable<'v>,
    {
        let arena = self.0.arena.borrow();
        let (v, extra) = arena.alloc_extra(x);
        let v = unsafe { ValueTyped::new_repr(&*v) };
        (v, extra)
    }

    pub(in crate::values::layout) fn alloc_str_init(
        self,
        len: usize,
        hash: StarlarkHashValue,
        init: impl FnOnce(*mut u8),
    ) -> StringValue<'v> {
        let arena = self.0.arena.borrow();
        let v = arena.alloc_str_init(len, hash, init);

        // We have an arena inside a RefCell which stores ValueMem<'v>
        // However, we promise not to clear the RefCell other than for GC
        // so we can make the `arena` available longer
        unsafe {
            let value = Value::new_ptr(&*v, true);
            StringValue::new_unchecked(value)
        }
    }

    /// Allocate a new value on a [`Heap`].
    pub fn alloc<T: AllocValue<'v>>(self, x: T) -> Value<'v> {
        x.alloc_value(self)
    }

    /// Allocate a value and return [`ValueTyped`] of it.
    /// Can fail if the [`AllocValue`] trait generates a different type on the heap.
    pub fn alloc_typed<T: AllocValue<'v> + StarlarkValue<'v>>(self, x: T) -> ValueTyped<'v, T> {
        ValueTyped::new(self.alloc(x)).expect("just allocated value must have the right type")
    }

    /// Allocate a value and return [`ValueOfUnchecked`] of it.
    pub fn alloc_typed_unchecked<T: AllocValue<'v>>(self, x: T) -> ValueOfUnchecked<'v, T> {
        ValueOfUnchecked::new(self.alloc(x))
    }

    /// Allocate a value and return [`ValueOf`] of it.
    pub fn alloc_value_of<T>(self, x: T) -> ValueOf<'v, &'v T>
    where
        T: AllocValue<'v>,
        &'v T: UnpackValue<'v>,
    {
        let value = self.alloc(x);
        ValueOf::unpack_value(value)
            .unwrap()
            .expect("just allocate value must be unpackable to the type of value")
    }

    pub(crate) unsafe fn visit_arena(
        self,
        forward_heap_kind: HeapKind,
        v: &mut impl ArenaVisitor<'v>,
    ) {
        unsafe { (*self.0.arena.get_mut()).visit_arena(HeapKind::Unfrozen, forward_heap_kind, v) }
    }

    /// Garbage collect any values that are unused. This function is _unsafe_ in
    /// the sense that any `Value<'v>` not returned by `Tracer` _will become
    /// invalid_. Furthermore, any references to values, e.g `&'v str` will
    /// also become invalid.
    pub(crate) unsafe fn garbage_collect(self, f: impl FnOnce(&Tracer<'v>)) {
        unsafe {
            // Record the highest peak, so it never decreases
            self.0.peak_allocated.set(self.peak_allocated_bytes());
            self.garbage_collect_internal(f)
        }
    }

    unsafe fn garbage_collect_internal(self, f: impl FnOnce(&Tracer<'v>)) {
        unsafe {
            // Must rewrite all Value's so they point at the new heap.
            // Take the arena out of the heap to make sure nobody allocates in it,
            // but hold the reference until the GC is done.
            let _arena = self.0.arena.take();

            let tracer = Tracer::<'v> {
                arena: Arena::default(),
                phantom: PhantomData,
            };
            f(&tracer);
            self.0.arena.set(tracer.arena);
        }
    }

    /// Obtain a summary of how much memory is currently allocated by this heap.
    pub fn allocated_summary(self) -> HeapSummary {
        self.0.arena.borrow().allocated_summary()
    }

    pub(crate) fn record_call_enter(self, function: Value<'v>) {
        let time = ProfilerInstant::now();
        assert!(mem::needs_drop::<CallEnter<NeedsDrop>>());
        assert!(!mem::needs_drop::<CallEnter<NoDrop>>());
        self.alloc_complex_no_freeze(CallEnter {
            function,
            time,
            maybe_drop: NeedsDrop,
        });
        self.alloc_complex_no_freeze(CallEnter {
            function,
            time,
            maybe_drop: NoDrop,
        });
    }

    pub(crate) fn record_call_exit(self) {
        let time = ProfilerInstant::now();
        assert!(mem::needs_drop::<CallExit<NeedsDrop>>());
        assert!(!mem::needs_drop::<CallExit<NoDrop>>());
        self.alloc_simple(CallExit {
            time,
            maybe_drop: NeedsDrop,
        });
        self.alloc_simple(CallExit {
            time,
            maybe_drop: NoDrop,
        });
    }
}

/// Used to perform garbage collection by [`Trace::trace`](crate::values::Trace::trace).
pub struct Tracer<'v> {
    arena: Arena<Bump>,
    phantom: PhantomData<&'v ()>,
}

impl<'v> Tracer<'v> {
    /// Walk over a value during garbage collection.
    pub fn trace(&self, value: &mut Value<'v>) {
        *value = self.adjust(*value)
    }

    /// Helper function to annotate that this field has been considered for tracing,
    /// but is not relevant because it has a static lifetime containing no relevant values.
    /// Does nothing.
    pub fn trace_static<T: ?Sized + 'static>(&self, value: &T) {
        // Nothing to do because T can't contain the lifetime 'v
        let _ = value;
    }

    pub(crate) fn reserve<T: AValue<'v, ExtraElem = ()>>(&self) -> (Value<'v>, Reservation<'v, T>) {
        let (v, r, extra) = self.reserve_with_extra::<T>(0);
        let extra = unsafe { &mut *extra };
        debug_assert!(extra.is_empty());
        (v, r)
    }

    pub(crate) fn reserve_with_extra<T: AValue<'v>>(
        &self,
        extra_len: usize,
    ) -> (
        Value<'v>,
        Reservation<'v, T>,
        *mut [MaybeUninit<T::ExtraElem>],
    ) {
        assert!(!T::IS_STR, "strings cannot be reserved");
        let (r, extra) = self.arena.reserve_with_extra::<T>(extra_len);
        let v = Value::new_ptr(unsafe { cast::ptr_lifetime(r.ptr()) }, false);
        (v, r, extra)
    }

    pub(crate) fn alloc_str(&self, x: &str) -> Value<'v> {
        let v = self.arena.alloc_str(x);
        unsafe { Value::new_ptr(&*v, true) }
    }

    fn adjust(&self, value: Value<'v>) -> Value<'v> {
        // Case 1, doesn't point at the old arena
        if !value.0.is_unfrozen() {
            return value;
        }
        let old_val = value.0.unpack_ptr().unwrap();

        // Case 2: We have already been replaced with a forwarding, or need to freeze
        match old_val.unpack() {
            AValueOrForwardUnpack::Forward(x) => unsafe { x.forward_ptr().unpack_unfrozen_value() },
            AValueOrForwardUnpack::Header(v) => unsafe { v.unpack().heap_copy(self) },
        }
    }
}

#[cfg(test)]
mod tests {
    use starlark_derive::starlark_module;

    use super::FrozenHeapRef;
    use super::Heap;
    use crate as starlark;
    use crate::assert::Assert;
    use crate::environment::GlobalsBuilder;
    use crate::values::StringValue;

    #[test]
    fn test_send_sync()
    where
        FrozenHeapRef: Send + Sync,
    {
    }

    #[test]
    fn test_string_reallocated_on_heap() {
        Heap::temp(|heap| {
            let first = heap.alloc_str("xx");
            let second = heap.alloc_str("xx");
            assert!(
                !first.to_value().ptr_eq(second.to_value()),
                "Plain allocations should recreate values. Note assertion negation."
            );
        });
    }

    #[test]
    fn test_interned_string_equal() {
        Heap::temp(|heap| {
            let first = heap.alloc_str_intern("xx");
            let second = heap.alloc_str_intern("xx");
            assert!(
                first.to_value().ptr_eq(second.to_value()),
                "Interned allocations should be equal."
            );
        });
    }

    #[starlark_module]
    fn validate_str_interning(globals: &mut GlobalsBuilder) {
        fn append_x<'v>(str: StringValue<'v>, heap: Heap<'v>) -> anyhow::Result<StringValue<'v>> {
            Ok(heap.alloc_str_intern(&(str.as_str().to_owned() + "x")))
        }
    }

    #[test]
    fn test_interned_str_starlark() {
        let mut a = Assert::new();
        a.globals_add(validate_str_interning);

        a.pass(
            r#"
x = append_x("foo")
assert_eq(x, "foox")
garbage_collect()
assert_eq(x, "foox")
        "#,
        );
    }
}

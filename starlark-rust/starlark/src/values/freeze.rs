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

//! Converting values from a mutable [`Heap`](crate::values::Heap) into a
//! [`FrozenHeap`](crate::values::FrozenHeap).
//!
//! Freezing uses the same forwarding scheme as the moving garbage collector to
//! preserve sharing and cycles. [`Freezer::freeze`] first returns pointer-encoded
//! or already-frozen values unchanged and follows an existing forwarding record.
//! For a live heap allocation, it dispatches through that allocation's AValue
//! vtable.
//!
//! # Choosing a freeze path
//!
//! A value can provide its frozen representation in several ways:
//!
//! * Built-in AValue layouts, such as lists and tuples, can implement their
//!   complete `heap_freeze` operation directly when their trailing storage needs
//!   specialized handling.
//! * A value allocated with [`Heap::alloc_complex`](crate::values::Heap::alloc_complex)
//!   implements [`FreezeDynamic`]. Most such values implement [`Freeze`]; the
//!   blanket [`FreezeDynamic`] implementation selects one statically known frozen Rust
//!   type. A `Freeze` implementation can reuse an existing frozen [`Value`] from
//!   [`Freeze::prepare_freeze`] instead of allocating it. A direct
//!   [`FreezeDynamic`] implementation can inspect the source and select its frozen
//!   type and allocation size at runtime.
//!
//! Every result must be equal to the source value and produce the same hash.
//!
//! # Allocating and publishing a frozen value
//!
//! Freezing a value copies it to the frozen heap and leaves a forwarding record in its
//! place, so that every other value pointing at it finds the copy. The forward has to
//! be in place *before* the value's fields are frozen, because a field may point back
//! at the value. Writing the forward takes the copy's address, so its space has to be
//! reserved first, and that takes its size. When the frozen type is fixed, the size is
//! known from the type; when it is chosen at runtime, the value has to be looked at
//! first. That is the whole reason for the shape of [`FreezeDynamic`]: freezing is a
//! look and then a consume, with the reservation and the forward between them.
//!
//! ```text
//!        unfrozen heap                                  frozen heap
//!  src: [ vtable | payload: T ]
//!
//!  1  plan = payload.prepare_freeze(freezer)   look, don't consume: reuse an existing
//!                                              value, or allocate for some type and size
//!  2  plan.target().reserve(freezer)           [ RESERVED(size) | ........ ]  a FreezeSlot
//!                                              (an existing value: forward src to it, done)
//!  3  payload = overwrite_with_forward(src)    [ FORWARD -> slot ]  read out first: the
//!                                              forward is two words and clobbers its start
//!  4  plan.freeze_into(payload, freezer, slot) freezes the fields (`Freezer::freeze`
//!                                              follows forwards, possibly into this very
//!                                              slot), builds the frozen value, and
//!                                              `slot.write(it)`, which publishes:
//!                                              [ vtable | frozen value ]  a live Value<'fv>
//! ```
//!
//! The driver is `AValueComplex::heap_freeze`. What is checked, and where:
//!
//! * [`FreezeSlot::write`] checks that what it is given is exactly what was reserved:
//!   the same allocation size, because heap walks stride by the published header's
//!   size, and the same drop region, because a destructor outside it never runs. A
//!   plan that selects one target and writes another gets a [`FreezeError`], not
//!   undefined behaviour. (Alignment needs no check: every value type is bounded by
//!   the arena's alignment at compile time.)
//! * The driver checks that the value a plan returns is the slot it was given, and
//!   that the slot is published. A plan can come by the slot's address before writing
//!   it, because a field that pointed back at the source freezes to the forward, so the
//!   returned value alone is not evidence of a write.
//! * Until `write` publishes the value, the reservation word stays in place. If a plan
//!   fails or unwinds, the frozen heap stays walkable and droppable. The sources already
//!   forwarded point at a reservation, however, so the whole freeze is abandoned;
//!   debug builds enforce this.
//!
//! The blanket [`FreezeDynamic`] implementation for [`Freeze`] follows the same
//! protocol: its plan selects `Freeze::Frozen` at the frozen heap's brand as a simple
//! target, and [`Freeze::freeze`] builds the value that `write` publishes.

use std::cell::OnceCell;
use std::cell::RefCell;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::ptr::NonNull;

use starlark_map::Hashed;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_syntax::slice_vec_ext::VecExt;

use crate::values::FreezeError;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::HeapSendable;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueSimpleBound;
use crate::values::layout::avalues::simple::AValueSimple;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueHeapEntry;
use crate::values::layout::heap::repr::AValueHeapEntryState;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value_alloc_size::ValueAllocSize;

/// Need to be implemented for non-simple `StarlarkValue`.
///
/// This is called on freeze of the heap. Must produce a replacement object to place
/// in the frozen heap.
///
/// `'v` is the brand of the heap the value is frozen out of, like the `'v` of
/// [`Trace<'v>`](crate::values::Trace): a type branded by `'v` implements `Freeze<'v>`, a
/// type that holds no values implements it for every `'v`.
///
/// For relatively simple cases it can be implemented with `#[derive(Freeze)]`:
///
/// ```
/// # struct AdditionalData;
///
/// use starlark::values::Freeze;
/// use starlark::values::Value;
///
/// #[derive(Freeze)]
/// struct MyType<'v> {
///     value: Value<'v>,
///     // This field does not implement `Freeze`, but we can use it as is for freeze.
///     #[freeze(identity)]
///     data: AdditionalData,
/// }
/// ```
pub trait Freeze<'v> {
    /// When type is frozen, it is frozen into this type.
    type Frozen<'fv>;

    /// Selects whether to allocate `Frozen` or reuse an existing frozen value.
    ///
    /// This is called while the source is still intact. Most implementations
    /// should use the default allocation plan.
    fn prepare_freeze<'fv>(
        &self,
        _freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<StaticFreezePlan<'v, 'fv, Self>>
    where
        Self: Sized,
        Self::Frozen<'fv>: StarlarkValue<'fv>,
    {
        Ok(StaticFreezePlan::allocate())
    }

    /// Freeze a value. The frozen value _must_ be equal to the original,
    /// and produce the same hash.
    ///
    /// Note during freeze, `Value` objects in `Self` might be already special forward-objects,
    /// trying to unpack these objects will crash the process.
    /// So the function is only allowed to access `Value` objects after it froze them.
    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>>;
}

/// Destination selected by [`Freeze::prepare_freeze`].
pub struct StaticFreezePlan<'v, 'fv, T>
where
    T: Freeze<'v>,
    T::Frozen<'fv>: StarlarkValue<'fv>,
{
    direct: Option<ValueTyped<'fv, T::Frozen<'fv>>>,
    marker: PhantomData<fn(&'v ()) -> T>,
}

impl<'v, 'fv, T> StaticFreezePlan<'v, 'fv, T>
where
    T: Freeze<'v>,
    T::Frozen<'fv>: StarlarkValue<'fv>,
{
    /// Allocates a new `T::Frozen` in the frozen heap. This is the default.
    pub fn allocate() -> Self {
        Self {
            direct: None,
            marker: PhantomData,
        }
    }

    /// Forwards to `value` instead of allocating a new frozen value.
    ///
    /// The value must be equal to the source and produce the same hash, like
    /// any freeze result, and must be owned by a heap that outlives the
    /// freeze destination — typically a statically allocated value.
    ///
    /// FIXME(JakobDegen): We may want to make it possible to *only* freeze
    /// directly — a [`Freeze::prepare_freeze`] that always returns a
    /// direct plan, with no by-value `freeze` implementation for the type.
    pub fn direct(value: ValueTyped<'fv, T::Frozen<'fv>>) -> Self {
        Self {
            direct: Some(value),
            marker: PhantomData,
        }
    }
}

impl<'v, T> FreezeDynamic<'v> for T
where
    T: Freeze<'v>,
    for<'a> T::Frozen<'a>: AValueSimpleBound<'a>,
{
    type Plan<'fv> = StaticFreezePlan<'v, 'fv, T>;

    fn prepare_freeze<'fv>(&self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Plan<'fv>> {
        Freeze::prepare_freeze(self, freezer)
    }
}

impl<'v, 'fv, T> FreezePlan<'v, 'fv, T> for StaticFreezePlan<'v, 'fv, T>
where
    T: Freeze<'v>,
    for<'a> T::Frozen<'a>: AValueSimpleBound<'a>,
{
    fn target(&self) -> FreezeTarget<'fv> {
        match self.direct {
            Some(value) => FreezeTarget::direct(value.to_value()),
            None => FreezeTarget::simple::<T::Frozen<'fv>>(),
        }
    }

    fn freeze_into(
        self,
        value: T,
        freezer: &Freezer<'v, 'fv>,
        slot: FreezeSlot<'fv>,
    ) -> FreezeResult<Value<'fv>> {
        if self.direct.is_some() {
            // The freeze driver forwards to a direct target without reserving
            // a destination or calling this method, so this arm only rejects
            // a driver bug; `value` is dropped unfrozen.
            return Err(FreezeError::new(
                "a direct branded freeze plan must not be initialized".to_owned(),
            ));
        }
        slot.write(value.freeze(freezer)?)
    }
}

/// Freezes a value whose concrete frozen representation is selected at runtime.
///
/// The module documentation walks through the protocol. Most implementations should
/// use [`Freeze`] instead; implement this trait directly when runtime data determines
/// the frozen type, layout, or size. In either case, the frozen value must be equal to
/// the source and produce the same hash.
pub trait FreezeDynamic<'v>: Sized {
    /// State prepared from this value before its forwarding pointer is installed.
    type Plan<'fv>: FreezePlan<'v, 'fv, Self>;

    /// Inspects the intact source and selects how it will be frozen.
    ///
    /// The plan must retain everything needed to choose and initialize the
    /// destination after the source is replaced by a forwarding record.
    ///
    /// The driver reserves nothing before preparation succeeds, so a failed
    /// preparation leaves the frozen heap untouched; only a failed
    /// [`FreezePlan::freeze_into`] leaves a (walkable) dead reservation
    /// behind.
    fn prepare_freeze<'fv>(&self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Plan<'fv>>;
}

/// A prepared, single-use runtime freeze operation.
///
/// [`target`](FreezePlan::target) is called before the source is replaced by a
/// forwarding record. An allocated target is reserved immediately, then the plan and
/// the source payload are consumed by [`freeze_into`](FreezePlan::freeze_into), which
/// must initialize exactly the target it selected, through [`FreezeSlot::write`], and
/// return the value that produced. Returning an error or unwinding before then leaves
/// the reservation header intact, so the frozen heap remains walkable without exposing
/// a partial value.
pub trait FreezePlan<'v, 'fv, T> {
    /// Returns the exact destination selected by this plan.
    ///
    /// A direct target is already frozen and bypasses [`FreezePlan::freeze_into`].
    fn target(&self) -> FreezeTarget<'fv>;

    /// Initializes the reserved destination from `value` and returns the published
    /// frozen value.
    ///
    /// The source already has a forwarding record at this point. Any
    /// [`Value`] fields in `value` must be frozen through [`Freezer`] before
    /// they are unpacked or otherwise inspected.
    fn freeze_into(
        self,
        value: T,
        freezer: &Freezer<'v, 'fv>,
        slot: FreezeSlot<'fv>,
    ) -> FreezeResult<Value<'fv>>;
}

/// Runtime-selected destination for a frozen value.
pub struct FreezeTarget<'fv>(FreezeTargetRepr<'fv>);

enum FreezeTargetRepr<'fv> {
    Direct(Value<'fv>),
    /// An allocation, described by the monomorphized function that reserves it and
    /// the number of trailing elements to reserve it with.
    Allocate {
        reserve: fn(FrozenHeap<'fv>, usize) -> FreezeSlot<'fv>,
        extra_len: usize,
    },
}

impl<'fv> FreezeTarget<'fv> {
    /// Uses an already allocated frozen value without creating a destination.
    ///
    /// Selecting a direct target is how a plan canonicalizes to an existing
    /// frozen value; no destination is ever allocated for it. The `'fv` brand
    /// attests that the value lives in frozen storage.
    ///
    /// The value must be heap-allocated: pointer-encoded values never reach
    /// the freeze machinery, and a forwarding record cannot encode them.
    pub fn direct(value: Value<'fv>) -> Self {
        debug_assert!(
            value.0.unpack_ptr().is_some(),
            "a direct freeze target must be a heap-allocated value"
        );
        Self(FreezeTargetRepr::Direct(value))
    }

    /// Allocates the final value as a normal fixed-size Starlark value, to be
    /// written by [`FreezeSlot::write`] with a `T`.
    pub fn simple<T: AValueSimpleBound<'fv>>() -> Self {
        Self::avalue::<AValueSimple<T>>(0)
    }

    /// Allocates the final value as an `A` with `extra_len` trailing elements, to be
    /// initialized in place through [`FreezeSlot::payload_ptr`] and published with
    /// [`FreezeSlot::publish`].
    pub(crate) fn avalue<A>(extra_len: usize) -> Self
    where
        A: AValue<'fv>,
        A::StarlarkValue: HeapSendable<'fv> + HeapSyncable<'fv>,
    {
        fn reserve<'fv, A>(heap: FrozenHeap<'fv>, extra_len: usize) -> FreezeSlot<'fv>
        where
            A: AValue<'fv>,
            A::StarlarkValue: HeapSendable<'fv> + HeapSyncable<'fv>,
        {
            let (reservation, _extra) = heap.reserve_with_extra::<A>(extra_len);
            FreezeSlot {
                pointer: reservation.into_entry_ptr(),
                needs_drop: A::NEEDS_DROP,
                _brand: PhantomData,
            }
        }

        Self(FreezeTargetRepr::Allocate {
            reserve: reserve::<A>,
            extra_len,
        })
    }

    /// Resolves this target into a concrete destination, reserving an
    /// allocation only when one is needed: a direct target resolves without
    /// touching the frozen heap. An unpublished reservation is permanent dead
    /// space in the frozen heap, which is why none is made speculatively.
    pub(crate) fn reserve(self, freezer: &Freezer<'_, 'fv>) -> FreezeDestination<'fv> {
        match self.0 {
            FreezeTargetRepr::Direct(value) => FreezeDestination::Direct(value),
            FreezeTargetRepr::Allocate { reserve, extra_len } => {
                FreezeDestination::Slot(reserve(freezer.frozen_heap(), extra_len))
            }
        }
    }
}

pub(crate) enum FreezeDestination<'fv> {
    Direct(Value<'fv>),
    Slot(FreezeSlot<'fv>),
}

/// Exclusive access to a reserved frozen destination.
///
/// Dropping an unwritten slot leaves its reservation header intact, so errors
/// and unwinding cannot publish a partially initialized value.
pub struct FreezeSlot<'fv> {
    /// The reserved entry; it holds a reservation word until publication
    /// replaces it with the value's header.
    pointer: *mut AValueHeapEntry,
    /// Whether the reservation is in the heap's drop region.
    needs_drop: bool,
    _brand: PhantomData<fn(&'fv ()) -> &'fv ()>,
}

impl<'fv> FreezeSlot<'fv> {
    /// Target for the source's forwarding record. The destination holds no
    /// value until the slot is published.
    pub(crate) fn forward_ptr(&self) -> ForwardPtr {
        ForwardPtr::new(self.pointer as usize)
    }

    /// The size the reservation was made with.
    fn reserved_size(&self) -> ValueAllocSize {
        // SAFETY: The slot exclusively owns its entry, which holds a reservation word
        // until `publish` replaces it.
        match unsafe { (*self.pointer).state() } {
            AValueHeapEntryState::Reservation(size) => size,
            _ => unreachable!("an unpublished slot still holds its reservation word"),
        }
    }

    /// Whether an `A` with `extra_len` trailing elements is exactly what was reserved:
    /// the same allocation size, which heap walks stride by, and the same drop region.
    /// Alignment is not a question, since every payload is bounded by the arena's at
    /// compile time (`PAYLOAD_ALIGNED`, named in [`payload_ptr`](FreezeSlot::payload_ptr)).
    pub(crate) fn fits<A: AValue<'fv>>(&self, extra_len: usize) -> bool {
        A::alloc_size_for_extra_len(extra_len) == self.reserved_size()
            && A::NEEDS_DROP == self.needs_drop
    }

    /// Writes a fixed-size Starlark value into this slot and publishes it.
    ///
    /// Fails if this slot was not reserved for a `T`, that is when a [`FreezePlan`]
    /// initializes a destination other than the target it selected.
    pub fn write<T: AValueSimpleBound<'fv>>(self, value: T) -> FreezeResult<Value<'fv>> {
        if !self.fits::<AValueSimple<T>>(0) {
            return Err(FreezeError::new(
                "freeze plan initialized a destination different from its selected target"
                    .to_owned(),
            ));
        }
        // SAFETY: The check proves the reservation has exactly an `AValueRepr<T>`'s
        // size and is in the region `T` needs, and `payload_ptr` that it is aligned for
        // `T`; the slot owns it exclusively, and the payload is complete once written.
        unsafe {
            self.payload_ptr::<AValueSimple<T>>().as_ptr().write(value);
            Ok(self.publish::<AValueSimple<T>>())
        }
    }

    /// The payload of the reserved allocation, for initializing an `A` in place; its
    /// trailing elements follow at `A::offset_of_extra()`.
    ///
    /// Writing through the pointer is only sound for an `A` that [`fits`](FreezeSlot::fits)
    /// with the trailing length being written, and the result is only a value once
    /// [`publish`](FreezeSlot::publish)ed.
    pub(crate) fn payload_ptr<A: AValue<'fv>>(&self) -> NonNull<A::StarlarkValue> {
        // At compile time: `A`'s payload is at most as aligned as the arena allocates, so
        // the reservation is aligned for it and the payload offset is the header's size.
        let () = AValueRepr::<A::StarlarkValue>::PAYLOAD_ALIGNED;
        // SAFETY: `pointer` is non-null, and the offset is the one-word header, within
        // any reservation (`MIN_ALLOC`).
        unsafe {
            NonNull::new_unchecked(
                self.pointer
                    .cast::<u8>()
                    .add(AValueRepr::<A::StarlarkValue>::offset_of_payload())
                    .cast(),
            )
        }
    }

    /// Publishes the slot as a live value by installing `A`'s header over the
    /// reservation word.
    ///
    /// # Safety
    ///
    /// An `A::StarlarkValue` and its trailing elements must be fully initialized behind
    /// [`payload_ptr`](FreezeSlot::payload_ptr), with the trailing length the slot was
    /// reserved for ([`fits`](FreezeSlot::fits)): heap walks stride by the published
    /// header's size, which must reproduce the reservation.
    pub(crate) unsafe fn publish<A: AValue<'fv>>(self) -> Value<'fv> {
        // At compile time: a string's pointer carries a tag that this does not set.
        // Strings are allocated through `alloc_str`, never reserved.
        const {
            assert!(
                !A::IS_STR,
                "a string cannot be published through a freeze slot"
            )
        };
        #[cfg(debug_assertions)]
        let reserved = self.reserved_size();
        let header = self.pointer.cast::<AValueHeader>();
        // SAFETY: A reserved entry is header-sized and header-aligned storage owned by
        // this slot, and the caller promises the payload behind it is complete, so
        // installing the header is what publishes the value.
        unsafe { header.write(AValueHeader::new::<A>()) };
        // SAFETY: The header was written just above.
        #[cfg(debug_assertions)]
        debug_assert_eq!(
            unsafe { (&*header).alloc_size() },
            reserved,
            "published value reports a different size than its reservation",
        );
        // SAFETY: The header was written just above and lives as long as its frozen
        // heap, and `A` is not a string (checked above), so the pointer carries no
        // string tag.
        unsafe { Value::new_frozen_ptr(&*header, false) }
    }
}

/// Whether `value`, which a plan returned, is the published contents of the slot
/// whose forward is `forward`. See the module documentation for why both halves are
/// needed.
pub(crate) fn is_published_at(value: Value<'_>, forward: ForwardPtr) -> bool {
    match value.0.unpack_ptr() {
        Some(entry) if forward.points_to(entry) => {
            matches!(entry.state(), AValueHeapEntryState::Value(_))
        }
        _ => false,
    }
}

macro_rules! impl_freeze_identity {
    ($($t:ty),*) => {
        $(
            impl<'v> Freeze<'v> for $t {
                type Frozen<'fv> = Self;

                fn freeze<'fv>(self, _freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
                    Ok(self)
                }
            }
        )*
    }
}

impl_freeze_identity!(String, i32, u32, i64, u64, usize, bool, ());

impl<'v, T: 'static> Freeze<'v> for PhantomData<&'v T> {
    type Frozen<'fv> = PhantomData<&'fv T>;

    fn freeze<'fv>(self, _freezer: &Freezer<'v, 'fv>) -> FreezeResult<PhantomData<&'fv T>> {
        Ok(PhantomData)
    }
}

impl<'v, T> Freeze<'v> for Vec<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = Vec<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Vec<T::Frozen<'fv>>> {
        self.into_try_map(|v| v.freeze(freezer))
    }
}

impl<'v, T> Freeze<'v> for RefCell<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = T::Frozen<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<T::Frozen<'fv>> {
        self.into_inner().freeze(freezer)
    }
}

impl<'v, T> Freeze<'v> for UnsafeCell<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = UnsafeCell<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(UnsafeCell::new(self.into_inner().freeze(freezer)?))
    }
}

impl<'v, T> Freeze<'v> for OnceCell<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = Option<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        self.into_inner().freeze(freezer)
    }
}

impl<'v, T> Freeze<'v> for Box<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = Box<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(Box::new((*self).freeze(freezer)?))
    }
}

impl<'v, T> Freeze<'v> for Box<[T]>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = Box<[T::Frozen<'fv>]>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        self.into_vec()
            .into_try_map(|v| v.freeze(freezer))
            .map(|v| v.into_boxed_slice())
    }
}

impl<'v, T> Freeze<'v> for Option<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = Option<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Option<T::Frozen<'fv>>> {
        self.map(|v| v.freeze(freezer)).transpose()
    }
}

impl<'v, K: Freeze<'v>> Freeze<'v> for Hashed<K> {
    type Frozen<'fv> = Hashed<K::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        // `freeze` must not change hash.
        Ok(Hashed::new_unchecked(
            self.hash(),
            self.into_key().freeze(freezer)?,
        ))
    }
}

impl<'v, K, V> Freeze<'v> for SmallMap<K, V>
where
    K: Freeze<'v>,
    V: Freeze<'v>,
{
    type Frozen<'fv> = SmallMap<K::Frozen<'fv>, V::Frozen<'fv>>;

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<SmallMap<K::Frozen<'fv>, V::Frozen<'fv>>> {
        let mut new = SmallMap::with_capacity(self.len());
        for (key, value) in self.into_iter_hashed() {
            let hash = key.hash();
            let key = key.into_key().freeze(freezer)?;
            // TODO(nga): verify hash unchanged after freeze.
            let key = Hashed::new_unchecked(hash, key);
            let value = value.freeze(freezer)?;
            new.insert_hashed_unique_unchecked(key, value);
        }
        Ok(new)
    }
}

impl<'v, T> Freeze<'v> for SmallSet<T>
where
    T: Freeze<'v>,
{
    type Frozen<'fv> = SmallSet<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        let mut new = SmallSet::with_capacity(self.len());
        for value in self.into_iter_hashed() {
            let value = value.freeze(freezer)?;
            // TODO(nga): verify hash unchanged after freeze.
            new.insert_hashed_unique_unchecked(value);
        }
        Ok(new)
    }
}

impl<'v> Freeze<'v> for Value<'v> {
    type Frozen<'fv> = Value<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Value<'fv>> {
        freezer.freeze(self)
    }
}

impl<'v, A: Freeze<'v>> Freeze<'v> for (A,) {
    type Frozen<'fv> = (A::Frozen<'fv>,);

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<(A::Frozen<'fv>,)> {
        Ok((self.0.freeze(freezer)?,))
    }
}

impl<'v, A: Freeze<'v>, B: Freeze<'v>> Freeze<'v> for (A, B) {
    type Frozen<'fv> = (A::Frozen<'fv>, B::Frozen<'fv>);

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(A::Frozen<'fv>, B::Frozen<'fv>)> {
        Ok((self.0.freeze(freezer)?, self.1.freeze(freezer)?))
    }
}

impl<'v, A: Freeze<'v>, B: Freeze<'v>, C: Freeze<'v>> Freeze<'v> for (A, B, C) {
    type Frozen<'fv> = (A::Frozen<'fv>, B::Frozen<'fv>, C::Frozen<'fv>);

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(A::Frozen<'fv>, B::Frozen<'fv>, C::Frozen<'fv>)> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
        ))
    }
}

impl<'v, A: Freeze<'v>, B: Freeze<'v>, C: Freeze<'v>, D: Freeze<'v>> Freeze<'v> for (A, B, C, D) {
    type Frozen<'fv> = (
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
    );

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
    )> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
            self.3.freeze(freezer)?,
        ))
    }
}

impl<'v, A: Freeze<'v>, B: Freeze<'v>, C: Freeze<'v>, D: Freeze<'v>, E: Freeze<'v>> Freeze<'v>
    for (A, B, C, D, E)
{
    type Frozen<'fv> = (
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
        E::Frozen<'fv>,
    );

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
        E::Frozen<'fv>,
    )> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
            self.3.freeze(freezer)?,
            self.4.freeze(freezer)?,
        ))
    }
}

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
//! * A value allocated with [`Heap::alloc_complex_branded`](crate::values::Heap::alloc_complex_branded)
//!   implements [`FreezeDynamic`]. Most such values implement [`FreezeBranded`]; the
//!   blanket [`FreezeDynamic`] implementation selects one statically known frozen Rust
//!   type. A `FreezeBranded` implementation can reuse an existing frozen [`Value`] from
//!   [`FreezeBranded::prepare_freeze`] instead of allocating it. A direct
//!   [`FreezeDynamic`] implementation can inspect the source and select its frozen
//!   type and allocation size at runtime.
//!
//! Every result must be equal to the source value and produce the same hash.
//!
//! # Allocating and publishing a frozen value
//!
//! The [`FreezeDynamic`] protocol separates inspecting the source from consuming it:
//!
//! 1. [`FreezeDynamic::prepare_freeze`] runs while the source header and payload are
//!    intact and returns a single-use [`FreezePlan`].
//! 2. [`FreezePlan::target`] either identifies an existing frozen [`Value`] or
//!    describes the exact AValue allocation to reserve. An existing value is
//!    forwarded to immediately and [`FreezePlan::freeze_into`] is not called.
//! 3. For a new allocation, the frozen heap writes a reservation header containing
//!    the allocation size and returns a [`FreezeSlot`]. The reservation makes the
//!    allocation walkable without reading any uninitialized payload bytes.
//! 4. The source header is replaced by a forwarding record to the reserved
//!    destination. The source payload is then moved into
//!    [`FreezePlan::freeze_into`]. Recursive [`Value`] fields must be passed
//!    back through [`Freezer::freeze`] before being inspected.
//! 5. The plan initializes the complete payload and any trailing elements, then
//!    returns an [`InitializedFreezeSlot`] obtained from [`FreezeSlot::write`] or
//!    an internal typed initializer. The freeze driver consumes that proof and
//!    publishes the allocation by replacing the reservation header with the final
//!    AValue vtable.
//!
//! If initialization fails or unwinds, the reservation header remains in place.
//! The partial payload is never exposed as a value, and the frozen heap remains
//! safe to walk and discard. Sources already overwritten with forwarding
//! records still point at the abandoned reservation, however, so an error
//! abandons the entire freeze; debug builds enforce this.
//!
//! The blanket [`FreezeDynamic`] implementation for [`FreezeBranded`] follows the same
//! protocol: its plan selects the `'static` instantiation of `FreezeBranded::Frozen`
//! as a simple target, [`FreezeBranded::freeze`] builds that payload after forwarding
//! is installed, and the initialized slot is returned for the driver to publish.

use std::any::TypeId;
use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::values::FreezeBranded;
use crate::values::FreezeError;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::HeapSendable;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueSimpleBound;
use crate::values::layout::avalues::simple::AValueSimple;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueHeapEntry;
#[cfg(debug_assertions)]
use crate::values::layout::heap::repr::AValueHeapEntryState;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::vtable::AValueVTable;

/// Freezes a value whose concrete frozen representation is selected at runtime.
///
/// Heap freezing first calls [`FreezeDynamic::prepare_freeze`] while the source payload
/// is still intact. The returned [`FreezePlan`] identifies either an existing
/// frozen value or the exact destination allocation. For an allocation, the
/// freezer reserves that destination and installs the source's forwarding record
/// before moving the source payload into [`FreezePlan::freeze_into`]. This order
/// allows recursive values to find the destination while it is initialized.
///
/// Most implementations should use [`FreezeBranded`] instead. Implement this
/// trait directly when runtime data determines the frozen type, layout, or size.
/// In either case, the frozen value must be equal to the source and produce the
/// same hash.
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
/// [`FreezePlan::target`] is called before the source is replaced by a forwarding
/// record. An allocated target is reserved immediately, then the plan and source
/// payload are consumed by [`FreezePlan::freeze_into`]. That method must initialize
/// exactly the target it selected and return an [`InitializedFreezeSlot`]. The
/// freeze driver then publishes that slot. Returning an error or unwinding before
/// publication leaves the reservation header intact, so the frozen heap remains
/// walkable without exposing a partial value.
pub trait FreezePlan<'v, 'fv, T> {
    /// Returns the exact destination selected by this plan.
    ///
    /// A direct target is already frozen and bypasses [`FreezePlan::freeze_into`].
    fn target(&self) -> FreezeTarget<'fv>;

    /// Initializes the reserved destination from `value`.
    ///
    /// The source already has a forwarding record at this point. Any
    /// [`Value`] fields in `value` must be frozen through [`Freezer`] before
    /// they are unpacked or otherwise inspected.
    fn freeze_into(
        self,
        value: T,
        freezer: &Freezer<'v, 'fv>,
        slot: FreezeSlot<'fv>,
    ) -> FreezeResult<InitializedFreezeSlot<'fv>>;
}

/// Runtime-selected destination for a frozen value.
pub struct FreezeTarget<'fv>(FreezeTargetRepr<'fv>);

enum FreezeTargetRepr<'fv> {
    Direct(Value<'fv>),
    Allocate(FreezeAllocation),
}

/// What reserving and validating a destination needs: its size (through the
/// monomorphized `reserve`) and its identity. The final vtable is supplied by
/// the typed initializer that proves the payload complete.
#[derive(Copy, Clone)]
struct FreezeAllocation {
    reserve: fn(&Freezer, usize) -> *mut AValueHeapEntry,
    // `AValueVTable::new` is const-promoted and its address is not canonical
    // across codegen units, so use Rust's canonical type identity for checks.
    avalue_type_id: TypeId,
    extra_len: usize,
    payload_offset: usize,
}

impl FreezeAllocation {
    fn new<A>(extra_len: usize) -> Self
    where
        A: AValue<'static> + 'static,
        A::StarlarkValue: HeapSendable<'static> + HeapSyncable<'static>,
    {
        fn reserve<A>(freezer: &Freezer, extra_len: usize) -> *mut AValueHeapEntry
        where
            A: AValue<'static> + 'static,
            A::StarlarkValue: HeapSendable<'static> + HeapSyncable<'static>,
        {
            let (reservation, _extra) = freezer.frozen_heap().reserve_with_extra::<A>(extra_len);
            reservation.into_entry_ptr()
        }

        Self {
            reserve: reserve::<A>,
            avalue_type_id: TypeId::of::<A>(),
            extra_len,
            payload_offset: AValueRepr::<A::StarlarkValue>::offset_of_payload(),
        }
    }
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

    /// Allocates the final value as a normal fixed-size Starlark value.
    pub fn simple<T>() -> Self
    where
        T: AValueSimpleBound<'static>,
    {
        Self::avalue::<AValueSimple<T>>(0)
    }

    pub(crate) fn avalue<A>(extra_len: usize) -> Self
    where
        A: AValue<'static> + 'static,
        A::StarlarkValue: HeapSendable<'static> + HeapSyncable<'static>,
    {
        Self(FreezeTargetRepr::Allocate(FreezeAllocation::new::<A>(
            extra_len,
        )))
    }

    /// Resolves this target into a concrete destination, reserving an
    /// allocation only when one is needed: a direct target resolves without
    /// touching the frozen heap. An unpublished reservation is permanent dead
    /// space in the frozen heap, which is why none is made speculatively.
    pub(crate) fn reserve(self, freezer: &Freezer<'_, 'fv>) -> FreezeDestination<'fv> {
        match self.0 {
            FreezeTargetRepr::Direct(value) => FreezeDestination::Direct(value),
            FreezeTargetRepr::Allocate(allocation) => {
                let pointer = (allocation.reserve)(freezer, allocation.extra_len);
                FreezeDestination::Slot(FreezeSlot {
                    allocation,
                    pointer,
                    _brand: PhantomData,
                })
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
/// Dropping an unfinished slot leaves its reservation header intact, so errors
/// and unwinding cannot publish a partially initialized value.
pub struct FreezeSlot<'fv> {
    allocation: FreezeAllocation,
    /// The reserved entry; it holds a reservation word until publication
    /// replaces it with the final value header.
    pointer: *mut AValueHeapEntry,
    /// Ties this reserved destination to its frozen heap.
    _brand: PhantomData<&'fv ()>,
}

impl<'fv> FreezeSlot<'fv> {
    /// Target for the source's forwarding record. The destination holds no
    /// value until the initialized slot is published.
    pub(crate) fn forward_ptr(&self) -> ForwardPtr {
        ForwardPtr::new(self.pointer as usize)
    }

    /// Writes a fixed-size Starlark value into this slot.
    ///
    /// Fails if this slot was reserved for a different type than `T`, that is
    /// when a [`FreezePlan`] initializes a destination other than the target
    /// it selected.
    pub fn write<T>(self, value: T) -> FreezeResult<InitializedFreezeSlot<'fv>>
    where
        T: AValueSimpleBound<'static>,
    {
        let expected = FreezeAllocation::new::<AValueSimple<T>>(0);
        self.check_matches(expected)?;
        // SAFETY: The target check proves that this slot was allocated for
        // `AValueSimple<T>`, and the reservation exclusively owns the payload.
        unsafe {
            self.payload_non_null::<AValueSimple<T>>()
                .as_ptr()
                .write(value);
        }
        Ok(InitializedFreezeSlot {
            slot: self,
            vtable: AValueVTable::new::<AValueSimple<T>>(),
        })
    }

    /// Writes a branded fixed-size Starlark value into this slot.
    ///
    /// The slot's identity is checked against the `'static` instantiation of
    /// `T::Frozen`; the payload is written at this heap's brand.
    pub fn write_branded<'v, T>(
        self,
        value: T::Frozen<'fv>,
    ) -> FreezeResult<InitializedFreezeSlot<'fv>>
    where
        T: FreezeBranded<'v>,
        T::Frozen<'static>: AValueSimpleBound<'static>,
    {
        let expected = FreezeAllocation::new::<AValueSimple<T::Frozen<'static>>>(0);
        self.check_matches(expected)?;
        // SAFETY: `T::Frozen<'fv>` and `T::Frozen<'static>` are one type
        // constructor instantiated at different heap brands and share a single
        // layout. The target check proves this slot was allocated for that
        // layout, and the reservation exclusively owns the payload.
        unsafe {
            self.payload_non_null::<AValueSimple<T::Frozen<'static>>>()
                .cast::<T::Frozen<'fv>>()
                .as_ptr()
                .write(value);
        }
        Ok(InitializedFreezeSlot {
            slot: self,
            vtable: AValueVTable::new::<AValueSimple<T::Frozen<'static>>>(),
        })
    }

    fn check_matches(&self, expected: FreezeAllocation) -> FreezeResult<()> {
        if self.allocation.avalue_type_id == expected.avalue_type_id
            && self.allocation.extra_len == expected.extra_len
            && self.allocation.payload_offset == expected.payload_offset
        {
            Ok(())
        } else {
            Err(FreezeError::new(
                "freeze plan initialized a destination different from its selected target"
                    .to_owned(),
            ))
        }
    }

    fn assert_matches(&self, expected: FreezeAllocation) {
        self.check_matches(expected)
            .expect("crate-internal freeze initializers must match their reserved target");
    }

    pub(crate) fn payload_non_null<A>(&self) -> NonNull<A::StarlarkValue>
    where
        A: AValue<'static> + 'static,
        A::StarlarkValue: HeapSendable<'static> + HeapSyncable<'static>,
    {
        self.assert_matches(FreezeAllocation::new::<A>(self.allocation.extra_len));
        // SAFETY: The checked allocation metadata supplies the payload offset,
        // and the reservation is non-null and uniquely owned.
        unsafe {
            NonNull::new_unchecked(
                self.pointer
                    .cast::<u8>()
                    .add(self.allocation.payload_offset)
                    .cast(),
            )
        }
    }
}

/// Proof that a freeze slot has been completely initialized.
///
/// The private `slot` field is part of the safety boundary for the safe
/// [`FreezePlan`] API. An implementation cannot claim successful initialization
/// without consuming its [`FreezeSlot`] through [`FreezeSlot::write`]. The
/// freeze driver alone publishes the initialized slot as a live AValue.
pub struct InitializedFreezeSlot<'fv> {
    slot: FreezeSlot<'fv>,
    /// The published header's vtable, recorded by the typed initializer that
    /// proved the payload complete.
    vtable: &'static AValueVTable,
}

impl<'fv> InitializedFreezeSlot<'fv> {
    pub(crate) fn publish(self) -> Value<'fv> {
        let vtable = self.vtable;
        let FreezeSlot {
            allocation: _,
            pointer,
            _brand: _,
        } = self.slot;
        // SAFETY: `pointer` is this slot's reserved heap entry: valid,
        // aligned, and exclusively owned until publication.
        #[cfg(debug_assertions)]
        let reserved = match unsafe { (&*pointer).state() } {
            AValueHeapEntryState::Reservation(size) => size,
            _ => unreachable!("an unpublished slot still holds its reservation word"),
        };
        let header = pointer.cast::<AValueHeader>();
        // SAFETY: A reserved entry is header-sized and header-aligned
        // storage owned by this slot, and construction of
        // `InitializedFreezeSlot` proves the payload behind it is complete,
        // so installing the final vtable is what publishes the value.
        unsafe { header.write(AValueHeader(vtable)) };
        // Heap walks stride by the published header's payload-derived size,
        // so it must reproduce the reservation exactly.
        // SAFETY: The header was written just above and the slot exclusively
        // owns the entry until publication returns.
        #[cfg(debug_assertions)]
        debug_assert_eq!(
            unsafe { (&*header).alloc_size() },
            reserved,
            "published value reports a different size than its reservation",
        );
        // SAFETY: The header was written just above and lives as long as its
        // frozen heap. Reservations are never strings, so the pointer carries
        // no string tag.
        unsafe { Value::new_frozen_ptr(&*header, false) }
    }
}

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

use std::hash::Hash;
use std::mem;
use std::mem::ManuallyDrop;
use std::ptr;

use dupe::Dupe;

use crate::any::AnyLifetime;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::layout::aligned_size::AlignedSize;
use crate::values::layout::avalue::AValue;
use crate::values::layout::heap::heap_type::HeapKind;
use crate::values::layout::value_alloc_size::ValueAllocSize;
use crate::values::layout::vtable::AValueDyn;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;

#[derive(Clone)]
#[repr(C)]
pub(crate) struct AValueHeader(pub(crate) &'static AValueVTable);

impl Hash for AValueHeader {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state);
    }
}

impl PartialEq for AValueHeader {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Eq for AValueHeader {}

// Implements Copy so this is fine
impl Dupe for AValueHeader {}

/// How object is represented in arena.
#[repr(C, align(8))]
pub(crate) struct AValueRepr<T> {
    pub(crate) header: AValueHeader,
    /// Payload of the object, i.e. `StarlarkValue`.
    /// Note that `T` may have larger alignment that `AValueHeader`,
    /// so we cannot add fixed offset to `self` to get to the payload.
    pub(crate) payload: T,
}

/// "Forward" pointer (pointer to another heap during GC).
///
/// This pointer has `TAG_STR` bit set if it points to a string.
///
/// Lower bit (which is the same bit as `TAG_UNFROZEN`) is always unset
/// regardless of whether it points to frozen or unfrozen value.
/// User of this struct must set this bit explicitly if needed.
#[derive(Copy, Clone, Dupe)]
pub(crate) struct ForwardPtr(usize);

impl ForwardPtr {
    pub(crate) fn new(ptr: usize) -> ForwardPtr {
        debug_assert_eq!(ptr & HEAP_ENTRY_TAG_MASK, 0);
        ForwardPtr(ptr)
    }

    /// Create a forward pointer to a frozen value. This is used during heap freeze.
    pub(crate) fn new_frozen(value: Value) -> ForwardPtr {
        debug_assert!(value.is_frozen());
        ForwardPtr::new(value.0.raw().ptr_value())
    }

    /// Create a forward pointer to an unfrozen value. This is used during heap GC.
    pub(crate) fn new_unfrozen(value: Value) -> ForwardPtr {
        debug_assert!(!value.is_frozen());
        ForwardPtr::new(value.0.raw().ptr_value() & !1)
    }

    /// It's caller responsibility to ensure that forward pointer points to a frozen value in the
    /// heap of `'v`.
    pub(crate) unsafe fn unpack_frozen_value<'v>(self) -> Value<'v> {
        unsafe { Value::new_frozen_ptr_usize_with_str_tag(self.0) }
    }

    /// It's caller responsibility to ensure that forward pointer points to an unfrozen value.
    pub(crate) unsafe fn unpack_unfrozen_value<'v>(self) -> Value<'v> {
        unsafe { Value::new_ptr_usize_with_str_tag(self.0) }
    }

    pub(crate) unsafe fn unpack_value<'v>(self, heap_kind: HeapKind) -> Value<'v> {
        unsafe {
            match heap_kind {
                HeapKind::Unfrozen => self.unpack_unfrozen_value(),
                HeapKind::Frozen => self.unpack_frozen_value(),
            }
        }
    }
}

/// This is object written over [`AValueRepr`] during GC.
#[repr(C)]
#[derive(Debug)]
pub(crate) struct AValueForward {
    /// Moved object pointer with lowest bit set.
    forward_ptr: usize,
    /// Size of `AValueRepr<T>` including extra.
    object_size: ValueAllocSize,
}

const FORWARD_TAG: usize = 0b01;
const RESERVATION_TAG: usize = 0b10;
const HEAP_ENTRY_TAG_MASK: usize = FORWARD_TAG | RESERVATION_TAG;

// Vtable pointers use tag 00. Reservations store their aligned byte size with
// tag 10, while forwarding records tag their destination pointer with 01.
const _: () = assert!(mem::align_of::<AValueVTable>() > HEAP_ENTRY_TAG_MASK);

impl AValueForward {
    pub(crate) fn new(forward_ptr: ForwardPtr, object_size: ValueAllocSize) -> AValueForward {
        debug_assert_eq!(forward_ptr.0 & HEAP_ENTRY_TAG_MASK, 0);
        Self {
            forward_ptr: forward_ptr.0 | FORWARD_TAG,
            object_size,
        }
    }

    /// Unpack forward pointer.
    pub(crate) fn forward_ptr(&self) -> ForwardPtr {
        debug_assert_eq!(self.forward_ptr & HEAP_ENTRY_TAG_MASK, FORWARD_TAG);
        ForwardPtr(self.forward_ptr & !FORWARD_TAG)
    }
}

/// State stored at the beginning of an allocation in the Starlark heap: a live value's header
/// or a forward.
///
/// This must stay exactly one word wide, even though `AValueForward` is two words: statically
/// allocated values with a zero-sized payload (such as `None`) are only one word long, and a
/// wider type would make every reference to them extend past the end of the object.
#[repr(C)]
pub(crate) union AValueHeapEntry {
    // We intentionally do not implement `Copy` for these types
    // to avoid accidentally copying them.
    header: ManuallyDrop<AValueHeader>,
    flags: usize,
}

const _: () = assert!(mem::size_of::<AValueHeapEntry>() == mem::size_of::<AValueHeader>());

impl AValueHeapEntry {
    #[inline]
    fn raw_word(&self) -> usize {
        // SAFETY: Every variant of this union is a single pointer-sized word:
        // `header` is `&'static AValueVTable` (a non-null pointer with the same
        // size and layout as `usize`) and `flags` is `usize` itself. Reading
        // the word as an integer only discards provenance, which tag inspection
        // does not need; pointer variants are re-read through a typed pointer
        // in `state()`.
        unsafe { self.flags }
    }

    #[inline]
    fn tag(&self) -> usize {
        self.raw_word() & HEAP_ENTRY_TAG_MASK
    }

    #[inline]
    fn is_value(&self) -> bool {
        self.tag() == 0
    }

    pub(crate) fn new_reservation(alloc_size: ValueAllocSize) -> AValueHeapEntry {
        let alloc_size = alloc_size.bytes() as usize;
        // `ValueAllocSize` is 8-byte aligned by construction, so the tag bits
        // are clear by type invariant; this only documents that fact. Decoding
        // in `reservation_size` re-checks alignment unconditionally because it
        // reads an untyped heap word.
        debug_assert_eq!(alloc_size & HEAP_ENTRY_TAG_MASK, 0);
        AValueHeapEntry {
            flags: alloc_size | RESERVATION_TAG,
        }
    }

    fn reservation_size(&self) -> ValueAllocSize {
        let bytes = self.raw_word() & !HEAP_ENTRY_TAG_MASK;
        ValueAllocSize::new(AlignedSize::new_bytes(bytes))
    }

    // Called on hot value-access paths (e.g. downcast); inlining is worth
    // several percent of interpreter time.
    #[inline(always)]
    pub(crate) fn state(&self) -> AValueHeapEntryState<'_> {
        match self.tag() {
            // SAFETY: The tag identifies a live value's vtable word.
            0 => AValueHeapEntryState::Value(unsafe { &self.header }),
            // SAFETY: Only objects in the arena are ever overwritten with a forward, and
            // those are at least `MIN_ALLOC` bytes, so the whole `AValueForward` is within
            // the object.
            FORWARD_TAG => AValueHeapEntryState::Forward(unsafe {
                &*(self as *const AValueHeapEntry as *const AValueForward)
            }),
            RESERVATION_TAG => AValueHeapEntryState::Reservation(self.reservation_size()),
            _ => panic!("invalid heap entry tag"),
        }
    }

    #[inline]
    pub(crate) unsafe fn value_header_unchecked(&self) -> &AValueHeader {
        unsafe {
            debug_assert!(self.is_value());
            &self.header
        }
    }

    pub(crate) fn value_header(&self) -> Option<&AValueHeader> {
        match self.state() {
            AValueHeapEntryState::Value(header) => Some(header),
            AValueHeapEntryState::Forward(_) | AValueHeapEntryState::Reservation(_) => None,
        }
    }

    pub(crate) fn forward(&self) -> Option<&AValueForward> {
        match self.state() {
            AValueHeapEntryState::Value(_) | AValueHeapEntryState::Reservation(_) => None,
            AValueHeapEntryState::Forward(forward) => Some(forward),
        }
    }

    /// Size of allocation for this object:
    /// following object is allocated at `self + alloc_size + align up`.
    pub(crate) fn alloc_size(&self) -> ValueAllocSize {
        match self.state() {
            AValueHeapEntryState::Value(ptr) => ptr.unpack().memory_size(),
            AValueHeapEntryState::Forward(forward) => {
                // Overwritten, so the next word will be the size of the memory
                forward.object_size
            }
            AValueHeapEntryState::Reservation(alloc_size) => alloc_size,
        }
    }
}

/// State of an [`AValueHeapEntry`].
pub(crate) enum AValueHeapEntryState<'a> {
    Value(&'a AValueHeader),
    Forward(&'a AValueForward),
    Reservation(ValueAllocSize),
}

impl AValueForward {
    pub(crate) fn assert_does_not_overwrite_extra<'v, T: AValue<'v>>() {
        assert!(mem::size_of::<AValueForward>() <= AValueRepr::<T>::offset_of_extra());
    }
}

impl AValueHeader {
    /// Alignment of objects in Starlark heap.
    /// We must use 8 byte alignment because we use three lowest bits for tags.
    /// Note the alignment of `AValueHeader` may be smaller than this.
    pub(crate) const ALIGN: usize = 8;

    pub(crate) fn new<'v, T: AValue<'v>>() -> AValueHeader {
        let header = AValueHeader::new_const::<T>();

        let vtable_ptr = header.0 as *const AValueVTable as usize;
        debug_assert_eq!(vtable_ptr & HEAP_ENTRY_TAG_MASK, 0);

        header
    }

    pub(crate) const fn new_const<'v, T: AValue<'v>>() -> AValueHeader {
        AValueHeader(AValueVTable::new::<T>())
    }

    #[inline]
    pub(crate) fn payload_ptr(&self) -> StarlarkValueRawPtr {
        StarlarkValueRawPtr::new_header(self)
    }

    pub(crate) unsafe fn payload<'v, T: StarlarkValue<'v>>(&self) -> &T {
        unsafe {
            debug_assert_eq!(self.0.static_type_of_value.get(), T::static_type_id());
            &*self.payload_ptr().value_ptr::<T>()
        }
    }

    pub(crate) unsafe fn unpack_value<'v>(&'v self, heap_kind: HeapKind) -> Value<'v> {
        match heap_kind {
            HeapKind::Unfrozen => Value::new_ptr_query_is_str(self),
            HeapKind::Frozen => Value::new_frozen_ptr(self, self.0.is_str),
        }
    }

    pub(crate) fn unpack<'v>(&'v self) -> AValueDyn<'v> {
        unsafe {
            // TODO: this assertion does not belong here.
            //   Instead, `Value` should be a `Pointer<AValueHeapEntry>`
            //   instead of `Pointer<AValueHeader>`,
            //   and assertion should be where we unpack the pointer.
            // A reservation or forward word is not a vtable, so dispatching
            // through it in release builds is undefined behavior. Reaching one
            // here requires holding a value from a heap whose freeze or GC is
            // in progress or was abandoned; consuming APIs (`Module::freeze`)
            // make that unreachable from safe code.
            debug_assert!(
                (*(self as *const AValueHeader as *const AValueHeapEntry)).is_value(),
                "value is not a live heap entry; value cannot be unpacked during GC or freeze"
            );
        }
        unsafe { AValueDyn::new(self.payload_ptr(), self.0) }
    }

    /// After performing the overwrite any existing pointers to this value
    /// are corrupted.
    pub unsafe fn overwrite_with_forward<'v, T: StarlarkValue<'v>>(
        me: *mut AValueRepr<T>,
        forward_ptr: ForwardPtr,
    ) -> T {
        unsafe {
            // TODO(nga): we don't need to do virtual call to obtain memory size
            let sz = (*me).header.unpack().memory_size();
            let p = me as *const AValueRepr<T>;
            let res = ptr::read(p).payload;
            let p = me as *mut AValueForward;
            *p = AValueForward::new(forward_ptr, sz);
            res
        }
    }

    fn as_heap_entry(&self) -> &AValueHeapEntry {
        unsafe { &*(self as *const AValueHeader as *const AValueHeapEntry) }
    }

    /// Size of allocation for this object: following object is allocated at `self + alloc_size`.
    pub(crate) fn alloc_size(&self) -> ValueAllocSize {
        self.as_heap_entry().alloc_size()
    }
}

impl<T> AValueRepr<T> {
    const _ASSERTIONS: () = {
        assert!(mem::align_of::<Self>() == AValueHeader::ALIGN);
    };

    pub(crate) const fn with_metadata(
        metadata: &'static AValueVTable,
        payload: T,
    ) -> AValueRepr<T> {
        AValueRepr {
            header: AValueHeader(metadata),
            payload,
        }
    }

    pub(crate) fn offset_of_payload() -> usize {
        mem::offset_of!(Self, payload)
    }

    /// Padding between header and payload.
    /// Non-zero when alignment of payload is larger than alignment of pointer.
    pub(crate) fn padding_after_header() -> usize {
        Self::offset_of_payload() - mem::size_of::<AValueHeader>()
    }

    /// Offset of value extra content relative to `AValueRepr` start.
    pub(crate) fn offset_of_extra<'v>() -> usize
    where
        T: AValue<'v>,
    {
        Self::offset_of_payload() + T::offset_of_extra()
    }

    pub(crate) fn from_payload_ptr_mut(payload_ptr: *mut T) -> *mut AValueRepr<T> {
        let payload_ptr = payload_ptr as usize;
        let header_ptr = payload_ptr - Self::offset_of_payload();
        header_ptr as *mut AValueRepr<T>
    }
}

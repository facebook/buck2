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

use std::any::Any;
use std::cell::Cell;
use std::cell::RefCell;
use std::cell::RefMut;
use std::cmp;
use std::collections::HashMap;
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
use std::sync::Mutex;

use allocative::Allocative;
use bumpalo::Bump;
use dupe::Dupe;
use dupe::IterDupedExt;
use pagable::PagableBoxDeserialize;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use starlark_map::small_set::SmallSet;
use strong_hash::StrongHash;

use crate::cast;
use crate::cast::transmute;
use crate::collections::StarlarkHashValue;
use crate::environment::GlobalFrozenHeapName;
use crate::environment::MethodFrozenHeapName;
use crate::eval::runtime::profile::instant::ProfilerInstant;
use crate::pagable::DeserTypeId;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::lookup_vtable;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::starlark_deserialize_context::HeapDeserializationState;
use crate::pagable::starlark_deserialize_context::StarlarkDeserializerImpl;
use crate::pagable::starlark_deserialize_context::ValueDeserSlot;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerializerImpl;
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
use crate::values::layout::heap::arena::ArenaOffset;
use crate::values::layout::heap::arena::ArenaVisitor;
use crate::values::layout::heap::arena::BumpKind;
use crate::values::layout::heap::arena::Reservation;
use crate::values::layout::heap::call_enter_exit::CallEnter;
use crate::values::layout::heap::call_enter_exit::CallExit;
use crate::values::layout::heap::call_enter_exit::NeedsDrop;
use crate::values::layout::heap::call_enter_exit::NoDrop;
use crate::values::layout::heap::fast_cell::FastCell;
use crate::values::layout::heap::profile::by_type::HeapSummary;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueOrForwardUnpack;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value::FrozenValue;
use crate::values::layout::value::Value;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::string::intern::interner::FrozenStringValueInterner;
use crate::values::string::intern::interner::StringValueInterner;

#[derive(Copy, Clone, Dupe)]
pub(crate) enum HeapKind {
    Unfrozen,
    Frozen,
}

/// An owned heap on which [`Value`]s can be allocated.
///
/// Private for now, but there's no reason it couldn't be public as long as access is restricted to
/// branded functions with signatures like those of `Heap::temp`
struct OwnedHeap {
    /// Peak memory seen when a garbage collection takes place (may be lower than currently allocated)
    peak_allocated: Cell<usize>,
    arena: FastCell<Arena<Bump>>,
    str_interner: RefCell<StringValueInterner<'static>>,
    /// Memory I depend on.
    refs: RefCell<SmallSet<FrozenHeapRef>>,
    ban_gc: Cell<bool>,
}

impl OwnedHeap {
    /// Create a new [`OwnedHeap`].
    fn new() -> Self {
        Self {
            peak_allocated: Default::default(),
            arena: Default::default(),
            str_interner: Default::default(),
            refs: Default::default(),
            ban_gc: Cell::new(true),
        }
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
        f(Heap(&heap, PhantomData))
    }

    /// Like `temp`, but `async`
    pub async fn temp_async<F, R>(f: F) -> R
    where
        F: for<'v2> AsyncFnOnce(Heap<'v2>) -> R,
    {
        // It's interesting to note that this is in fact more expressive than `temp` alone. While
        // it's possible for `temp` to return a future which the user can then await externally,
        // that future can't capture a reference to the heap. Here though, we allow the future
        // "returned" by this function to do so. We make that sound by also capturing the heap
        // itself in the future.
        let heap = OwnedHeap::new();
        f(Heap(&heap, PhantomData)).await
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
        self.add_reference(v.owner());

        // SAFETY: We just added a reference to this heap
        unsafe { v.unchecked_frozen_value().to_value() }
    }

    /// Similar to `access_owned_frozen_value`, but typed.
    pub fn access_owned_frozen_value_typed<T: for<'a> StarlarkValue<'a>>(
        self,
        v: &OwnedFrozenValueTyped<T>,
    ) -> ValueTyped<'v, T> {
        self.add_reference(v.owner());

        // SAFETY: We just added a reference to this heap
        unsafe { v.value_typed().to_value_typed() }
    }

    /// Add a dependency onto the provided frozen heap.
    pub fn add_reference(&self, h: &FrozenHeapRef) {
        let mut refs = self.0.refs.borrow_mut();
        if !refs.contains(h) {
            refs.insert(h.dupe());
        }
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

/// Object-safe trait for user-defined heap names that supports hashing and downcasting.
///
/// Automatically implemented for any type that is `StrongHash + Any + Send + Sync + Debug`.
/// `StrongHash` is required (rather than `Hash`) because heap identities are
/// derived from this and must be deterministic across processes.
pub trait UserHeapName: std::fmt::Display + Any + Send + Sync + Debug + 'static {
    /// Strong-hash this value through a trait object.
    fn dyn_strong_hash(&self, state: &mut dyn Hasher);
    /// Downcast support.
    fn as_any(&self) -> &dyn Any;

    fn clone_name(&self) -> Box<dyn UserHeapName>;
}

impl<T: std::fmt::Display + Clone + StrongHash + Any + Send + Sync + Debug + 'static> UserHeapName
    for T
{
    fn dyn_strong_hash(&self, mut state: &mut dyn Hasher) {
        self.strong_hash(&mut state);
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn clone_name(&self) -> Box<dyn UserHeapName> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn UserHeapName> {
    fn clone(&self) -> Box<dyn UserHeapName> {
        UserHeapName::clone_name(self.as_ref())
    }
}

/// Name/identifier for a frozen heap, used for heap graph tracking and metrics.
#[derive(Clone, derive_more::Display, Debug)]
pub enum FrozenHeapName {
    /// For starlark Methods heaps.
    Method(MethodFrozenHeapName),
    /// For the global starlark environment heap.
    Global(GlobalFrozenHeapName),
    /// For starlark singleton heaps
    Singleton(SingletonFrozenHeapName),
    /// For user/downstream code.
    User(Box<dyn UserHeapName>),
}

impl StrongHash for FrozenHeapName {
    fn strong_hash<H: Hasher>(&self, state: &mut H) {
        // Inner Method/Global/Singleton variants implement `Hash` (deterministic
        // here because we control the `Hasher`); the User variant goes through
        // the `StrongHash` trait object.
        std::mem::discriminant(self).hash(state);
        match self {
            FrozenHeapName::Method(m) => m.hash(state),
            FrozenHeapName::Global(g) => g.hash(state),
            FrozenHeapName::Singleton(s) => s.hash(state),
            FrozenHeapName::User(b) => b.dyn_strong_hash(state),
        }
    }
}

/// Testing sentinel for starlark crate's own tests.
/// Used as `FrozenHeapName::User(Box::new(StarlarkTestHeapName))`.
#[derive(Debug, StrongHash, Hash, Clone, derive_more::Display)]
#[display("StarlarkTestHeapName")]
pub(crate) struct StarlarkTestHeapName;

impl StarlarkTestHeapName {
    pub(crate) fn frozen_heap_name() -> FrozenHeapName {
        FrozenHeapName::User(Box::new(Self))
    }
}

/// A frozen heap name derived from source location, for singleton heaps.
///
/// This type can only be created via the [`singleton_heap_name!`](crate::singleton_heap_name)
/// macro, which captures `file!()`, `line!()`, and `column!()` at the call site.
/// This ensures each name is unique and stable across process runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SingletonFrozenHeapName {
    file: &'static str,
    line: u32,
    col: u32,
}

impl SingletonFrozenHeapName {
    /// Internal constructor. Do not call directly; use [`singleton_heap_name!`](crate::singleton_heap_name).
    #[doc(hidden)]
    pub const fn _new(file: &'static str, line: u32, col: u32) -> Self {
        Self { file, line, col }
    }
}

impl std::fmt::Display for SingletonFrozenHeapName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

/// Create a [`SingletonFrozenHeapName`] capturing the current source location.
///
/// Each call site produces a unique, stable name based on `file!()`, `line!()`, `column!()`.
///
/// ```
/// use starlark::singleton_heap_name;
/// let name = singleton_heap_name!();
/// ```
#[macro_export]
macro_rules! singleton_heap_name {
    () => {
        $crate::values::SingletonFrozenHeapName::_new(file!(), line!(), column!())
    };
}

/// `FrozenHeap` when it is no longer modified and can be share between threads.
/// Although, `arena` is not safe to share between threads, but at least `refs` is.
#[derive(Allocative)]
#[allow(clippy::non_send_fields_in_send_ty)]
struct FrozenFrozenHeap {
    arena: Arena<ChunkAllocator>,
    refs: Box<[FrozenHeapRef]>,
    // TODO(nero): remove Option here, make it required.
    #[allocative(skip)] // We don't really expect it to be big
    name: Option<FrozenHeapName>,
    peak_allocated_bytes: Option<usize>,
}

// Safe because we never mutate the Arena other than with &mut
unsafe impl Sync for FrozenFrozenHeap {}
unsafe impl Send for FrozenFrozenHeap {}

/// Intermediate state after reading heap metadata and allocating arena bumps,
/// but before deserializing values. Exposes base addresses so the deserializer
/// context can set them before phase 2.
pub(crate) struct PartiallyDeserializedHeap {
    arena: Arena<ChunkAllocator>,
    drop_base: usize,
    non_drop_base: usize,
    refs: Vec<FrozenHeapRef>,
    /// Value tracking info (headers already written, ready for deserialization).
    deser_state: HeapDeserializationState,
}

impl PartiallyDeserializedHeap {
    pub(crate) fn drop_base(&self) -> usize {
        self.drop_base
    }

    pub(crate) fn non_drop_base(&self) -> usize {
        self.non_drop_base
    }

    /// Take the deserialization state and raw heap components.
    /// The caller constructs `FrozenFrozenHeap` after phase 2 completes.
    pub(crate) fn take_deser_state_and_finish(
        self,
    ) -> (
        HeapDeserializationState,
        Arena<ChunkAllocator>,
        Vec<FrozenHeapRef>,
    ) {
        (self.deser_state, self.arena, self.refs)
    }
}

impl FrozenFrozenHeap {
    /// Serialization format:
    /// ```text
    /// [refs_count: usize]
    /// for each ref:
    ///     [pagable serialized arc]
    /// [drop_total_bytes: u32]
    /// [non_drop_total_bytes: u32]
    /// [total_value_count: u32]
    /// // Offset table — fixed-size, 8 raw LE bytes per entry.
    /// // (value_count + 1) entries: one per value + one sentinel end entry.
    /// // Written as placeholder, then patched via write_at after value data.
    /// for i in 0..=value_count:
    ///   [stream_offset: u32 LE]    // relative to base_pos
    ///   [arc_offset: u32 LE]       // relative to base arc_index
    /// // Metadata — postcard encoded, variable size.
    /// for each value:
    ///   [arena_offset: ArenaOffset]
    ///   [deser_type_id: u32]    // sorted index into the vtable registry
    ///   [alloc_size: u32]
    /// // base_pos starts here — offsets are relative to this point.
    /// // Value data — postcard encoded, sequential.
    /// for each value:
    ///   [value_data...]
    /// ```
    fn serialize_inner(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        let heap_name = self
            .name
            .as_ref()
            .expect("The name of the FrozenFrozenHeap should exist in starlark pagable serialize");
        let heap_id = HeapRefId::from_heap_name(heap_name);
        // TODO(nero): right now FrozenHeapName hasn't been implemented for Pagable. so just serialize HeapRefId;
        heap_id.pagable_serialize(serializer)?;

        fn bump_total_bytes(headers: &[&AValueHeader]) -> u32 {
            headers
                .iter()
                .map(|h| h.unpack().memory_size().bytes())
                .sum()
        }

        self.refs.len().pagable_serialize(serializer)?;
        for heap_ref in self.refs.iter() {
            heap_ref.pagable_serialize(serializer)?;
        }

        let drop_headers = self.arena.collect_drop_headers_ordered();
        let non_drop_headers = self.arena.collect_undrop_headers_ordered();

        // Serialize both total_bytes upfront so the deserializer can
        // pre-allocate both arena bumps before reading any values.
        bump_total_bytes(&drop_headers).pagable_serialize(serializer)?;
        bump_total_bytes(&non_drop_headers).pagable_serialize(serializer)?;

        // Collect all values: drop bump first, then non-drop bump.
        let total_count = drop_headers.len() + non_drop_headers.len();
        (total_count as u32).pagable_serialize(serializer)?;

        // Write offset table placeholder: (value_count + 1) entries × 8 bytes each.
        // The extra entry is the end sentinel (total stream bytes + total arcs).
        let table_pos = serializer.position();
        let table_entry_count = total_count + 1;
        let table_byte_size = table_entry_count * 8;
        for _ in 0..table_byte_size {
            0u8.pagable_serialize(serializer)?;
        }

        // Write metadata for each value (postcard encoded).
        let all_bumps: [(BumpKind, &[&AValueHeader]); 2] = [
            (BumpKind::Drop, &drop_headers),
            (BumpKind::NonDrop, &non_drop_headers),
        ];
        for (bump_kind, headers) in &all_bumps {
            let mut bump_offset: u32 = 0;
            for header in *headers {
                let avalue = header.unpack();
                let alloc_size = avalue.memory_size().bytes();
                ArenaOffset {
                    bump: *bump_kind,
                    offset: bump_offset,
                }
                .pagable_serialize(serializer)?;
                avalue
                    .vtable()
                    .deser_type_id
                    .pagable_serialize(serializer)?;
                alloc_size.pagable_serialize(serializer)?;
                bump_offset += alloc_size;
            }
        }

        // Record base_pos — all offsets are relative to here.
        let base_pos = serializer.position();
        // Get or create shared state. Ensure this heap and all its transitive
        // dependencies have offset maps registered before we serialize arena
        // values (which may contain cross-heap FrozenValue pointers).
        let state = StarlarkSerializerImpl::get_or_create_state(serializer);
        state.ensure_offset_maps_registered_inner(heap_id, &self.refs, || {
            self.arena.build_ptr_to_offset_map()
        });
        let mut ctx = StarlarkSerializerImpl::new(serializer, state);

        // Serialize value data, recording start cursor per value.
        let mut entry_cursors: Vec<(u32, u32)> = Vec::with_capacity(table_entry_count);
        for (_bump_kind, headers) in &all_bumps {
            for header in *headers {
                let start = ctx.pagable().position();
                entry_cursors.push((
                    (start.byte_pos - base_pos.byte_pos) as u32,
                    (start.arc_index - base_pos.arc_index) as u32,
                ));
                header.unpack().starlark_serialize(&mut ctx)?;
            }
        }
        // End sentinel: position after all value data.
        let end = ctx.pagable().position();
        entry_cursors.push((
            (end.byte_pos - base_pos.byte_pos) as u32,
            (end.arc_index - base_pos.arc_index) as u32,
        ));

        // Patch the offset table with actual values.
        let mut table_bytes = vec![0u8; table_byte_size];
        for (i, (stream_offset, arc_offset)) in entry_cursors.iter().enumerate() {
            let off = i * 8;
            table_bytes[off..off + 4].copy_from_slice(&stream_offset.to_le_bytes());
            table_bytes[off + 4..off + 8].copy_from_slice(&arc_offset.to_le_bytes());
        }
        // SAFETY: table_pos.byte_pos points to the placeholder written earlier,
        // and table_bytes has the correct size.
        unsafe { ctx.pagable().write_at(table_pos.byte_pos, &table_bytes) };

        Ok(())
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        let heap_id = HeapRefId::pagable_deserialize(deserializer)?;

        let partial = Self::deserialize_phase1(deserializer)?;
        let drop_base = partial.drop_base();
        let non_drop_base = partial.non_drop_base();

        let (deser_state, arena, refs) = partial.take_deser_state_and_finish();

        // Get or create shared state.
        let state = StarlarkDeserializerImpl::get_or_create_state(deserializer.as_dyn());
        let mut ctx = StarlarkDeserializerImpl::new(
            deserializer.as_dyn(),
            state.dupe(),
            Arc::new(Mutex::new(deser_state)),
        );

        // Register bases in shared state.
        state.register_bases(heap_id, drop_base, non_drop_base);

        let heap = Self::deserialize_phase2(arena, refs, &mut ctx)?;

        Ok(heap)
    }

    fn deserialize_phase1<'de, D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<PartiallyDeserializedHeap> {
        // Deserialize refs first (before arena values) so referenced heaps'
        // bases are registered in the context.
        let refs_count = usize::pagable_deserialize(deserializer)?;
        let mut refs = Vec::with_capacity(refs_count);
        for _ in 0..refs_count {
            refs.push(FrozenHeapRef::pagable_deserialize(deserializer)?);
        }

        let drop_total_bytes = u32::pagable_deserialize(deserializer)?;
        let non_drop_total_bytes = u32::pagable_deserialize(deserializer)?;

        // Read total value count.
        let total_count = u32::pagable_deserialize(deserializer)? as usize;
        // Read offset table: (value_count + 1) entries × 8 raw bytes each.
        // Last entry is the end sentinel.
        let table_entry_count = total_count + 1;
        let mut offset_table = Vec::with_capacity(table_entry_count);
        for _ in 0..table_entry_count {
            let mut buf = [0u8; 8];
            for b in &mut buf {
                *b = u8::pagable_deserialize(deserializer)?;
            }
            let stream_offset = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]);
            let arc_offset = u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]);
            offset_table.push((stream_offset, arc_offset));
        }

        // Read metadata for each value.
        let mut value_meta = Vec::with_capacity(total_count);
        for _ in 0..total_count {
            let arena_offset = ArenaOffset::pagable_deserialize(deserializer)?;
            let deser_type_id = DeserTypeId::pagable_deserialize(deserializer)?;
            let vtable = lookup_vtable(deser_type_id)?;
            let alloc_size = u32::pagable_deserialize(deserializer)?;
            value_meta.push((arena_offset, vtable, alloc_size));
        }
        // Record base_pos — all stream_offsets are relative to here.
        let base_pos = deserializer.position();

        let arena = Arena::default();

        let mut drop_cursor = arena.alloc_raw_drop_cursor(drop_total_bytes);
        let drop_base = drop_cursor.as_ref().map_or(0, |c| c.base());

        let mut non_drop_cursor = arena.alloc_raw_non_drop_cursor(non_drop_total_bytes);
        let non_drop_base = non_drop_cursor.as_ref().map_or(0, |c| c.base());

        // Write AValueHeaders to arena and build value info.
        let mut ptr_to_index = HashMap::new();
        let mut slots = Vec::with_capacity(total_count);
        // Use only the first total_count entries (skip the end sentinel).
        for (i, ((arena_offset, vtable, alloc_size), &(stream_offset, arc_offset))) in value_meta
            .iter()
            .zip(offset_table[..total_count].iter())
            .enumerate()
        {
            let cursor = match arena_offset.bump {
                BumpKind::Drop => drop_cursor.as_mut(),
                BumpKind::NonDrop => non_drop_cursor.as_mut(),
            };
            let cursor = cursor.expect("cursor must exist for bump with values");
            unsafe {
                let header_ptr = cursor.next(*alloc_size);
                // Write sentinel vtable — any access before deserialization
                // will panic with "accessing uninitialized deserialized value".
                // The real vtable is stored in the slot and written after
                // starlark_deserialize completes.
                ptr::write(
                    header_ptr,
                    AValueHeader(AValueVTable::uninitialized_sentinel()),
                );
                let raw_ptr = StarlarkValueRawPtr::new_header(&*header_ptr);
                let header_addr = header_ptr as *const _ as usize;
                ptr_to_index.insert(header_addr, i);
                slots.push(ValueDeserSlot::new(
                    stream_offset,
                    arc_offset,
                    vtable,
                    raw_ptr,
                    header_ptr,
                ));
            }
        }

        // The last offset table entry is the end sentinel.
        let &(end_stream_offset, end_arc_offset) = offset_table.last().unwrap();
        let end_pos = PagableCursor {
            byte_pos: base_pos.byte_pos + end_stream_offset as usize,
            arc_index: base_pos.arc_index + end_arc_offset as usize,
        };
        let deser_state = HeapDeserializationState::new(slots, ptr_to_index, base_pos, end_pos);

        Ok(PartiallyDeserializedHeap {
            arena,
            drop_base,
            non_drop_base,
            refs,
            deser_state,
        })
    }

    fn deserialize_phase2(
        arena: Arena<ChunkAllocator>,
        refs: Vec<FrozenHeapRef>,
        ctx: &mut StarlarkDeserializerImpl<'_, '_>,
    ) -> crate::Result<FrozenFrozenHeap> {
        let count = ctx.current_heap_deser_state().value_count();
        for i in 0..count {
            let target = ctx.current_heap_deser_state().try_claim(i);
            if let Some(target) = target {
                // SAFETY: abs_pos is computed from the offset table written during
                // serialization — it points to the start of this value's data.
                unsafe { ctx.pagable().seek(target.abs_pos) };
                (target.vtable.starlark_deserialize)(target.raw_ptr, ctx)?;
                // Replace the sentinel vtable with the real one now that
                // deserialization is complete. The sentinel must stay in place
                // until this point so that any access to the value before it
                // is fully deserialized will panic.
                unsafe { target.write_vtable_to_header() };
            }
        }
        let end = ctx.current_heap_deser_state().end_position();
        // SAFETY: end_position is past the last value's data in this heap.
        unsafe { ctx.pagable().seek(end) };

        Ok(FrozenFrozenHeap {
            arena,
            refs: refs.into_boxed_slice(),
            name: None,
            peak_allocated_bytes: None,
        })
    }
}

/// PagableSerialize for FrozenFrozenHeap — delegates to StarlarkSerialize
/// by creating a local StarlarkSerializerImpl from the pagable serializer.
impl PagableSerialize for FrozenFrozenHeap {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.serialize_inner(serializer)
            .map_err(|e| e.into_anyhow())
    }
}

/// PagableBoxDeserialize for FrozenFrozenHeap — creates a local StarlarkDeserializerImpl,
/// runs the two-phase deserialization, and returns the heap.
impl<'de> PagableBoxDeserialize<'de> for FrozenFrozenHeap {
    fn deserialize_box<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Box<Self>> {
        let heap =
            FrozenFrozenHeap::deserialize_inner(deserializer).map_err(|e| e.into_anyhow())?;
        Ok(Box::new(heap))
    }
}

/// PagableSerialize for FrozenHeapRef — serializes the inner Arc via pagable arc mechanism.
impl PagableSerialize for FrozenHeapRef {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        let is_some = self.0.is_some();
        is_some.pagable_serialize(serializer)?;
        if let Some(ref arc) = self.0 {
            serializer.serialize_arc(arc)?;
        }
        Ok(())
    }
}

/// PagableDeserialize for FrozenHeapRef — deserializes the inner Arc via pagable arc mechanism.
impl<'de> PagableDeserialize<'de> for FrozenHeapRef {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let is_some = bool::pagable_deserialize(deserializer)?;
        if is_some {
            let arc: Arc<FrozenFrozenHeap> = pagable::arc_erase::deserialize_arc(deserializer)?;
            Ok(FrozenHeapRef(Some(arc)))
        } else {
            Ok(FrozenHeapRef::default())
        }
    }
}

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

    /// Peak number of bytes allocated on the live heap that produced this frozen heap.
    pub fn peak_allocated_bytes(&self) -> Option<usize> {
        self.0.as_ref().and_then(|a| a.peak_allocated_bytes)
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

    /// Get the name of this heap.
    ///
    /// Names can be assigned when finalizing frozen heaps; in practice, this is done when freezing
    /// modules, see [`Module::freeze_named`](crate::environment::Module::freeze_named).
    ///
    /// The name is intentionally made available here and not at a higher point like the module
    /// level so that it can be inspected even when traversing the dependency graph of frozen heaps.
    pub fn name(&self) -> Option<&FrozenHeapName> {
        self.0.as_ref().and_then(|a| a.name.as_ref())
    }

    /// Get the frozen heaps that this frozen heap depends on.
    pub fn refs(&self) -> impl Iterator<Item = &FrozenHeapRef> {
        self.0.as_ref().map(|h| h.refs.iter()).into_iter().flatten()
    }

    /// Get the frozen heaps that this frozen heap depends on, as a slice.
    pub(crate) fn refs_slice(&self) -> &[FrozenHeapRef] {
        match &self.0 {
            Some(inner) => &inner.refs,
            None => &[],
        }
    }

    pub(crate) fn iter_values(&self) -> impl Iterator<Item = FrozenValue> {
        struct FrozenValueCollector(Vec<FrozenValue>);
        let mut items = FrozenValueCollector(Vec::new());
        if let Some(heap) = &self.0 {
            impl<'v> ArenaVisitor<'v> for FrozenValueCollector {
                fn enter_bump(&mut self) {}

                fn regular_value(&mut self, value: &'v super::repr::AValueOrForward) {
                    self.0.push(
                        unsafe {
                            value
                                .unpack_header()
                                .expect("static heap should not contain forwards")
                                .unpack_value(HeapKind::Frozen)
                        }
                        .unpack_frozen()
                        .expect("value from frozen heap should be frozen"),
                    );
                }

                fn call_enter(&mut self, _function: Value<'v>, _time: ProfilerInstant) {}

                fn call_exit(&mut self, _time: ProfilerInstant) {}
            }
            unsafe {
                heap.arena
                    .visit_arena(HeapKind::Frozen, HeapKind::Frozen, &mut items)
            };
        }
        items.0.into_iter()
    }

    /// Build a map from raw header pointer address to `ArenaOffset`
    /// for all values in this heap's arena.
    pub(crate) fn build_ptr_to_offset_map(&self) -> HashMap<usize, ArenaOffset> {
        match &self.0 {
            Some(inner) => inner.arena.build_ptr_to_offset_map(),
            None => HashMap::new(),
        }
    }

    /// Collect live value headers from the non-drop bump in allocation order.
    #[cfg(all(test, feature = "pagable"))]
    pub(crate) fn collect_undrop_headers_ordered(&self) -> Vec<&AValueHeader> {
        match &self.0 {
            Some(inner) => inner.arena.collect_undrop_headers_ordered(),
            None => Vec::new(),
        }
    }

    /// Collect live value headers from the drop bump in allocation order.
    #[cfg(all(test, feature = "pagable"))]
    pub(crate) fn collect_drop_headers_ordered(&self) -> Vec<&AValueHeader> {
        match &self.0 {
            Some(inner) => inner.arena.collect_drop_headers_ordered(),
            None => Vec::new(),
        }
    }
}

impl FrozenHeap {
    /// Create a new [`FrozenHeap`].
    pub fn new() -> Self {
        Self::default()
    }

    /// After all values have been allocated, convert the [`FrozenHeap`] into a
    /// named [`FrozenHeapRef`] which can be [`clone`](Clone::clone)d, shared between threads,
    /// and ensures the underlying values allocated on the [`FrozenHeap`] remain valid.
    ///
    /// The `name` identifies this heap and should be unique across heaps.
    /// See [`FrozenHeapRef::name`] for more details.
    pub fn into_ref_named(self, name: FrozenHeapName) -> FrozenHeapRef {
        self.into_ref_impl(Some(name), None)
    }

    /// After all values have been allocated, convert the [`FrozenHeap`] into a
    /// [`FrozenHeapRef`] which can be [`clone`](Clone::clone)d, shared between threads,
    /// and ensures the underlying values allocated on the [`FrozenHeap`] remain valid.
    ///
    /// When the `pagable` feature is enabled, this method is hidden to enforce
    /// that all heaps are named. Use [`into_ref_named`](Self::into_ref_named) instead.
    #[cfg(not(feature = "pagable"))]
    pub fn into_ref(self) -> FrozenHeapRef {
        self.into_ref_impl(None, None)
    }

    pub(crate) fn into_ref_impl(
        self,
        name: Option<FrozenHeapName>,
        peak_allocated_bytes: Option<usize>,
    ) -> FrozenHeapRef {
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
                name,
                peak_allocated_bytes,
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

    pub(crate) fn reserve_with_extra<'v, 'v2, T>(
        &'v self,
        extra_len: usize,
    ) -> (
        FrozenValue,
        Reservation<'v2, T>,
        *mut [MaybeUninit<T::ExtraElem>],
    )
    where
        T: AValue<'v2>,
        T::StarlarkValue: HeapSendable<'v2>,
        T::StarlarkValue: HeapSyncable<'v2>,
    {
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

    /// Allow gcing in this heap
    ///
    /// # SAFETY
    ///
    /// This is basically impossible to reason about, hence its existence in the first place
    pub(crate) unsafe fn allow_gc(&self) {
        self.0.ban_gc.set(false);
    }

    /// Garbage collect any values that are unused. This function is _unsafe_ in
    /// the sense that any `Value<'v>` not returned by `Tracer` _will become
    /// invalid_. Furthermore, any references to values, e.g `&'v str` will
    /// also become invalid.
    pub(crate) unsafe fn garbage_collect(self, f: impl FnOnce(&Tracer<'v>)) {
        if self.0.ban_gc.get() {
            return;
        }

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

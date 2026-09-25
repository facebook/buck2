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

//! Implementation of StarlarkDeserializeContext.

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::OnceLock;
use std::sync::RwLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::thread::ThreadId;

use allocative::Allocative;
use dashmap::DashMap;
use dashmap::mapref::entry::Entry;
use dupe::Dupe;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableDeserializerRecipe;
use pagable::PageInScope;
use pagable::PageInState;
use pagable::StorageState;
use pagable::storage::handle::PagableStorageHandle;

use crate::pagable::DeserTypeId;
use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::lookup_vtable;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerState;
use crate::pagable::static_value::get_static_value_by_id;
use crate::values::Value;
use crate::values::layout::heap::allocator::alloc::allocator::ChunkAllocator;
use crate::values::layout::heap::arena::Arena;
use crate::values::layout::heap::arena::BumpKind;
use crate::values::layout::heap::arena::ChunkInfo;
use crate::values::layout::heap::edge::HeapEdge;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::sealed::ArcKey;
use crate::values::layout::heap::sealed::FrozenHeapArc;
use crate::values::layout::heap::sealed::FrozenHeapPtr;
use crate::values::layout::heap::sealed::WeakFrozenHeapRef;
use crate::values::layout::heap::sealed::cached_heap_deserialization_state_retained_bytes;
use crate::values::layout::heap::sealed::load_and_bind_heap_by_id;
use crate::values::layout::heap::sealed::register_heap_key_index;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::types::int::inline_int::InlineInt;

/// Per-slot metadata for partial-deser. Immutable after `deserialize_metadata`.
#[derive(Allocative)]
pub(crate) struct ValueDeserSlot {
    /// Byte offset of this value's data relative to base_pos.
    stream_offset: u32,
    /// Arc index offset relative to base_pos.arc_index.
    arc_offset: u32,
    /// This value's vtable, used for deserialization dispatch.
    #[allocative(skip)]
    vtable: &'static AValueVTable,
    /// Which bump (drop or non-drop) this value lives in.
    bump_kind: BumpKind,
    /// Size in bytes to allocate for this value's header + payload.
    alloc_size: NonZeroU32,
}

impl ValueDeserSlot {
    pub(crate) fn new(
        stream_offset: u32,
        arc_offset: u32,
        vtable: &'static AValueVTable,
        bump_kind: BumpKind,
        alloc_size: NonZeroU32,
    ) -> Self {
        Self {
            stream_offset,
            arc_offset,
            vtable,
            bump_kind,
            alloc_size,
        }
    }
}

/// Info returned by `try_claim` — everything the caller needs to deserialize a value.
pub(crate) struct DeserializeRecipe {
    /// Absolute cursor position of this value's data.
    pub(crate) abs_pos: PagableCursor,
    /// Vtable for deserialization dispatch.
    pub(crate) vtable: &'static AValueVTable,
    /// Raw pointer to the pre-allocated header in the arena.
    pub(crate) raw_ptr: StarlarkValueRawPtr,
    /// Pointer to the AValueHeader in the arena (for vtable patching after deserialization).
    pub(crate) header_ptr: *mut AValueHeader,
}

impl DeserializeRecipe {
    /// Write the real vtable to the header, replacing the sentinel.
    /// Must be called after `starlark_deserialize` completes.
    pub(crate) unsafe fn write_vtable_to_header(&self) {
        unsafe {
            std::ptr::write(self.header_ptr, AValueHeader(self.vtable));
        }
    }
}

/// Decoded form of an [`AtomicSlotState`].
enum SlotState {
    NotStarted,
    /// Claimed and mid-deserialization; carries the pre-allocated header (its
    /// vtable is still the sentinel).
    InProgress(*mut AValueHeader),
    Failed,
    Done(*mut AValueHeader),
}

impl SlotState {
    /// Header pointer if the slot is finalized, else `None`.
    fn done_ptr(self) -> Option<*mut AValueHeader> {
        match self {
            SlotState::Done(ptr) => Some(ptr),
            _ => None,
        }
    }

    /// [`ClaimResult`] for a slot observed by a non-winning caller, or `None` if
    /// it is not started yet (the caller should attempt to claim it).
    fn observed_claim_result(self) -> Option<ClaimResult> {
        match self {
            SlotState::NotStarted => None,
            SlotState::InProgress(ptr) => Some(ClaimResult::InProgress(ptr)),
            SlotState::Failed => Some(ClaimResult::Failed),
            SlotState::Done(_) => Some(ClaimResult::Done),
        }
    }
}

/// Per-slot init state and the single owner of the [`SlotState`] encoding, which
/// packs all four states into one `u64`:
/// 1. `0` (`INIT_NOT_STARTED`) — not started.
/// 2. in progress — bit 0 (`IN_PROGRESS_FLAG`) set; the header pointer is in the
///    remaining bits.
/// 3. done — any other non-zero value (both low bits clear); the value *is* the
///    header pointer.
/// 4. `0b10` (`INIT_FAILED_FLAG`) — failed.
///
/// The low bits are free for the flags because `AValueHeader` is ≥ 8-byte
/// aligned (checked below).
#[derive(Allocative)]
#[repr(transparent)]
struct AtomicSlotState(AtomicU64);

const _: () = {
    assert!(AValueHeader::ALIGN > AtomicSlotState::INIT_STATE_MASK as usize);
};

impl AtomicSlotState {
    const INIT_NOT_STARTED: u64 = 0;
    const IN_PROGRESS_FLAG: u64 = 0b1;
    const INIT_FAILED_FLAG: u64 = 0b10;
    const INIT_STATE_MASK: u64 = Self::IN_PROGRESS_FLAG | Self::INIT_FAILED_FLAG;

    fn not_started() -> Self {
        AtomicSlotState(AtomicU64::new(Self::INIT_NOT_STARTED))
    }

    fn load(&self, order: Ordering) -> SlotState {
        let v = self.0.load(order);
        if v == Self::INIT_NOT_STARTED {
            SlotState::NotStarted
        } else if v == Self::INIT_FAILED_FLAG {
            SlotState::Failed
        } else if v & Self::IN_PROGRESS_FLAG != 0 {
            SlotState::InProgress((v & !Self::INIT_STATE_MASK) as *mut AValueHeader)
        } else {
            SlotState::Done(v as *mut AValueHeader)
        }
    }

    /// Publish the claim: store the pre-allocated `header` with the in-progress
    /// flag set. The caller holds the arena lock, so this is the not-started ->
    /// in-progress transition.
    fn publish_in_progress(&self, header: *mut AValueHeader) {
        self.0
            .store((header as u64) | Self::IN_PROGRESS_FLAG, Ordering::Release);
    }

    /// Promote in-progress -> done by clearing the state flags, keeping the
    /// header the claim already published. Takes no pointer: the only valid value
    /// is the one stored at claim time.
    fn finalize(&self) {
        let prev = self.0.fetch_and(!Self::INIT_STATE_MASK, Ordering::AcqRel);
        debug_assert!(
            prev & Self::IN_PROGRESS_FLAG != 0,
            "finalize on a slot that was not in progress: {:#x}",
            prev,
        );
    }

    /// Publish the claim as failed.
    fn fail(&self) {
        self.0.store(Self::INIT_FAILED_FLAG, Ordering::Release);
    }
}

#[derive(Allocative)]
struct InitWaiters {
    state: Mutex<InitWaiterState>,
    #[allocative(skip)]
    cv: OnceLock<Condvar>,
}

#[derive(Allocative, Default)]
struct InitWaiterState {
    /// Failure records are sparse because successful slots never need this
    /// diagnostic state.
    failures: Vec<SlotFailure>,
}

#[derive(Allocative)]
struct SlotFailure {
    index: usize,
    cause: Arc<str>,
}

impl InitWaiters {
    fn new() -> Self {
        Self {
            state: Mutex::new(InitWaiterState::default()),
            cv: OnceLock::new(),
        }
    }
}

pub(crate) enum ClaimResult {
    Claimed(DeserializeRecipe),
    Done,
    /// Slot is mid-deserialization. Carries its pre-allocated header, whose
    /// vtable is still the sentinel (value not materialized yet).
    InProgress(*mut AValueHeader),
    Failed,
}

/// Metadata + init state — lazily parsed from the recipe on first
/// `try_claim`.
#[derive(Allocative)]
pub(crate) struct HeapMetadata {
    /// All values in this heap.
    slots: Vec<ValueDeserSlot>,
    /// Absolute cursor position of value data start (base for relative offsets).
    base_pos: PagableCursor,
    /// Per-slot init state. See [`AtomicSlotState`].
    init_states: Vec<AtomicSlotState>,
    /// Reverse map from each claimed payload address to its original recipe
    /// index. Lazy allocation order is not necessarily recipe order.
    ///
    /// Populated when a slot is claimed: the mapping is a
    /// property of the arena allocation, and recording it up front means no
    /// reader can observe a payload address without also finding its index. An
    /// aborted claim therefore leaves a correct-but-unreferenced entry, which is
    /// preferable to the address resolving to nothing.
    original_indices_by_payload: RwLock<HashMap<usize, u32>>,
    /// Coordinates waiters that lost a per-slot initialization race.
    init_waiters: InitWaiters,
}

#[derive(Allocative)]
struct HeapArenaState {
    #[allocative(skip)]
    arena: NonNull<Arena<ChunkAllocator>>,
    serialization_index_dirty: bool,
}

/// A heap's header once loaded: how to reopen its data, and where the lazily
/// parsed slot metadata starts within it. The dependencies the header names
/// are bound on the heap itself.
#[derive(Allocative)]
struct HeapHeaderState {
    recipe: Arc<dyn PagableDeserializerRecipe>,
    metadata_start: PagableCursor,
}

/// The owning `FrozenFrozenHeap`'s arena and the information needed to lazily
/// parse this heap's slot metadata from its data. A restored heap passes
/// through stages, each on first need: skeleton, header loaded, slot metadata
/// parsed, values restored one by one.
#[derive(Allocative)]
pub(crate) struct HeapDeserializationState {
    heap_id: HeapRefId,
    /// Scope state that owns cross-heap resolution for this heap.
    scope: Arc<StarlarkDeserScope>,
    /// Where the heap's data is, for a skeleton bound from a ref list without
    /// loading its header. `None` for a heap read in place.
    #[allocative(skip)]
    source: Option<ArcKey>,
    /// The header, once loaded.
    header: OnceLock<HeapHeaderState>,
    /// Locked pointer into the owning `FrozenFrozenHeap`'s arena and the state
    /// needed to refresh its serialization index after lazy allocation.
    arena: Mutex<HeapArenaState>,
    /// Lazy: parsed on first `try_claim` / `value_count` call.
    metadata: OnceLock<HeapMetadata>,
}

// SAFETY: `arena` points into a heap-allocated `FrozenFrozenHeap` kept alive
// for the state's lifetime; concurrent allocations are serialized by the Mutex.
unsafe impl Sync for HeapDeserializationState {}
unsafe impl Send for HeapDeserializationState {}

impl HeapDeserializationState {
    /// # Safety
    /// `arena` must point to a `FrozenFrozenHeap.arena` whose containing
    /// `FrozenFrozenHeap` will be kept alive for at least
    /// as long as this `HeapDeserializationState`.
    pub(crate) unsafe fn new(
        scope: Arc<StarlarkDeserScope>,
        heap_id: HeapRefId,
        source: Option<ArcKey>,
        arena: *const Arena<ChunkAllocator>,
    ) -> Self {
        Self {
            scope,
            heap_id,
            source,
            header: OnceLock::new(),
            arena: Mutex::new(HeapArenaState {
                // SAFETY: caller's contract — `arena` is a valid pointer.
                arena: unsafe { NonNull::new_unchecked(arena as *mut _) },
                serialization_index_dirty: true,
            }),
            metadata: OnceLock::new(),
        }
    }

    pub(crate) fn unregister_heap(&self, heap_ptr: FrozenHeapPtr) {
        self.scope.unregister_heap(self.heap_id, heap_ptr);
    }

    /// Where the heap's data is, if it is still in storage.
    pub(crate) fn source(&self) -> Option<&ArcKey> {
        self.source.as_ref()
    }

    /// Whether the header is loaded: the heap's dependencies are bound and the
    /// recipe continuing into its slot metadata is retained. Slot metadata and
    /// values may still be unread.
    pub(crate) fn is_header_loaded(&self) -> bool {
        self.header.get().is_some()
    }

    /// Record the header as loaded. Two loaders of one header record the same
    /// thing; the heap, and its retained bytes, are counted once, for the load
    /// kept.
    pub(crate) fn set_header(
        &self,
        recipe: Arc<dyn PagableDeserializerRecipe>,
        metadata_start: PagableCursor,
    ) {
        let header = HeapHeaderState {
            recipe,
            metadata_start,
        };
        if self.header.set(header).is_err() {
            return;
        }
        // Counts headers loaded and retained, whether or not any value is ever
        // claimed - a dependency loaded for its own dependencies lands here too.
        if partial_deser_stats::enabled() {
            partial_deser_stats::add(&partial_deser_stats::HEAPS_LOADED, 1);
            if let Some(header) = self.header.get() {
                partial_deser_stats::add(
                    &partial_deser_stats::HEAP_RETAINED_BLOB_BYTES,
                    recipe_retained_data_len(&*header.recipe),
                );
            }
        }
    }

    fn header(&self) -> crate::Result<&HeapHeaderState> {
        self.header.get().ok_or_else(|| {
            anyhow::anyhow!(
                "heap {:?}: values requested before its header was loaded",
                self.heap_id
            )
            .into()
        })
    }

    /// Number of values in this heap.
    pub(crate) fn value_count(&self, storage: &PagableStorageHandle) -> crate::Result<usize> {
        Ok(self.metadata(storage)?.slots.len())
    }

    /// Parse the metadata region from the recipe: `total_count`,
    /// `drop_value_count`, offset table, per-value metadata. Called
    /// once on first `metadata()`; subsequent calls hit `OnceLock`.
    fn parse_metadata(
        &self,
        storage: &PagableStorageHandle,
    ) -> crate::Result<(HeapMetadata, Option<partial_deser_stats::MetadataStats>)> {
        let header = self.header()?;
        let mut de = header.recipe.open(storage);
        // SAFETY: `metadata_start` was captured while loading this header; it is
        // a valid position in the recipe.
        unsafe { de.seek(header.metadata_start) };

        let total_count = u32::pagable_deserialize(&mut *de)? as usize;
        let drop_value_count = u32::pagable_deserialize(&mut *de)? as usize;
        let table_entry_count = total_count + 1;
        let mut offset_table = Vec::with_capacity(table_entry_count);
        for _ in 0..table_entry_count {
            let mut buf = [0u8; 8];
            for b in &mut buf {
                *b = u8::pagable_deserialize(&mut *de)?;
            }
            let stream_offset = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]);
            let arc_offset = u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]);
            offset_table.push((stream_offset, arc_offset));
        }

        let mut slots: Vec<ValueDeserSlot> = Vec::with_capacity(total_count);
        for (i, &(stream_offset, arc_offset)) in offset_table.iter().take(total_count).enumerate() {
            let deser_type_id = DeserTypeId::pagable_deserialize(&mut *de)?;
            let vtable = lookup_vtable(deser_type_id)?;
            // A zero alloc_size is never valid (every value occupies at least its
            // header), so a zero here means a corrupt page-out stream.
            let alloc_size = NonZeroU32::new(u32::pagable_deserialize(&mut *de)?)
                .ok_or(PagableError::ZeroAllocSize { index: i })?;
            let bump_kind = if i < drop_value_count {
                BumpKind::Drop
            } else {
                BumpKind::NonDrop
            };
            slots.push(ValueDeserSlot::new(
                stream_offset,
                arc_offset,
                vtable,
                bump_kind,
                alloc_size,
            ));
        }
        // base_pos is the cursor right after per-value metadata — i.e. now.
        let base_pos = de.position();

        // Recorded by `metadata` only for the parse that is kept: two threads
        // can race to parse one heap, and counting here would count it twice.
        let stats = partial_deser_stats::enabled().then(|| partial_deser_stats::MetadataStats {
            retained_blob_bytes: recipe_retained_data_len(&*header.recipe),
            values: total_count as u64,
            // Walks every slot, so it is behind the same branch.
            value_alloc_bytes: slots.iter().map(|s| s.alloc_size.get() as u64).sum(),
            // The sentinel entry makes the value region's extent last minus
            // first offset. `saturating_sub`: these come off the stream, and a
            // diagnostic must not panic on a malformed table.
            value_serialized_bytes: match (offset_table.first(), offset_table.last()) {
                (Some(first), Some(last)) => last.0.saturating_sub(first.0) as u64,
                _ => 0,
            },
        });
        let init_states: Vec<AtomicSlotState> = (0..total_count)
            .map(|_| AtomicSlotState::not_started())
            .collect();
        Ok((
            HeapMetadata {
                slots,
                base_pos,
                init_states,
                original_indices_by_payload: RwLock::new(HashMap::new()),
                init_waiters: InitWaiters::new(),
            },
            stats,
        ))
    }

    /// Get parsed metadata, parsing on first call. Subsequent calls hit `OnceLock`.
    fn metadata(&self, storage: &PagableStorageHandle) -> crate::Result<&HeapMetadata> {
        if let Some(m) = self.metadata.get() {
            return Ok(m);
        }
        let (parsed, stats) = self.parse_metadata(storage)?;
        // `set` reports whether this parse is the one kept, so a heap is
        // counted once however many threads raced to parse it.
        if self.metadata.set(parsed).is_ok()
            && let Some(stats) = stats
        {
            stats.record();
        }
        Ok(self
            .metadata
            .get()
            .expect("metadata is populated: this thread set it, or lost to one that did"))
    }

    /// Return the header pointer for slot `index` if it's been finalized.
    #[inline]
    pub(crate) fn loaded_header_ptr(&self, index: usize) -> Option<*mut AValueHeader> {
        let m = self.metadata.get()?;
        m.init_states[index].load(Ordering::Acquire).done_ptr()
    }

    /// Return the original recipe index for a claimed payload pointer.
    pub(crate) fn original_value_index(&self, raw_ptr: usize) -> Option<u32> {
        self.metadata
            .get()?
            .original_indices_by_payload
            .read()
            .expect("original index map lock poisoned")
            .get(&raw_ptr)
            .copied()
    }

    pub(crate) fn serialization_index_is_dirty(&self) -> bool {
        self.arena
            .lock()
            .expect("arena lock poisoned")
            .serialization_index_dirty
    }

    pub(crate) fn refresh_serialization_index(
        &self,
        is_registered: impl FnOnce() -> bool,
        register: impl FnOnce(Vec<ChunkInfo>),
    ) {
        let mut state = self.arena.lock().expect("arena lock poisoned");
        if !state.serialization_index_dirty && is_registered() {
            return;
        }

        // SAFETY: `state.arena` remains valid for this state's lifetime, and
        // the lock excludes lazy allocation while the index is constructed.
        let entries = unsafe { state.arena.as_ref().build_chunk_index() };
        register(entries);
        state.serialization_index_dirty = false;
    }

    /// Try to claim a slot for deserialization.
    ///
    /// Claims are serialized by the arena lock: the winner allocates the header
    /// and publishes its pointer into the slot's atomic *before* releasing the
    /// lock, so a claimed slot always carries its pointer and no reader ever has
    /// to wait for it to appear. This is why observing a started slot never blocks.
    ///
    /// The winner also records this slot's `payload address -> recipe index`
    /// mapping before publishing, so the wire identity of a lazily allocated
    /// value is in place for the whole time that value is reachable.
    ///
    /// On win, returns `Claimed(recipe)` with a freshly-allocated `header_ptr`
    /// pointing to a sentinel-vtable header in the arena. The caller must run
    /// `recipe.vtable.starlark_deserialize`, call
    /// `recipe.write_vtable_to_header()`, then `finalize_claim(index)`.
    /// On loss, returns the slot's terminal state or its in-progress deserialization pointer.
    pub(crate) fn try_claim(
        &self,
        index: usize,
        storage: &PagableStorageHandle,
    ) -> crate::Result<ClaimResult> {
        let m = self.metadata(storage)?;
        let state = &m.init_states[index];

        // Fast path: a started slot (done, failed, or in-progress) is fully
        // decodable without taking the lock.
        if let Some(result) = state.load(Ordering::Acquire).observed_claim_result() {
            return Ok(result);
        }

        // The arena lock serializes claims: its holder performs the not-started
        // -> in-progress transition and publishes the header pointer before
        // releasing, so an in-progress slot is never visible without its pointer.
        let mut arena = self.arena.lock().expect("arena lock poisoned");

        // Re-check under the lock; the slot may have been claimed since the load
        // above.
        if let Some(result) = state.load(Ordering::Acquire).observed_claim_result() {
            return Ok(result);
        }

        let slot = &m.slots[index];
        // SAFETY: pointer valid for the state's lifetime; we hold the lock so
        // concurrent allocation is excluded.
        let header_ptr = unsafe {
            arena
                .arena
                .as_ref()
                .alloc_raw_one(slot.bump_kind, slot.alloc_size)
        };
        arena.serialization_index_dirty = true;
        // SAFETY: sentinel vtable so any access before `starlark_deserialize`
        // would panic.
        unsafe {
            std::ptr::write(
                header_ptr,
                AValueHeader(AValueVTable::uninitialized_sentinel()),
            );
        }
        // SAFETY: the claim owns this header. `payload_ptr` is address
        // arithmetic and never reads the (still sentinel) vtable.
        let raw_ptr = unsafe { StarlarkValueRawPtr::new_header(&*header_ptr) };
        m.original_indices_by_payload
            .write()
            .expect("original index map lock poisoned")
            .insert(
                raw_ptr.ptr as usize,
                u32::try_from(index).expect("recipe index should fit in u32"),
            );
        state.publish_in_progress(header_ptr);
        drop(arena);

        // On the winning claim only, so each value is counted once.
        if partial_deser_stats::enabled() {
            partial_deser_stats::add(&partial_deser_stats::CLAIMED_VALUES, 1);
            partial_deser_stats::add(
                &partial_deser_stats::CLAIMED_ALLOC_BYTES,
                slot.alloc_size.get() as u64,
            );
        }

        Ok(ClaimResult::Claimed(DeserializeRecipe {
            abs_pos: PagableCursor {
                byte_pos: m.base_pos.byte_pos + slot.stream_offset as usize,
                arc_index: m.base_pos.arc_index + slot.arc_offset as usize,
            },
            vtable: slot.vtable,
            raw_ptr,
            header_ptr,
        }))
    }

    /// Block on the slot's condvar until it is published done or failed.
    fn wait_for_init(&self, m: &HeapMetadata, state: &AtomicSlotState) -> ClaimResult {
        let cv = m.init_waiters.cv.get_or_init(Condvar::new);
        let mut guard = m
            .init_waiters
            .state
            .lock()
            .expect("init waiter lock poisoned");

        loop {
            match state.load(Ordering::Acquire) {
                SlotState::Done(_) => return ClaimResult::Done,
                SlotState::Failed => return ClaimResult::Failed,
                // Not started or still in progress — keep waiting.
                SlotState::NotStarted | SlotState::InProgress(_) => {}
            }

            guard = cv.wait(guard).expect("init waiter lock poisoned");
        }
    }

    /// Block until slot `index` is done or failed.
    pub(crate) fn wait_for_slot(
        &self,
        index: usize,
        storage: &PagableStorageHandle,
    ) -> crate::Result<ClaimResult> {
        let m = self.metadata(storage)?;
        let state = &m.init_states[index];
        Ok(self.wait_for_init(m, state))
    }

    /// Publish slot `index` as done; waiters in `wait_for_slot` then return. Call
    /// after `write_vtable_to_header`. No header argument — see `finalize`.
    pub(crate) fn finalize_claim(&self, index: usize) {
        self.publish_and_notify(index, |state, _waiters| state.finalize());
    }

    /// Publish slot `index` as failed. Call if the winning deserializer errors
    /// before `finalize_claim`.
    #[cold]
    pub(crate) fn abort_claim(&self, index: usize, error: &crate::Error) {
        let cause = Arc::<str>::from(format!("{error:#}"));
        self.publish_and_notify(index, |state, waiters| {
            waiters.failures.push(SlotFailure { index, cause });
            state.fail();
        });
    }

    /// Reconstruct the typed error for a slot whose original deserializer
    /// already failed.
    #[cold]
    fn partial_deserialization_error(&self, value_index: u32) -> PagableError {
        let m = self
            .metadata
            .get()
            .expect("failed slot must have parsed metadata");
        let index = value_index as usize;
        let cause = m
            .init_waiters
            .state
            .lock()
            .expect("init waiter lock poisoned")
            .failures
            .iter()
            .find(|failure| failure.index == index)
            .map(|failure| failure.cause.dupe())
            .unwrap_or_else(|| Arc::from("original deserialization error was not recorded"));

        PagableError::PartialDeserializationFailed {
            heap_id: self.heap_id,
            value_index,
            value_type: m.slots[index].vtable.type_name,
            cause,
        }
    }

    /// Apply a terminal transition and its diagnostic state under the
    /// init-waiter lock, then wake any waiters. Publishing both under one lock
    /// prevents lost wakeups and makes the failure cause visible before the
    /// atomic slot state becomes `Failed`.
    fn publish_and_notify(
        &self,
        index: usize,
        transition: impl FnOnce(&AtomicSlotState, &mut InitWaiterState),
    ) {
        let m = self
            .metadata
            .get()
            .expect("publish_and_notify called before metadata parse");
        let mut waiters = m
            .init_waiters
            .state
            .lock()
            .expect("init waiter lock poisoned");
        transition(&m.init_states[index], &mut waiters);
        if let Some(cv) = m.init_waiters.cv.get() {
            cv.notify_all();
        }
    }
}

/// The live heap for each heap identity, shared by every page-in over one
/// storage.
///
/// A [`HeapRefId`] names one heap's content, so which root page-in first bound
/// an allocation does not matter to any other: a pointer resolves into the
/// same heap whichever root reads it. One map per storage keeps a binding per
/// live heap; one map per root would copy each root's whole dependency closure.
#[derive(Allocative)]
pub(crate) struct StarlarkHeapBindings {
    bindings: DashMap<HeapRefId, WeakFrozenHeapRef>,
    registrations_until_prune: AtomicUsize,
}

const MIN_HEAP_BINDING_PRUNE_INTERVAL: usize = 64;

impl Default for StarlarkHeapBindings {
    fn default() -> Self {
        Self {
            bindings: DashMap::default(),
            registrations_until_prune: AtomicUsize::new(MIN_HEAP_BINDING_PRUNE_INTERVAL),
        }
    }
}

impl StarlarkHeapBindings {
    fn maybe_prune_expired(&self) {
        // Native heaps have no deserialization state to unregister them on drop.
        // Zero reserves the sweep for one caller while other registrations proceed.
        if self.registrations_until_prune.fetch_update(
            Ordering::Relaxed,
            Ordering::Relaxed,
            |remaining| remaining.checked_sub(1),
        ) != Ok(1)
        {
            return;
        }

        // Upgrading here could drop the last strong reference under a shard lock;
        // restored heap destruction re-enters this map to unregister the heap.
        self.bindings.retain(|_, heap| !heap.is_expired());
        // Retain scans capacity, so the next interval can track live entries only
        // if excess capacity from expired entries is also reclaimed.
        self.bindings.shrink_to_fit();
        self.registrations_until_prune.store(
            self.bindings.len().max(MIN_HEAP_BINDING_PRUNE_INTERVAL),
            Ordering::Relaxed,
        );
    }
}

impl StorageState for StarlarkHeapBindings {}

/// What Starlark deserialization within one root page-in shares: the storage's
/// heap bindings.
#[derive(Allocative)]
pub(crate) struct StarlarkDeserScope {
    heap_bindings: Arc<StarlarkHeapBindings>,
    /// Storage-scoped: scopes over the same storage share it, so claims
    /// coordinate across roots.
    #[allocative(skip)]
    wait_graph: Arc<StarlarkDeserWaitGraph>,
}

impl PageInState for StarlarkDeserScope {}

/// Estimate memory retained by cached Starlark heap deserialization state.
///
/// All heap states are visited with one builder so shared scopes and serialized
/// recipe data are counted once. The heap arenas themselves are not followed.
pub fn starlark_deserialization_state_retained_bytes(storage: &PagableStorageHandle) -> usize {
    cached_heap_deserialization_state_retained_bytes(storage)
}

/// `recipe.retained_data_len()`, compiled out where the OSS build's published
/// `pagable` predates the method. Unreachable there: `enabled()` is false
/// without `fbcode_build`. Remove with that check once a release has it.
fn recipe_retained_data_len(recipe: &dyn PagableDeserializerRecipe) -> u64 {
    #[cfg(fbcode_build)]
    return recipe.retained_data_len() as u64;
    #[cfg(not(fbcode_build))]
    {
        let _ = recipe;
        unreachable!("counting is disabled outside fbcode_build")
    }
}

/// Process-wide counters contrasting what a page-in must load with what it
/// uses. A value is only reachable through its heap, so claiming one value
/// pays for the whole heap's blob and slot table: `heap_*` is that cost,
/// `claimed_*` the part actually asked for.
mod partial_deser_stats {
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;

    pub(super) static HEAPS_LOADED: AtomicU64 = AtomicU64::new(0);
    pub(super) static HEAP_RETAINED_BLOB_BYTES: AtomicU64 = AtomicU64::new(0);
    pub(super) static HEAPS_WITH_METADATA: AtomicU64 = AtomicU64::new(0);
    pub(super) static USED_HEAP_RETAINED_BLOB_BYTES: AtomicU64 = AtomicU64::new(0);
    pub(super) static HEAP_VALUE_SERIALIZED_BYTES: AtomicU64 = AtomicU64::new(0);
    pub(super) static HEAP_VALUES: AtomicU64 = AtomicU64::new(0);
    pub(super) static HEAP_VALUE_ALLOC_BYTES: AtomicU64 = AtomicU64::new(0);
    pub(super) static CLAIMED_VALUES: AtomicU64 = AtomicU64::new(0);
    pub(super) static CLAIMED_ALLOC_BYTES: AtomicU64 = AtomicU64::new(0);

    /// Off unless `BUCK2_STARLARK_PARTIAL_DESER_STATS` is set: the claim site
    /// runs millions of times per page-in and shared counters there are a
    /// contended cache line. Always on under `cfg(test)` in the fbcode build —
    /// the `fbcode_build` gate below runs first — so the counting paths stay
    /// exercised there; the counters are process-global and never reset, so
    /// tests must not assert on absolute values. Callers branch on this
    /// before computing anything a counter needs, not just before storing it.
    pub(super) fn enabled() -> bool {
        // fbcode-only until a `pagable` release with `retained_data_len` is
        // published (see `recipe_retained_data_len`).
        if !cfg!(fbcode_build) {
            return false;
        }
        if cfg!(test) {
            return true;
        }
        static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *ON.get_or_init(|| std::env::var_os("BUCK2_STARLARK_PARTIAL_DESER_STATS").is_some())
    }

    /// What one heap's metadata parse contributes, held until the parse is
    /// known to be the one kept - see `HeapDeserializationState::metadata`.
    pub(super) struct MetadataStats {
        pub(super) retained_blob_bytes: u64,
        pub(super) values: u64,
        pub(super) value_alloc_bytes: u64,
        pub(super) value_serialized_bytes: u64,
    }

    impl MetadataStats {
        pub(super) fn record(&self) {
            add(&HEAPS_WITH_METADATA, 1);
            add(&USED_HEAP_RETAINED_BLOB_BYTES, self.retained_blob_bytes);
            add(&HEAP_VALUES, self.values);
            add(&HEAP_VALUE_ALLOC_BYTES, self.value_alloc_bytes);
            add(&HEAP_VALUE_SERIALIZED_BYTES, self.value_serialized_bytes);
        }
    }

    /// Relaxed throughout: these are monotonic counters read as a gauge, never
    /// used to order other memory.
    pub(super) fn add(counter: &AtomicU64, n: u64) {
        counter.fetch_add(n, Ordering::Relaxed);
    }

    pub(super) fn get(counter: &AtomicU64) -> u64 {
        counter.load(Ordering::Relaxed)
    }
}

/// A snapshot of [`starlark_partial_deser_stats`].
#[derive(Debug, Clone, Copy, Dupe, Default, PartialEq, Eq)]
pub struct PartialDeserStats {
    /// Heaps restored from storage to the header-loaded stage: dependencies
    /// bound and the data's recipe retained, slot metadata and values still
    /// lazy. A skeleton bound without a load is not counted; a heap loaded
    /// only as another heap's dependency is.
    pub heaps_loaded: u64,
    /// Serialized bytes those blobs held so any value in them could still be
    /// claimed later. Cumulative like the rest: bytes are not subtracted when
    /// a heap is dropped. Recipes and heap blobs are one-to-one today; a
    /// shared blob would be counted once per heap.
    pub heap_retained_blob_bytes: u64,
    /// Of `heaps_loaded`, those whose slot table was parsed — a resolve
    /// reached them. The remainder were loaded but never used.
    pub heaps_with_metadata: u64,
    /// The `heap_retained_blob_bytes` belonging to those heaps.
    pub used_heap_retained_blob_bytes: u64,
    /// Serialized value-region bytes of the heaps in `heaps_with_metadata`.
    pub heap_value_serialized_bytes: u64,
    /// Values contained in those heaps.
    pub heap_values: u64,
    /// In-memory bytes those heaps would occupy if every value were materialized.
    pub heap_value_alloc_bytes: u64,
    /// Values actually claimed for deserialization.
    pub claimed_values: u64,
    /// In-memory bytes of the claimed values — the part of
    /// `heap_value_alloc_bytes` actually built.
    pub claimed_alloc_bytes: u64,
}

/// Process-wide partial-deserialization counters since daemon start, or `None`
/// if counting is off, so callers report "not measured" rather than zeros.
pub fn starlark_partial_deser_stats() -> Option<PartialDeserStats> {
    use partial_deser_stats as s;
    if !s::enabled() {
        return None;
    }
    Some(PartialDeserStats {
        heaps_loaded: s::get(&s::HEAPS_LOADED),
        heap_retained_blob_bytes: s::get(&s::HEAP_RETAINED_BLOB_BYTES),
        heaps_with_metadata: s::get(&s::HEAPS_WITH_METADATA),
        used_heap_retained_blob_bytes: s::get(&s::USED_HEAP_RETAINED_BLOB_BYTES),
        heap_value_serialized_bytes: s::get(&s::HEAP_VALUE_SERIALIZED_BYTES),
        heap_values: s::get(&s::HEAP_VALUES),
        heap_value_alloc_bytes: s::get(&s::HEAP_VALUE_ALLOC_BYTES),
        claimed_values: s::get(&s::CLAIMED_VALUES),
        claimed_alloc_bytes: s::get(&s::CLAIMED_ALLOC_BYTES),
    })
}

/// Exact process-local value identity used only while a claim or wait guard is
/// active. The caller retains the owning heap, so `heap_ptr` cannot be reused
/// while this identity is present in the graph.
#[derive(Debug, Clone, Copy, Dupe, Eq, PartialEq, Hash)]
struct HeapValueId {
    heap_ptr: FrozenHeapPtr,
    value_index: u32,
}

/// Storage-scoped wait-for graph for detecting cyclic-deserialization deadlocks.
///
/// The Arc cache can share a partially deserialized heap between root page-in
/// scopes, so every root using the same storage must coordinate through one
/// graph. Exact heap pointers keep unrelated same-name heaps independent.
///
/// Keyed by [`ThreadId`], which identifies one in-flight deserialization only
/// because the deserialize path is synchronous and blocks by parking the OS
/// thread (`wait_for_slot`).
///
/// ATTENTION: if deserialization ever becomes async, one thread could drive two
/// at once and corrupt these keys — key by a per-deserialization token instead.
#[derive(Default)]
pub(crate) struct StarlarkDeserWaitGraph {
    /// Maps an exact heap value to the thread currently deserializing it.
    ///
    /// Written on every claim and release; read only when a thread is about to
    /// block, to walk the wait-for chain.
    claimers: DashMap<HeapValueId, ThreadId>,
    /// Maps a thread to the exact heap value it is blocked waiting on.
    ///
    /// A thread about to block inserts its edge here and then follows the
    /// chain - the value's claimer, what that thread waits on, its claimer,
    /// and so on - looking for itself, all under this lock
    /// (`begin_wait_and_check_cycle`). Holding the lock across both steps is
    /// what makes the check sound: two threads about to wait on each other
    /// cannot both walk before either's edge is in, so one of them sees the
    /// cycle. `claimers` is not covered by the lock and can change during a
    /// walk, but every edge of a real deadlock belongs to a thread already
    /// blocked here, and those edges cannot change.
    waiters: Mutex<HashMap<ThreadId, HeapValueId>>,
}

impl StorageState for StarlarkDeserWaitGraph {}

impl StarlarkDeserWaitGraph {
    fn lock_waiters(&self) -> MutexGuard<'_, HashMap<ThreadId, HeapValueId>> {
        self.waiters.lock().expect("wait-for graph lock poisoned")
    }

    /// Record that `thread` has claimed `value` for deserialization. The
    /// returned guard removes the `claimers` edge on drop, so every exit path
    /// from a claimed deserialization unwinds it exactly once.
    fn claim(self: &Arc<Self>, value: HeapValueId, thread: ThreadId) -> ClaimGuard {
        self.claimers.insert(value, thread);
        ClaimGuard {
            graph: self.dupe(),
            value,
        }
    }

    /// Record that `thread` is about to wait on `value` and, atomically with that
    /// insert, report whether waiting would deadlock (a wait-for cycle). Hold the
    /// returned guard for the whole wait so other threads' cycle checks see it.
    fn begin_wait_and_check_cycle(
        self: &Arc<Self>,
        thread: ThreadId,
        value: HeapValueId,
    ) -> (WaitGuard, bool) {
        let mut waiters = self.lock_waiters();
        waiters.insert(thread, value);
        let cycle = self.has_cycle(&waiters, thread, value);
        drop(waiters);
        (
            WaitGuard {
                graph: self.dupe(),
                thread,
            },
            cycle,
        )
    }

    /// True if blocking on `start_value` would deadlock: the wait-for chain
    /// leads back to `my_thread` (covers same-thread re-entry too).
    fn has_cycle(
        &self,
        waiters: &HashMap<ThreadId, HeapValueId>,
        my_thread: ThreadId,
        start_value: HeapValueId,
    ) -> bool {
        let mut current = start_value;
        // Bounded by `waiters`: each step lands on a distinct waiting thread.
        // `claimers` is not frozen by this lock, but a real deadlock's edges
        // all belong to blocked threads and cannot move mid-walk, and a chain
        // that churn cuts or extends passes through a live thread and is not
        // one. The caller's own wait edge must already be in, as
        // `begin_wait_and_check_cycle` ensures.
        debug_assert!(
            waiters.contains_key(&my_thread),
            "cycle check requires the caller's wait edge to be present"
        );
        for _ in 0..waiters.len() {
            let Some(claimer) = self.claimers.get(&current).map(|c| *c) else {
                return false;
            };
            if claimer == my_thread {
                return true;
            }
            let Some(&waiting_for) = waiters.get(&claimer) else {
                return false;
            };
            current = waiting_for;
        }
        false
    }
}

/// Clears a claim's `claimers` edge on drop, so every exit path from a claimed
/// deserialization unwinds it exactly once.
struct ClaimGuard {
    graph: Arc<StarlarkDeserWaitGraph>,
    value: HeapValueId,
}

impl Drop for ClaimGuard {
    fn drop(&mut self) {
        self.graph.claimers.remove(&self.value);
    }
}

/// Clears this thread's `waiters` edge on drop.
struct WaitGuard {
    graph: Arc<StarlarkDeserWaitGraph>,
    thread: ThreadId,
}

impl Drop for WaitGuard {
    fn drop(&mut self) {
        self.graph.lock_waiters().remove(&self.thread);
    }
}

/// Describe a heap-identity collision. Cold because naming the heap allocates a
/// `String` that the (overwhelmingly common) non-conflicting path never needs.
#[cold]
fn conflicting_heap_binding(
    heap_id: HeapRefId,
    bound: &FrozenHeapArc,
    bound_heap_ptr: FrozenHeapPtr,
    conflicting: Option<&FrozenHeapArc>,
    conflicting_heap_ptr: FrozenHeapPtr,
) -> PagableError {
    PagableError::ConflictingHeapBinding {
        heap_id,
        heap_name: bound
            .name()
            .map_or_else(|| "<unnamed>".to_owned(), |name| name.to_string()),
        bound_heap_ptr: bound_heap_ptr.addr(),
        bound_origin: bound.allocation_origin(),
        conflicting_heap_ptr: conflicting_heap_ptr.addr(),
        conflicting_origin: conflicting.map(FrozenHeapArc::allocation_origin),
    }
}

impl StarlarkDeserScope {
    pub(crate) fn new(
        heap_bindings: Arc<StarlarkHeapBindings>,
        wait_graph: Arc<StarlarkDeserWaitGraph>,
    ) -> Self {
        Self {
            heap_bindings,
            wait_graph,
        }
    }

    pub(crate) fn wait_graph(&self) -> &Arc<StarlarkDeserWaitGraph> {
        &self.wait_graph
    }

    /// Register a heap for cross-heap value resolution.
    pub(crate) fn register_heap(
        &self,
        heap_id: HeapRefId,
        heap: WeakFrozenHeapRef,
    ) -> Result<(), PagableError> {
        let heap_ptr = heap.heap_ptr();
        match self.heap_bindings.bindings.entry(heap_id) {
            Entry::Vacant(entry) => {
                entry.insert(heap);
            }
            Entry::Occupied(mut entry) => {
                if entry.get().heap_ptr() == heap_ptr {
                    return Ok(());
                }
                if let Some(bound) = entry.get().upgrade() {
                    return Err(conflicting_heap_binding(
                        heap_id,
                        &bound,
                        entry.get().heap_ptr(),
                        heap.upgrade().as_ref(),
                        heap_ptr,
                    ));
                }
                entry.insert(heap);
            }
        }
        self.heap_bindings.maybe_prune_expired();
        Ok(())
    }

    pub(crate) fn is_heap_bound(
        &self,
        heap_id: HeapRefId,
        heap: &FrozenHeapArc,
    ) -> Result<bool, PagableError> {
        let heap_ptr = heap
            .downgrade()
            .expect("a heap being bound must have an allocation")
            .heap_ptr();
        let Some(entry) = self.heap_bindings.bindings.get(&heap_id) else {
            return Ok(false);
        };
        if entry.heap_ptr() == heap_ptr {
            return Ok(true);
        }
        if let Some(bound) = entry.upgrade() {
            return Err(conflicting_heap_binding(
                heap_id,
                &bound,
                entry.heap_ptr(),
                Some(heap),
                heap_ptr,
            ));
        }
        Ok(false)
    }

    pub(crate) fn unregister_heap(&self, heap_id: HeapRefId, heap_ptr: FrozenHeapPtr) {
        if let Entry::Occupied(entry) = self.heap_bindings.bindings.entry(heap_id)
            && entry.get().heap_ptr() == heap_ptr
        {
            entry.remove();
        }
    }

    pub(crate) fn get_heap(&self, heap_id: &HeapRefId) -> Option<FrozenHeapArc> {
        self.heap_bindings
            .bindings
            .get(heap_id)
            .and_then(|heap| heap.upgrade())
    }

    /// Bound heaps whose header is not loaded, so whose dependencies are not
    /// bound yet.
    fn heaps_without_header(&self) -> Vec<FrozenHeapArc> {
        self.heap_bindings
            .bindings
            .iter()
            .filter_map(|entry| entry.value().upgrade())
            .filter(|heap| !heap.is_header_loaded())
            .collect()
    }
}

/// Bind a heap a pointer names but nothing has bound yet.
///
/// Try [`StarlarkHeapKeyIndex`], then load headers breadth-first through `origin`'s
/// serialized refs. Targets outside that closure, such as relocated values,
/// require the index.
///
/// `origin` retains index hits directly and BFS hits through their ref path.
/// Without an origin, search all unread heaps and rely on the arc cache for
/// retention.
///
/// [`StarlarkHeapKeyIndex`]: crate::values::layout::heap::sealed::heap_key_index::StarlarkHeapKeyIndex
#[cold]
fn resolve_missing_heap(
    scope: &Arc<StarlarkDeserScope>,
    storage: &PagableStorageHandle,
    page_in_scope: &PageInScope,
    origin: Option<&FrozenHeapArc>,
    heap_id: HeapRefId,
) -> crate::Result<FrozenHeapArc> {
    if let Some(found) = load_and_bind_heap_by_id(scope, storage, page_in_scope, heap_id)? {
        if let Some(origin) = origin {
            origin.retain_dependency(&found);
        }
        return Ok(found);
    }

    let mut queue: VecDeque<FrozenHeapArc> = match origin {
        Some(origin) => VecDeque::from([origin.dupe()]),
        None => scope.heaps_without_header().into(),
    };
    let mut seen: HashSet<FrozenHeapPtr> = HashSet::new();
    while let Some(heap) = queue.pop_front() {
        let Some(ptr) = heap.downgrade().map(|weak| weak.heap_ptr()) else {
            continue;
        };
        if !seen.insert(ptr) {
            continue;
        }
        if !heap.is_header_loaded() {
            heap.ensure_header_loaded(scope, storage)?;
            if let Some(found) = scope.get_heap(&heap_id) {
                return Ok(found);
            }
        }
        // Within the serialized closure, retained edges only shortcut ref paths.
        // Targets outside it require the index, whose entries never expire.
        // Restart drops retained edges too, so serialized refs suffice here.
        queue.extend(heap.refs_slice().iter().map(|dep| dep.heap_arc().dupe()));
    }
    Err(PagableError::HeapNotBoundInPageInScope { heap_id }.into())
}

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a `PagableDeserializer` and a shared `StarlarkDeserScope` to
/// resolve value references during deserialization.
pub(crate) struct StarlarkDeserializerImpl<'a, 'de, 'fv> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
    /// Shared registry of per-heap deserialization state. Cross-heap pointer
    /// resolution looks up the target heap by `heap_id` here.
    scope: Arc<StarlarkDeserScope>,
    /// The heap whose values this context deserializes, when known: where a
    /// pointer into a heap not bound yet is looked for, and what retains it
    /// once found (see `resolve_missing_heap`).
    origin: Option<FrozenHeapArc>,
    /// The brand this context deserializes at; see [`recover_from_pagable`](Self::recover_from_pagable).
    brand: PhantomData<Value<'fv>>,
}

impl<'de> StarlarkDeserializerImpl<'_, 'de, '_> {
    /// Recover a `StarlarkDeserializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc` / `deserialize_arc`) and run `f` with it. All heap
    /// state is reachable via the root's `StarlarkDeserScope` registry.
    ///
    /// The context deserializes at a brand of its own, introduced here for `f` alone. The brand
    /// stands for the heap whose data `deserializer` is positioned in: the heap of the value
    /// being materialized, or, for the owning carriers (`OwnedFrozen`, `Globals`), the heap that
    /// was just deserialized ahead of the root value. Every value the context hands out was
    /// serialized from that heap, and a value in a frozen heap only ever points into that heap
    /// or into a heap it references (that is what the brand it was allocated at guaranteed), so
    /// the brand is honest for cross-heap pointers too. Being closure-introduced, it names
    /// nothing outside `f`; the callers that carry a result out of `f` are the framework itself,
    /// which either writes it into that heap (the `AValue` vtable, see `AValueSimple`) or pairs
    /// it with the heap's owner (`OwnedFrozen::unchecked_new`).
    pub(crate) fn recover_from_pagable<R>(
        deserializer: &mut dyn PagableDeserializer<'de>,
        f: impl for<'fv> FnOnce(&mut StarlarkDeserializerImpl<'_, 'de, 'fv>) -> R,
    ) -> R {
        Self::recover_from_pagable_impl(deserializer, None, f)
    }

    /// [`recover_from_pagable`](Self::recover_from_pagable) for a context
    /// whose values are known to live in `origin`. Prefer it wherever the
    /// heap is known: it is what lets a pointer into a heap not bound yet be
    /// found, and retained, on `origin`'s behalf.
    pub(crate) fn recover_from_pagable_in<R>(
        deserializer: &mut dyn PagableDeserializer<'de>,
        origin: &FrozenHeapArc,
        f: impl for<'fv> FnOnce(&mut StarlarkDeserializerImpl<'_, 'de, 'fv>) -> R,
    ) -> R {
        Self::recover_from_pagable_impl(deserializer, Some(origin.dupe()), f)
    }

    fn recover_from_pagable_impl<R>(
        deserializer: &mut dyn PagableDeserializer<'de>,
        origin: Option<FrozenHeapArc>,
        f: impl for<'fv> FnOnce(&mut StarlarkDeserializerImpl<'_, 'de, 'fv>) -> R,
    ) -> R {
        let scope = Self::get_or_create_scope(deserializer);
        f(&mut StarlarkDeserializerImpl {
            pagable: deserializer,
            scope,
            origin,
            brand: PhantomData,
        })
    }

    /// Get or create the Starlark scope belonging to this root page-in. Also
    /// the point at which the storage starts indexing heaps by identity, ahead
    /// of any heap this page-in binds.
    pub(crate) fn get_or_create_scope(
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> Arc<StarlarkDeserScope> {
        // Looked up inside the closure so only creating a scope pays the probe.
        let storage_context = deserializer.storage_context();
        deserializer.page_in_scope().get_or_init(|| {
            register_heap_key_index(storage_context);
            StarlarkDeserScope::new(
                storage_context.get_or_init(StarlarkHeapBindings::default),
                storage_context.get_or_init(StarlarkDeserWaitGraph::default),
            )
        })
    }
}

impl<'de, 'fv> StarlarkDeserializeContext<'de, 'fv> for StarlarkDeserializerImpl<'_, 'de, 'fv> {
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self.pagable
    }

    fn deserialize_value(&mut self) -> crate::Result<Value<'fv>> {
        let serialized = SerializedFrozenValue::pagable_deserialize(self.pagable)?;
        match serialized {
            SerializedFrozenValue::HeapPtr {
                heap_id,
                value_index,
                is_str,
            } => self.ensure_initialized(heap_id, value_index, is_str),
            SerializedFrozenValue::InlineInt(v) => {
                let inline = InlineInt::try_from(v)
                    .map_err(|_| anyhow::anyhow!("Integer {} does not fit in InlineInt", v))?;
                Ok(Value::new_int(inline))
            }
            SerializedFrozenValue::Static(id) => {
                let v = get_static_value_by_id(id).ok_or_else(|| {
                    anyhow::anyhow!("Static value ID {:?} not found in inventory registry", id)
                })?;
                Ok(HeapEdge::immortal().rebrand(v))
            }
        }
    }
}

impl<'a, 'de, 'fv> StarlarkDeserializerImpl<'a, 'de, 'fv> {
    /// Resolve a serialized HeapPtr into a value. Deserialize the target slot
    /// if needed; reads the header pointer from the slot's atomic.
    ///
    /// The pointers handed out here are the ones the framework wrote into the heap the serialized
    /// pointer names, which is a heap the brand reaches (see `recover_from_pagable`); the
    /// `'fv`-branded results are the framework handing out its own pointers.
    fn ensure_initialized(
        &mut self,
        heap_id: HeapRefId,
        value_index: u32,
        is_str: bool,
    ) -> crate::Result<Value<'fv>> {
        let target_heap = match self.scope.get_heap(&heap_id) {
            Some(heap) => heap,
            None => resolve_missing_heap(
                &self.scope,
                &self.pagable.storage(),
                self.pagable.page_in_scope(),
                self.origin.as_ref(),
                heap_id,
            )?,
        };
        if let Some(origin) = &self.origin {
            origin.retain_dependency(&target_heap);
        }
        if target_heap.deser_state().is_none() {
            // Page-in reused a native heap that remained resident after page-out.
            // Use the serialization state to resolve its value.
            let target_heap_ptr = target_heap
                .downgrade()
                .expect("a registered deserialization heap must have an allocation")
                .heap_ptr();
            let Some(value) = self
                .pagable
                .storage_context()
                .get::<StarlarkSerState>()
                .and_then(|state| {
                    state.lookup_registered_value(target_heap_ptr, value_index, is_str)
                })
            else {
                return Err(PagableError::NativeHeapValueNotRegistered {
                    heap_id,
                    value_index,
                }
                .into());
            };
            return Ok(value);
        }

        let storage = self.pagable.storage();
        // A skeleton bound from a ref list has not loaded its header, and its
        // values and its own dependencies come with it.
        target_heap.ensure_header_loaded(&self.scope, &storage)?;
        let ptr = resolve_in_loaded_heap(&self.scope, &storage, &target_heap, value_index)?;
        // SAFETY: a claim initializes its aligned header to a valid sentinel
        // before release-publishing the pointer; readers acquire that state.
        // The target heap is retained for `'fv` (see `recover_from_pagable`).
        // `new_frozen_ptr` only tags the address using the serialized string
        // flag, without reading the header or payload. A cycle-breaking value
        // is construction-only until initialization succeeds.
        let header = unsafe { &*ptr };
        Ok(Value::new_frozen_ptr(header, is_str))
    }
}

/// Resolve a pointer for graph construction in a heap whose blob is already read.
///
/// Takes only shareable state so pointers can be resolved on any thread.
///
/// Returns a raw pointer rather than a `Value`: a `Value<'v>` needs a heap in
/// scope that keeps it alive, which the caller supplies. The pointee lives as
/// long as the target heap.
///
/// This is not a transitive readiness barrier: breaking a wait-for cycle can
/// return the other claimer's unfinished slot. That pointer must not be read
/// until initialization succeeds.
pub(crate) fn resolve_in_loaded_heap(
    scope: &StarlarkDeserScope,
    storage: &PagableStorageHandle,
    target_heap: &FrozenHeapArc,
    value_index: u32,
) -> crate::Result<*mut AValueHeader> {
    let target_heap_ptr = target_heap
        .downgrade()
        .expect("a registered deserialization heap must have an allocation")
        .heap_ptr();
    // A resident heap has no deserialization state; callers handle it first.
    let target_state = target_heap
        .deser_state()
        .expect("resolving in a loaded heap requires deserialization state");

    let value_count = target_state.value_count(storage)?;
    if value_index as usize >= value_count {
        return Err(anyhow::anyhow!(
            "value_index {value_index} out of range for heap {:?} (size {value_count})",
            target_state.heap_id
        )
        .into());
    }

    // Fast path: slot is already done.
    if let Some(ptr) = target_state.loaded_header_ptr(value_index as usize) {
        return Ok(ptr);
    }

    let wait_graph = scope.wait_graph();
    // Process-local heap identity prevents unrelated same-name heaps from
    // sharing active claim/wait edges in the storage-global graph.
    let in_progress_key = HeapValueId {
        heap_ptr: target_heap_ptr,
        value_index,
    };
    let my_thread = std::thread::current().id();

    // Slow path: try to claim. The caller's `PagableDeserializer` may be reading
    // a different stream (e.g. the body of an `Arc<T>` deser-fn), so it cannot be
    // seeked. Open a fresh deserializer from the target heap's own recipe instead.
    match target_state.try_claim(value_index as usize, storage)? {
        ClaimResult::Claimed(target) => {
            // Guard clears the `claimers` edge on every exit below.
            let _claim = wait_graph.claim(in_progress_key, my_thread);

            // `recipe.open()` produces a fresh deserializer so concurrent resolves
            // of the same heap have independent cursors.
            let recipe = target_state.header()?.recipe.dupe();

            let result = {
                let mut de = recipe.open(storage);
                // SAFETY: `target.abs_pos` was computed from the target heap's
                // offset table during `deserialize_metadata`; it is a valid
                // position in the recipe's bytes for this heap.
                unsafe { de.seek(target.abs_pos) };
                // The nested context's lifetime is the target heap's: `de` is
                // positioned in the target value's data, and the vtable writes
                // the result into that heap.
                StarlarkDeserializerImpl::recover_from_pagable_in(
                    &mut *de,
                    target_heap,
                    |nested_ctx| (target.vtable.starlark_deserialize)(target.raw_ptr, nested_ctx),
                )
            };

            if let Err(e) = result {
                target_state.abort_claim(value_index as usize, &e);
                return Err(e);
            }
            // Replace the sentinel vtable with the real one before publishing done.
            unsafe { target.write_vtable_to_header() };
            target_state.finalize_claim(value_index as usize);
        }
        ClaimResult::InProgress(ptr) => {
            // Slot is mid-deserialization (re-entrant or another thread).
            // `_wait` must outlive the block below so other threads' cycle
            // checks observe this wait.
            let (_wait, cycle) = wait_graph.begin_wait_and_check_cycle(my_thread, in_progress_key);
            if cycle {
                // This construction edge is not proof that the value is ready to read.
                return Ok(ptr);
            }
            // No cycle - safe to block until the claimer finishes.
            match target_state.wait_for_slot(value_index as usize, storage)? {
                ClaimResult::Done => {}
                ClaimResult::Failed => {
                    return Err(target_state
                        .partial_deserialization_error(value_index)
                        .into());
                }
                _ => unreachable!(),
            }
        }
        ClaimResult::Done => {}
        ClaimResult::Failed => {
            return Err(target_state
                .partial_deserialization_error(value_index)
                .into());
        }
    }

    Ok(target_state
        .loaded_header_ptr(value_index as usize)
        .expect("slot must be done after resolving it"))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::sync::Barrier;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::thread::ThreadId;
    use std::time::Duration;

    use dupe::Dupe;

    use super::HeapValueId;
    use super::MIN_HEAP_BINDING_PRUNE_INTERVAL;
    use super::StarlarkDeserScope;
    use super::StarlarkDeserWaitGraph;
    use super::StarlarkHeapBindings;
    use crate::values::OwnedFrozen;
    use crate::values::Value;
    use crate::values::layout::heap::name::StarlarkTestHeapName;
    use crate::values::layout::heap::sealed::FrozenHeapPtr;

    fn registered_native_heap(scope: &StarlarkDeserScope) -> OwnedFrozen<Value<'static>> {
        let heap: OwnedFrozen<Value<'static>> =
            OwnedFrozen::build(StarlarkTestHeapName::frozen_heap_name(), |heap| {
                heap.alloc("native")
            });
        assert!(heap.heap_arc().deser_state().is_none());
        scope
            .register_heap(
                heap.heap_arc().heap_ref_id().unwrap(),
                heap.heap_arc().downgrade().unwrap(),
            )
            .unwrap();
        heap
    }

    #[test]
    fn test_heap_bindings_reclaim_expired_native_heaps() {
        let bindings = Arc::new(StarlarkHeapBindings::default());
        let scope = StarlarkDeserScope::new(bindings.dupe(), Default::default());
        let live = registered_native_heap(&scope);
        for _ in 0..4 * MIN_HEAP_BINDING_PRUNE_INTERVAL {
            drop(registered_native_heap(&scope));
        }
        assert!(
            bindings.bindings.len() <= MIN_HEAP_BINDING_PRUNE_INTERVAL + 1,
            "dead native bindings must not accumulate across repeated page-ins: {} entries",
            bindings.bindings.len()
        );
        assert_eq!(
            scope
                .get_heap(&live.heap_arc().heap_ref_id().unwrap())
                .as_ref(),
            Some(live.heap_arc()),
            "cleanup must preserve the live allocation"
        );
    }

    #[test]
    fn test_heap_bindings_prune_interval_scales_with_live_heaps() {
        let bindings = Arc::new(StarlarkHeapBindings::default());
        let scope = StarlarkDeserScope::new(bindings.dupe(), Default::default());
        let live: Vec<_> = (0..4 * MIN_HEAP_BINDING_PRUNE_INTERVAL)
            .map(|_| registered_native_heap(&scope))
            .collect();
        let interval = bindings.registrations_until_prune.load(Ordering::Relaxed);
        assert_eq!(interval, live.len());
        for heap in &live {
            let id = heap.heap_arc().heap_ref_id().unwrap();
            assert_eq!(scope.get_heap(&id).as_ref(), Some(heap.heap_arc()));
            scope
                .register_heap(id, heap.heap_arc().downgrade().unwrap())
                .unwrap();
        }
        assert_eq!(
            bindings.registrations_until_prune.load(Ordering::Relaxed),
            interval,
            "re-registering the same allocation must not charge another sweep"
        );

        drop(live);
        for _ in 0..interval {
            drop(registered_native_heap(&scope));
        }
        assert_eq!(bindings.bindings.len(), 1);
        assert!(
            bindings.bindings.capacity() < interval,
            "cleanup must reclaim excess capacity before shortening the next interval"
        );
        assert_eq!(
            bindings.registrations_until_prune.load(Ordering::Relaxed),
            MIN_HEAP_BINDING_PRUNE_INTERVAL
        );
    }

    #[test]
    fn test_heap_bindings_prune_with_concurrent_registrations() {
        let bindings = Arc::new(StarlarkHeapBindings::default());
        let scope = StarlarkDeserScope::new(bindings.dupe(), Default::default());
        let ready = Barrier::new(4);
        let live = std::thread::scope(|threads| {
            let handles: Vec<_> = (0..4)
                .map(|_| {
                    threads.spawn(|| {
                        let live = registered_native_heap(&scope);
                        ready.wait();
                        for _ in 0..4 * MIN_HEAP_BINDING_PRUNE_INTERVAL {
                            drop(registered_native_heap(&scope));
                        }
                        live
                    })
                })
                .collect();
            handles
                .into_iter()
                .map(|handle| handle.join().unwrap())
                .collect::<Vec<_>>()
        });

        // Finish the current interval after all concurrent drops have completed.
        let interval = bindings.registrations_until_prune.load(Ordering::Relaxed);
        assert!(interval > 0);
        for _ in 0..interval {
            drop(registered_native_heap(&scope));
        }
        assert_eq!(bindings.bindings.len(), live.len() + 1);
        for heap in &live {
            assert_eq!(
                scope
                    .get_heap(&heap.heap_arc().heap_ref_id().unwrap())
                    .as_ref(),
                Some(heap.heap_arc())
            );
        }
    }

    fn value_id(n: usize) -> HeapValueId {
        HeapValueId {
            heap_ptr: FrozenHeapPtr::testing_new(0x1000 + n * 0x100),
            value_index: 0,
        }
    }

    // Only used as a key, so an exited thread's id serves.
    fn spawn_for_id() -> ThreadId {
        let handle = std::thread::spawn(|| {});
        let id = handle.thread().id();
        handle.join().unwrap();
        id
    }

    #[test]
    fn test_two_live_claimers_detect_cycle_and_clean_up() {
        let graph = Arc::new(StarlarkDeserWaitGraph::default());
        let claims_ready = Arc::new(Barrier::new(2));
        let waits_ready = Arc::new(Barrier::new(2));
        let (tx, rx) = std::sync::mpsc::channel();
        let threads: Vec<_> = (0..2)
            .map(|i| {
                let tx = tx.clone();
                let graph = graph.dupe();
                let claims_ready = claims_ready.dupe();
                let waits_ready = waits_ready.dupe();
                std::thread::spawn(move || {
                    let me = std::thread::current().id();
                    let claim = graph.claim(value_id(i), me);
                    claims_ready.wait();
                    let (wait, cycle) = graph.begin_wait_and_check_cycle(me, value_id(1 - i));
                    // Both edges stay live until both verdicts have been computed.
                    waits_ready.wait();
                    drop(wait);
                    drop(claim);
                    tx.send(cycle).expect("test receiver is alive");
                })
            })
            .collect();
        drop(tx);
        let cycles = (0..2)
            .map(|_| {
                usize::from(
                    rx.recv_timeout(Duration::from_secs(10))
                        .expect("claimers must finish"),
                )
            })
            .sum::<usize>();
        for thread in threads {
            thread.join().expect("claimer panicked");
        }
        assert_eq!(cycles, 1, "the second wait edge must close the cycle");
        assert!(graph.claimers.is_empty());
        assert!(graph.lock_waiters().is_empty());
    }

    /// `has_cycle` reads `claimers` while other threads add and remove entries.
    /// Drives the claim/wait protocol from one thread under that churn, so a
    /// regression in the lock split shows as a wrong verdict rather than as a
    /// rare hang in `test_cross_thread_cycle_does_not_deadlock`.
    #[test]
    fn test_cycle_verdicts_hold_under_concurrent_claim_churn() {
        let graph = Arc::new(StarlarkDeserWaitGraph::default());
        let thread_a = spawn_for_id();
        let thread_b = spawn_for_id();

        let stop = Arc::new(AtomicBool::new(false));
        let churners: Vec<_> = (0..2)
            .map(|i| {
                let graph = graph.dupe();
                let stop = stop.dupe();
                std::thread::spawn(move || {
                    let me = std::thread::current().id();
                    // Runs until the main loop is done. The bound only guards
                    // against a main loop that never finishes, and fails loudly
                    // so it cannot quietly end the churn early instead.
                    let mut n = 0usize;
                    while !stop.load(Ordering::Relaxed) {
                        assert!(
                            n < 10_000_000,
                            "churn ran out before the main loop finished"
                        );
                        // Ids disjoint from the test's own.
                        let guard = graph.claim(value_id(0x10000 + i * 0x1000 + (n % 64)), me);
                        drop(guard);
                        std::thread::yield_now();
                        n += 1;
                    }
                })
            })
            .collect();

        for round in 0..1000 {
            let value_a = value_id(round * 2);
            let value_b = value_id(round * 2 + 1);
            let claim_a = graph.claim(value_a, thread_a);
            let claim_b = graph.claim(value_b, thread_b);

            // B waits on A's value: chain ends at a non-waiting thread.
            let (wait_b, cycle) = graph.begin_wait_and_check_cycle(thread_b, value_a);
            assert!(
                !cycle,
                "B waiting on {value_a:?} must not be a cycle: its claimer A waits on nothing"
            );

            // A waiting on B's value closes the loop: A -> B -> A.
            let (wait_a, cycle) = graph.begin_wait_and_check_cycle(thread_a, value_b);
            assert!(
                cycle,
                "A -> {value_b:?} -> B -> {value_a:?} -> A must be a cycle"
            );

            drop(wait_a);
            drop(wait_b);
            // `wait_b` was dropped above: a thread holds one `waiters` entry,
            // and a second live guard would overwrite it.
            let (wait_b2, cycle) = graph.begin_wait_and_check_cycle(thread_b, value_a);
            assert!(!cycle, "the cycle must dissolve once A stops waiting");

            drop(wait_b2);
            drop(claim_b);
            // The value is unclaimed now, so the walk ends there.
            let (wait_b3, cycle) = graph.begin_wait_and_check_cycle(thread_b, value_b);
            assert!(!cycle, "an unclaimed value cannot be part of a cycle");
            drop(wait_b3);
            drop(claim_a);
        }

        stop.store(true, Ordering::Relaxed);
        for churner in churners {
            churner.join().unwrap();
        }
    }
}

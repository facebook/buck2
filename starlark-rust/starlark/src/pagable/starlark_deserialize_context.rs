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
use std::num::NonZeroU32;
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::MutexGuard;
use std::sync::OnceLock;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::thread::ThreadId;

use dashmap::DashMap;
use dupe::Dupe;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableDeserializerRecipe;
use pagable::storage::handle::PagableStorageHandle;

use crate::pagable::DeserTypeId;
use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::lookup_vtable;
use crate::pagable::serialized_frozen_value::SerializedFrozenValue;
use crate::pagable::starlark_deserialize::StarlarkDeserializeContext;
use crate::pagable::static_value::get_frozen_value_by_static_id;
use crate::values::FrozenValue;
use crate::values::layout::heap::allocator::alloc::allocator::ChunkAllocator;
use crate::values::layout::heap::arena::Arena;
use crate::values::layout::heap::arena::BumpKind;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::types::int::inline_int::InlineInt;

/// Per-slot metadata for partial-deser. Immutable after `deserialize_metadata`.
pub(crate) struct ValueDeserSlot {
    /// Byte offset of this value's data relative to base_pos.
    stream_offset: u32,
    /// Arc index offset relative to base_pos.arc_index.
    arc_offset: u32,
    /// This value's vtable, used for deserialization dispatch.
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

struct InitWaiters {
    lock: Mutex<()>,
    cv: OnceLock<Condvar>,
}

impl InitWaiters {
    fn new() -> Self {
        Self {
            lock: Mutex::new(()),
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
pub(crate) struct HeapMetadata {
    /// All values in this heap.
    slots: Vec<ValueDeserSlot>,
    /// Absolute cursor position of value data start (base for relative offsets).
    base_pos: PagableCursor,
    /// Per-slot init state. See [`AtomicSlotState`].
    init_states: Vec<AtomicSlotState>,
    /// Coordinates waiters that lost a per-slot initialization race.
    init_waiters: InitWaiters,
}

/// Locked pointer to the owning `FrozenFrozenHeap`'s arena plus the
/// information needed to lazily parse this heap's slot metadata from its
/// recipe. The metadata is materialized only on first `try_claim`.
pub(crate) struct HeapDeserializationState {
    /// Cursor within the recipe's data where the lazy metadata region starts
    metadata_start: PagableCursor,
    /// Reopen this heap's data to lazily parse metadata.
    recipe: Arc<dyn PagableDeserializerRecipe>,
    /// Locked pointer into the owning `FrozenFrozenHeap`'s arena. The mutex
    /// serializes concurrent `alloc_raw_one` calls during partial deseralization.
    arena: Mutex<NonNull<Arena<ChunkAllocator>>>,
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
        metadata_start: PagableCursor,
        recipe: Arc<dyn PagableDeserializerRecipe>,
        arena: *const Arena<ChunkAllocator>,
    ) -> Self {
        Self {
            metadata_start,
            recipe,
            // SAFETY: caller's contract — `arena` is a valid pointer.
            arena: Mutex::new(unsafe { NonNull::new_unchecked(arena as *mut _) }),
            metadata: OnceLock::new(),
        }
    }

    /// Number of values in this heap.
    pub(crate) fn value_count(&self, storage: &PagableStorageHandle) -> crate::Result<usize> {
        Ok(self.metadata(storage)?.slots.len())
    }

    /// Parse the metadata region from the recipe: `total_count`,
    /// `drop_value_count`, offset table, per-value metadata. Called
    /// once on first `metadata()`; subsequent calls hit `OnceLock`.
    fn parse_metadata(&self, storage: &PagableStorageHandle) -> crate::Result<HeapMetadata> {
        let mut de = self.recipe.open(storage);
        // SAFETY: `metadata_start` was captured during `deserialize_skeleton`
        // from this recipe's data; it is a valid position in the recipe.
        unsafe { de.seek(self.metadata_start) };

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
        let init_states: Vec<AtomicSlotState> = (0..total_count)
            .map(|_| AtomicSlotState::not_started())
            .collect();
        Ok(HeapMetadata {
            slots,
            base_pos,
            init_states,
            init_waiters: InitWaiters::new(),
        })
    }

    /// Get parsed metadata, parsing on first call. Subsequent calls hit `OnceLock`.
    fn metadata(&self, storage: &PagableStorageHandle) -> crate::Result<&HeapMetadata> {
        if let Some(m) = self.metadata.get() {
            return Ok(m);
        }
        let parsed = self.parse_metadata(storage)?;
        Ok(self.metadata.get_or_init(|| parsed))
    }

    /// Return the header pointer for slot `index` if it's been finalized.
    #[inline]
    pub(crate) fn loaded_header_ptr(&self, index: usize) -> Option<*mut AValueHeader> {
        let m = self.metadata.get()?;
        m.init_states[index].load(Ordering::Acquire).done_ptr()
    }

    /// Try to claim a slot for deserialization.
    ///
    /// Claims are serialized by the arena lock: the winner allocates the header
    /// and publishes its pointer into the slot's atomic *before* releasing the
    /// lock, so a claimed slot always carries its pointer and no reader ever has
    /// to wait for it to appear. This is why observing a started slot never blocks.
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
        let arena = self.arena.lock().expect("arena lock poisoned");

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
                .as_ref()
                .alloc_raw_one(slot.bump_kind, slot.alloc_size)
        };
        // SAFETY: sentinel vtable so any access before `starlark_deserialize`
        // would panic.
        unsafe {
            std::ptr::write(
                header_ptr,
                AValueHeader(AValueVTable::uninitialized_sentinel()),
            );
        }
        state.publish_in_progress(header_ptr);
        drop(arena);

        let raw_ptr = unsafe { StarlarkValueRawPtr::new_header(&*header_ptr) };
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
            .lock
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
        self.publish_and_notify(index, AtomicSlotState::finalize);
    }

    /// Publish slot `index` as failed. Call if the winning deserializer errors
    /// before `finalize_claim`.
    pub(crate) fn abort_claim(&self, index: usize) {
        self.publish_and_notify(index, AtomicSlotState::fail);
    }

    /// Apply a terminal transition to slot `index` under the init-waiter lock,
    /// then wake any waiters. Doing the store and the notify under one lock pairs
    /// with `wait_for_init` to avoid lost wakeups.
    fn publish_and_notify(&self, index: usize, transition: impl FnOnce(&AtomicSlotState)) {
        let m = self
            .metadata
            .get()
            .expect("publish_and_notify called before metadata parse");
        let _guard = m
            .init_waiters
            .lock
            .lock()
            .expect("init waiter lock poisoned");
        transition(&m.init_states[index]);
        if let Some(cv) = m.init_waiters.cv.get() {
            cv.notify_all();
        }
    }
}

/// Shared deserialization state across all heaps deserialized in a session.
/// Stored in `SessionContext` as `Arc<StarlarkDeserState>` so that
/// independently-deserialized heaps (via pagable arcs) can all register
/// their per-value addresses and resolve cross-heap references.
pub(crate) struct StarlarkDeserState {
    /// Per-heap `HeapDeserializationState`. Holds the slot array (immutable
    /// metadata) and `init_states` (per-slot atomic that doubles as the
    /// header-pointer lookup table once finalized).
    heap_deser_states: DashMap<HeapRefId, Arc<HeapDeserializationState>>,
    /// Cross-thread wait-for graph for cyclic-deser deadlock detection.
    wait_graph: Arc<WaitForGraph>,
}

/// Cross-thread "wait-for" graph for detecting cyclic-deserialization deadlocks.
///
/// Keyed by [`ThreadId`], which identifies one in-flight deserialization only
/// because the deserialize path is synchronous and blocks by parking the OS
/// thread (`wait_for_slot`).
///
/// ATTENTION: if deserialization ever becomes async, one thread could drive two
/// at once and corrupt these keys — key by a per-deserialization token instead.
#[derive(Default)]
struct WaitForGraph {
    inner: Mutex<WaitForGraphInner>,
}

#[derive(Default)]
struct WaitForGraphInner {
    /// Maps `(heap_id, value_index)` → the thread currently deserializing it.
    claimers: HashMap<(HeapRefId, u32), ThreadId>,
    /// Maps thread → the `(heap_id, value_index)` it is blocked waiting on.
    waiters: HashMap<ThreadId, (HeapRefId, u32)>,
}

impl WaitForGraph {
    fn lock(&self) -> MutexGuard<'_, WaitForGraphInner> {
        self.inner.lock().expect("wait-for graph lock poisoned")
    }

    /// Record that `thread` has claimed `value` for deserialization. The
    /// returned guard removes the `claimers` edge on drop, so every exit path
    /// from a claimed deserialization unwinds it exactly once.
    fn claim(self: &Arc<Self>, value: (HeapRefId, u32), thread: ThreadId) -> ClaimGuard {
        self.lock().claimers.insert(value, thread);
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
        value: (HeapRefId, u32),
    ) -> (WaitGuard, bool) {
        let mut inner = self.lock();
        inner.waiters.insert(thread, value);
        let cycle = inner.has_cycle(thread, value);
        drop(inner);
        (
            WaitGuard {
                graph: self.dupe(),
                thread,
            },
            cycle,
        )
    }
}

impl WaitForGraphInner {
    /// True if blocking on `start_value` would deadlock: the wait-for chain
    /// leads back to `my_thread` (covers same-thread re-entry too).
    fn has_cycle(&self, my_thread: ThreadId, start_value: (HeapRefId, u32)) -> bool {
        let mut current = start_value;
        for _ in 0..self.claimers.len() {
            let Some(&claimer) = self.claimers.get(&current) else {
                return false;
            };
            if claimer == my_thread {
                return true;
            }
            let Some(&waiting_for) = self.waiters.get(&claimer) else {
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
    graph: Arc<WaitForGraph>,
    value: (HeapRefId, u32),
}

impl Drop for ClaimGuard {
    fn drop(&mut self) {
        self.graph.lock().claimers.remove(&self.value);
    }
}

/// Clears this thread's `waiters` edge on drop.
struct WaitGuard {
    graph: Arc<WaitForGraph>,
    thread: ThreadId,
}

impl Drop for WaitGuard {
    fn drop(&mut self) {
        self.graph.lock().waiters.remove(&self.thread);
    }
}

impl StarlarkDeserState {
    pub(crate) fn new() -> Self {
        Self {
            heap_deser_states: DashMap::new(),
            wait_graph: Arc::new(WaitForGraph::default()),
        }
    }

    /// Register a heap's deserialization state.
    pub(crate) fn register_heap(&self, heap_id: HeapRefId, state: Arc<HeapDeserializationState>) {
        self.heap_deser_states.insert(heap_id, state);
    }

    pub(crate) fn get_heap(&self, heap_id: &HeapRefId) -> Option<Arc<HeapDeserializationState>> {
        self.heap_deser_states.get(heap_id).map(|r| r.dupe())
    }
}

/// Concrete implementation of StarlarkDeserializeContext.
///
/// Wraps a `PagableDeserializer` and a shared `StarlarkDeserState` to
/// resolve `FrozenValue` references during deserialization.
pub struct StarlarkDeserializerImpl<'a, 'de> {
    pagable: &'a mut dyn PagableDeserializer<'de>,
    /// Shared registry of per-heap deserialization state. Cross-heap pointer
    /// resolution looks up the target heap by `heap_id` here.
    state: Arc<StarlarkDeserState>,
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Recover a `StarlarkDeserializerImpl` after a hop through a pagable-only
    /// boundary (typically `serialize_arc` / `deserialize_arc`). All heap
    /// state is reachable via the shared `StarlarkDeserState` registry.
    pub fn recover_from_pagable(
        deserializer: &'a mut dyn PagableDeserializer<'de>,
    ) -> crate::Result<Self> {
        let state = Self::get_or_create_state(deserializer);
        Ok(Self::new(deserializer, state))
    }

    /// Create a new deserializer with shared state.
    pub(crate) fn new(
        pagable: &'a mut dyn PagableDeserializer<'de>,
        state: Arc<StarlarkDeserState>,
    ) -> Self {
        Self { pagable, state }
    }

    /// Get or create the shared `StarlarkDeserState` from the deserializer's `SessionContext`.
    pub(crate) fn get_or_create_state(
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> Arc<StarlarkDeserState> {
        deserializer
            .session_context()
            .get_or_insert_with(|| Arc::new(StarlarkDeserState::new()))
    }
}

impl<'de> StarlarkDeserializeContext<'de> for StarlarkDeserializerImpl<'_, 'de> {
    fn pagable(&mut self) -> &mut dyn PagableDeserializer<'de> {
        self.pagable
    }

    fn deserialize_frozen_value(&mut self) -> crate::Result<FrozenValue> {
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
                Ok(FrozenValue::new_int(inline))
            }
            SerializedFrozenValue::Static(id) => {
                let fv = get_frozen_value_by_static_id(id).ok_or_else(|| {
                    anyhow::anyhow!("Static value ID {:?} not found in inventory registry", id)
                })?;
                Ok(fv)
            }
        }
    }
}

impl<'a, 'de> StarlarkDeserializerImpl<'a, 'de> {
    /// Resolve a serialized HeapPtr into a `FrozenValue`. Deserialize the
    /// target slot if needed; reads the header pointer from the slot's
    /// atomic.
    fn ensure_initialized(
        &mut self,
        heap_id: HeapRefId,
        value_index: u32,
        is_str: bool,
    ) -> crate::Result<FrozenValue> {
        let target_state = self
            .state
            .get_heap(&heap_id)
            .ok_or(PagableError::HeapBasesNotRegistered { heap_id })?;

        let storage = self.pagable.storage();

        let value_count = target_state.value_count(&storage)?;
        if value_index as usize >= value_count {
            return Err(anyhow::anyhow!(
                "value_index {} out of range for heap {:?} (size {})",
                value_index,
                heap_id,
                value_count,
            )
            .into());
        }

        // Fast path: slot is already done.
        if let Some(ptr) = target_state.loaded_header_ptr(value_index as usize) {
            let header = unsafe { &*ptr };
            return Ok(FrozenValue::new_ptr(header, is_str));
        }

        // Identifies this value for the cycle bookkeeping in the match below.
        let in_progress_key = (heap_id, value_index);
        let my_thread = std::thread::current().id();

        // Slow path: try to claim. The current `self.pagable` (PagableDeserializer)
        // may be reading a different stream (e.g. the body of an `Arc<T>` deser-fn),
        // so we can't seek it. Open a fresh deserializer from the target heap's
        // own recipe instead.
        match target_state.try_claim(value_index as usize, &storage)? {
            ClaimResult::Claimed(target) => {
                // Guard clears the `claimers` edge on every exit below.
                let _claim = self.state.wait_graph.claim(in_progress_key, my_thread);

                // `recipe.open()` produces a fresh deserializer so concurrent `ensure_initialized`
                // calls on the same heap have independent cursors.
                let recipe = target_state.recipe.dupe();

                let result = {
                    let mut de = recipe.open(&storage);
                    // SAFETY: `target.abs_pos` was computed from the target heap's
                    // offset table during `deserialize_metadata`; it is a valid
                    // position in the recipe's bytes for this heap.
                    unsafe { de.seek(target.abs_pos) };
                    let nested_de: &mut dyn PagableDeserializer<'_> = &mut *de;
                    let mut nested_ctx =
                        StarlarkDeserializerImpl::new(nested_de, self.state.dupe());
                    (target.vtable.starlark_deserialize)(target.raw_ptr, &mut nested_ctx)
                };

                if let Err(e) = result {
                    target_state.abort_claim(value_index as usize);
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
                let (_wait, cycle) = self
                    .state
                    .wait_graph
                    .begin_wait_and_check_cycle(my_thread, in_progress_key);
                if cycle {
                    // Break the cycle by handing back the in-progress header.
                    // Safe only because if the claimer fails, the whole deser
                    // unit fails too — so this dangling sentinel is never read.
                    //
                    // SAFETY: allocated by the claimer in the heap's arena, kept
                    // alive by the `Arc<FrozenFrozenHeap>` in the deser state.
                    let header = unsafe { &*ptr };
                    return Ok(FrozenValue::new_ptr(header, is_str));
                }
                // No cycle — safe to block until the claimer finishes.
                match target_state.wait_for_slot(value_index as usize, &storage)? {
                    ClaimResult::Done => {}
                    ClaimResult::Failed => {
                        return Err(anyhow::anyhow!(
                            "partial deserialization failed for heap {:?} value_index {}",
                            heap_id,
                            value_index,
                        )
                        .into());
                    }
                    _ => unreachable!(),
                }
            }
            ClaimResult::Done => {}
            ClaimResult::Failed => {
                return Err(anyhow::anyhow!(
                    "partial deserialization failed for heap {:?} value_index {}",
                    heap_id,
                    value_index,
                )
                .into());
            }
        }

        let ptr = target_state
            .loaded_header_ptr(value_index as usize)
            .expect("slot must be done after ensure_initialized");
        let header = unsafe { &*ptr };
        Ok(FrozenValue::new_ptr(header, is_str))
    }
}

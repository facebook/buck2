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
use std::collections::HashSet;
use std::convert::Infallible;
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
use std::sync::MutexGuard;
use std::sync::OnceLock;
use std::sync::PoisonError;
use std::sync::TryLockError;
use std::sync::Weak;

use allocative::Allocative;
use allocative::FlameGraphBuilder;
use bumpalo::Bump;
use dupe::Dupe;
use dupe::IterDupedExt;
use pagable::PagableCursor;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::PartialPagableArc;
use pagable::PartialPagableWeak;
use pagable::storage::handle::PagableStorageHandle;
use rand::RngExt;
use starlark_map::Equivalent;
use starlark_map::small_set::SmallSet;
use strong_hash::StrongHash;

use crate::any::IsStaticType;
use crate::any::ProvidesStaticType;
use crate::cast::transmute;
use crate::collections::StarlarkHashValue;
use crate::environment::GlobalFrozenHeapName;
use crate::environment::MethodFrozenHeapName;
use crate::eval::runtime::profile::instant::ProfilerInstant;
use crate::pagable::error::PagableError;
use crate::pagable::heap_ref_id::HeapRefId;
use crate::pagable::starlark_deserialize_context::HeapDeserializationState;
use crate::pagable::starlark_deserialize_context::StarlarkDeserScope;
use crate::pagable::starlark_deserialize_context::StarlarkDeserializerImpl;
use crate::pagable::starlark_serialize::StarlarkSerializeContext;
use crate::pagable::starlark_serialize_context::StarlarkSerState;
use crate::pagable::starlark_serialize_context::StarlarkSerializerImpl;
use crate::pagable::static_value::StaticHeapId;
use crate::pagable::static_value::get_static_heap_by_id;
use crate::pagable::static_value::get_static_heap_id;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::HeapSendable;
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
use crate::values::layout::heap::arena::ChunkInfo;
use crate::values::layout::heap::arena::FrozenReservation;
use crate::values::layout::heap::arena::ValueReservation;
use crate::values::layout::heap::call_enter_exit::CallEnter;
use crate::values::layout::heap::call_enter_exit::CallExit;
use crate::values::layout::heap::call_enter_exit::NeedsDrop;
use crate::values::layout::heap::call_enter_exit::NoDrop;
use crate::values::layout::heap::edge::HeapEdge;
use crate::values::layout::heap::fast_cell::FastCell;
use crate::values::layout::heap::owned_frozen::FnOncish;
use crate::values::layout::heap::owned_frozen::FnOncish2;
use crate::values::layout::heap::profile::by_type::HeapSummary;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueHeapEntryState;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::send::HeapSyncable;
use crate::values::layout::value::Value;
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
    refs: HeapReferences,
    ban_gc: Cell<bool>,
}

/// The frozen heaps a heap depends on.
///
/// A module's value heap and the frozen heap it is building hold one set between them, see
/// `ModuleHeaps`; every other heap has a set of its own. The two heaps of a module are used from
/// one thread; the lock, rather than a `RefCell`, only keeps the owned heaps `Send` (see
/// `HeapSendable`).
#[derive(Clone, Dupe, Default)]
pub(in crate::values::layout::heap) struct HeapReferences(Arc<Mutex<SmallSet<OwnedFrozen<()>>>>);

impl HeapReferences {
    fn lock(&self) -> MutexGuard<'_, SmallSet<OwnedFrozen<()>>> {
        self.0.lock().unwrap_or_else(PoisonError::into_inner)
    }

    fn insert(&self, heap: OwnedFrozenRef<'_, ()>) {
        // The empty heap keeps nothing alive.
        if heap.heap_ref.0.is_none() {
            return;
        }
        let mut refs = self.lock();
        // The same heap is added over and over (once per value brought out of it), so the lookup
        // saves the refcount round trip of `to_owned` in the common case.
        if !refs.contains(&heap) {
            refs.insert(heap.to_owned());
        }
    }

    /// The number of heaps, or `None` while the set is locked: for `Debug`, which must not block.
    fn try_len(&self) -> Option<usize> {
        match self.0.try_lock() {
            Ok(refs) => Some(refs.len()),
            Err(TryLockError::Poisoned(e)) => Some(e.into_inner().len()),
            Err(TryLockError::WouldBlock) => None,
        }
    }

    fn to_vec(&self) -> Vec<OwnedFrozen<()>> {
        self.lock().iter().duped().collect()
    }
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

/// An unfrozen heap: the values allocated on it may be mutable and are garbage collected. Each
/// [`Module`](crate::environment::Module) has one.
///
/// The handle is `Copy` and exists only inside a closure ([`Heap::temp`],
/// [`Module::with_temp_heap`](crate::environment::Module::with_temp_heap)); the values allocated
/// on it are branded with the closure's lifetime `'v`, see the `branding` module.
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

    #[cfg(test)]
    pub(crate) fn referenced_heaps(self) -> Vec<OwnedFrozen<()>> {
        self.0.refs.to_vec()
    }

    /// The set of heaps this heap depends on, for `ModuleHeaps` to share with the frozen heap it
    /// builds alongside this one.
    pub(in crate::values::layout::heap) fn references(self) -> HeapReferences {
        self.0.refs.dupe()
    }

    /// Add a dependency onto the provided frozen heap.
    pub fn add_reference(self, h: OwnedFrozenRef<'_, ()>) {
        self.0.refs.insert(h);
    }
}

/// A frozen heap under construction: an owned heap on which values can be allocated through the
/// [`FrozenHeap`] handle that [`with`](OwnedFrozenHeap::with) hands out, until it is
/// [`seal`](OwnedFrozenHeap::seal)ed into an [`OwnedFrozen<()>`] that can be shared between
/// threads.
#[derive(Default)]
pub struct OwnedFrozenHeap {
    /// My memory.
    arena: Arena<ChunkAllocator>,
    /// Memory I depend on.
    refs: HeapReferences,
    /// String interner. Its entries are allocated in this heap and stored with the brand erased;
    /// `FrozenHeap::alloc_str_hashed` restores it.
    str_interner: RefCell<StringValueInterner<'static>>,
}

/// Object-safe trait for user-defined heap names that supports hashing and downcasting.
///
/// Automatically implemented for any type that is `StrongHash + Any + Send + Sync + Debug`.
/// `StrongHash` is required (rather than `Hash`) because heap identities are
/// derived from this and must be deterministic across processes.
#[pagable::pagable_typetag]
pub trait UserHeapName:
    std::fmt::Display + pagable::typetag::PagableTagged + Any + Send + Sync + Debug + 'static
{
    /// Strong-hash this value through a trait object.
    fn dyn_strong_hash(&self, state: &mut dyn Hasher);
    /// Downcast support.
    fn as_any(&self) -> &dyn Any;
    /// Clone this name through a trait object.
    fn clone_name(&self) -> Box<dyn UserHeapName>;
}

impl<
    T: std::fmt::Display
        + pagable::typetag::PagableTagged
        + Clone
        + StrongHash
        + Any
        + Send
        + Sync
        + Debug
        + 'static,
> UserHeapName for T
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
#[derive(Clone, derive_more::Display, Debug, pagable::Pagable)]
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

impl FrozenHeapName {
    /// Create a user heap name backed by an owned string.
    pub fn user(name: impl Into<String>) -> Self {
        Self::User(Box::new(StringUserHeapName(name.into())))
    }
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
#[derive(Debug, StrongHash, Hash, Clone, derive_more::Display, pagable::Pagable)]
#[pagable::pagable_typetag(UserHeapName)]
#[display("StarlarkTestHeapName")]
pub(crate) struct StarlarkTestHeapName;

impl StarlarkTestHeapName {
    pub(crate) fn frozen_heap_name() -> FrozenHeapName {
        FrozenHeapName::User(Box::new(Self))
    }
}

/// Owned-string user heap name for callers without a dedicated name type.
#[derive(Debug, StrongHash, Hash, Clone, derive_more::Display, pagable::Pagable)]
#[pagable::pagable_typetag(UserHeapName)]
#[display("{}", _0)]
pub struct StringUserHeapName(String);

/// A frozen heap name derived from source location, for singleton heaps.
///
/// This type can only be created via the [`singleton_heap_name!`](crate::singleton_heap_name)
/// macro, which captures `file!()`, `line!()`, and `column!()` at the call site.
/// This ensures each name is unique and stable across process runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, pagable::Pagable)]
pub struct SingletonFrozenHeapName {
    file: pagable::StaticStr,
    line: u32,
    col: u32,
}

impl SingletonFrozenHeapName {
    /// Internal constructor. Do not call directly; use [`singleton_heap_name!`](crate::singleton_heap_name).
    #[doc(hidden)]
    pub const fn _new(file: pagable::StaticStr, line: u32, col: u32) -> Self {
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
    () => {{
        $crate::__derive_refs::static_str!(__SINGLETON_HEAP_FILE = file!());
        $crate::values::SingletonFrozenHeapName::_new(__SINGLETON_HEAP_FILE, line!(), column!())
    }};
}

/// `FrozenHeap` when it is no longer modified and can be shared between threads.
#[derive(Debug, Clone, Copy, Allocative, PagableSerialize, PagableDeserialize)]
struct HeapSerializationNonce(u128);

impl HeapSerializationNonce {
    fn random() -> Self {
        Self(rand::rng().random())
    }
}

#[derive(Allocative)]
#[allow(clippy::non_send_fields_in_send_ty)]
struct FrozenFrozenHeap {
    // Keeps content-identical heap incarnations in distinct cache entries while Buck2 still has
    // load-bearing frozen-value pointer identity. Remove this once distinct equal-content
    // allocations are interchangeable; the nonce prevents independent serializations from
    // sharing a `DataKey`.
    serialization_nonce: HeapSerializationNonce,
    arena: Arena<ChunkAllocator>,
    refs: Box<[OwnedFrozen<()>]>,
    // TODO(nero): remove Option here, make it required.
    #[allocative(skip)] // We don't really expect it to be big
    name: Option<FrozenHeapName>,
    peak_allocated_bytes: Option<usize>,
    /// Live serialization states this heap is registered with. Production
    /// paging creates at most one `StarlarkSerState` per process, so at most
    /// one live entry exists there; multiple live entries occur only under
    /// in-process test runners (plain `cargo test` runs every test of a
    /// binary in one process, each test with its own state). Not a
    /// `OnceLock<Weak<StarlarkSerState>>` because a dead `Weak` could never
    /// be replaced: a heap that outlives a state (e.g. a static heap) could
    /// then never register with a later state. `register_ser_state` prunes
    /// dead entries for the same reason.
    #[allocative(skip)]
    ser_states: Mutex<Vec<Weak<StarlarkSerState>>>,
    #[allocative(skip)]
    deser_state: OnceLock<Arc<HeapDeserializationState>>,
}

/// Process-local identity of an exact `FrozenFrozenHeap` allocation.
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, Hash, Allocative)]
pub(crate) struct FrozenHeapPtr(usize);

impl FrozenHeapPtr {
    pub(crate) fn addr(self) -> usize {
        self.0
    }
}

#[derive(Clone, Dupe, Allocative)]
pub(crate) struct WeakFrozenHeapRef(PartialPagableWeak<FrozenFrozenHeap>);

impl WeakFrozenHeapRef {
    pub(crate) fn upgrade(&self) -> Option<FrozenHeapArc> {
        self.0.upgrade().map(|heap| FrozenHeapArc(Some(heap)))
    }

    pub(crate) fn heap_ptr(&self) -> FrozenHeapPtr {
        FrozenHeapPtr(self.0.as_ptr() as usize)
    }
}

// SAFETY: read-only access to already-allocated arena memory is safe across
// threads. Concurrent allocations during partial-deser are serialized by
// `HeapDeserializationState.arena_alloc_lock`.
unsafe impl Sync for FrozenFrozenHeap {}
unsafe impl Send for FrozenFrozenHeap {}

impl FrozenFrozenHeap {
    fn register_ser_state(&self, state: &Arc<StarlarkSerState>) -> pagable::Result<()> {
        let mut registered = self.ser_states.lock().expect("ser states lock poisoned");
        registered.retain(|existing| existing.strong_count() != 0);
        let state = Arc::downgrade(state);
        if !registered
            .iter()
            .any(|existing| Weak::ptr_eq(existing, &state))
        {
            registered.push(state);
        }
        Ok(())
    }

    /// Serialization format:
    /// ```text
    /// [heap_name: FrozenHeapName]
    /// [serialization_nonce: HeapSerializationNonce]
    /// [refs_count: usize]
    /// for each ref:
    ///     [pagable serialized arc]
    /// [body_byte_length: u32 LE raw]     // bytes from end of header to end of value data
    /// [body_arc_count:   u32 LE raw]     // arcs serialized in the body region
    /// // ─── metadata_start captured here; everything below is parsed lazily ───
    /// [total_value_count: u32]   // drop_value_count + non_drop_value_count
    /// [total_count:      u32]            // number of values
    /// [drop_value_count: u32]            // value_index < drop_value_count ⇒ drop bump
    /// // Offset table — fixed-size, 8 raw LE bytes per entry.
    /// // (total_count + 1) entries: one per value + one sentinel end entry.
    /// // Written as placeholder, then patched via write_at after value data.
    /// // Entries are indexed by `value_index` (drop bump first, then non-drop).
    /// for i in 0..=total_count:
    ///   [stream_offset: u32 LE]          // relative to base_pos
    ///   [arc_offset:    u32 LE]          // relative to base arc_index
    /// // Metadata — postcard encoded, variable size.
    /// // Indexed by `value_index`. Bump kind for value_index `i` is
    /// // `Drop` if `i < drop_value_count`, otherwise `NonDrop`.
    /// for each value:
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
        heap_name.pagable_serialize(serializer)?;
        self.serialization_nonce.pagable_serialize(serializer)?;

        self.refs.len().pagable_serialize(serializer)?;
        for heap_ref in self.refs.iter() {
            heap_ref.pagable_serialize(serializer)?;
        }

        let drop_headers = self.arena.collect_drop_headers_ordered();
        let non_drop_headers = self.arena.collect_undrop_headers_ordered();
        let drop_value_count = drop_headers.len();
        let total_count = drop_value_count + non_drop_headers.len();

        // Write the body header placeholder: 8 raw LE bytes
        // (body_byte_length, body_arc_count). Patched after the rest of the
        // body is serialized.
        let body_header_pos = serializer.position();
        for _ in 0..8 {
            0u8.pagable_serialize(serializer)?;
        }

        // `metadata_start` is captured here so the lazy parser begins at here.
        let metadata_start = serializer.position();
        (total_count as u32).pagable_serialize(serializer)?;
        (drop_value_count as u32).pagable_serialize(serializer)?;

        // Write offset table placeholder: (value_count + 1) entries × 8 bytes each.
        // The extra entry is the end sentinel (total stream bytes + total arcs).
        // Entries are indexed by `value_index` (drop first, then non-drop).
        let table_pos = serializer.position();
        let table_entry_count = total_count + 1;
        let table_byte_size = table_entry_count * 8;
        for _ in 0..table_byte_size {
            0u8.pagable_serialize(serializer)?;
        }

        // Values in serialization order: drop bump first, then non-drop bump.
        // This ordering defines `value_index` consistently between ser and
        // deser. Bump kind for value_index `i` is implicit:
        // [0, drop_value_count) → Drop, [drop_value_count, total_count) → NonDrop.
        let all_headers_in_order: Vec<&&AValueHeader> =
            drop_headers.iter().chain(non_drop_headers.iter()).collect();

        // Write metadata for each value (postcard encoded).
        for header in &all_headers_in_order {
            let avalue = header.unpack();
            let alloc_size = avalue.memory_size().bytes();
            avalue
                .vtable()
                .deser_type_id
                .pagable_serialize(serializer)?;
            alloc_size.pagable_serialize(serializer)?;
        }

        // Record base_pos — all offsets are relative to here.
        let base_pos = serializer.position();
        // `OwnedFrozen<()>` registered this heap and its transitive dependencies
        // before deferring this Arc for serialization.
        let state = StarlarkSerializerImpl::get_or_create_state(serializer);
        let mut ctx = StarlarkSerializerImpl::new_with_root_ptr(
            serializer,
            state,
            FrozenHeapPtr(self as *const Self as usize),
        );

        // Serialize value data, recording start cursor per value.
        let mut entry_cursors: Vec<(u32, u32)> = Vec::with_capacity(table_entry_count);
        for (value_index, header) in all_headers_in_order.iter().enumerate() {
            let start = ctx.pagable().position();
            entry_cursors.push((
                (start.byte_pos - base_pos.byte_pos) as u32,
                (start.arc_index - base_pos.arc_index) as u32,
            ));
            let value = header.unpack();
            value.starlark_serialize(&mut ctx).map_err(|error| {
                self.enrich_value_serialization_error(
                    error,
                    heap_name,
                    value_index,
                    value.vtable().type_name,
                    &all_headers_in_order,
                )
            })?;
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

        // Patch the body header: (body_byte_length, body_arc_count).
        let body_byte_length = (end.byte_pos - metadata_start.byte_pos) as u32;
        let body_arc_count = (end.arc_index - metadata_start.arc_index) as u32;
        let mut header_bytes = [0u8; 8];
        header_bytes[0..4].copy_from_slice(&body_byte_length.to_le_bytes());
        header_bytes[4..8].copy_from_slice(&body_arc_count.to_le_bytes());
        // SAFETY: body_header_pos points to the 8-byte placeholder.
        unsafe {
            ctx.pagable()
                .write_at(body_header_pos.byte_pos, &header_bytes)
        };

        Ok(())
    }

    /// Read the heap identity prefix; pair with [`deserialize_skeleton`](Self::deserialize_skeleton).
    pub fn deserialize_heap_identity<'de, D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<(HeapRefId, FrozenHeapName, HeapSerializationNonce)> {
        let name = FrozenHeapName::pagable_deserialize(deserializer)?;
        let serialization_nonce = HeapSerializationNonce::pagable_deserialize(deserializer)?;
        let heap_id = HeapRefId::from_heap_name(&name);
        Ok((heap_id, name, serialization_nonce))
    }

    /// Deserialize the heap references and advance past the lazily-read body.
    fn deserialize_refs_and_skip_body<'de, D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<(Box<[OwnedFrozen<()>]>, PagableCursor)> {
        let refs_count = usize::pagable_deserialize(deserializer)?;
        let mut refs = Vec::with_capacity(refs_count);
        for _ in 0..refs_count {
            refs.push(OwnedFrozen::<()>::pagable_deserialize(deserializer)?);
        }

        let mut header = [0u8; 8];
        for b in &mut header {
            *b = u8::pagable_deserialize(deserializer)?;
        }
        let body_byte_length =
            u32::from_le_bytes([header[0], header[1], header[2], header[3]]) as usize;
        let body_arc_count =
            u32::from_le_bytes([header[4], header[5], header[6], header[7]]) as usize;
        let metadata_start = deserializer.position();

        // SAFETY: the serialized body header records the exact cursor delta to
        // the end of this heap body.
        unsafe {
            deserializer.seek(PagableCursor {
                byte_pos: metadata_start.byte_pos + body_byte_length,
                arc_index: metadata_start.arc_index + body_arc_count,
            })
        };

        Ok((refs.into_boxed_slice(), metadata_start))
    }

    /// Read refs + body header given an already-read `heap_id`, then seek
    /// past the heap body. Returns a `PartialPagableArc<Self>` with an empty arena.
    /// Slot metadata is parsed lazily on first `ensure_initialized` via the
    /// recipe stashed in `HeapDeserializationState`. Values are materialized
    /// on demand by [`StarlarkDeserializerImpl::ensure_initialized`].
    ///
    /// Returns a stable arc allocation directly: `HeapDeserializationState` holds a raw
    /// pointer into `arena`, so the address must be stable.
    /// `Arc::from(Box<T>)` would reallocate and dangle the pointer.
    pub fn deserialize_skeleton<'de, D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
        heap_id: HeapRefId,
        name: FrozenHeapName,
        serialization_nonce: HeapSerializationNonce,
        recipe: Arc<dyn pagable::PagableDeserializerRecipe>,
    ) -> crate::Result<PartialPagableArc<Self>> {
        let scope = StarlarkDeserializerImpl::get_or_create_scope(deserializer.as_dyn());

        // Refs are read eagerly — referenced heaps must be registered before
        // any of this heap's values can be resolved later.
        let (refs, metadata_start) = Self::deserialize_refs_and_skip_body(deserializer)?;

        let heap = PartialPagableArc::new(FrozenFrozenHeap {
            serialization_nonce,
            arena: Arena::default(),
            refs,
            name: Some(name),
            peak_allocated_bytes: None,
            ser_states: Mutex::new(Vec::new()),
            deser_state: OnceLock::new(),
        });
        let arena_ptr: *const Arena<ChunkAllocator> = &heap.arena;

        // SAFETY: `arena_ptr` points into `*heap`. The returned arc keeps
        // the state and arena in the same allocation lifetime, and state lookup
        // retains the heap before borrowing this state.
        let deser_state = Arc::new(unsafe {
            HeapDeserializationState::new(scope.dupe(), heap_id, metadata_start, recipe, arena_ptr)
        });
        assert!(
            heap.deser_state.set(deser_state).is_ok(),
            "a deserialized heap state must only be initialized once",
        );

        scope.register_heap(
            heap_id,
            WeakFrozenHeapRef(PartialPagableArc::downgrade(&heap)),
        )?;

        Ok(heap)
    }
}

pub(crate) fn cached_heap_deserialization_state_retained_bytes(
    storage: &PagableStorageHandle,
) -> usize {
    let heaps = storage.deserialized_arcs::<PartialPagableArc<FrozenFrozenHeap>>();
    // One builder must span every heap so its shared-allocation visited set prevents a scope or
    // recipe reachable from multiple states from being counted once per heap.
    let mut builder = FlameGraphBuilder::default();
    for heap in &heaps {
        if let Some(state) = heap.deser_state.get() {
            builder.visit_root(state.as_ref());
        }
    }
    builder.finish().flamegraph().total_size()
}

impl Drop for FrozenFrozenHeap {
    fn drop(&mut self) {
        if let Some(state) = self.deser_state.get() {
            state.unregister_heap(FrozenHeapPtr(self as *const Self as usize));
        }

        let heap_ptr = FrozenHeapPtr(self as *const Self as usize);
        let chunk_bases = self.arena.allocated_chunk_bases();
        for state in self
            .ser_states
            .get_mut()
            .expect("ser states lock poisoned")
            .iter()
            .filter_map(Weak::upgrade)
        {
            state.unregister_heap(heap_ptr, chunk_bases.iter().copied());
        }
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

/// Wire tags for a serialized heap handle.
const HEAP_REF_TAG_NONE: u8 = 0;
const HEAP_REF_TAG_ARC: u8 = 1;
const HEAP_REF_TAG_STATIC: u8 = 2;

/// The wire format of [`OwnedFrozen<()>`], which delegates here: the inner Arc via the pagable arc
/// mechanism.
///
/// Registered static heaps (`globals_static!` / `methods_static!`) are
/// referenced by `StaticHeapId` instead: their values serialize as
/// `StaticValueId`s, so the body is never needed on page-in, and paging the
/// process-wide arc would stamp a `DataKey` on shared state that other
/// serialization consumers cannot inline afterwards.
impl PagableSerialize for FrozenHeapArc {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        let Some(arc) = &self.0 else {
            return HEAP_REF_TAG_NONE.pagable_serialize(serializer);
        };
        if let Some(id) = get_static_heap_id(self) {
            HEAP_REF_TAG_STATIC.pagable_serialize(serializer)?;
            return id.pagable_serialize(serializer);
        }
        HEAP_REF_TAG_ARC.pagable_serialize(serializer)?;
        let state = StarlarkSerializerImpl::get_or_create_state(serializer);
        state.ensure_chunk_index_registered(self)?;
        serializer.serialize_arc(arc)
    }
}

/// Custom `deserialize_arc` callback stashes each heap's recipe in
/// `HeapDeserializationState` keyed by `HeapRefId` for cross heap resolution.
impl<'de> PagableDeserialize<'de> for FrozenHeapArc {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let tag = u8::pagable_deserialize(deserializer)?;
        match tag {
            HEAP_REF_TAG_NONE => Ok(FrozenHeapArc::default()),
            HEAP_REF_TAG_ARC => {
                let arc_box = deserializer.deserialize_arc(
                    std::any::TypeId::of::<PartialPagableArc<FrozenFrozenHeap>>(),
                    deserialize_heap_arc_with_recipe,
                )?;
                let arc = arc_box
                    .as_arc_any()
                    .downcast_ref::<PartialPagableArc<FrozenFrozenHeap>>()
                    .ok_or_else(|| {
                        pagable::Error::msg(
                            "frozen heap: type mismatch downcasting PartialPagableArc<FrozenFrozenHeap>",
                        )
                    })?
                    .clone();
                let heap = FrozenHeapArc(Some(arc));
                heap.register_in_deser_scope(deserializer.as_dyn())?;
                Ok(heap)
            }
            HEAP_REF_TAG_STATIC => {
                let id = StaticHeapId::pagable_deserialize(deserializer)?;
                let heap = get_static_heap_by_id(id)
                    .ok_or_else(|| {
                        pagable::Error::msg(format!(
                            "frozen heap: static heap {id:?} is not registered in this process"
                        ))
                    })?
                    .dupe();
                // Publish the live heap graph so cross-heap value pointers into it (or its
                // dependencies) resolve.
                heap.register_in_deser_scope(deserializer.as_dyn())?;
                Ok(heap)
            }
            _ => Err(pagable::Error::msg(format!(
                "frozen heap: invalid wire tag {tag}"
            ))),
        }
    }
}

/// Creates a heap's lazy-deserialization state after the generic Arc cache misses.
fn deserialize_heap_arc_with_recipe(
    de: &mut dyn PagableDeserializer<'_>,
    recipe: Arc<dyn pagable::PagableDeserializerRecipe>,
) -> pagable::Result<Box<dyn pagable::arc_erase::ArcEraseDyn>> {
    let (heap_id, name, serialization_nonce) =
        FrozenFrozenHeap::deserialize_heap_identity(de).map_err(|e| e.into_anyhow())?;
    let arc =
        FrozenFrozenHeap::deserialize_skeleton(de, heap_id, name, serialization_nonce, recipe)
            .map_err(|e| e.into_anyhow())?;
    Ok(Box::new(arc))
}

impl Debug for OwnedFrozenHeap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut x = f.debug_struct("FrozenHeap");
        x.field("bytes", &self.arena.allocated_bytes());
        x.field("refs", &self.refs.try_len());
        x.finish()
    }
}

impl Debug for FrozenFrozenHeap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut x = f.debug_struct("FrozenHeap");
        x.field("serialization_nonce", &self.serialization_nonce);
        x.field("bytes", &self.arena.allocated_bytes());
        x.field("refs", &self.refs.len());
        x.finish()
    }
}

/// The shared allocation behind a sealed [`FrozenHeap`], keeping alive all values on it; `None` is
/// the empty heap.
///
/// This is the owner inside every [`OwnedFrozen`]. The public handle to it is [`OwnedFrozen<()>`],
/// whose identity semantics are these: [`Hash`]/[`Eq`] by allocation, consistent within a process
/// but non-deterministic across executions and between distinct but observably identical heaps.
#[derive(Default, Clone, Dupe, Debug, Allocative)]
pub(crate) struct FrozenHeapArc(Option<PartialPagableArc<FrozenFrozenHeap>>);

fn _test_frozen_heap_arc_send_sync()
where
    FrozenHeapArc: Send + Sync,
{
}

impl Hash for FrozenHeapArc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(arc) = &self.0 {
            let x: &FrozenFrozenHeap = Deref::deref(&arc);
            ptr::hash(x, state);
        }
    }
}

impl PartialEq for FrozenHeapArc {
    fn eq(&self, other: &FrozenHeapArc) -> bool {
        match (&self.0, &other.0) {
            (Some(a), Some(b)) => PartialPagableArc::ptr_eq(a, b),
            (None, None) => true,
            (Some(_), None) | (None, Some(_)) => false,
        }
    }
}

impl Eq for FrozenHeapArc {}

/// How page-in obtained a heap allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapAllocationOrigin {
    /// Reconstructed from stored bytes.
    Deserialized,
    /// A live allocation page-in reused instead of reconstructing.
    Native,
}

impl HeapAllocationOrigin {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Self::Deserialized => "deserialized",
            Self::Native => "native",
        }
    }
}

impl FrozenHeapArc {
    /// Only page-in installs a `HeapDeserializationState`, so its presence
    /// separates a reconstructed allocation from a live one.
    pub(crate) fn allocation_origin(&self) -> HeapAllocationOrigin {
        match self.deser_state() {
            Some(_) => HeapAllocationOrigin::Deserialized,
            None => HeapAllocationOrigin::Native,
        }
    }

    pub(crate) fn deser_state(&self) -> Option<&HeapDeserializationState> {
        self.0
            .as_ref()
            .and_then(|heap| heap.deser_state.get())
            .map(Arc::as_ref)
    }

    fn register_in_deser_scope(
        &self,
        deserializer: &mut dyn PagableDeserializer<'_>,
    ) -> pagable::Result<()> {
        let scope = deserializer
            .page_in_scope()
            .get_or_init(StarlarkDeserScope::new);
        self.register_heap_graph_in_deser_scope(&scope)
    }

    fn register_heap_graph_in_deser_scope(
        &self,
        scope: &StarlarkDeserScope,
    ) -> pagable::Result<()> {
        let name = self
            .name()
            .ok_or_else(|| pagable::Error::msg("deserialized frozen heap must have a name"))?;
        let heap_id = HeapRefId::from_heap_name(name);
        let heap = self
            .downgrade()
            .expect("a deserialized heap must have an inner allocation");
        if scope
            .is_heap_bound(heap_id, self)
            .map_err(pagable::Error::new)?
        {
            return Ok(());
        }

        // A cached owner can contain value pointers into any transitive
        // dependency, so publish the complete graph before the owner binding.
        for dep in self.refs_slice() {
            dep.heap_arc().register_heap_graph_in_deser_scope(scope)?;
        }

        scope
            .register_heap(heap_id, heap)
            .map_err(pagable::Error::new)
    }

    // The public identity API on `OwnedFrozen`/`OwnedFrozenRef` forwards to the following.

    pub(crate) fn allocated_bytes(&self) -> usize {
        self.0.as_ref().map_or(0, |a| a.arena.allocated_bytes())
    }

    pub(crate) fn peak_allocated_bytes(&self) -> Option<usize> {
        self.0.as_ref().and_then(|a| a.peak_allocated_bytes)
    }

    pub(crate) fn available_bytes(&self) -> usize {
        self.0.as_ref().map_or(0, |a| a.arena.available_bytes())
    }

    pub(crate) fn allocated_summary(&self) -> HeapSummary {
        self.0
            .as_ref()
            .map_or_else(HeapSummary::default, |a| a.arena.allocated_summary())
    }

    pub(crate) fn name(&self) -> Option<&FrozenHeapName> {
        self.0.as_ref().and_then(|a| a.name.as_ref())
    }

    /// The frozen heaps that this frozen heap depends on.
    pub(crate) fn refs_slice(&self) -> &[OwnedFrozen<()>] {
        match &self.0 {
            Some(inner) => &inner.refs,
            None => &[],
        }
    }

    pub(crate) fn register_ser_state(&self, state: &Arc<StarlarkSerState>) -> pagable::Result<()> {
        if let Some(inner) = &self.0 {
            inner.register_ser_state(state)?;
        }
        Ok(())
    }

    pub(crate) fn downgrade(&self) -> Option<WeakFrozenHeapRef> {
        self.0
            .as_ref()
            .map(|heap| WeakFrozenHeapRef(PartialPagableArc::downgrade(heap)))
    }

    /// The values in this heap, for the pagable static registry.
    ///
    /// The heap is a static, which is what the `'static` brand means.
    pub(crate) fn iter_values(&'static self) -> impl Iterator<Item = Value<'static>> {
        struct ValueCollector<'v>(Vec<Value<'v>>);
        let mut items = ValueCollector(Vec::new());
        if let Some(heap) = &self.0 {
            impl<'v> ArenaVisitor<'v> for ValueCollector<'v> {
                fn enter_bump(&mut self) {}

                fn regular_entry(&mut self, entry: &'v super::repr::AValueHeapEntry) {
                    self.0.push(unsafe {
                        entry
                            .value_header()
                            .expect("static heap should contain only values")
                            .unpack_value(HeapKind::Frozen)
                    });
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

    /// See [`Arena::build_chunk_index`].
    pub(crate) fn build_chunk_index(&self) -> Vec<ChunkInfo> {
        match &self.0 {
            Some(inner) => inner.arena.build_chunk_index(),
            None => Vec::new(),
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

impl OwnedFrozenHeap {
    /// Create a new, empty heap.
    pub fn new() -> Self {
        Self::default()
    }

    /// A heap that depends on the same heaps as the heap `refs` came from, now and as either of
    /// them gains dependencies, see `ModuleHeaps`.
    pub(in crate::values::layout::heap) fn sharing_references(refs: HeapReferences) -> Self {
        Self {
            refs,
            ..Self::default()
        }
    }

    /// Allocate on this heap through a [`FrozenHeap`] handle that is valid within `f`.
    ///
    /// Values allocated through the handle are branded with its lifetime, so they cannot outlive
    /// the call. To keep them, [`seal`](OwnedFrozenHeap::seal) the heap afterwards and pair them
    /// with the result; [`OwnedFrozen::build`] does both steps for a single root value.
    pub fn with<R>(&self, f: impl for<'fh> FnOnce(FrozenHeap<'fh>) -> R) -> R {
        f(FrozenHeap(self, PhantomData))
    }

    /// After all values have been allocated, seal the heap into a named [`OwnedFrozen<()>`], which
    /// can be [`clone`](Clone::clone)d, shared between threads, and keeps the values allocated on
    /// the heap alive.
    ///
    /// The `name` identifies this heap and should be unique across heaps.
    /// See [`OwnedFrozen::name`] for more details.
    pub fn seal(self, name: FrozenHeapName) -> OwnedFrozen<()> {
        self.seal_impl(Some(name), None)
    }

    /// Allocate a root value through the handle, then seal the heap and return the value kept
    /// alive by it.
    ///
    /// `f` can only produce the value at the handle's brand, so the value is paired with its
    /// owner by construction; [`OwnedFrozen::build`] is this on a fresh heap. The heap is sealed
    /// whether `f` succeeds or fails; if `f` panics it is dropped unsealed instead, which is fine
    /// here because nothing outside `f` can hold a value at its brand (a module's builder is
    /// different, see `ModuleHeaps`).
    pub fn seal_with<T, E, F>(self, name: FrozenHeapName, f: F) -> Result<OwnedFrozen<T>, E>
    where
        T: IsStaticType,
        for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fh> F: FnOnce(FrozenHeap<'fh>) -> Result<T::Reinfect<'fh>, E>,
    {
        // The brand of the value is the borrow of `self`, which has to end before `self` can be
        // sealed, so the brand is erased before sealing rather than by `unchecked_new` afterwards.
        //
        // SAFETY: `'fh` is the brand of `self`, which is sealed right below into the owner the
        // value is paired with. Being closure-introduced, `'fh` names nothing else.
        let v = self.with(|fh| f(fh).map(|v| unsafe { OwnedFrozen::<T>::erase_brand(v) }));
        let sealed = self.seal_impl(Some(name), None);
        // SAFETY: `sealed` is the heap that `'fh` named.
        v.map(|v| unsafe { OwnedFrozen::from_erased(sealed, v) })
    }

    pub(crate) fn seal_impl(
        self,
        name: Option<FrozenHeapName>,
        peak_allocated_bytes: Option<usize>,
    ) -> OwnedFrozen<()> {
        let OwnedFrozenHeap {
            mut arena, refs, ..
        } = self;
        arena.finish();
        // A snapshot: a module's value heap goes on adding to a set it shares with this heap.
        let refs = refs.to_vec();
        if arena.is_empty() && refs.is_empty() {
            OwnedFrozen::default()
        } else {
            let heap = PartialPagableArc::new(FrozenFrozenHeap {
                serialization_nonce: HeapSerializationNonce::random(),
                arena,
                refs: refs.into_boxed_slice(),
                name,
                peak_allocated_bytes,
                ser_states: Mutex::new(Vec::new()),
                deser_state: OnceLock::new(),
            });
            OwnedFrozen::for_heap(FrozenHeapArc(Some(heap)))
        }
    }

    /// Number of bytes allocated on this heap, not including any memory
    /// allocated outside of the starlark heap.
    pub fn allocated_bytes(&self) -> usize {
        self.arena.allocated_bytes()
    }

    /// Whether anything has been allocated on this heap.
    pub(crate) fn has_allocations(&self) -> bool {
        !self.arena.is_empty()
    }
}

/// A handle to an [`OwnedFrozenHeap`] on which values can be allocated. The values will be
/// annotated with the heap lifetime, see the `branding` module for what that means.
///
/// Obtained from [`OwnedFrozenHeap::with`], [`FrozenHeap::temp`] or [`OwnedFrozen::build`].
#[derive(Copy, Clone, Dupe)]
// `PhantomData` is needed to make the type invariant in `'fh` - without that, branding doesn't mean
// anything.
pub struct FrozenHeap<'fh>(&'fh OwnedFrozenHeap, PhantomData<fn(&'fh ()) -> &'fh ()>);

// The handle is a borrow of the heap, not a value kept alive by it: rebranding it through a
// `HeapEdge` would let the borrow outlive the scope that introduced it.
static_assertions::assert_not_impl_any!(FrozenHeap<'static>: ProvidesStaticType<'static>);

impl<'fh> Debug for FrozenHeap<'fh> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0, f)
    }
}

impl<'fh> FrozenHeap<'fh> {
    /// Create a heap and use it within the closure.
    ///
    /// Heap is discarded at the end of the closure.
    pub fn temp<R>(f: impl for<'fh2> FnOnce(FrozenHeap<'fh2>) -> R) -> R {
        OwnedFrozenHeap::new().with(f)
    }

    #[cfg(test)]
    pub(crate) fn referenced_heaps(self) -> Vec<OwnedFrozen<()>> {
        self.0.refs.to_vec()
    }

    /// Keep the argument heap alive as long as this heap is kept alive. Used if a value in this
    /// heap points at values in another frozen heap.
    pub fn add_reference(self, heap: OwnedFrozenRef<'_, ()>) {
        self.0.refs.insert(heap);
    }

    pub(in crate::values::layout) fn string_interner(
        self,
    ) -> RefMut<'fh, StringValueInterner<'static>> {
        self.0.str_interner.borrow_mut()
    }

    pub(in crate::values::layout) fn alloc_raw<T>(
        self,
        x: AValueImpl<'fh, T>,
    ) -> ValueTyped<'fh, T::StarlarkValue>
    where
        T: AValue<'fh, ExtraElem = ()>,
        T::StarlarkValue: HeapSendable<'fh>,
        T::StarlarkValue: HeapSyncable<'fh>,
    {
        let v: &'fh AValueRepr<AValueImpl<T>> = self.0.arena.alloc(x);
        ValueTyped::new_frozen_repr(v)
    }

    pub(in crate::values::layout) fn alloc_raw_extra<T>(
        self,
        x: AValueImpl<'fh, T>,
    ) -> (
        ValueTyped<'fh, T::StarlarkValue>,
        *mut [MaybeUninit<T::ExtraElem>],
    )
    where
        T: AValue<'fh>,
        T::StarlarkValue: HeapSendable<'fh>,
        T::StarlarkValue: HeapSyncable<'fh>,
    {
        let (v, extra) = self.0.arena.alloc_extra(x);
        let v = unsafe { ValueTyped::new_frozen_repr(&*v) };
        (v, extra)
    }

    #[inline]
    pub(in crate::values::layout) fn alloc_str_init(
        self,
        len: usize,
        hash: StarlarkHashValue,
        init: impl FnOnce(*mut u8),
    ) -> StringValue<'fh> {
        let v = self.0.arena.alloc_str_init(len, hash, init);

        unsafe { StringValue::new_unchecked(Value::new_frozen_ptr(&*v, true)) }
    }

    /// Allocate a new value on this heap.
    pub fn alloc<T: AllocFrozenValue<'fh>>(self, val: T) -> Value<'fh> {
        val.alloc_frozen_value(self)
    }

    /// Allocate a value and return [`ValueTyped`] of it.
    /// Can fail if the [`AllocFrozenValue`] trait generates a different type on the heap.
    pub fn alloc_typed<T: AllocFrozenValue<'fh> + StarlarkValue<'fh>>(
        self,
        val: T,
    ) -> ValueTyped<'fh, T> {
        ValueTyped::new(self.alloc(val)).expect("just allocated value must have the right type")
    }

    /// Allocate a value and return [`ValueOfUnchecked`] of it.
    pub fn alloc_typed_unchecked<T: AllocFrozenValue<'fh>>(
        self,
        val: T,
    ) -> ValueOfUnchecked<'fh, T> {
        ValueOfUnchecked::new(self.alloc(val))
    }

    /// Number of bytes allocated on this heap, not including any memory
    /// allocated outside of the starlark heap.
    pub fn allocated_bytes(self) -> usize {
        self.0.arena.allocated_bytes()
    }

    /// Number of bytes allocated by the heap but not yet filled.
    pub fn available_bytes(self) -> usize {
        self.0.arena.available_bytes()
    }

    /// Obtain a summary of how much memory is currently allocated by this heap.
    pub fn allocated_summary(self) -> HeapSummary {
        self.0.arena.allocated_summary()
    }

    pub(crate) fn reserve_with_extra<'v2, T>(
        self,
        extra_len: usize,
    ) -> (
        FrozenReservation<'fh, 'v2, T>,
        *mut [MaybeUninit<T::ExtraElem>],
    )
    where
        T: AValue<'v2>,
        T::StarlarkValue: HeapSendable<'v2>,
        T::StarlarkValue: HeapSyncable<'v2>,
    {
        let (r, extra) = self.0.arena.reserve_with_extra::<T>(extra_len);
        (FrozenReservation(r, PhantomData), extra)
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
    pub fn peak_allocated_bytes(self) -> usize {
        cmp::max(self.allocated_bytes(), self.0.peak_allocated.get())
    }

    /// Number of bytes allocated by the heap but not yet filled.
    pub fn available_bytes(self) -> usize {
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
    pub(crate) unsafe fn allow_gc(self) {
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

    pub(crate) fn reserve<T: AValue<'v, ExtraElem = ()>>(&self) -> ValueReservation<'v, T> {
        let (r, extra) = self.reserve_with_extra::<T>(0);
        let extra = unsafe { &mut *extra };
        debug_assert!(extra.is_empty());
        r
    }

    pub(crate) fn reserve_with_extra<T: AValue<'v>>(
        &self,
        extra_len: usize,
    ) -> (ValueReservation<'v, T>, *mut [MaybeUninit<T::ExtraElem>]) {
        assert!(!T::IS_STR, "strings cannot be reserved");
        let (r, extra) = self.arena.reserve_with_extra::<T>(extra_len);
        (ValueReservation(r), extra)
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
        match old_val.state() {
            AValueHeapEntryState::Forward(x) => unsafe { x.forward_ptr().unpack_unfrozen_value() },
            AValueHeapEntryState::Value(v) => unsafe { v.unpack().heap_copy(self) },
            AValueHeapEntryState::Reservation(_) => {
                unreachable!("a heap reservation cannot appear in the source heap")
            }
        }
    }
}

// Error-path diagnostics for unresolved frozen pointers. These types and
// searches are not used by successful serialization.
#[derive(Debug, derive_more::Display)]
enum FrozenValueIndexDiagnostic {
    #[display(
        "target_physical_index={physical_index}, target_original_recipe_index=<native heap>, target_type={value_type}, target_owner_is_restored=false"
    )]
    Native {
        physical_index: usize,
        value_type: &'static str,
    },
    #[display(
        "target_physical_index=<restored allocation order is not wire order>, target_original_recipe_index={original_recipe_index}, target_owner_is_restored=true"
    )]
    Restored { original_recipe_index: u32 },
}

#[derive(Debug, derive_more::Display)]
#[display("target_owner_heap={heap_name}, target_owner_ptr={heap_ptr:#x}, {index}")]
pub(crate) struct FrozenValueLocationDiagnostic {
    heap_name: String,
    heap_ptr: usize,
    index: FrozenValueIndexDiagnostic,
}

pub(crate) enum FrozenValueOwnerSearchResult {
    Found {
        location: FrozenValueLocationDiagnostic,
        heaps_scanned: usize,
    },
    NotFound {
        heaps_scanned: usize,
    },
}

impl FrozenFrozenHeap {
    #[cold]
    fn locate_value_for_diagnostic(&self, raw_ptr: usize) -> Option<FrozenValueLocationDiagnostic> {
        let heap_ptr = self as *const Self as usize;
        let heap_name = self
            .name
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "<unnamed>".to_owned());

        if let Some(state) = self.deser_state.get() {
            let original_recipe_index = state.original_value_index(raw_ptr)?;
            return Some(FrozenValueLocationDiagnostic {
                heap_name,
                heap_ptr,
                index: FrozenValueIndexDiagnostic::Restored {
                    original_recipe_index,
                },
            });
        }

        let drop_headers = self.arena.collect_drop_headers_ordered();
        let non_drop_headers = self.arena.collect_undrop_headers_ordered();
        let (physical_index, header) = drop_headers
            .iter()
            .chain(non_drop_headers.iter())
            .enumerate()
            .find(|(_, header)| header.payload_ptr().ptr as usize == raw_ptr)?;
        Some(FrozenValueLocationDiagnostic {
            heap_name,
            heap_ptr,
            index: FrozenValueIndexDiagnostic::Native {
                physical_index,
                value_type: header.unpack().vtable().type_name,
            },
        })
    }

    #[cold]
    fn find_reachable_value_for_diagnostic(&self, raw_ptr: usize) -> FrozenValueOwnerSearchResult {
        let mut pending = vec![self];
        let mut visited = HashSet::new();

        while let Some(heap) = pending.pop() {
            let heap_ptr = heap as *const Self as usize;
            if !visited.insert(heap_ptr) {
                continue;
            }

            if let Some(location) = heap.locate_value_for_diagnostic(raw_ptr) {
                return FrozenValueOwnerSearchResult::Found {
                    location,
                    heaps_scanned: visited.len(),
                };
            }

            for heap_ref in heap.refs.iter() {
                if let Some(referenced_heap) = heap_ref.heap_arc().0.as_ref() {
                    pending.push(Deref::deref(referenced_heap));
                }
            }
        }

        FrozenValueOwnerSearchResult::NotFound {
            heaps_scanned: visited.len(),
        }
    }

    #[cold]
    fn enrich_value_serialization_error(
        &self,
        error: crate::Error,
        heap_name: &FrozenHeapName,
        value_index: usize,
        value_type: &'static str,
        all_headers_in_order: &[&&AValueHeader],
    ) -> crate::Error {
        let heap_ptr = self as *const Self as usize;
        let missing_target = match error.kind() {
            crate::ErrorKind::Other(error) => match error.downcast_ref::<PagableError>() {
                Some(PagableError::FrozenValueNotRegistered { raw_ptr, .. }) => Some(*raw_ptr),
                _ => None,
            },
            _ => None,
        };
        let target_diagnostic = missing_target.map_or_else(
            || "target location is unavailable for this error".to_owned(),
            |raw_ptr| {
                let source_snapshot_index = all_headers_in_order
                    .iter()
                    .position(|header| header.payload_ptr().ptr as usize == raw_ptr);
                let owner_diagnostic = match self.find_reachable_value_for_diagnostic(raw_ptr) {
                    FrozenValueOwnerSearchResult::Found { location, .. } => location.to_string(),
                    FrozenValueOwnerSearchResult::NotFound { heaps_scanned } => format!(
                        "target owner was not found in the source heap or its {heaps_scanned} reachable heap allocations",
                    ),
                };
                format!(
                    "{owner_diagnostic}; target_source_snapshot_index={source_snapshot_index:?}",
                )
            },
        );

        error.with_context(format!(
            "serializing heap `{heap_name}` allocation {heap_ptr:#x} value_index {value_index} type `{value_type}`; source_heap_is_restored={}, direct_ref_count={}; {target_diagnostic}",
            self.deser_state.get().is_some(),
            self.refs.len(),
        ))
    }
}

impl FrozenHeapArc {
    #[cold]
    pub(crate) fn locate_value_for_diagnostic(
        &self,
        raw_ptr: usize,
    ) -> Option<FrozenValueLocationDiagnostic> {
        self.0
            .as_ref()
            .and_then(|heap| heap.locate_value_for_diagnostic(raw_ptr))
    }
}

/// A value in a frozen heap that is automatically kept alive.
///
/// This type is a `T` together with the sealed frozen heap that keeps that `T` alive. The
/// `'static` in a type like `OwnedFrozen<Value<'static>>` stands in for the brand of that heap,
/// which has no name, and no accessor hands the value back at `'static`:
///
/// * [`add_to_heap`](OwnedFrozen::add_to_heap) records the owning heap as a reference of the
///   given heap and hands the value back at that heap's brand. This is what you want nearly all
///   of the time.
/// * [`by_ref`](OwnedFrozen::by_ref) runs a closure on the value at a brand `'fv` private to the
///   closure, so nothing derived from it can escape;
///   [`by_ref_with_reconstructor`](OwnedFrozen::by_ref_with_reconstructor) also provides an
///   `OwnedFrozenReconstructor`, which re-pairs derived values with the owner or mints a
///   [`HeapEdge`].
/// * [`map`](OwnedFrozen::map), [`try_map`](OwnedFrozen::try_map) and
///   [`maybe_map`](OwnedFrozen::maybe_map) produce an `OwnedFrozen<U>` of the same heap.
/// * [`as_ref`](OwnedFrozen::as_ref) borrows the owner as an [`OwnedFrozenRef`], which uses the
///   borrow as the brand and hands the value out directly.
///
/// The `branding` module explains why the accessors are shaped this way.
///
/// `OwnedFrozen<()>` is a bare heap handle: it keeps a heap alive without picking out any value in
/// it, and offers only the heap-identity API (`name`, `refs`, the size accessors). It compares and
/// hashes by heap identity. Sealing an [`OwnedFrozenHeap`] produces one, see
/// [`OwnedFrozenHeap::seal`], and it is the currency of [`Heap::add_reference`] and
/// [`FrozenHeap::add_reference`].
pub struct OwnedFrozen<T> {
    heap_ref: FrozenHeapArc,
    // This is morally storing a `T::Reinfect<'fv>` for `'fv` the lifetime associated with the
    // frozen heap. It would be a little bit more natural to store a `T::Reinfect<'static>` here;
    // `T` is guaranteed by the safety contract on `ProvidesStaticType` to be the same, but if we
    // had a `T::Reinfect<'static>` we wouldn't need to rely on that.
    //
    // The problem is that that would require us to add a `T: IsStaticType` bound to this *type*.
    // That does mostly turn out fine, except that it turns out to be the *exact* pattern that
    // consistently hits the compiler bug in <https://github.com/rust-lang/rust/issues/102211>,
    // making this type ~unusable in async contexts. Once that bug is fixed, it may be worth to
    // revisit.
    v: T,
    _no_auto_traits: PhantomData<dyn Any>,
}

// This module only has the safety-critical impls for this type. Additional conveniences and trait
// impls are found in `owned_frozen.rs` and based on the safe APIs provided here
impl<T> OwnedFrozen<T>
where
    for<'fv> T: IsStaticType<Reinfect<'fv> = T>,
{
    /// Get a reference to the inner value
    pub fn get<'a>(&'a self) -> &'a T {
        &self.v
    }
}

impl<T: IsStaticType> OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
{
    /// Create a new `OwnedFrozen` from the given heap and a value associated with that heap.
    ///
    /// # SAFETY
    ///
    /// The `'fv` provided must be kept alive by the passed `owner`.
    pub unsafe fn unchecked_new<'fv>(owner: OwnedFrozen<()>, v: T::Reinfect<'fv>) -> Self
    where
        // See comments on `Send` and `Sync` impls below
        for<'fv2> T::Reinfect<'fv2>: HeapSendable<'fv2> + HeapSyncable<'fv2>,
    {
        // SAFETY: The caller guarantees that `owner` keeps `'fv` alive.
        unsafe { Self::from_erased(owner, Self::erase_brand(v)) }
    }

    /// Pair `v`, which had its brand forgotten by [`erase_brand`](OwnedFrozen::erase_brand), with
    /// the heap that keeps it alive.
    ///
    /// # SAFETY
    ///
    /// `owner` must keep the brand that `v` was erased from alive.
    pub(crate) unsafe fn from_erased(owner: OwnedFrozen<()>, v: T) -> Self
    where
        // See comments on `Send` and `Sync` impls below
        for<'fv2> T::Reinfect<'fv2>: HeapSendable<'fv2> + HeapSyncable<'fv2>,
    {
        Self {
            heap_ref: owner.heap_ref,
            v,
            _no_auto_traits: PhantomData,
        }
    }

    /// Forget the brand of `v`.
    ///
    /// # SAFETY
    ///
    /// The result must be stored alongside an owner that keeps `'fv` alive.
    pub(crate) unsafe fn erase_brand<'fv>(v: T::Reinfect<'fv>) -> T {
        // SAFETY: `IsStaticType` guarantees that `T::Reinfect<'fv>` and `T` differ only in
        // lifetimes; keeping `'fv` alive is the caller's obligation.
        unsafe { transmute!(T::Reinfect<'fv>, T, v) }
    }

    /// Give `v`, which had its brand forgotten by [`erase_brand`](OwnedFrozen::erase_brand), a
    /// brand again.
    ///
    /// # SAFETY
    ///
    /// The heap identified by `'fv` must keep `v` alive.
    pub(crate) unsafe fn restore_brand<'fv>(v: T) -> T::Reinfect<'fv> {
        // SAFETY: As for `erase_brand`.
        unsafe { transmute!(T, T::Reinfect<'fv>, v) }
    }

    /// Build a value in a fresh frozen heap and return it kept alive by that heap.
    ///
    /// The heap is private to `f`, which can only get data out of it by returning it at the
    /// heap's brand, so the result is paired with its owner by construction. Use this instead of
    /// allocating into a heap of your own and reaching for
    /// [`unchecked_new`](OwnedFrozen::unchecked_new).
    ///
    /// The `name` identifies the heap; see [`OwnedFrozen::name`].
    pub fn build<F>(name: FrozenHeapName, f: F) -> Self
    where
        // See comments on `Send` and `Sync` impls below
        for<'fv2> T::Reinfect<'fv2>: HeapSendable<'fv2> + HeapSyncable<'fv2>,
        for<'fh> F: FnOnce(FrozenHeap<'fh>) -> T::Reinfect<'fh>,
    {
        match OwnedFrozenHeap::new().seal_with(name, |heap| Ok::<_, Infallible>(f(heap))) {
            Ok(v) => v,
        }
    }

    /// Use this value within the given heap: records this value's heap as a reference of `heap`,
    /// which keeps it alive from then on, and hands the value back at `heap`'s brand.
    ///
    /// ```
    /// use starlark::environment::FrozenModule;
    /// use starlark::environment::Module;
    /// use starlark::values::FrozenHeapName;
    /// use starlark::values::OwnedFrozen;
    /// use starlark::values::Value;
    ///
    /// fn copy<'v>(from: &FrozenModule, to: &Module<'v>) {
    ///     let x: OwnedFrozen<Value<'static>> = from.get_owned("value").unwrap();
    ///     let v: Value<'v> = x.add_to_heap(to.heap());
    ///     to.set("value", v);
    /// }
    ///
    /// let from = Module::with_temp_heap(|from| {
    ///     from.set("value", from.heap().alloc("a string"));
    ///     from.freeze_named(FrozenHeapName::user("from")).unwrap()
    /// });
    /// Module::with_temp_heap(|to| {
    ///     copy(&from, &to);
    ///     assert_eq!(to.get("value").unwrap().unpack_str(), Some("a string"));
    /// });
    /// ```
    ///
    /// When `to` is frozen, the reference is carried into its sealed heap, so the resulting
    /// `FrozenModule` keeps `from`'s heap alive too. The `branding` module explains the brand.
    pub fn add_to_heap<'v>(self, heap: Heap<'v>) -> T::Reinfect<'v> {
        heap.add_reference(self.owner());

        // SAFETY: The heap we just added the reference to keeps this alive for `'v`
        unsafe { transmute!(T, T::Reinfect<'v>, self.v) }
    }

    /// Access the underlying value and a reconstructor in a closure
    pub fn by_ref_with_reconstructor<'s, F, R>(&'s self, f: F) -> R
    where
        // Note: This `'a` is intentionally not `'s`. The danger that poses is that `'fv` is
        // supposed to be a brand and hence and arbitrary lifetime, but using `'s` would allow the
        // user to prove `'fv: 's` which makes the lifetime no longer arbitrary. In the extreme
        // case, if the caller supplies `'s = 'static`, they can prove `'fv = 'static`, meaning the
        // `'fv` is no longer unique at all.
        //
        // It's not clear that this is actually a problem, since if `'s = 'static` this thing lives
        // forever and the poison is mostly gone anyway, but it's still very hard to reason about.
        for<'a, 'fv> F: FnOnce(&'a T::Reinfect<'fv>, OwnedFrozenReconstructor<'fv>) -> R,
    {
        // SAFETY: See the comment on the type
        f(
            unsafe { transmute!(&T, &T::Reinfect<'_>, &self.v) },
            OwnedFrozenReconstructor {
                heap_ref: &self.heap_ref,
                _invariant: PhantomData,
            },
        )
    }

    /// Borrow a part of the underlying value as an [`OwnedFrozenRef`], using the borrow as the
    /// brand.
    ///
    /// This is how a value is picked out of an owner whose type is not `Copy`, where
    /// [`as_ref`](OwnedFrozen::as_ref) is unavailable.
    pub fn maybe_map_ref<'s, U, F>(&'s self, f: F) -> Option<OwnedFrozenRef<'s, U>>
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'a, 'fv> F: FnOncish<&'a T::Reinfect<'fv>, Option<U::Reinfect<'fv>>>,
    {
        // SAFETY: See the comment on the type
        let v = f(unsafe { transmute!(&T, &T::Reinfect<'_>, &self.v) })?;
        // SAFETY: `f` is generic over the brand, so up to unbranded (frozen) values it can only
        // return values derived from its input, which our heap keeps alive
        Some(unsafe { OwnedFrozenRef::unchecked_new(self.owner(), v) })
    }

    /// Map the underlying value and access a reconstructor
    pub fn try_by_value_with_reconstructor<U, E, R, F>(self, f: F) -> (Result<OwnedFrozen<U>, E>, R)
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fv> F: FnOncish2<
                T::Reinfect<'fv>,
                OwnedFrozenReconstructor<'fv>,
                (Result<U::Reinfect<'fv>, E>, R),
            >,
    {
        // SAFETY: See the comment on the type
        let (v, extra) = f(
            unsafe { transmute!(T, T::Reinfect<'_>, self.v) },
            OwnedFrozenReconstructor {
                // We have to transmute this lifetime because we want to allow our borrow of
                // `self.heap_ref` to expire when we move `self.heap_ref` below, but the lifetime of
                // that borrow is also the lifetime of the `'fv` in `v` which would have to expire
                // then too.
                //
                // SAFETY: `self.heap_ref` is not moved until after `f` returns, so the
                // lifetime-extended reference stays valid for the duration of the call.
                heap_ref: unsafe { transmute!(&FrozenHeapArc, &FrozenHeapArc, &self.heap_ref) },
                _invariant: PhantomData,
            },
        );
        match v {
            // SAFETY: `v`'s `'fv` is the brand of the heap owned by `self.heap_ref`, which we
            // pass in as the owner.
            Ok(v) => unsafe {
                (
                    Ok(OwnedFrozen::unchecked_new(
                        OwnedFrozen::for_heap(self.heap_ref),
                        v,
                    )),
                    extra,
                )
            },
            Err(e) => (Err(e), extra),
        }
    }
}

/// SAFETY: We would like to write the following impls:
///
/// ```rust,ignore
/// unsafe impl<T: IsStaticType> Send for OwnedFrozen<T>
/// where
///     for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
/// {
/// }
/// unsafe impl<T: IsStaticType> Sync for OwnedFrozen<T>
/// where
///     for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
/// {
/// }
/// ```
///
/// The justification for such impls would be effectively the ones discussed in the `send` module;
/// `for<'fv> HeapSendable<'fv> + HeapSyncable<'fv>` bounds are functionally `Send + Sync` up to any
/// values contained in them, and those values must be frozen values so sending/syncing them is ok.
///
/// However, actually writing such an impl once more runs headfirst into
/// <https://github.com/rust-lang/rust/issues/102211> where the compiler completely fails to prove
/// them in any async context (there's a test for this in `owned_frozen.rs`). So instead, we impl
/// `Send + Sync` unconditionally here and impose those bounds at construction time. That's a little
/// less flexible but otherwise ok.
unsafe impl<T> Send for OwnedFrozen<T> {}
unsafe impl<T> Sync for OwnedFrozen<T> {}

/// The heap-identity API: facts about the owning heap, independent of the value.
///
/// ```
/// use starlark::values::FrozenHeapName;
/// use starlark::values::OwnedFrozen;
/// use starlark::values::Value;
///
/// let v: OwnedFrozen<Value<'static>> =
///     OwnedFrozen::build(FrozenHeapName::user("example"), |heap| {
///         heap.alloc("contents")
///     });
/// assert_eq!(v.name().unwrap().to_string(), "example");
/// assert!(v.allocated_bytes() > 0);
/// assert_eq!(v.refs().count(), 0);
/// ```
impl<T> OwnedFrozen<T> {
    /// The owning heap, as a borrowed handle.
    pub fn owner(&self) -> OwnedFrozenRef<'_, ()> {
        OwnedFrozenRef::for_heap(&self.heap_ref)
    }

    pub(crate) fn heap_arc(&self) -> &FrozenHeapArc {
        &self.heap_ref
    }

    /// The name of the owning heap.
    ///
    /// Names are assigned when sealing frozen heaps, see [`OwnedFrozenHeap::seal`]; in
    /// practice, this is done when freezing modules, see
    /// [`Module::freeze_named`](crate::environment::Module::freeze_named).
    ///
    /// The name is made available here and not at a higher point like the module level so that it
    /// can be inspected even when traversing the dependency graph of frozen heaps via
    /// [`refs`](OwnedFrozen::refs).
    pub fn name(&self) -> Option<&FrozenHeapName> {
        self.heap_ref.name()
    }

    /// The frozen heaps that the owning heap depends on.
    pub fn refs(&self) -> impl Iterator<Item = OwnedFrozenRef<'_, ()>> {
        self.heap_ref.refs_slice().iter().map(OwnedFrozen::owner)
    }

    /// Number of bytes allocated on the owning heap, not including any memory allocated outside of
    /// the starlark heap.
    pub fn allocated_bytes(&self) -> usize {
        self.heap_ref.allocated_bytes()
    }

    /// Peak number of bytes allocated on the live heap that produced the owning heap, if it was
    /// produced by freezing one.
    pub fn peak_allocated_bytes(&self) -> Option<usize> {
        self.heap_ref.peak_allocated_bytes()
    }

    /// Number of bytes allocated by the owning heap but not filled. These bytes will _never_ be
    /// filled, as no further allocations can be made on a sealed heap.
    pub fn available_bytes(&self) -> usize {
        self.heap_ref.available_bytes()
    }

    /// A summary of how much memory is allocated by the owning heap. Doesn't include the heaps it
    /// keeps alive by reference.
    pub fn allocated_summary(&self) -> HeapSummary {
        self.heap_ref.allocated_summary()
    }
}

impl OwnedFrozen<()> {
    /// A handle to the heap behind `heap_ref`.
    pub(crate) fn for_heap(heap_ref: FrozenHeapArc) -> Self {
        Self {
            heap_ref,
            v: (),
            _no_auto_traits: PhantomData,
        }
    }
}

/// The empty heap, which keeps nothing alive.
impl Default for OwnedFrozen<()> {
    fn default() -> Self {
        Self::for_heap(FrozenHeapArc::default())
    }
}

/// Heap identity: two handles are equal iff they refer to the same heap allocation. Consistent
/// within a process, but non-deterministic across executions and between distinct but observably
/// identical heaps.
impl PartialEq for OwnedFrozen<()> {
    fn eq(&self, other: &Self) -> bool {
        self.heap_ref == other.heap_ref
    }
}

impl Eq for OwnedFrozen<()> {}

impl Hash for OwnedFrozen<()> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.heap_ref.hash(state)
    }
}

impl PagableSerialize for OwnedFrozen<()> {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.heap_ref.pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for OwnedFrozen<()> {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        Ok(Self::for_heap(FrozenHeapArc::pagable_deserialize(
            deserializer,
        )?))
    }
}

/// Marker providing the ability to reconstruct `OwnedFrozen` values.
///
/// This type is provided as an argument to a number of the closures in `OwnedFrozen` APIs. It
/// allows constructing more `OwnedFrozen`s referring to the same heap:
///
/// ```rust,ignore
/// let v: OwnedFrozen<(Value<'static>, Value<'static>)> = ...;
/// let v: (OwnedFrozen<Value<'static>>, OwnedFrozen<Value<'static>>) = v
///     .by_ref_with_reconstructor(|vs, reconstructor| {
///         let v0 = reconstructor.reconstruct(vs.0);
///         let v1 = reconstructor.reconstruct(vs.1);
///         (v0, v1)
///     });
/// ```
///
/// Usually this is not needed and combinations of `map`, `clone` suffice instead.
#[derive(Copy, Clone, Dupe)]
pub struct OwnedFrozenReconstructor<'fv> {
    heap_ref: &'fv FrozenHeapArc,
    // Ensure this is invariant in `'fv`; other than that, it's fine for it to be `Send + Sync`,
    // though not very useful
    _invariant: PhantomData<fn(&'fv ()) -> &'fv ()>,
}

impl<'fv> OwnedFrozenReconstructor<'fv> {
    pub fn reconstruct<T: IsStaticType>(&self, v: T::Reinfect<'fv>) -> OwnedFrozen<T>
    where
        for<'fv2> T::Reinfect<'fv2>: HeapSendable<'fv2> + HeapSyncable<'fv2> + Sized,
    {
        // SAFETY: The heap ref keeps the value alive for `'fv`
        unsafe { OwnedFrozen::unchecked_new(OwnedFrozen::for_heap(self.heap_ref.dupe()), v) }
    }

    /// Make this heap a dependency of the given heap, witnessed by the returned edge.
    ///
    /// This is the escape hatch out of the closure-based `OwnedFrozen` APIs: `'fv`-branded
    /// values can be rebranded for the given heap and returned from the closure.
    pub fn edge<'v>(&self, heap: Heap<'v>) -> HeapEdge<'v, 'fv> {
        heap.add_reference(OwnedFrozenRef::for_heap(self.heap_ref));

        // SAFETY: The reference we just added keeps our heap alive for `'v`, and `'fv` is a
        // closure-introduced brand
        unsafe { HeapEdge::unchecked_new() }
    }

    /// Like [`edge`](OwnedFrozenReconstructor::edge), but for a frozen heap
    pub fn frozen_edge<'v>(&self, heap: FrozenHeap<'v>) -> HeapEdge<'v, 'fv> {
        heap.add_reference(OwnedFrozenRef::for_heap(self.heap_ref));

        // SAFETY: The reference we just added keeps our heap alive for `'v`, and `'fv` is a
        // closure-introduced brand
        unsafe { HeapEdge::unchecked_new() }
    }
}

/// A value in a frozen heap, kept alive by a borrowed heap.
///
/// This is the borrowed counterpart of [`OwnedFrozen`]: instead of owning the heap, it borrows
/// one for `'f`, and `'f` doubles as the brand under which the value is handed out. That
/// makes access much lighter-weight than `OwnedFrozen`'s closure-based APIs — [`value`] hands out
/// the branded value directly — and the type is `Copy` when `T` is.
///
/// Because `'f` is an ordinary lifetime rather than a closure-introduced one, it is a weaker
/// brand: two `OwnedFrozenRef`s for different heaps may share the same `'f`. The soundness of
/// this type does not depend on brand uniqueness — only on the heap ref outliving `'f` — but
/// APIs that accept `'f`-branded values back cannot exist on this type; use the brand-generic
/// [`try_map`] and friends instead.
///
/// Create one with [`OwnedFrozen::as_ref`].
///
/// [`value`]: OwnedFrozenRef::value
/// [`try_map`]: OwnedFrozenRef::try_map
pub struct OwnedFrozenRef<'f, T> {
    heap_ref: &'f FrozenHeapArc,
    // Morally a `T::Reinfect<'f>`, stored brand-erased for the same reasons as `OwnedFrozen::v`
    v: T,
    _no_auto_traits: PhantomData<dyn Any>,
}

// This type has the same relationship to its safety-critical impls as `OwnedFrozen`: everything
// here upholds the invariant that `v` is kept alive by the heap behind `heap_ref`; conveniences
// live in `owned_frozen.rs`.
impl<'f, T: IsStaticType> OwnedFrozenRef<'f, T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
{
    /// Create a new `OwnedFrozenRef` from the given heap and a value associated with that heap.
    ///
    /// # SAFETY
    ///
    /// The value must be kept alive by the heap behind `owner`.
    pub unsafe fn unchecked_new(owner: OwnedFrozenRef<'f, ()>, v: T::Reinfect<'f>) -> Self
    where
        // See comments on the `Send` and `Sync` impls for `OwnedFrozen`
        for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv>,
    {
        Self {
            heap_ref: owner.heap_ref,
            // SAFETY: Caller promised
            v: unsafe { transmute!(T::Reinfect<'f>, T, v) },
            _no_auto_traits: PhantomData,
        }
    }

    /// Get the value, branded with `'f`
    pub fn value(&self) -> T::Reinfect<'f>
    where
        T: Copy,
    {
        // SAFETY: The heap ref keeps the value alive for `'f`
        unsafe { transmute!(T, T::Reinfect<'f>, self.v) }
    }

    /// Get access to this value within the provided heap
    ///
    /// See the `branding` module for more details.
    pub fn add_to_heap<'v>(self, heap: Heap<'v>) -> T::Reinfect<'v> {
        heap.add_reference(self.owner());

        // SAFETY: The heap we just added the reference to keeps this alive for `'v`
        unsafe { transmute!(T, T::Reinfect<'v>, self.v) }
    }

    /// Like [`add_to_heap`](OwnedFrozenRef::add_to_heap), but for a frozen heap
    pub fn add_to_frozen_heap<'v>(self, heap: FrozenHeap<'v>) -> T::Reinfect<'v> {
        heap.add_reference(self.owner());

        // SAFETY: The heap we just added the reference to keeps this alive for `'v`
        unsafe { transmute!(T, T::Reinfect<'v>, self.v) }
    }

    /// Convert to an [`OwnedFrozen`] of the same value
    pub fn to_owned(&self) -> OwnedFrozen<T>
    where
        T: Copy,
        for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv>,
    {
        // SAFETY: The heap ref keeps the value alive
        unsafe {
            OwnedFrozen::unchecked_new(OwnedFrozen::for_heap(self.heap_ref.dupe()), self.value())
        }
    }

    /// Fallibly transform the contained value
    pub fn try_map<U, E, F>(self, f: F) -> Result<OwnedFrozenRef<'f, U>, E>
    where
        U: IsStaticType,
        for<'fv> U::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
        for<'fv> F: FnOncish<T::Reinfect<'fv>, Result<U::Reinfect<'fv>, E>>,
    {
        let owner = self.owner();
        // SAFETY: The heap ref keeps the value alive for `'f`
        let v = f(unsafe { transmute!(T, T::Reinfect<'f>, self.v) })?;
        // SAFETY: `f` is generic over the brand, so up to unbranded (frozen) values it can only
        // return values derived from its input, which our heap keeps alive
        Ok(unsafe { OwnedFrozenRef::unchecked_new(owner, v) })
    }
}

impl<'f, T: Copy> Copy for OwnedFrozenRef<'f, T> {}

impl<'f, T: Copy> Clone for OwnedFrozenRef<'f, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'f, T: Copy> Dupe for OwnedFrozenRef<'f, T> {}

/// SAFETY: As for `OwnedFrozen`: the bounds that would justify conditional impls are instead
/// imposed at construction time, to avoid <https://github.com/rust-lang/rust/issues/102211>.
/// Additionally, the `heap_ref` field is fine to share because `FrozenHeapArc` is `Sync`.
unsafe impl<'f, T> Send for OwnedFrozenRef<'f, T> {}
unsafe impl<'f, T> Sync for OwnedFrozenRef<'f, T> {}

/// The heap-identity API, see the [`OwnedFrozen`] counterparts.
impl<'f, T> OwnedFrozenRef<'f, T> {
    /// The owning heap, as a borrowed handle.
    pub fn owner(&self) -> OwnedFrozenRef<'f, ()> {
        OwnedFrozenRef::for_heap(self.heap_ref)
    }

    pub(crate) fn heap_arc(&self) -> &'f FrozenHeapArc {
        self.heap_ref
    }

    /// See [`OwnedFrozen::name`].
    pub fn name(&self) -> Option<&'f FrozenHeapName> {
        self.heap_ref.name()
    }

    /// See [`OwnedFrozen::refs`].
    pub fn refs(&self) -> impl Iterator<Item = OwnedFrozenRef<'f, ()>> + use<'f, T> {
        let heap_ref: &'f FrozenHeapArc = self.heap_ref;
        heap_ref.refs_slice().iter().map(OwnedFrozen::owner)
    }

    /// See [`OwnedFrozen::allocated_bytes`].
    pub fn allocated_bytes(&self) -> usize {
        self.heap_ref.allocated_bytes()
    }

    /// See [`OwnedFrozen::peak_allocated_bytes`].
    pub fn peak_allocated_bytes(&self) -> Option<usize> {
        self.heap_ref.peak_allocated_bytes()
    }

    /// See [`OwnedFrozen::available_bytes`].
    pub fn available_bytes(&self) -> usize {
        self.heap_ref.available_bytes()
    }

    /// See [`OwnedFrozen::allocated_summary`].
    pub fn allocated_summary(&self) -> HeapSummary {
        self.heap_ref.allocated_summary()
    }
}

impl<'f> OwnedFrozenRef<'f, ()> {
    /// A handle to the heap behind `heap_ref`.
    pub(crate) fn for_heap(heap_ref: &'f FrozenHeapArc) -> Self {
        Self {
            heap_ref,
            v: (),
            _no_auto_traits: PhantomData,
        }
    }
}

impl Debug for OwnedFrozenRef<'_, ()> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.heap_ref, f)
    }
}

/// Heap identity, as for [`OwnedFrozen<()>`].
impl PartialEq for OwnedFrozenRef<'_, ()> {
    fn eq(&self, other: &Self) -> bool {
        *self.heap_ref == *other.heap_ref
    }
}

impl Eq for OwnedFrozenRef<'_, ()> {}

impl Hash for OwnedFrozenRef<'_, ()> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.heap_ref.hash(state)
    }
}

/// Heap identity, as for [`OwnedFrozen<()>`]: looks a borrowed heap up in a set of owned ones.
impl Equivalent<OwnedFrozen<()>> for OwnedFrozenRef<'_, ()> {
    fn equivalent(&self, key: &OwnedFrozen<()>) -> bool {
        *self.heap_ref == key.heap_ref
    }
}

impl<T: IsStaticType> OwnedFrozen<T>
where
    for<'fv> T::Reinfect<'fv>: Sized,
{
    /// Borrow this value as an [`OwnedFrozenRef`], using the borrow as the brand
    pub fn as_ref(&self) -> OwnedFrozenRef<'_, T>
    where
        T: Copy,
    {
        OwnedFrozenRef {
            heap_ref: &self.heap_ref,
            v: self.v,
            _no_auto_traits: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use dupe::Dupe;
    use starlark_derive::starlark_module;

    use super::FrozenHeapName;
    use super::Heap;
    use super::OwnedFrozen;
    use super::OwnedFrozenHeap;
    use super::OwnedFrozenRef;
    use crate as starlark;
    use crate::assert::Assert;
    use crate::environment::GlobalsBuilder;
    use crate::values::StringValue;

    #[test]
    fn test_send_sync()
    where
        OwnedFrozen<()>: Send + Sync,
    {
    }

    fn sealed_heap(name: &str) -> OwnedFrozen<()> {
        let heap = OwnedFrozenHeap::new();
        heap.with(|heap| {
            heap.alloc("contents");
        });
        heap.seal(FrozenHeapName::user(name))
    }

    #[test]
    fn test_heap_identity_api() {
        let dep = sealed_heap("dep");
        let heap = OwnedFrozenHeap::new();
        heap.with(|heap| {
            heap.alloc("contents");
            heap.add_reference(dep.owner());
        });
        let owned = heap.seal(FrozenHeapName::user("heap"));

        assert_eq!(owned.name().unwrap().to_string(), "heap");
        assert!(owned.allocated_bytes() > 0);
        assert_eq!(owned.peak_allocated_bytes(), None);
        assert!(owned.allocated_summary().total_allocated_bytes() > 0);
        let refs: Vec<OwnedFrozenRef<()>> = owned.refs().collect();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].name().unwrap().to_string(), "dep");
        assert!(refs[0] == dep.owner());

        let borrowed = owned.as_ref();
        assert_eq!(borrowed.name().unwrap().to_string(), "heap");
        assert_eq!(borrowed.allocated_bytes(), owned.allocated_bytes());
        assert_eq!(borrowed.available_bytes(), owned.available_bytes());
        assert_eq!(borrowed.peak_allocated_bytes(), None);
        assert_eq!(
            borrowed.allocated_summary().total_allocated_bytes(),
            owned.allocated_summary().total_allocated_bytes()
        );
        assert_eq!(borrowed.refs().count(), 1);
        assert!(borrowed.refs().next().unwrap() == refs[0]);
    }

    #[test]
    fn test_heap_identity_eq_hash() {
        let a = sealed_heap("a");
        let same_content = sealed_heap("a");
        let empty = OwnedFrozen::<()>::default();

        assert!(a == a.dupe());
        assert!(a != same_content);
        assert!(a != empty);
        assert!(empty == OwnedFrozen::<()>::default());
        let set: HashSet<OwnedFrozen<()>> = [a.dupe(), a.dupe(), same_content.dupe(), empty.dupe()]
            .into_iter()
            .collect();
        assert_eq!(set.len(), 3);

        assert!(a.as_ref() == a.as_ref());
        assert!(a.as_ref() != same_content.as_ref());
        assert!(a.as_ref() != empty.as_ref());
        let set: HashSet<OwnedFrozenRef<()>> = [
            a.as_ref(),
            a.as_ref(),
            same_content.as_ref(),
            empty.as_ref(),
        ]
        .into_iter()
        .collect();
        assert_eq!(set.len(), 3);
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

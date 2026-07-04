/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Bounded equivalents of `futures::future::join_all`/`try_join_all`.
//!
//! # Why this can be better than what `futures` has
//!
//! It is not magic, it's a different API contract: The set of futures here is fixed at
//! construction - the constructors take a `Vec`, and there is no insertion - and the only
//! supported operation is driving all of them to completion (or dropping everything). `futures`'
//! equivalents are built on `FuturesUnordered`, which additionally supports insertion at any
//! time, `Stream` semantics with per-item removal, and tasks whose wakers outlive the collection.
//! Paying for that generality means one `Arc` allocation *per future* (48 bytes of intrusive
//! doubly-linked-list bookkeeping plus the 16 byte refcount header), atomic list surgery on every
//! completion, and a weak-count upgrade on every wake. `join_all` alternatively uses a "poll
//! every future on every wake" mode for small sets (quadratic, avoids the per-future allocations),
//! and picks between the two behind your back based on a magic size threshold.
//!
//! Because our set is bounded up front we can instead:
//!  - store all the futures in one allocation, with the completed future's slot reused to hold
//!    its result,
//!  - keep all the per-future wake bookkeeping in a single shared allocation (16 bytes per
//!    future), with wakes recorded by index into an intrusive list, so a wake costs an
//!    uncontended lock acquisition and each poll only visits futures that were actually woken.
//!
//! Like `futures`' `join_all`, small sets skip the wake bookkeeping entirely and just poll every
//! still-running future whenever the join is polled. That is quadratic, but below the threshold
//! the total number of polls is small and the polls are cheap, while the bookkeeping costs would
//! be paid even when everything completes on the first poll. The two modes here behave
//! identically otherwise - same in-place result storage, same eager cancellation.
//!
//! Two more deliberate differences from `futures`:
//!  - `try_join_all` here drops the remaining futures as soon as one fails, like
//!    `buck2_util::future::try_join_all` and unlike `futures`' version
//!    (<https://github.com/rust-lang/futures-rs/issues/2866>).
//!  - There is no fairness/yielding: polling drains woken futures until there are none, without
//!    ever voluntarily returning to the executor. Experience in buck2 is that cooperative
//!    yielding costs real time (cf. the various `tokio::task::unconstrained` calls), so this
//!    primitive doesn't do it.

use std::future::Future;
use std::mem::ManuallyDrop;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;
use std::task::Context;
use std::task::Poll;
use std::task::RawWaker;
use std::task::RawWakerVTable;
use std::task::Waker;

use futures::task::AtomicWaker;
use mini_vec::MiniVec;
use parking_lot::Mutex;

/// Drives all the futures to completion, returning their outputs in input order.
///
/// Equivalent to `futures::future::join_all`, but see the module docs.
pub fn join_all<F: Future>(futs: Vec<F>) -> JoinAll<F> {
    JoinAll {
        core: JoinCore::new(futs),
    }
}

/// Drives all the futures to completion, returning their successes in input order, or the first
/// error. On error, the remaining futures are dropped immediately.
///
/// Equivalent to `buck2_util::future::try_join_all`, but see the module docs.
pub fn try_join_all<F, T, E>(futs: Vec<F>) -> TryJoinAll<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    TryJoinAll {
        core: JoinCore::new(futs),
    }
}

#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct JoinAll<F: Future> {
    core: JoinCore<F, AllOutcomes>,
}

impl<F: Future> Future for JoinAll<F> {
    type Output = Vec<F::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Fine because the futures live behind a box; see `JoinCore::entries`.
        let this = self.get_mut();
        this.core.poll(cx).map(|r| match r {
            Ok(v) => v,
        })
    }
}

#[must_use = "futures do nothing unless you `.await` or poll them"]
pub struct TryJoinAll<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    core: JoinCore<F, TryOutcomes>,
}

impl<F, T, E> Future for TryJoinAll<F, T, E>
where
    F: Future<Output = Result<T, E>>,
{
    type Output = Result<Vec<T>, E>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // Fine because the futures live behind a box; see `JoinCore::entries`.
        self.get_mut().core.poll(cx)
    }
}

/// What to do with each future's output: Either keep it for the final `Vec`, or bail out of the
/// whole join. The two impls are what distinguish `join_all` from `try_join_all`.
trait Outcomes<O> {
    type Keep;
    type Bail;

    fn split(o: O) -> Result<Self::Keep, Self::Bail>;
}

struct AllOutcomes;

enum Never {}

impl<O> Outcomes<O> for AllOutcomes {
    type Keep = O;
    type Bail = Never;

    fn split(o: O) -> Result<O, Never> {
        Ok(o)
    }
}

struct TryOutcomes;

impl<T, E> Outcomes<Result<T, E>> for TryOutcomes {
    type Keep = T;
    type Bail = E;

    fn split(o: Result<T, E>) -> Result<T, E> {
        o
    }
}

/// Below this many futures, the per-future wake bookkeeping costs more than it saves and
/// polling every still-running future on every wake is used instead (see the module docs).
/// `futures`' `join_all` uses the same value.
const TRACKING_THRESHOLD: usize = 30;

struct JoinCore<F: Future, M: Outcomes<F::Output>> {
    /// The futures, each slot reused for its result once it completes.
    ///
    /// The box gives the entries stable addresses, so the futures count as pinned even though
    /// the `JoinCore` itself is freely movable, and it is never touched again after a bail or
    /// completion. Entries are only ever dropped or overwritten in place.
    entries: Box<[Entry<F, M::Keep>]>,
    /// Wake bookkeeping. Kept alive beyond `self` by any wakers that outlive us. `None` for
    /// small sets, which poll everything on every wake instead of tracking.
    shared: Option<SharedHandle>,
    /// Reusable buffer that `woken` gets swapped into for draining.
    scratch: MiniVec<u32>,
    /// Number of entries still `Running`.
    remaining: u32,
    /// Whether the initial poll-everything sweep has happened.
    started: bool,
}

impl<F: Future, M: Outcomes<F::Output>> JoinCore<F, M> {
    fn new(futs: Vec<F>) -> Self {
        Self::with_threshold(futs, TRACKING_THRESHOLD)
    }

    fn with_threshold(futs: Vec<F>, threshold: usize) -> Self {
        let n = futs.len();
        JoinCore {
            entries: futs.into_iter().map(Entry::new).collect(),
            shared: if n > threshold {
                Some(SharedHandle::alloc(n))
            } else {
                None
            },
            scratch: MiniVec::new(),
            remaining: n.try_into().unwrap(),
            started: false,
        }
    }

    fn poll(&mut self, cx: &mut Context<'_>) -> Poll<Result<Vec<M::Keep>, M::Bail>> {
        assert!(
            self.remaining != 0 || !self.entries.is_empty() || !self.started,
            "polled after completion"
        );
        let Self {
            entries,
            shared,
            scratch,
            remaining,
            started,
        } = self;
        match shared {
            None => {
                // Small set: Just poll everything that's still running, handing the branches
                // our own waker.
                *started = true;
                for i in 0..entries.len() {
                    if let Some(bail) = Self::poll_entry(entries, remaining, i, cx.waker()) {
                        return Poll::Ready(Err(bail));
                    }
                }
            }
            Some(shared) => {
                // Register before checking for wakes so that a wake between the check and
                // returning `Pending` is not lost.
                shared.get().parent.register(cx.waker());

                if !*started {
                    *started = true;
                    for i in 0..entries.len() {
                        let waker = shared.borrowed_waker(i);
                        if let Some(bail) = Self::poll_entry(entries, remaining, i, &waker) {
                            return Poll::Ready(Err(bail));
                        }
                    }
                }
                loop {
                    // Swap out the woken list instead of holding its lock: Polling an entry may
                    // wake things, and the wake path takes the same lock.
                    std::mem::swap(&mut *shared.get().woken.lock(), scratch);
                    if scratch.is_empty() {
                        break;
                    }
                    for idx in std::mem::take(scratch) {
                        let waker = shared.borrowed_waker(idx as usize);
                        if let Some(bail) =
                            Self::poll_entry(entries, remaining, idx as usize, &waker)
                        {
                            return Poll::Ready(Err(bail));
                        }
                    }
                }
            }
        }

        if self.remaining == 0 {
            let mut out = Vec::with_capacity(self.entries.len());
            for e in &mut self.entries {
                out.push(e.take_keep());
            }
            self.entries = Box::new([]);
            Poll::Ready(Ok(out))
        } else {
            Poll::Pending
        }
    }

    /// Polls the entry with the given waker if it is still running. On completion the output is
    /// either stored back into the entry or, if it means the whole join is done for, returned;
    /// in that case all remaining futures are dropped before returning.
    ///
    /// Takes the fields instead of `&mut self` so that callers can keep `self.shared` borrowed.
    fn poll_entry(
        entries: &mut Box<[Entry<F, M::Keep>]>,
        remaining: &mut u32,
        i: usize,
        waker: &Waker,
    ) -> Option<M::Bail> {
        // SAFETY: Entries live behind the box, so they never move; they are only ever dropped
        // or overwritten (via `Pin::set`) in place.
        let mut entry = unsafe { Pin::new_unchecked(&mut entries[i]) };
        // SAFETY: Standard structural pinning projection to the `Running` variant's future.
        let fut = match unsafe { entry.as_mut().get_unchecked_mut() } {
            Entry::Running(fut) => unsafe { Pin::new_unchecked(fut) },
            // A stale wake for a future that already completed.
            _ => return None,
        };
        let mut cx = Context::from_waker(waker);
        match fut.poll(&mut cx) {
            Poll::Pending => None,
            Poll::Ready(out) => {
                *remaining -= 1;
                match M::split(out) {
                    Ok(keep) => {
                        // `Pin::set` runs the future's destructor in place before overwriting,
                        // which is fine for pinned data; same pattern as `MaybeDone`.
                        entry.set(Entry::Done(keep));
                        None
                    }
                    Err(bail) => {
                        // Drop everything else right away; unlike `futures`' try_join_all, the
                        // remaining futures get cancelled eagerly.
                        *entries = Box::new([]);
                        Some(bail)
                    }
                }
            }
        }
    }
}

/// One slot of the join: A future, then its (kept) output, in the same storage.
///
/// State transitions while the future exists happen strictly in place (`Pin::set`, which drops
/// the old value before overwriting); the plain-`mem::replace` extraction in `take_keep` is only
/// reachable once no future is left in the slot. This is `futures`' `MaybeDone` pattern.
enum Entry<F, K> {
    Running(F),
    Done(K),
    Taken,
}

impl<F, K> Entry<F, K> {
    fn new(fut: F) -> Self {
        Entry::Running(fut)
    }

    fn take_keep(&mut self) -> K {
        // Moving the entry is fine here even though it was pinned: The future - the only thing
        // whose address anyone cares about - completed and was destroyed in place; all that's
        // left is the output, which was passed around by value to begin with. (`MaybeDone`'s
        // `take_output` makes the same argument.)
        match std::mem::replace(self, Entry::Taken) {
            Entry::Done(keep) => keep,
            _ => unreachable!("take_keep on an entry that isn't Done"),
        }
    }
}

/// The wake bookkeeping of one tracked join, in a single manually managed allocation laid out as
/// `[Shared, index table: [u32]]`, where index slot `i` just holds `i`.
///
/// The waker for branch `i` is a raw pointer to its index slot: Waking reads the index through
/// the pointer and offsets *backwards* by it to find the header. This makes the entire per-branch
/// wake state 4 bytes; a woken branch is recorded by pushing its index onto the `woken` list
/// (which is empty, ie hasn't even allocated, most of the time).
///
/// This lives in a manual refcounted allocation because wakers are `'static`: A future may hand
/// its waker to something that fires it after the join itself is gone, so everything a waker
/// touches must be kept alive by the waker itself. The refcount is all that is; the join future
/// is the only thing that ever *polls*.
#[repr(C)]
struct Shared {
    /// One count for the owning join (dropped by `SharedHandle`) plus one per live waker clone.
    refcount: AtomicUsize,
    /// The number of branches; determines the allocation's layout.
    n: u32,
    /// Indexes of branches woken since the join was last polled. Pushes are deduplicated only in
    /// so far as `wake` on an already-woken branch is a no-op once recorded here and not yet
    /// consumed - a branch legitimately re-woken after being polled again simply appears again.
    ///
    /// The mutex also makes wake vs. drain ordering trivially correct, in the style of
    /// [`crate::atomic_waker_set::AtomicWakerSet`]: One uncontended lock per wake is cheap.
    woken: Mutex<MiniVec<u32>>,
    /// The join future's own waker.
    parent: AtomicWaker,
}

/// The index table sits directly after the header; `Shared`'s size is a multiple of its (8 byte)
/// alignment, which satisfies `u32`'s.
const INDEX_TABLE_OFFSET: usize = std::mem::size_of::<Shared>();

/// Owns one count on a `Shared` allocation.
struct SharedHandle(NonNull<Shared>);

// SAFETY: `Shared` contains only synchronized data (atomics and a mutex), and the handle owns a
// refcount keeping the allocation alive.
unsafe impl Send for SharedHandle {}
unsafe impl Sync for SharedHandle {}

impl SharedHandle {
    fn alloc(n: usize) -> SharedHandle {
        let layout = Shared::layout(n);
        // SAFETY: The layout is never zero-sized (there's always a header).
        let base = unsafe { std::alloc::alloc(layout) };
        let Some(ptr) = NonNull::new(base.cast::<Shared>()) else {
            std::alloc::handle_alloc_error(layout);
        };
        // SAFETY: Freshly allocated with room for the header followed by the index table.
        unsafe {
            ptr.write(Shared {
                refcount: AtomicUsize::new(1),
                n: n.try_into().unwrap(),
                woken: Mutex::new(MiniVec::new()),
                parent: AtomicWaker::new(),
            });
            for i in 0..n {
                base.add(INDEX_TABLE_OFFSET + i * 4).cast::<u32>().write(i as u32);
            }
        }
        SharedHandle(ptr)
    }

    fn get(&self) -> &Shared {
        // SAFETY: We hold a refcount.
        unsafe { self.0.as_ref() }
    }

    /// A waker for the `i`th branch that does NOT hold a refcount; the caller keeps the
    /// allocation alive for the duration of the poll. Clones of it (which is what futures store)
    /// do.
    fn borrowed_waker(&self, i: usize) -> ManuallyDrop<Waker> {
        debug_assert!(i < self.get().n as usize);
        // SAFETY: In bounds of the allocation per the layout.
        let data = unsafe {
            self.0
                .as_ptr()
                .cast::<u8>()
                .add(INDEX_TABLE_OFFSET + i * 4)
                .cast::<()>()
        };
        // SAFETY: See the vtable functions.
        ManuallyDrop::new(unsafe { Waker::from_raw(RawWaker::new(data, &VTABLE)) })
    }
}

impl Drop for SharedHandle {
    fn drop(&mut self) {
        // SAFETY: Releases the count this handle owns.
        unsafe { decref(self.0.as_ptr()) };
    }
}

impl Shared {
    fn layout(n: usize) -> std::alloc::Layout {
        std::alloc::Layout::new::<Shared>()
            .extend(std::alloc::Layout::array::<u32>(n).unwrap())
            .unwrap()
            .0
            .pad_to_align()
    }
}

/// Recovers the header from a waker's data pointer (which points into the index table).
///
/// # Safety
///
/// `data` must be a waker data pointer produced by `borrowed_waker` (or a clone), and the
/// allocation must be live.
unsafe fn shared_from_data(data: *const ()) -> (*const Shared, u32) {
    let slot = data.cast::<u32>();
    // SAFETY: The slot holds this branch's index, written at allocation time and never changed.
    let i = unsafe { *slot };
    // SAFETY: Walking backwards within the same allocation, exactly undoing `borrowed_waker`.
    let shared = unsafe { slot.cast::<u8>().sub(INDEX_TABLE_OFFSET + i as usize * 4) };
    (shared.cast::<Shared>(), i)
}

/// # Safety
///
/// `shared` must be live and the caller must own a count on it.
unsafe fn decref(shared: *const Shared) {
    // SAFETY: Per the contract, this releases a count we own.
    if unsafe { &(*shared).refcount }.fetch_sub(1, std::sync::atomic::Ordering::Release) == 1 {
        std::sync::atomic::fence(std::sync::atomic::Ordering::Acquire);
        // SAFETY: That was the last count; nothing can touch the allocation anymore.
        unsafe {
            let n = (*shared).n as usize;
            let ptr = shared.cast_mut();
            std::ptr::drop_in_place(ptr);
            std::alloc::dealloc(ptr.cast::<u8>(), Shared::layout(n));
        }
    }
}

/// The waker data pointer points into a `Shared`'s index table; each waker clone owns one count
/// on the allocation (borrowed wakers, which hold none, are never dropped).
static VTABLE: RawWakerVTable = RawWakerVTable::new(clone_raw, wake_raw, wake_by_ref_raw, drop_raw);

unsafe fn clone_raw(data: *const ()) -> RawWaker {
    // SAFETY: Data pointer contract of the vtable.
    let (shared, _) = unsafe { shared_from_data(data) };
    // SAFETY: The existing waker holds (or its poll-scope caller guarantees) a count, so the
    // allocation is live and we can take another count.
    unsafe { &(*shared).refcount }.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    RawWaker::new(data, &VTABLE)
}

unsafe fn wake_raw(data: *const ()) {
    // SAFETY: Forwarding the vtable contract.
    unsafe {
        wake_by_ref_raw(data);
        drop_raw(data);
    }
}

unsafe fn wake_by_ref_raw(data: *const ()) {
    // SAFETY: Data pointer contract of the vtable.
    let (shared, i) = unsafe { shared_from_data(data) };
    // SAFETY: The waker holds a count, so the allocation is live.
    let shared = unsafe { &*shared };
    {
        let mut woken = shared.woken.lock();
        if woken.contains(&i) {
            // Already recorded; whoever recorded it also woke the parent.
            return;
        }
        woken.push(i);
    }
    shared.parent.wake();
}

unsafe fn drop_raw(data: *const ()) {
    // SAFETY: Data pointer contract of the vtable.
    let (shared, _) = unsafe { shared_from_data(data) };
    // SAFETY: Releases the count this waker owns.
    unsafe { decref(shared) };
}

#[cfg(test)]
mod tests {
    use std::future::poll_fn;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::task::Wake;

    use super::*;

    /// A waker that counts how many times it has been woken.
    struct CountingWaker(AtomicUsize);

    impl Wake for CountingWaker {
        fn wake(self: Arc<Self>) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }

        fn wake_by_ref(self: &Arc<Self>) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }

    fn poll_once<F: Future>(fut: &mut Pin<Box<F>>, waker: &Waker) -> Poll<F::Output> {
        fut.as_mut().poll(&mut Context::from_waker(waker))
    }

    /// A future that stashes its waker and completes with `value` once externally triggered.
    struct Triggered {
        value: u32,
        fired: Arc<Mutex<(bool, Option<Waker>)>>,
    }

    fn triggered(value: u32) -> (Triggered, Arc<Mutex<(bool, Option<Waker>)>>) {
        let state = Arc::new(Mutex::new((false, None)));
        (
            Triggered {
                value,
                fired: state.clone(),
            },
            state,
        )
    }

    fn fire(state: &Arc<Mutex<(bool, Option<Waker>)>>) {
        let waker = {
            let mut guard = state.lock();
            guard.0 = true;
            guard.1.take()
        };
        if let Some(w) = waker {
            w.wake();
        }
    }

    impl Future for Triggered {
        type Output = u32;

        fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<u32> {
            let mut guard = self.fired.lock();
            if guard.0 {
                Poll::Ready(self.value)
            } else {
                guard.1 = Some(cx.waker().clone());
                Poll::Pending
            }
        }
    }

    #[test]
    fn test_join_all_immediate() {
        let futs: Vec<_> = (0..100u32).map(|i| async move { i * 2 }).collect();
        let mut j = Box::pin(join_all(futs));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(
            poll_once(&mut j, &waker),
            Poll::Ready((0..100u32).map(|i| i * 2).collect::<Vec<_>>())
        );
    }

    #[test]
    fn test_join_all_empty() {
        let mut j = Box::pin(join_all(Vec::<std::future::Ready<u32>>::new()));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Ready(vec![]));
    }

    #[test]
    fn test_results_in_input_order_despite_reverse_completion() {
        let (f0, s0) = triggered(0);
        let (f1, s1) = triggered(1);
        let (f2, s2) = triggered(2);
        let mut j = Box::pin(join_all(vec![f0, f1, f2]));

        let counter = Arc::new(CountingWaker(AtomicUsize::new(0)));
        let waker = Waker::from(counter.clone());
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);

        // Complete in reverse order, polling in between.
        fire(&s2);
        assert_eq!(counter.0.load(Ordering::SeqCst), 1);
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);
        fire(&s1);
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);
        fire(&s0);
        assert_eq!(poll_once(&mut j, &waker), Poll::Ready(vec![0, 1, 2]));
    }

    #[test]
    fn test_wake_is_edge_triggered_per_future() {
        let (f0, s0) = triggered(0);
        let (f1, _s1) = triggered(1);
        let mut j = Box::pin(join_all(vec![f0, f1]));

        let counter = Arc::new(CountingWaker(AtomicUsize::new(0)));
        let waker = Waker::from(counter.clone());
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);

        // Two wakes of the same future before the next poll: only one parent wake.
        fire(&s0);
        let stray = {
            let guard = s0.lock();
            guard.1.clone()
        };
        assert!(stray.is_none(), "waker should have been consumed by fire");
        assert_eq!(counter.0.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_try_join_all_ok() {
        let futs: Vec<_> = (0..50u32)
            .map(|i| async move { Ok::<_, String>(i) })
            .collect();
        let mut j = Box::pin(try_join_all(futs));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(
            poll_once(&mut j, &waker),
            Poll::Ready(Ok((0..50).collect::<Vec<_>>()))
        );
    }

    /// The remaining futures must be dropped as soon as one future fails. This is the semantic
    /// difference to `futures`' `try_join_all`, and this test fails with that version.
    #[test]
    fn test_try_join_all_cancels_eagerly() {
        struct SetOnDrop(Arc<AtomicUsize>);
        impl Drop for SetOnDrop {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }

        let drops = Arc::new(AtomicUsize::new(0));
        let mut futs = Vec::new();
        for _ in 0..10 {
            let guard = SetOnDrop(drops.clone());
            futs.push(
                async move {
                    let _guard = guard;
                    // Never completes.
                    poll_fn(|_| Poll::<()>::Pending).await;
                    Ok(())
                }
                .boxed_local(),
            );
        }
        futs.push(async move { Err("boom") }.boxed_local());

        let mut j = Box::pin(try_join_all(futs));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Ready(Err("boom")));
        assert_eq!(
            drops.load(Ordering::SeqCst),
            10,
            "all pending futures must be dropped when the join fails"
        );
    }

    /// Dropping the join must cancel (drop) all the futures.
    #[test]
    fn test_drop_cancels() {
        struct SetOnDrop(Arc<AtomicUsize>);
        impl Drop for SetOnDrop {
            fn drop(&mut self) {
                self.0.fetch_add(1, Ordering::SeqCst);
            }
        }

        let drops = Arc::new(AtomicUsize::new(0));
        let futs: Vec<_> = (0..5)
            .map(|_| {
                let guard = SetOnDrop(drops.clone());
                async move {
                    let _guard = guard;
                    poll_fn(|_| Poll::<()>::Pending).await
                }
            })
            .collect();

        let mut j = Box::pin(join_all(futs));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);
        drop(j);
        assert_eq!(drops.load(Ordering::SeqCst), 5);
    }

    /// A waker that outlives the join must stay safe to invoke.
    #[test]
    fn test_stray_waker_after_drop() {
        let (f0, s0) = triggered(0);
        let mut j = Box::pin(join_all(vec![f0]));

        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);

        // Steal the branch waker that `f0` stashed, then kill the join.
        let stray = s0.lock().1.take().unwrap();
        drop(j);

        // Must not crash or touch freed memory (the Arc<Shared> is kept alive by the waker).
        stray.wake_by_ref();
        stray.clone().wake();
    }

    struct SelfWake(bool);
    impl Future for SelfWake {
        type Output = u32;

        fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<u32> {
            if self.0 {
                Poll::Ready(7)
            } else {
                self.0 = true;
                cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }

    /// In tracked mode, a future that wakes itself must get re-polled within the same call:
    /// The drain loop keeps going until nothing is woken, since we never yield.
    #[test]
    fn test_self_wake_is_drained_in_same_poll_when_tracked() {
        let mut j = Box::pin(JoinAll {
            // Forced into tracked mode
            core: JoinCore::with_threshold(vec![SelfWake(false)], 0),
        });
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Ready(vec![7]));
    }

    /// In small (untracked) mode the branches hold the join's own waker, so a self-wake goes to
    /// the executor and completion needs a re-poll, like `futures`' `join_all`.
    #[test]
    fn test_self_wake_defers_to_repoll_when_small() {
        let mut j = Box::pin(join_all(vec![SelfWake(false)]));
        let counter = Arc::new(CountingWaker(AtomicUsize::new(0)));
        let waker = Waker::from(counter.clone());
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);
        assert_eq!(counter.0.load(Ordering::SeqCst), 1);
        assert_eq!(poll_once(&mut j, &waker), Poll::Ready(vec![7]));
    }

    /// The tracked path must also work end to end with real cross-"thread" completions.
    #[test]
    fn test_tracked_mode_large_set() {
        let mut futs = Vec::new();
        let mut states = Vec::new();
        for i in 0..40u32 {
            let (f, s) = triggered(i);
            futs.push(f);
            states.push(s);
        }
        let mut j = Box::pin(join_all(futs));
        let waker = Waker::from(Arc::new(CountingWaker(AtomicUsize::new(0))));
        assert_eq!(poll_once(&mut j, &waker), Poll::Pending);
        // Fire in a scrambled order, polling intermittently.
        let mut result = Poll::Pending;
        for (i, s) in states.iter().enumerate().rev() {
            fire(s);
            if i % 3 == 0 {
                result = poll_once(&mut j, &waker);
            }
        }
        if result.is_pending() {
            result = poll_once(&mut j, &waker);
        }
        assert_eq!(result, Poll::Ready((0..40u32).collect::<Vec<_>>()));
    }

    use futures::FutureExt;
}

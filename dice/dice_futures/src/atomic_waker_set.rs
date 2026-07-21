/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::Ordering;
use std::task::Waker;

use parking_lot::Mutex;

/// A synchronization primitive that represents a set of wakers.
///
/// You might alternatively know this primitive under the terms "event listener" or "event counter."
///
/// Tokio provides a higher level `Notify` type that can provide the same API, but with different
/// tradeoffs. In particular, this type prioritizes simplicity and minimal memory footprint at the
/// expense of most other properties. Throughput is only mediocre.
pub struct AtomicWakerSet {
    /// Our synchronization approach is quite straightforward: We have one mutex here, and this one
    /// mutex protects everything in this data structure. All operations on any nodes in the
    /// structure must hold the mutex.
    ///
    /// There are alternative implementation strategies with different tradeoffs:
    ///  - You could go slab based instead of intrusive. That makes some problems easier, but it
    ///    costs extra memory and you have to worry about excess capacity, so is avoided here.
    ///  - You could switch to a singly linked list. That has a bunch of benefits including saved
    ///    memory and non-blocking inserts. However, it means quadratic behavior on cancellation,
    ///    which is probably unacceptable.
    mutex: Mutex<*mut AtomicWakerSetEntry>,
}

/// An entry in an `AtomicWakerSet`.
///
/// This type is inherently quite unsafe. Most importantly, it does not internally hold a reference
/// to the set of which the entry is a member. This is a small memory optimization as usually the
/// user of this type will have such a reference available anyway. However, that means that
/// management of the lifetime of this object is entirely on the user.
pub struct AtomicWakerSetEntry {
    /// All three data fields are protected by the lock in the `AtomicWakerSet`, unless the value is
    /// in a state where it has not been inserted into a set, in which case this value owns all the
    /// fields.
    waker: UnsafeCell<Option<Waker>>,
    /// Together, the `next` and `prev` fields form an intrusive linked list of registered entries,
    /// with the mutex in the set holding a pointer to the head.
    ///
    /// `next` is:
    ///
    ///  - 1 to indicate that this entry is not inserted into any set.
    ///  - 0/null to indicate that this is the last element of the set/tail of the list.
    ///  - Otherwise a pointer to the next element of the set.
    ///
    /// This field also makes an exception to the standard ownership rule, as it's important that we
    /// don't need to acquire the lock to check if we're disconnected. As such, we allow the owner
    /// to check for disconnection by performing an acquire load on this field. `wake_all` finalizes
    /// an entry from another thread, so it must release-store `1` here to synchronize with that
    /// acquire load; an owner finalizing its own entry in `disconnect` is on the same thread and so
    /// can store plainly. Writing another entry's `next` (the splice in `disconnect`) still has to
    /// be atomic, since that entry's owner may be performing its lock-free load concurrently.
    next: AtomicPtr<AtomicWakerSetEntry>,
    prev: UnsafeCell<*mut AtomicWakerSetEntry>,
    _pinned: PhantomPinned,
    /// Prevent autotrait impls
    _data: PhantomData<*mut ()>,
}

const NOT_INSERTED: *mut AtomicWakerSetEntry = std::ptr::without_provenance_mut(1);

impl AtomicWakerSet {
    /// Prepare a new set
    pub fn new() -> Self {
        AtomicWakerSet {
            mutex: Mutex::new(std::ptr::null_mut()),
        }
    }

    /// Wake all the wakers in the set.
    pub fn wake_all(&self) {
        unsafe {
            let mut guard = self.mutex.lock();
            let mut cur = *guard;
            // We're about to disconnect everything, so just do this now
            *guard = std::ptr::null_mut();

            // Accumulate all the wakers. We cannot wake them until we release the lock
            let mut wakers: smallvec::SmallVec<[Waker; 128]> = Default::default();
            while !cur.is_null() {
                wakers.extend((*(*cur).waker.get()).take());
                // Disconnect the entry. This is important so that when it itself invokes
                // `disconnect`, it gets the fast path. This must be the last thing we do, since as
                // soon as we execute this our lock no longer protects the entry and the memory
                // might be deallocated.
                let next_cur = (*cur).next.swap(NOT_INSERTED, Ordering::Release);
                cur = next_cur;
            }
            // Need to hold the guard until here since we're changing list structure
            drop(guard);
            for w in wakers {
                w.wake();
            }
        }
    }
}

impl Default for AtomicWakerSet {
    fn default() -> Self {
        Self::new()
    }
}

// SAFETY: The set owns nothing thread-affine - just the list head behind `mutex`,
// pointing at entries owned elsewhere - so it is safe to move between threads.
unsafe impl Send for AtomicWakerSet {}
// SAFETY: All access to the list head and to entry fields is serialized by `mutex`
// (for `waker`/`prev`) or performed atomically (for `next`), and the stored `Waker`s
// are `Send + Sync`, so concurrent `&self` access from multiple threads is sound.
unsafe impl Sync for AtomicWakerSet {}

impl AtomicWakerSetEntry {
    /// Prepare a new entry
    pub fn new() -> Self {
        AtomicWakerSetEntry {
            waker: UnsafeCell::new(None),
            next: AtomicPtr::new(NOT_INSERTED),
            prev: UnsafeCell::new(std::ptr::null_mut()),
            _pinned: PhantomPinned,
            _data: PhantomData,
        }
    }

    /// Register the entry into the set with the given waker.
    ///
    /// This is the analogue to the `AtomicWaker::register` API with largely similar semantics: The
    /// waker will be woken next time `wake_all` is called on the set. If a waker was already
    /// registered, it is replaced. Furthermore, this operation happens-after any previous
    /// `wake_all` calls that did not include this operation.
    ///
    /// Unlike `AtomicWaker`, this does not even notionally support being called concurrently; the
    /// self argument is taken by `&mut`.
    ///
    /// # Safety
    ///
    ///  - Eventually, after calling `register`, you must call `disconnect`, evicting this entry
    ///    from the set. `disconnect` must be called with the same exact `set` as `register`.
    ///    Important: You may not drop the entry until after you have disconnected.
    ///  - There is no lifetime attached to this type. It is your responsibility to ensure that
    ///    `set` is kept alive until the `disconnect` call.
    ///  - Until you next disconnect, you may only invoke `register` again with the same `set`.
    pub unsafe fn register(self: Pin<&mut Self>, set: &AtomicWakerSet, waker: Waker) {
        unsafe {
            let this = Pin::into_inner_unchecked(self.into_ref());
            let is_already_inserted = this.next.load(Ordering::Acquire) != NOT_INSERTED;
            if is_already_inserted {
                // Spurious wakeup. With some more work we could optimize this to fast path out in
                // the case of an equality comparison, but so far we haven't bothered.
                let guard = set.mutex.lock();
                // Owning the lock gives us the right to change the waker
                #[expect(clippy::mem_replace_option_with_some)]
                let old = std::mem::replace(&mut *this.waker.get(), Some(waker));
                // Don't drop the previous waker while we hold the lock
                drop(guard);
                drop(old);
            } else {
                // Not inserted anywhere yet, own our own fields. Prepare the waker and prev fields
                *this.waker.get() = Some(waker);
                *this.prev.get() = std::ptr::null_mut();
                // We need to insert into the stack. The wake -> register happens before
                // relationship is established by this lock
                let mut guard = set.mutex.lock();
                let old_head = *guard;
                let this_ptr = this as *const _ as *mut _;
                *this.next.as_ptr() = old_head;
                // We become the new head, so the old head (if any) must point back at us.
                if !old_head.is_null() {
                    *(*old_head).prev.get() = this_ptr;
                }
                *guard = this_ptr;
            }
        }
    }

    /// Evicts this entry from the provided set.
    ///
    /// After disconnecting, the entry is safe to either discard or reuse.
    ///
    /// # Safety
    ///
    /// Must be called on the same set on which the entry is registered.
    pub unsafe fn disconnect(self: Pin<&mut Self>, set: &AtomicWakerSet) {
        unsafe {
            let this = Pin::into_inner_unchecked(self.into_ref());
            let is_inserted = this.next.load(Ordering::Acquire) != NOT_INSERTED;
            if is_inserted {
                // Slow removal, indicative of a cancellation instead of a previous wakeup
                let mut guard = set.mutex.lock();
                // Need to check that we're still inserted
                let next = *this.next.as_ptr();
                let still_inserted = next != NOT_INSERTED;
                if still_inserted {
                    let prev = *this.prev.get();
                    if !next.is_null() {
                        *(*next).prev.get() = prev;
                    }
                    if !prev.is_null() {
                        // `prev.next` is read lock-free by `prev`'s owner, so this write must be
                        // atomic to avoid a data race. The value is only consumed under the lock, so
                        // `Relaxed` is sufficient.
                        (*prev).next.store(next, Ordering::Relaxed);
                    } else {
                        // We were the first entry
                        *guard = next;
                    }
                    // Finalize: mark ourselves not-inserted so a later `register` reuses us and a
                    // later `disconnect` fast-paths. Plain store is fine here - see `next`'s docs.
                    *this.next.as_ptr() = NOT_INSERTED;
                }
            }
        }
    }
}

impl Default for AtomicWakerSetEntry {
    fn default() -> Self {
        Self::new()
    }
}

// SAFETY: The entry's contents are not thread-affine and the stored `Waker` is `Send`,
// so an entry (e.g. embedded in a `Send` future) can be moved between threads.
unsafe impl Send for AtomicWakerSetEntry {}
// SAFETY: While registered, the entry is accessed both by its owner and by a `wake_all`
// caller on another thread, but every such access is synchronized by the set's `mutex`
// (`waker`/`prev`) or performed atomically (`next`), and the stored `Waker` is `Sync`.
unsafe impl Sync for AtomicWakerSetEntry {}

#[cfg(test)]
mod tests {
    use std::pin::pin;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::task::Wake;
    use std::task::Waker;

    use super::*;

    /// A waker that counts how many times it has been woken.
    struct CountingWaker(AtomicUsize);

    impl CountingWaker {
        fn new() -> Arc<Self> {
            Arc::new(CountingWaker(AtomicUsize::new(0)))
        }

        fn count(&self) -> usize {
            self.0.load(Ordering::SeqCst)
        }
    }

    impl Wake for CountingWaker {
        fn wake(self: Arc<Self>) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }

        fn wake_by_ref(self: &Arc<Self>) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn registered_entry_is_woken() {
        let set = AtomicWakerSet::new();
        let waker = CountingWaker::new();

        let mut entry = pin!(AtomicWakerSetEntry::new());
        unsafe {
            entry.as_mut().register(&set, Waker::from(waker.clone()));
            set.wake_all();
            entry.as_mut().disconnect(&set);
        }

        assert_eq!(waker.count(), 1, "a registered entry must be woken once");
    }

    #[test]
    fn disconnected_entry_is_not_woken() {
        let set = AtomicWakerSet::new();
        let waker = CountingWaker::new();

        let mut entry = pin!(AtomicWakerSetEntry::new());
        unsafe {
            entry.as_mut().register(&set, Waker::from(waker.clone()));
            entry.as_mut().disconnect(&set);
            set.wake_all();
        }

        assert_eq!(waker.count(), 0, "a disconnected entry must not be woken");
    }

    #[test]
    fn disconnecting_one_entry_keeps_the_others_registered() {
        let set = AtomicWakerSet::new();
        let waker_a = CountingWaker::new();
        let waker_b = CountingWaker::new();

        let mut a = pin!(AtomicWakerSetEntry::new());
        let mut b = pin!(AtomicWakerSetEntry::new());
        unsafe {
            a.as_mut().register(&set, Waker::from(waker_a.clone()));
            b.as_mut().register(&set, Waker::from(waker_b.clone()));

            // Cancel only A. B must stay registered.
            a.as_mut().disconnect(&set);
            set.wake_all();

            b.as_mut().disconnect(&set);
        }

        assert_eq!(waker_a.count(), 0, "A was disconnected, must not be woken");
        assert_eq!(waker_b.count(), 1, "B was still registered, must be woken");
    }

    #[test]
    fn entry_can_be_reused_after_disconnect() {
        let set = AtomicWakerSet::new();
        let waker = CountingWaker::new();

        let mut entry = pin!(AtomicWakerSetEntry::new());
        unsafe {
            entry.as_mut().register(&set, Waker::from(waker.clone()));
            entry.as_mut().disconnect(&set);

            // Reuse the same entry. It must once again be woken by `wake_all`.
            entry.as_mut().register(&set, Waker::from(waker.clone()));
            set.wake_all();
            entry.as_mut().disconnect(&set);
        }

        assert_eq!(
            waker.count(),
            1,
            "a reused entry must be woken after re-registering"
        );
    }

    #[test]
    fn all_entries_in_a_larger_set_are_woken() {
        let set = AtomicWakerSet::new();
        let wakers: Vec<_> = (0..5).map(|_| CountingWaker::new()).collect();

        let mut entries: Vec<_> = (0..5)
            .map(|_| Box::pin(AtomicWakerSetEntry::new()))
            .collect();
        unsafe {
            for (entry, waker) in entries.iter_mut().zip(&wakers) {
                entry.as_mut().register(&set, Waker::from(waker.clone()));
            }
            set.wake_all();
            for entry in &mut entries {
                entry.as_mut().disconnect(&set);
            }
        }

        for (i, waker) in wakers.iter().enumerate() {
            assert_eq!(waker.count(), 1, "entry {i} must be woken exactly once");
        }
    }
}

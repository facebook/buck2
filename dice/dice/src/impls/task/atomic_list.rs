/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::atomic::Ordering;

/// A lock-free, append-only stack used as a task's waiter list (see `DiceTaskInternal::wakers`).
///
/// Many threads `push` values concurrently; `drain` atomically takes the whole list and runs
/// `T::drain` on each value (e.g. waking a waiter) while leaving the list open for new pushes, and
/// `close` does the same but permanently rejects further pushes. A `push` that returns `false`
/// means the list was closed.
///
/// # Synchronization
///
/// This type provides the following synchronization guarantee: Every `push` operation happens-after
/// the most recent previous `drain` or `close` operation. This is the same guarantee that
/// `AtomicWaker` provides and as a result the patterns that allow for correct use of this API are
/// similar to the ones typical with `AtomicWaker`.
pub(crate) struct SimpleAtomicList<T: SimpleAtomicListNodeSpec> {
    head: std::sync::atomic::AtomicPtr<SimpleAtomicListNode<T>>,
}

/// Describes the value type stored in a `SimpleAtomicList` and what to do with each value when the
/// list is drained or closed.
pub(crate) trait SimpleAtomicListNodeSpec {
    type Value;
    fn drain(v: Self::Value);
}

pub(crate) enum SimpleAtomicListNode<T: SimpleAtomicListNodeSpec> {
    Tail,
    Node {
        next: *mut SimpleAtomicListNode<T>,
        value: T::Value,
    },
}

impl<T: SimpleAtomicListNodeSpec> SimpleAtomicListNode<T> {
    fn drain(self) {
        let mut this = self;
        loop {
            match this {
                Self::Tail => break,
                Self::Node { next, value } => {
                    T::drain(value);
                    this = unsafe { *Box::from_raw(next) };
                }
            }
        }
    }
}

impl<T: SimpleAtomicListNodeSpec> SimpleAtomicList<T> {
    pub(crate) fn new() -> Self {
        Self {
            head: std::sync::atomic::AtomicPtr::new(Box::into_raw(Box::new(
                SimpleAtomicListNode::Tail,
            ))),
        }
    }

    /// Drains all values in the list. The list will continue to accept new values.
    pub(crate) fn drain(&self) {
        let new_head = Box::into_raw(Box::new(SimpleAtomicListNode::Tail));
        // Serves a couple synchronization roles:
        //  - `Acquire` is an implementation requirement: it lets us read the contents (`T::Value`)
        //    of every node we take, via the release chain that `push` maintains.
        //  - `Release` publishes the freshly allocated Tail so a later drainer can read it
        //  - `Release` will eventually match with an `Acquire` load in `push` which provides the
        //    happens-before relationship that is a part of the API contract.
        let head = self.head.swap(new_head, Ordering::AcqRel);
        if head.is_null() {
            return;
        }
        unsafe { Box::from_raw(head) }.drain();
    }

    /// Drains all values in the list and prevents new values from being added.
    pub(crate) fn close(&self) {
        let new_head = std::ptr::null_mut();
        // Synchronization here is effectively the same as in drain, except that there's no freshly
        // allocated tail
        let head = self.head.swap(new_head, Ordering::AcqRel);
        if head.is_null() {
            return;
        }
        unsafe { Box::from_raw(head) }.drain();
    }

    /// Pushes a value onto the list.
    ///
    /// Returns true if the value was pushed, false if the list is closed. See the type-level
    /// "Synchronization" docs for the happens-before guarantees.
    pub(crate) fn push(&self, v: T::Value) -> bool {
        // `Acquire` synchronizes with the `Release` in `drain` or `close` and creates the
        // happens-before edge required by the API
        let mut head = self.head.load(Ordering::Acquire);
        if head.is_null() {
            return false;
        }

        // new_node must be inserted in the list, or converted back to a Box.
        let new_node: *mut SimpleAtomicListNode<T> =
            Box::into_raw(Box::new(SimpleAtomicListNode::Node {
                next: head,
                value: v,
            }));
        loop {
            head = match self.head.compare_exchange_weak(
                head,
                new_node,
                // `AcqRel` is needed to create a synchronization chain between all push operations.
                // That ensures that the eventual drainer happens-after previous writes to all nodes
                // in the list, and that subsequent pushes happen-after the previous drain call as
                // required by the API.
                Ordering::AcqRel,
                // `Acquire` here is only needed for the `head.is_null()` case below, which falls in
                // the same bucket as the load at the top of the function
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    return true;
                }
                Err(new_next) => new_next,
            };

            if head.is_null() {
                // SAFETY: new_node was not added to the list, convert back to Box so it gets deallocated.
                drop(unsafe { Box::from_raw(new_node) });
                return false;
            }

            // SAFETY: new_node is non-null as it's allocated above, and it has no references.
            match unsafe { new_node.as_mut() } {
                Some(SimpleAtomicListNode::Node { next, .. }) => {
                    *next = head;
                }
                _ => unreachable!(),
            }
        }
    }
}

impl<T: SimpleAtomicListNodeSpec> Drop for SimpleAtomicList<T> {
    fn drop(&mut self) {
        self.close()
    }
}

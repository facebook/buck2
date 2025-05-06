/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::mem::ManuallyDrop;
use std::slice;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use allocative::Visitor;
use atomic::Atomic;

use crate::atomic_value::AtomicValue;

/// Fixed capacity hashtable.
/// When dropped, it will not drop the entries.
pub(crate) struct FixedCapTable<T: AtomicValue> {
    /// Current list of entries. Null if the entry is missing, otherwise a leaked `Box<T>`.
    entries: Box<[Atomic<T::Raw>]>,
    /// Number of entries in the table. We always use relaxed operations for this, meaning
    /// that any particular return value can always be wrong in either direction.
    size: AtomicUsize,
}

pub(crate) struct IterPtrs<'a, T: AtomicValue> {
    iter: slice::Iter<'a, Atomic<T::Raw>>,
}

impl<T: AtomicValue> Iterator for IterPtrs<'_, T> {
    type Item = T::Raw;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                Some(entry) => {
                    let ptr = entry.load(Ordering::Acquire);
                    if !T::is_null(ptr) {
                        return Some(ptr);
                    }
                }
                None => return None,
            }
        }
    }
}

impl<T: AtomicValue + Allocative> FixedCapTable<T> {
    pub(crate) fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>, current: bool) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let mut visitor = visitor.enter_unique(
                allocative::Key::new("entries"),
                mem::size_of_val(&self.entries),
            );
            {
                let mut visitor = visitor.enter(
                    allocative::Key::new("capacity"),
                    mem::size_of_val::<[Atomic<T::Raw>]>(&*self.entries),
                );
                // We only visit pointers from current table.
                if current {
                    for ptr in self.iter_ptrs() {
                        let mut visitor =
                            visitor.enter(allocative::Key::new("entry"), mem::size_of::<T::Raw>());
                        unsafe {
                            // SAFETY: We "forget" the value after the iteration,
                            //   so it is safe to convert it to owned value.
                            let value = T::from_raw(ptr);
                            let value = ManuallyDrop::new(value);
                            if mem::size_of_val(&value) > mem::size_of::<T::Raw>() {
                                // Size will be computed incorrectly.
                            }

                            T::visit(&value, &mut visitor);
                        }
                        visitor.exit();
                    }
                }
                visitor.exit();
            }
            visitor.exit();
        }
        visitor.exit();
    }
}

/// Iterator over the entries.
pub struct Iter<'a, T: AtomicValue> {
    iter: IterPtrs<'a, T>,
}

impl<'a, T: AtomicValue> Iter<'a, T> {
    #[inline]
    pub(crate) fn empty() -> Iter<'a, T> {
        Iter {
            iter: IterPtrs { iter: [].iter() },
        }
    }
}

impl<'a, T: AtomicValue + 'a> Iterator for Iter<'a, T> {
    type Item = T::Ref<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|ptr| unsafe { T::deref(ptr) })
    }
}

impl<T: AtomicValue> FixedCapTable<T> {
    pub(crate) fn with_capacity(cap: usize) -> FixedCapTable<T> {
        assert!(cap.is_power_of_two());
        let mut entries = Vec::new();
        entries.resize_with(cap, || Atomic::new(T::null()));
        FixedCapTable {
            entries: entries.into_boxed_slice(),
            size: AtomicUsize::new(0),
        }
    }

    #[inline]
    pub(crate) fn capacity(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub(crate) fn need_resize(&self) -> bool {
        self.size.load(Ordering::Relaxed) >= self.entries.len() / 2
    }

    #[inline]
    pub(crate) fn lookup(
        &self,
        hash: u64,
        eq: impl Fn(T::Ref<'_>) -> bool,
    ) -> Option<T::Ref<'_>> {
        let mut index = hash as usize & (self.entries.len() - 1);
        for _ in 0..self.entries.len() {
            let entry = self.entries[index].load(Ordering::Acquire);
            if T::is_null(entry) {
                return None;
            }
            let entry = unsafe { T::deref(entry) };
            if eq(entry) {
                return Some(entry);
            }
            index = (index + 1) & (self.entries.len() - 1);
        }
        None
    }

    pub(crate) fn insert_unique_unchecked(&mut self, hash: u64, value: T::Raw) {
        let mut index = hash as usize & (self.entries.len() - 1);
        loop {
            let entry = self.entries[index].get_mut();
            if T::is_null(*entry) {
                *self.entries[index].get_mut() = value;
                *self.size.get_mut() += 1;
                return;
            }
            index = (index + 1) & (self.entries.len() - 1);
        }
    }

    /// Insert or lookup value.
    ///
    /// If the value is already in the table, it will be returned.
    /// If the capacity is full, the value will be returned as an error.
    /// Otherwise, the value will be inserted and returned.
    pub(crate) fn insert<'a>(
        &self,
        hash: u64,
        value: T,
        eq: impl Fn(T::Ref<'_>, T::Ref<'_>) -> bool,
    ) -> Result<(T::Ref<'a>, Option<T>), T> {
        let value = T::into_raw(value);
        assert!(!T::is_null(value));

        let mut index = hash as usize & (self.entries.len() - 1);

        // We do not know if the table is full already,
        // or becoming full concurrently with us.
        // So we have to stop after `capacity` iterations.
        for _ in 0..self.entries.len() {
            let entry = self.entries[index].load(Ordering::Acquire);
            let entry = if T::is_null(entry) {
                match self.entries[index].compare_exchange(
                    T::null(),
                    value,
                    // If the compare_exchange succeeds, the pointer has been inserted
                    // into the table. This means we need `Release` to ensure that the
                    // data behind the `Box<T>` is visible to other threads.
                    Ordering::Release,
                    // The code below reads the data behind the pointer, this means we
                    // need acquire so that this thread sees any previous changes to that
                    // data.
                    Ordering::Acquire,
                ) {
                    Ok(_) => {
                        self.size.fetch_add(1, Ordering::Relaxed);
                        return Ok((unsafe { T::deref(value) }, None));
                    }
                    Err(entry) => {
                        // Someone has just inserted the value into the bucket.
                        entry
                    }
                }
            } else {
                entry
            };
            if eq(unsafe { T::deref(value) }, unsafe { T::deref(entry) }) {
                return Ok(unsafe { (T::deref(entry), Some(T::from_raw(value))) });
            }
            index = (index + 1) & (self.entries.len() - 1);
        }
        Err(unsafe { T::from_raw(value) })
    }

    /// Iterate over the pointers to the entries.
    #[inline]
    pub(crate) fn iter_ptrs(&self) -> IterPtrs<'_, T> {
        IterPtrs {
            iter: self.entries.iter(),
        }
    }

    /// Iterate over the entries.
    #[inline]
    pub(crate) fn iter(&self) -> Iter<'_, T> {
        Iter {
            iter: self.iter_ptrs(),
        }
    }

    /// Drop all entries.
    pub(crate) unsafe fn drop_entries(&mut self) {
        for entry in self.iter_ptrs() {
            unsafe {
                let _drop = T::from_raw(entry);
            }
        }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.size.load(Ordering::Relaxed)
    }
}

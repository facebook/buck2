/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ptr;
use std::slice;
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

/// Fixed capacity hashtable.
/// When dropped, it will not drop the entries.
pub(crate) struct FixedCapTable<T> {
    /// Current list of entries. Null if the entry is missing, otherwise a leaked `Box<T>`.
    entries: Box<[AtomicPtr<T>]>,
    /// Number of entries in the table. We always use relaxed operations for this, meaning
    /// that any particular return value can always be wrong in either direction.
    size: AtomicUsize,
}

pub(crate) struct IterPtrs<'a, T> {
    iter: slice::Iter<'a, AtomicPtr<T>>,
}

impl<'a, T> Iterator for IterPtrs<'a, T> {
    type Item = *mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                Some(entry) => {
                    let ptr = entry.load(Ordering::Acquire);
                    if !ptr.is_null() {
                        return Some(ptr);
                    }
                }
                None => return None,
            }
        }
    }
}

/// Iterator over the entries.
pub struct Iter<'a, T> {
    iter: IterPtrs<'a, T>,
}

impl<'a, T> Iter<'a, T> {
    #[inline]
    pub(crate) fn empty() -> Iter<'a, T> {
        Iter {
            iter: IterPtrs { iter: [].iter() },
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|ptr| unsafe { &*ptr })
    }
}

impl<T> FixedCapTable<T> {
    pub(crate) fn with_capacity(cap: usize) -> FixedCapTable<T> {
        assert!(cap.is_power_of_two());
        let mut entries = Vec::new();
        entries.resize_with(cap, || AtomicPtr::new(ptr::null_mut()));
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
    pub(crate) fn lookup(&self, hash: u64, eq: impl Fn(&T) -> bool) -> Option<&T> {
        let mut index = hash as usize & (self.entries.len() - 1);
        for _ in 0..self.entries.len() {
            let entry = self.entries[index].load(Ordering::Acquire);
            if entry.is_null() {
                return None;
            }
            let entry = unsafe { &*entry };
            if eq(entry) {
                return Some(entry);
            }
            index = (index + 1) & (self.entries.len() - 1);
        }
        None
    }

    pub(crate) fn insert_unique_unchecked(&mut self, hash: u64, value: *mut T) {
        let mut index = hash as usize & (self.entries.len() - 1);
        loop {
            let entry = self.entries[index].get_mut();
            if entry.is_null() {
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
    pub(crate) fn insert(
        &self,
        hash: u64,
        value: Box<T>,
        eq: impl Fn(&T, &T) -> bool,
    ) -> Result<&T, Box<T>> {
        let value = Box::into_raw(value);
        let mut index = hash as usize & (self.entries.len() - 1);

        // We do not know if the table is full already,
        // or becoming full concurrently with us.
        // So we have to stop after `capacity` iterations.
        for _ in 0..self.entries.len() {
            let entry = self.entries[index].load(Ordering::Acquire);
            let entry = if entry.is_null() {
                match self.entries[index].compare_exchange(
                    ptr::null_mut(),
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
                        return Ok(unsafe { &*value });
                    }
                    Err(entry) => {
                        // Someone has just inserted the value into the bucket.
                        entry
                    }
                }
            } else {
                entry
            };
            if eq(unsafe { &*value }, unsafe { &*entry }) {
                unsafe {
                    let _drop = Box::from_raw(value);
                };
                return Ok(unsafe { &*entry });
            }
            index = (index + 1) & (self.entries.len() - 1);
        }
        Err(unsafe { Box::from_raw(value) })
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
                let _drop = Box::from_raw(entry);
            }
        }
    }
}

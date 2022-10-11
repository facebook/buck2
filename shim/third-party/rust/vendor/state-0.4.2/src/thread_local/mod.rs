// Copyright 2017 Amanieu d'Antras
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Per-object thread-local storage
//!
//! This library provides the `ThreadLocal` type which allows a separate copy of
//! an object to be used for each thread. This allows for per-object
//! thread-local storage, unlike the standard library's `thread_local!` macro
//! which only allows static thread-local storage.
//!
//! Per-thread objects are not destroyed when a thread exits. Instead, objects
//! are only destroyed when the `ThreadLocal` containing them is destroyed.
//!
//! You can also iterate over the thread-local values of all thread in a
//! `ThreadLocal` object using the `iter_mut` and `into_iter` methods. This can
//! only be done if you have mutable access to the `ThreadLocal` object, which
//! guarantees that you are the only thread currently accessing it.
//!
//! A `CachedThreadLocal` type is also provided which wraps a `ThreadLocal` but
//! also uses a special fast path for the first thread that writes into it. The
//! fast path has very low overhead (<1ns per access) while keeping the same
//! performance as `ThreadLocal` for other threads.
//!
//! Note that since thread IDs are recycled when a thread exits, it is possible
//! for one thread to retrieve the object of another thread. Since this can only
//! occur after a thread has exited this does not lead to any race conditions.

#![warn(missing_docs)]

mod thread_id;
mod unreachable;

use std::cell::UnsafeCell;
use std::fmt;
use std::panic::UnwindSafe;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::Mutex;

use self::unreachable::{UncheckedOptionExt, UncheckedResultExt};

/// Thread-local variable wrapper
///
/// See the [module-level documentation](index.html) for more.
pub struct ThreadLocal<T> {
    // Pointer to the current top-level hash table
    table: AtomicPtr<Table<T>>,

    // Lock used to guard against concurrent modifications. This is only taken
    // while writing to the table, not when reading from it. This also guards
    // the counter for the total number of values in the hash table.
    lock: Mutex<usize>,
}

struct Table<T> {
    // Hash entries for the table
    entries: Box<[TableEntry<T>]>,

    // Number of bits used for the hash function
    hash_bits: usize,

    // Previous table, half the size of the current one
    prev: Option<Box<Table<T>>>,
}

struct TableEntry<T> {
    // Current owner of this entry, or 0 if this is an empty entry
    owner: AtomicUsize,

    // The object associated with this entry. This is only ever accessed by the
    // owner of the entry.
    data: UnsafeCell<Option<Box<T>>>,
}

// ThreadLocal is always Sync, even if T isn't
unsafe impl<T: Send> Sync for ThreadLocal<T> {}

impl<T: Send> Default for ThreadLocal<T> {
    fn default() -> ThreadLocal<T> {
        ThreadLocal::new()
    }
}

impl<T> Drop for ThreadLocal<T> {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.table.load(Ordering::Relaxed));
        }
    }
}

// Implementation of Clone for TableEntry, needed to make vec![] work
impl<T: Send> Clone for TableEntry<T> {
    fn clone(&self) -> TableEntry<T> {
        TableEntry {
            owner: AtomicUsize::new(0),
            data: UnsafeCell::new(None),
        }
    }
}

// Hash function for the thread id
#[cfg(target_pointer_width = "32")]
#[inline]
fn hash(id: usize, bits: usize) -> usize {
    id.wrapping_mul(0x9E3779B9) >> (32 - bits)
}
#[cfg(target_pointer_width = "64")]
#[inline]
fn hash(id: usize, bits: usize) -> usize {
    id.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> (64 - bits)
}

impl<T: Send> ThreadLocal<T> {
    /// Creates a new empty `ThreadLocal`.
    pub fn new() -> ThreadLocal<T> {
        let entry = TableEntry {
            owner: AtomicUsize::new(0),
            data: UnsafeCell::new(None),
        };
        let table = Table {
            entries: vec![entry; 2].into_boxed_slice(),
            hash_bits: 1,
            prev: None,
        };
        ThreadLocal {
            table: AtomicPtr::new(Box::into_raw(Box::new(table))),
            lock: Mutex::new(0),
        }
    }

    /// Returns the element for the current thread, if it exists.
    pub fn get(&self) -> Option<&T> {
        let id = thread_id::get();
        self.get_fast(id)
    }

    /// Returns the element for the current thread, or creates it if it doesn't
    /// exist.
    pub fn get_or<F>(&self, create: F) -> &T
    where
        F: FnOnce() -> T,
    {
        unsafe {
            self.get_or_try(|| Ok::<T, ()>(create()))
                .unchecked_unwrap_ok()
        }
    }

    /// Returns the element for the current thread, or creates it if it doesn't
    /// exist. If `create` fails, that error is returned and no element is
    /// added.
    pub fn get_or_try<F, E>(&self, create: F) -> Result<&T, E>
    where
        F: FnOnce() -> Result<T, E>,
    {
        let id = thread_id::get();
        match self.get_fast(id) {
            Some(x) => Ok(x),
            None => Ok(self.insert(id, Box::new(create()?), true)),
        }
    }

    // Simple hash table lookup function
    fn lookup(id: usize, table: &Table<T>) -> Option<&UnsafeCell<Option<Box<T>>>> {
        // Because we use a Mutex to prevent concurrent modifications (but not
        // reads) of the hash table, we can avoid any memory barriers here. No
        // elements between our hash bucket and our value can have been modified
        // since we inserted our thread-local value into the table.
        for entry in table.entries.iter().cycle().skip(hash(id, table.hash_bits)) {
            let owner = entry.owner.load(Ordering::Relaxed);
            if owner == id {
                return Some(&entry.data);
            }
            if owner == 0 {
                return None;
            }
        }
        unreachable!();
    }

    // Fast path: try to find our thread in the top-level hash table
    fn get_fast(&self, id: usize) -> Option<&T> {
        let table = unsafe { &*self.table.load(Ordering::Acquire) };
        match Self::lookup(id, table) {
            Some(x) => unsafe { Some((*x.get()).as_ref().unchecked_unwrap()) },
            None => self.get_slow(id, table),
        }
    }

    // Slow path: try to find our thread in the other hash tables, and then
    // move it to the top-level hash table.
    #[cold]
    fn get_slow(&self, id: usize, table_top: &Table<T>) -> Option<&T> {
        let mut current = &table_top.prev;
        while let Some(ref table) = *current {
            if let Some(x) = Self::lookup(id, table) {
                let data = unsafe { (*x.get()).take().unchecked_unwrap() };
                return Some(self.insert(id, data, false));
            }
            current = &table.prev;
        }
        None
    }

    #[cold]
    fn insert(&self, id: usize, data: Box<T>, new: bool) -> &T {
        // Lock the Mutex to ensure only a single thread is modify the hash
        // table at once.
        let mut count = self.lock.lock().unwrap();
        if new {
            *count += 1;
        }
        let table_raw = self.table.load(Ordering::Relaxed);
        let table = unsafe { &*table_raw };

        // If the current top-level hash table is more than 75% full, add a new
        // level with 2x the capacity. Elements will be moved up to the new top
        // level table as they are accessed.
        let table = if *count > table.entries.len() * 3 / 4 {
            let entry = TableEntry {
                owner: AtomicUsize::new(0),
                data: UnsafeCell::new(None),
            };
            let new_table = Box::into_raw(Box::new(Table {
                entries: vec![entry; table.entries.len() * 2].into_boxed_slice(),
                hash_bits: table.hash_bits + 1,
                prev: unsafe { Some(Box::from_raw(table_raw)) },
            }));
            self.table.store(new_table, Ordering::Release);
            unsafe { &*new_table }
        } else {
            table
        };

        // Insert the new element into the top-level hash table
        for entry in table.entries.iter().cycle().skip(hash(id, table.hash_bits)) {
            let owner = entry.owner.load(Ordering::Relaxed);
            if owner == 0 {
                unsafe {
                    entry.owner.store(id, Ordering::Relaxed);
                    *entry.data.get() = Some(data);
                    return (*entry.data.get()).as_ref().unchecked_unwrap();
                }
            }
            if owner == id {
                // This can happen if create() inserted a value into this
                // ThreadLocal between our calls to get_fast() and insert(). We
                // just return the existing value and drop the newly-allocated
                // Box.
                unsafe {
                    return (*entry.data.get()).as_ref().unchecked_unwrap();
                }
            }
        }
        unreachable!();
    }
}

impl<T: Send + fmt::Debug> fmt::Debug for ThreadLocal<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ThreadLocal {{ local_data: {:?} }}", self.get())
    }
}

impl<T: Send + UnwindSafe> UnwindSafe for ThreadLocal<T> {}

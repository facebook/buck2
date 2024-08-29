/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::mem;

use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;

use crate::directory::entry::DirectoryEntry;

/// A trait shared by iterators on Directories. Unlike a regular Iterator, this returns an accessor
/// to give us the current path in addition to the current item (which borrows from the iterator
/// itself, which is why this cannot be an iterator).
pub trait DirectoryIterator: Sized {
    /// The way this iterator will report its current path.
    type PathStack<'a>: DirectoryIteratorPathStack + 'a
    where
        Self: 'a;

    /// The items this iterator will yield.
    type Item;

    /// Provide the next item.
    fn next<'a>(&'a mut self) -> Option<(Self::PathStack<'a>, Self::Item)>;

    /// Compute all paths in this iterator. This returns a regular Iterator since we no longer
    /// need to borrow from self in next.
    fn with_paths(self) -> DirectoryIteratorWithPaths<Self> {
        DirectoryIteratorWithPaths { inner: self }
    }

    /// Compute none of the paths in this iterator. Here again, this is a reglar Iteraotr.
    fn without_paths(self) -> DirectoryIteratorWithoutPaths<Self> {
        DirectoryIteratorWithoutPaths { inner: self }
    }

    /// Only take the paths from this iterator.
    fn paths(self) -> impl Iterator<Item = ForwardRelativePathBuf> {
        self.with_paths().map(|(path, _)| path)
    }

    fn filter_map<F, B>(self, f: F) -> impl DirectoryIterator<Item = B>
    where
        F: FnMut(Self::Item) -> Option<B>,
    {
        struct FilterMap<T, F> {
            inner: T,
            f: F,
        }

        impl<T, F, B> DirectoryIterator for FilterMap<T, F>
        where
            T: DirectoryIterator,
            F: FnMut(T::Item) -> Option<B>,
        {
            type PathStack<'a> = T::PathStack<'a> where Self: 'a;
            type Item = B;

            fn next<'b>(&'b mut self) -> Option<(T::PathStack<'b>, B)> {
                loop {
                    let (path, item) = self.inner.next()?;
                    if let Some(item) = (self.f)(item) {
                        // SAFETY: This is a complication introduced by the lending-iterator pattern
                        // of this trait. The compiler otherwise does not understand that our borrow
                        // of `self.inner` expires in the none case. However, because `item` does
                        // not have a lifetime tied to `'b`, it indeed does.
                        let path =
                            unsafe { mem::transmute::<T::PathStack<'_>, T::PathStack<'b>>(path) };
                        return Some((path, item));
                    }
                }
            }
        }

        FilterMap { inner: self, f }
    }

    /// Only include leaves.
    fn leaves<D, L>(self) -> impl DirectoryIterator<Item = L>
    where
        Self: DirectoryIterator<Item = DirectoryEntry<D, L>>,
    {
        self.filter_map(|entry| entry.leaf())
    }
}

/// The stack of paths for this DirectoryIterator. This must allow iterating over the path
/// components htat make up the DirectoryIterator's current location.
pub trait DirectoryIteratorPathStack {
    fn path(&self) -> impl Iterator<Item = &FileName>;

    fn get(&self) -> ForwardRelativePathBuf {
        let mut path = ForwardRelativePathBuf::with_capacity_for_concat(self.path());
        path.extend(self.path());
        path
    }
}

/// A thin struct that can be used to produce a path on demand.
pub struct DirectoryIteratorPathAccessor<'a, T> {
    pub(super) stack: &'a T,
    pub(super) leaf: Option<&'a FileName>,
}

impl<'a, T> DirectoryIteratorPathStack for DirectoryIteratorPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    fn path(&self) -> impl Iterator<Item = &FileName> {
        self.stack.path().chain(self.leaf)
    }
}

impl<'a, T> DirectoryIteratorPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    pub fn name(&self) -> Option<&'a FileName> {
        self.leaf
    }
}

impl<'a, T> fmt::Display for DirectoryIteratorPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl<'a, T> fmt::Debug for DirectoryIteratorPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DirectoryIteratorPathAccessor({})", self.get())
    }
}

/// Iterate over a DirectoryIterator with the paths.
pub struct DirectoryIteratorWithPaths<T> {
    inner: T,
}

impl<T> Iterator for DirectoryIteratorWithPaths<T>
where
    T: DirectoryIterator,
{
    type Item = (ForwardRelativePathBuf, <T as DirectoryIterator>::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let (path, item) = self.inner.next()?;
        let path = path.get();
        Some((path, item))
    }
}

/// Iterate over a DirectoryIterator without the paths.
pub struct DirectoryIteratorWithoutPaths<T> {
    inner: T,
}

impl<T> Iterator for DirectoryIteratorWithoutPaths<T>
where
    T: DirectoryIterator,
{
    type Item = <T as DirectoryIterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let (_, item) = self.inner.next()?;
        Some(item)
    }
}

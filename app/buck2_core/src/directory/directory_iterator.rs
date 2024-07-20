/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::fs::paths::file_name::FileName;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;

/// A trait shared by iterators on Directories. Unlike a regular Iterator, this returns an accessor
/// to give us the current path in addition to the current item (which borrows from the iterator
/// itself, which is why this cannot be an iterator).
pub trait DirectoryIterator {
    /// The way this iterator will report its current path.
    type PathStack: DirectoryIteratorPathStack;

    /// The items this iterator will yield.
    type Item;

    /// Provide the next item.
    fn next<'a>(
        &'a mut self,
    ) -> Option<(
        DirectoryIteratorPathAccessor<'a, Self::PathStack>,
        Self::Item,
    )>;

    /// Compute all paths in this iterator. This returns a regular Iterator since we no longer
    /// need to borrow from self in next.
    fn with_paths(self) -> DirectoryIteratorWithPaths<Self>
    where
        Self: Sized,
    {
        DirectoryIteratorWithPaths { inner: self }
    }

    /// Compute none of the paths in this iterator. Here again, this is a reglar Iteraotr.
    fn without_paths(self) -> DirectoryIteratorWithoutPaths<Self>
    where
        Self: Sized,
    {
        DirectoryIteratorWithoutPaths { inner: self }
    }
}

/// The stack of paths for this DirectoryIterator. This must allow iterating over the path
/// components htat make up the DirectoryIterator's current location.
pub trait DirectoryIteratorPathStack {
    fn path(&self) -> impl Iterator<Item = &FileName>;
}

/// A thin struct that can be used to produce a path on demand.
pub struct DirectoryIteratorPathAccessor<'a, T> {
    pub(super) stack: &'a T,
    pub(super) leaf: Option<&'a FileName>,
}

impl<'a, T> DirectoryIteratorPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    fn path(&self) -> impl Iterator<Item = &FileName> {
        self.stack.path().chain(self.leaf)
    }

    pub fn name(&self) -> Option<&'a FileName> {
        self.leaf
    }

    pub fn get(&self) -> ForwardRelativePathBuf {
        // Evaluate the size of our path.
        let mut size = 0;
        for name in self.path() {
            size += name.as_str().len() + 1
        }

        // Remove extra "/" we accounted for.
        size = size.saturating_sub(1);

        // Produce it.
        let mut path = ForwardRelativePathBuf::with_capacity(size);
        path.extend(self.path());
        path
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

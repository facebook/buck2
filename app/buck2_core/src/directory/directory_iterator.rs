/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::iter::once;

use crate::fs::paths::FileName;
use crate::fs::paths::ForwardRelativePathBuf;

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
    // NOTE: Ideally we'd want this to return an iterator defined as an associated type, but that's
    // annoying to spell out without using type Foo = impl ... This is available behind
    // `type_alias_impl_trait`, but that causes lots of compiler crashes at this time.
    fn for_each_path<'a, F>(&'a self, f: F)
    where
        F: FnMut(&'a FileName);
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
    fn for_each_path<'this, F>(&'this self, mut f: F)
    where
        F: FnMut(&'this FileName),
    {
        self.stack.for_each_path(&mut f);
        if let Some(leaf) = self.leaf {
            f(leaf);
        }
    }

    pub fn name(&self) -> Option<&'a FileName> {
        self.leaf
    }

    pub fn get(&self) -> ForwardRelativePathBuf {
        // Evaluate the size of our path.
        let mut size = 0;
        self.for_each_path(|name| size += name.as_str().len() + 1);

        // Remove extra "/" we accounted for.
        if size > 0 {
            size -= 1;
        }

        // Produce it.
        let mut first = true;
        let mut path = String::with_capacity(size);
        self.for_each_path(|name| {
            if !first {
                path.push('/');
            }
            first = false;
            path.push_str(name.as_str());
        });

        #[cfg(test)]
        {
            assert_eq!(
                size,
                path.len(),
                "reserved the wrong capacity ({}) for {:?}",
                size,
                path
            );
        }

        // Concatenating FileName guarantees we get a ForwardRelativePathBuf.
        ForwardRelativePathBuf::unchecked_new(path)
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

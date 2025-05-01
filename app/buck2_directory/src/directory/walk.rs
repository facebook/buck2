/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;
use std::marker::PhantomData;
use std::vec;

use buck2_core::fs::paths::file_name::FileName;
use either::Either;

use crate::directory::directory_iterator::DirectoryIterator;
use crate::directory::directory_iterator::DirectoryIteratorPathAccessor;
use crate::directory::directory_iterator::DirectoryIteratorPathStack;
use crate::directory::directory_ref::DirectoryRef;
use crate::directory::entry::DirectoryEntry;

pub trait WalkType<'a> {
    type Leaf: 'a;
    type Directory: DirectoryRef<'a, Leaf = Self::Leaf>;
    type Entries: Iterator<
        Item = (
            &'a FileName,
            DirectoryEntry<Self::Directory, &'a Self::Leaf>,
        ),
    >;

    fn directory_entries(directory: Self::Directory) -> Self::Entries;
}

pub(crate) struct WalkFrame<'a, T: WalkType<'a>> {
    name: Option<&'a FileName>,
    entries: T::Entries,
    _phantom: PhantomData<T>,
}

impl<'a, T: WalkType<'a>> WalkFrame<'a, T> {
    pub(crate) fn new(entries: T::Entries) -> Self {
        Self {
            name: None,
            entries,
            _phantom: PhantomData,
        }
    }
}

pub struct Walk<'a, T: WalkType<'a>> {
    pub(crate) stack: Vec<WalkFrame<'a, T>>,
}

impl<'a, T: WalkType<'a>> Walk<'a, T> {
    pub(crate) fn new(directory: T::Directory) -> Self {
        Walk {
            stack: vec![WalkFrame::new(T::directory_entries(directory))],
        }
    }
}

impl<'a, T: WalkType<'a>> DirectoryIteratorPathStack for Walk<'a, T> {
    fn path(&self) -> impl Iterator<Item = &FileName> {
        self.stack.iter().filter_map(|frame| frame.name)
    }
}

impl<'a, T: WalkType<'a>> DirectoryIterator for Walk<'a, T> {
    type PathStack<'b>
        = DirectoryIteratorPathAccessor<'b, Self>
    where
        Self: 'b;
    type Item = DirectoryEntry<T::Directory, &'a T::Leaf>;

    fn next<'b>(&'b mut self) -> Option<(DirectoryIteratorPathAccessor<'b, Self>, Self::Item)> {
        loop {
            let frame = self.stack.last_mut()?;

            if let Some((name, entry)) = frame.entries.next() {
                let leaf_name = match entry {
                    DirectoryEntry::Dir(dir) => {
                        self.stack.push(WalkFrame {
                            name: Some(name),
                            entries: T::Entries::from(T::directory_entries(dir)),
                            _phantom: PhantomData,
                        });
                        None
                    }
                    DirectoryEntry::Leaf(..) => Some(name),
                };

                return Some((
                    DirectoryIteratorPathAccessor {
                        leaf: leaf_name,
                        stack: self,
                    },
                    entry,
                ));
            }

            self.stack.pop();
        }
    }
}

fn entry_walk_impl<'a, T: WalkType<'a>>(
    entry: DirectoryEntry<T::Directory, &'a T::Leaf>,
) -> DirectoryEntryWalk<'a, T::Leaf, Walk<'a, T>> {
    match entry {
        DirectoryEntry::Dir(d) => DirectoryEntryWalk::Dir {
            inner: Walk::<T>::new(d),
        },
        DirectoryEntry::Leaf(d) => DirectoryEntryWalk::Leaf { entry: Some(d) },
    }
}

pub type UnorderedDirectoryWalk<'a, D> = Walk<'a, UnorderedDirectoryWalkType<'a, D>>;
pub type OrderedDirectoryWalk<'a, D> = Walk<'a, OrderedDirectoryWalkType<'a, D>>;

pub fn unordered_entry_walk<'a, D: DirectoryRef<'a>>(
    entry: DirectoryEntry<D, &'a D::Leaf>,
) -> DirectoryEntryWalk<'a, D::Leaf, UnorderedDirectoryWalk<'a, D>> {
    entry_walk_impl::<UnorderedDirectoryWalkType<D>>(entry)
}

pub fn ordered_entry_walk<'a, D: DirectoryRef<'a>>(
    entry: DirectoryEntry<D, &'a D::Leaf>,
) -> DirectoryEntryWalk<'a, D::Leaf, OrderedDirectoryWalk<'a, D>> {
    entry_walk_impl::<OrderedDirectoryWalkType<D>>(entry)
}

pub enum DirectoryEntryWalk<'a, L, I> {
    Dir { inner: I },
    Leaf { entry: Option<&'a L> },
}

impl<'a, D, L, I> DirectoryIterator for DirectoryEntryWalk<'a, L, I>
where
    I: DirectoryIterator<Item = DirectoryEntry<D, &'a L>>,
    D: 'a,
{
    type PathStack<'b>
        = DirectoryEntryWalkPathAccessor<<I as DirectoryIterator>::PathStack<'b>>
    where
        Self: 'b;

    type Item = DirectoryEntry<D, &'a L>;

    fn next<'this>(
        &'this mut self,
    ) -> Option<(
        DirectoryEntryWalkPathAccessor<<I as DirectoryIterator>::PathStack<'this>>,
        DirectoryEntry<D, &'a L>,
    )> {
        match self {
            DirectoryEntryWalk::Dir { inner } => {
                let (accessor, item) = inner.next()?;
                Some((
                    DirectoryEntryWalkPathAccessor {
                        inner: Some(accessor),
                    },
                    item,
                ))
            }
            DirectoryEntryWalk::Leaf { entry } => {
                let entry = entry.take()?;
                Some((
                    DirectoryEntryWalkPathAccessor { inner: None },
                    DirectoryEntry::Leaf(entry),
                ))
            }
        }
    }
}

#[derive(Debug)]
pub struct DirectoryEntryWalkPathAccessor<T> {
    inner: Option<T>,
}

impl<T> DirectoryIteratorPathStack for DirectoryEntryWalkPathAccessor<T>
where
    T: DirectoryIteratorPathStack,
{
    fn path(&self) -> impl Iterator<Item = &FileName> {
        match &self.inner {
            Some(inner) => Either::Left(inner.path()),
            None => Either::Right(iter::empty()),
        }
    }
}

pub struct UnorderedDirectoryWalkType<'a, D: DirectoryRef<'a>>(PhantomData<&'a D>);
pub struct OrderedDirectoryWalkType<'a, D: DirectoryRef<'a>>(PhantomData<&'a D>);

impl<'a, D: DirectoryRef<'a>> WalkType<'a> for UnorderedDirectoryWalkType<'a, D> {
    type Leaf = D::Leaf;
    type Directory = D;
    type Entries = D::Entries;

    fn directory_entries(directory: Self::Directory) -> Self::Entries {
        directory.entries()
    }
}

impl<'a, D: DirectoryRef<'a>> WalkType<'a> for OrderedDirectoryWalkType<'a, D> {
    type Leaf = D::Leaf;
    type Directory = D;
    type Entries = vec::IntoIter<(&'a FileName, DirectoryEntry<D, &'a D::Leaf>)>;

    fn directory_entries(directory: Self::Directory) -> Self::Entries {
        let mut entries = Vec::from_iter(directory.entries());
        entries.sort_by_key(|(name, _)| *name);
        entries.into_iter()
    }
}

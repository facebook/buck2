/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use derivative::Derivative;

use super::Directory;
use super::DirectoryEntries;
use super::DirectoryEntry;
use super::DirectoryIterator;
use super::DirectoryIteratorPathAccessor;
use super::DirectoryIteratorPathStack;
use super::FingerprintedDirectory;
use super::FingerprintedDirectoryEntries;
use super::FingerprintedOrderedDirectoryEntries;
use super::OrderedDirectoryEntries;
use crate::fs::paths::file_name::FileName;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;

pub trait WalkType<'a> {
    type Leaf: 'a;
    type Directory: Copy + 'a;
    type UnorderedEntries;
    type Entries: From<Self::UnorderedEntries>
        + Iterator<
            Item = (
                &'a FileName,
                DirectoryEntry<Self::Directory, &'a Self::Leaf>,
            ),
        >;

    fn directory_entries(directory: Self::Directory) -> Self::UnorderedEntries;
}

pub(crate) struct WalkFrame<'a, T: WalkType<'a>> {
    name: Option<&'a FileName>,
    entries: T::Entries,
    _phantom: PhantomData<T>,
}

impl<'a, T: WalkType<'a>> WalkFrame<'a, T> {
    pub(crate) fn new(entries: T::UnorderedEntries) -> Self {
        Self {
            name: None,
            entries: T::Entries::from(entries),
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
    type PathStack = Self;
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

pub type FingerprintedUnorderedDirectoryWalk<'a, L, H> =
    Walk<'a, FingerprintedUnorderedDirectoryWalkType<'a, L, H>>;
pub type UnorderedDirectoryWalk<'a, L, H> = Walk<'a, UnorderedDirectoryWalkType<'a, L, H>>;
pub type FingerprintedOrderedDirectoryWalk<'a, L, H> =
    Walk<'a, FingerprintedOrderedDirectoryWalkType<'a, L, H>>;
pub type OrderedDirectoryWalk<'a, L, H> = Walk<'a, OrderedDirectoryWalkType<'a, L, H>>;

pub fn fingerprinted_unordered_entry_walk<'a, L, H>(
    entry: DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>,
) -> DirectoryEntryWalk<'a, L, FingerprintedUnorderedDirectoryWalk<'a, L, H>> {
    entry_walk_impl::<FingerprintedUnorderedDirectoryWalkType<L, H>>(entry)
}

pub fn unordered_entry_walk<'a, L, H>(
    entry: DirectoryEntry<&'a dyn Directory<L, H>, &'a L>,
) -> DirectoryEntryWalk<'a, L, UnorderedDirectoryWalk<'a, L, H>> {
    entry_walk_impl::<UnorderedDirectoryWalkType<L, H>>(entry)
}

pub fn fingerprinted_ordered_entry_walk<'a, L, H>(
    entry: DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>,
) -> DirectoryEntryWalk<'a, L, FingerprintedOrderedDirectoryWalk<'a, L, H>> {
    entry_walk_impl::<FingerprintedOrderedDirectoryWalkType<L, H>>(entry)
}

pub fn ordered_entry_walk<'a, L, H>(
    entry: DirectoryEntry<&'a dyn Directory<L, H>, &'a L>,
) -> DirectoryEntryWalk<'a, L, OrderedDirectoryWalk<'a, L, H>> {
    entry_walk_impl::<OrderedDirectoryWalkType<L, H>>(entry)
}

pub enum DirectoryEntryWalk<'a, L, I> {
    Dir { inner: I },
    Leaf { entry: Option<&'a L> },
}

impl<'a, D, L, I> DirectoryEntryWalk<'a, L, I>
where
    I: DirectoryIterator<Item = DirectoryEntry<D, &'a L>>,
    D: 'a,
{
    pub fn next<'this>(
        &'this mut self,
    ) -> Option<(
        DirectoryEntryWalkPathAccessor<'this, <I as DirectoryIterator>::PathStack>,
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

#[derive(Derivative)]
#[derivative(Debug(bound = "T: DirectoryIteratorPathStack"))]
pub struct DirectoryEntryWalkPathAccessor<'a, T> {
    inner: Option<DirectoryIteratorPathAccessor<'a, T>>,
}

impl<'a, T> DirectoryEntryWalkPathAccessor<'a, T>
where
    T: DirectoryIteratorPathStack,
{
    pub fn get(&self) -> ForwardRelativePathBuf {
        match self.inner.as_ref() {
            Some(i) => i.get(),
            None => ForwardRelativePathBuf::unchecked_new("".to_owned()),
        }
    }
}

pub struct FingerprintedUnorderedDirectoryWalkType<'a, L, H>(PhantomData<&'a (L, H)>);
pub struct UnorderedDirectoryWalkType<'a, L, H>(PhantomData<&'a (L, H)>);
pub struct FingerprintedOrderedDirectoryWalkType<'a, L, H>(PhantomData<&'a (L, H)>);
pub struct OrderedDirectoryWalkType<'a, L, H>(PhantomData<&'a (L, H)>);

impl<'a, L, H> WalkType<'a> for FingerprintedUnorderedDirectoryWalkType<'a, L, H> {
    type Leaf = L;
    type Directory = &'a dyn FingerprintedDirectory<L, H>;
    type UnorderedEntries = FingerprintedDirectoryEntries<'a, L, H>;
    type Entries = FingerprintedDirectoryEntries<'a, L, H>;

    fn directory_entries(directory: Self::Directory) -> Self::UnorderedEntries {
        directory.fingerprinted_entries()
    }
}

impl<'a, L, H> WalkType<'a> for UnorderedDirectoryWalkType<'a, L, H> {
    type Leaf = L;
    type Directory = &'a dyn Directory<L, H>;
    type UnorderedEntries = DirectoryEntries<'a, L, H>;
    type Entries = DirectoryEntries<'a, L, H>;

    fn directory_entries(directory: Self::Directory) -> Self::UnorderedEntries {
        directory.entries()
    }
}

impl<'a, L, H> WalkType<'a> for FingerprintedOrderedDirectoryWalkType<'a, L, H> {
    type Leaf = L;
    type Directory = &'a dyn FingerprintedDirectory<L, H>;
    type UnorderedEntries = FingerprintedDirectoryEntries<'a, L, H>;
    type Entries = FingerprintedOrderedDirectoryEntries<'a, L, H>;

    fn directory_entries(directory: Self::Directory) -> Self::UnorderedEntries {
        directory.fingerprinted_entries()
    }
}

impl<'a, L, H> WalkType<'a> for OrderedDirectoryWalkType<'a, L, H> {
    type Leaf = L;
    type Directory = &'a dyn Directory<L, H>;
    type UnorderedEntries = DirectoryEntries<'a, L, H>;
    type Entries = OrderedDirectoryEntries<'a, L, H>;

    fn directory_entries(directory: Self::Directory) -> Self::UnorderedEntries {
        directory.entries()
    }
}

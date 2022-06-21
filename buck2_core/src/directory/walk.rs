/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

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
use crate::fs::paths::FileName;
use crate::fs::paths::ForwardRelativePathBuf;

macro_rules! impl_directory_walk {
    (
        $walk_ty: ident,
        $entry_walk_fn: ident,
        $dir_ty: ident,
        $entries_ty: ident,
        $entries_method: ident,
        $mod: ident,
    ) => {
        mod $mod {
            use std::fmt;

            use $crate::directory::DirectoryEntry;
            use $crate::directory::DirectoryIterator;
            use $crate::directory::DirectoryIteratorPathAccessor;
            use $crate::directory::DirectoryIteratorPathStack;

            use super::$dir_ty;
            use super::$entries_ty;
            use super::DirectoryEntryWalk;
            use crate::fs::paths::FileName;
            use crate::fs::paths::ForwardRelativePathBuf;

            struct WalkFrame<'a, L, H> {
                name: Option<&'a FileName>,
                entries: $entries_ty<'a, L, H>,
            }

            pub struct $walk_ty<'a, L, H> {
                stack: Vec<WalkFrame<'a, L, H>>,
            }

            impl<'a, L, H> $walk_ty<'a, L, H> {
                pub fn new<D>(root: &'a D) -> Self
                where
                    D: $dir_ty<L, H> + ?Sized,
                {
                    Self {
                        stack: vec![WalkFrame {
                            name: None,
                            entries: $entries_ty::from($dir_ty::$entries_method(root)),
                        }],
                    }
                }
            }

            impl<'a, L, H> DirectoryIterator for $walk_ty<'a, L, H> {
                type PathStack = Self;
                type Item = DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>;

                fn next<'b>(
                    &'b mut self,
                ) -> Option<(DirectoryIteratorPathAccessor<'b, Self>, Self::Item)> {
                    loop {
                        let frame = self.stack.last_mut()?;

                        if let Some((name, entry)) = frame.entries.next() {
                            let leaf_name = match entry {
                                DirectoryEntry::Dir(dir) => {
                                    self.stack.push(WalkFrame {
                                        name: Some(name),
                                        entries: $entries_ty::from($dir_ty::$entries_method(dir)),
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

            impl<'a, L, H> DirectoryIteratorPathStack for $walk_ty<'a, L, H> {
                fn for_each_path<'this, F>(&'this self, mut f: F)
                where
                    F: FnMut(&'this FileName),
                {
                    let it = self.stack.iter().filter_map(|frame| match frame {
                        WalkFrame { name, .. } => name.as_deref(),
                    });

                    for path in it {
                        f(path);
                    }
                }
            }

            pub fn $entry_walk_fn<'a, D, L, H>(
                entry: DirectoryEntry<&'a D, &'a L>,
            ) -> DirectoryEntryWalk<'a, L, $walk_ty<'a, L, H>>
            where
                D: $dir_ty<L, H> + ?Sized,
            {
                match entry {
                    DirectoryEntry::Dir(d) => DirectoryEntryWalk::Dir {
                        inner: $walk_ty::new(d),
                    },
                    DirectoryEntry::Leaf(d) => DirectoryEntryWalk::Leaf { entry: Some(d) },
                }
            }
        }

        pub use $mod::$entry_walk_fn;
        pub use $mod::$walk_ty;
    };
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
            Self::Dir { inner } => {
                let (accessor, item) = inner.next()?;
                Some((
                    DirectoryEntryWalkPathAccessor {
                        inner: Some(accessor),
                    },
                    item,
                ))
            }
            Self::Leaf { entry } => {
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

impl_directory_walk!(
    FingerprintedUnorderedDirectoryWalk,
    fingerprinted_unordered_entry_walk,
    FingerprintedDirectory,
    FingerprintedDirectoryEntries,
    fingerprinted_entries,
    fingerprinted_unordered_directory_walk_impl,
);

impl_directory_walk!(
    UnorderedDirectoryWalk,
    unordered_entry_walk,
    Directory,
    DirectoryEntries,
    entries,
    unordered_directory_walk_impl,
);

impl_directory_walk!(
    FingerprintedOrderedDirectoryWalk,
    fingerprinted_ordered_entry_walk,
    FingerprintedDirectory,
    FingerprintedOrderedDirectoryEntries,
    fingerprinted_entries,
    fingerprinted_ordered_directory_walk_impl,
);

impl_directory_walk!(
    OrderedDirectoryWalk,
    ordered_entry_walk,
    Directory,
    OrderedDirectoryEntries,
    entries,
    ordered_directory_walk_impl,
);

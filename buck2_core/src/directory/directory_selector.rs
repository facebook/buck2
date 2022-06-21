/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::iter::once;

use either::Either;
use gazebo::prelude::*;
use thiserror::Error;

use super::Directory;
use super::DirectoryBuilder;
use super::DirectoryEntries;
use super::DirectoryEntry;
use super::DirectoryIterator;
use super::DirectoryIteratorPathAccessor;
use super::DirectoryIteratorPathStack;
use super::FingerprintedDirectory;
use super::FingerprintedDirectoryEntries;
use super::FingerprintedOrderedDirectoryEntries;
use super::HasDirectoryDigest;
use super::OrderedDirectoryEntries;
use crate::fs::paths::FileName;
use crate::fs::paths::FileNameBuf;
use crate::fs::paths::ForwardRelativePath;
use crate::fs::paths::IntoFileNameBufIterator;

#[derive(Debug, Error)]
pub enum DirectorySearchError<L> {
    #[error("Search traverses a leaf")]
    CannotTraverseLeaf { leaf: L },
}

impl<L> DirectorySearchError<L> {
    pub fn into_leaf(self) -> L {
        let Self::CannotTraverseLeaf { leaf } = self;
        leaf
    }
}

#[derive(Debug, Error)]
pub enum DirectoryFilterError {
    #[error("Filter traverses a leaf")]
    CannotTraverseLeaf,
}

/// A query builder for filtering or search operations on directories. It's a tree of paths and
/// what action we want to take on them.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DirectorySelector {
    /// Traverse only the netries that match this filename.
    Traverse(HashMap<FileNameBuf, DirectorySelector>),
    /// Take this entire tree.
    Take,
}

impl DirectorySelector {
    pub fn empty() -> Self {
        Self::Traverse(Default::default())
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Self::Traverse(d) => d.is_empty(),
            Self::Take => false,
        }
    }

    /// Add a path to this DirectorySelector.
    pub fn select(&mut self, path: impl IntoFileNameBufIterator) {
        let mut path = path.into_iter();
        self.select_inner(path)
    }

    fn select_inner(&mut self, mut path: impl Iterator<Item = FileNameBuf>) {
        let entry = match path.next() {
            Some(e) => e,
            None => {
                *self = Self::Take;
                return;
            }
        };

        match self {
            Self::Traverse(s) => s
                .entry(entry)
                .or_insert_with(DirectorySelector::empty)
                .select_inner(path),
            Self::Take => {}
        }
    }

    /// Filter a DirectoryBuilder by only retaining matching entries.
    pub fn filter<L, H>(&self, dir: &mut DirectoryBuilder<L, H>) -> Result<(), DirectoryFilterError>
    where
        L: Clone,
        H: HasDirectoryDigest,
    {
        let mut res = Ok(());

        match self {
            Self::Traverse(filter) => filter_inner(dir, filter, &mut res),
            Self::Take => {}
        };

        res
    }
}

fn filter_inner<L, H>(
    dir: &mut DirectoryBuilder<L, H>,
    filter: &HashMap<FileNameBuf, DirectorySelector>,
    res: &mut Result<(), DirectoryFilterError>,
) where
    L: Clone,
    H: HasDirectoryDigest,
{
    let dir = dir.as_mut();

    let entries = std::mem::take(dir);

    for (k, mut v) in entries.into_iter() {
        let selector = match filter.get(&k) {
            Some(s) => s,
            None => continue,
        };

        match selector {
            DirectorySelector::Traverse(next_map) => match v {
                DirectoryEntry::Dir(ref mut d) => filter_inner(d, next_map, res),
                DirectoryEntry::Leaf(..) => {
                    *res = Err(DirectoryFilterError::CannotTraverseLeaf);
                }
            },
            DirectorySelector::Take => {}
        };

        dir.insert(k, v);
    }
}

macro_rules! impl_directory_search {
    (
        $search_ty: ident,
        $dir_ty: ident,
        $entries_ty: ident,
        $entries_method: ident,
        $mod: ident,
        $selector_search_method: ident,
    ) => {
        mod $mod {
            use super::*;

            enum SearchFrame<'a, 'b, L, H> {
                /// The search consists of just returning the root.
                ReturnRoot {
                    root: Option<DirectoryEntry<&'b dyn $dir_ty<L, H>, &'b L>>,
                },

                /// Continue the search.
                Search {
                    search: &'a HashMap<FileNameBuf, DirectorySelector>,
                    name: Option<&'b FileName>,
                    entries: $entries_ty<'b, L, H>,
                },
            }

            pub struct $search_ty<'a, 'b, L, H> {
                stack: Vec<SearchFrame<'a, 'b, L, H>>,
            }

            impl<'a, 'b, L, H> $search_ty<'a, 'b, L, H> {
                pub fn new<D>(selector: &'a DirectorySelector, root: &'b D) -> Self
                where
                    D: $dir_ty<L, H>,
                {
                    match selector {
                        DirectorySelector::Traverse(ref search) => Self {
                            stack: vec![SearchFrame::Search {
                                search,
                                name: None,
                                entries: $entries_ty::from(root.$entries_method()),
                            }],
                        },
                        DirectorySelector::Take => Self {
                            stack: vec![SearchFrame::ReturnRoot {
                                root: Some(DirectoryEntry::Dir(root as &dyn $dir_ty<L, H>)),
                            }],
                        },
                    }
                }
            }

            impl<'a, 'b, L, H> DirectoryIterator for $search_ty<'a, 'b, L, H> {
                type PathStack = Self;
                type Item = Result<
                    DirectoryEntry<&'b dyn $dir_ty<L, H>, &'b L>,
                    DirectorySearchError<&'b L>,
                >;

                fn next<'c>(
                    &'c mut self,
                ) -> Option<(DirectoryIteratorPathAccessor<'c, Self>, Self::Item)> {
                    loop {
                        let frame = self.stack.last_mut()?;

                        match frame {
                            SearchFrame::ReturnRoot { root } => {
                                let root = root.take()?;
                                return Some((
                                    DirectoryIteratorPathAccessor {
                                        leaf: None,
                                        stack: self,
                                    },
                                    Ok(root),
                                ));
                            }
                            SearchFrame::Search {
                                search, entries, ..
                            } => {
                                if let Some((name, entry)) = entries.next() {
                                    let search = search.get(name);

                                    match search {
                                        Some(DirectorySelector::Traverse(t)) => match entry {
                                            // Traverse into this directory ... assuming it's a directory :)
                                            DirectoryEntry::Dir(d) => {
                                                self.stack.push(SearchFrame::Search {
                                                    name: Some(name),
                                                    search: t,
                                                    entries: $entries_ty::from(d.$entries_method()),
                                                });
                                                continue;
                                            }
                                            DirectoryEntry::Leaf(leaf) => {
                                                return Some((
                                                    DirectoryIteratorPathAccessor {
                                                        leaf: Some(name),
                                                        stack: self,
                                                    },
                                                    Err(DirectorySearchError::CannotTraverseLeaf {
                                                        leaf,
                                                    }),
                                                ));
                                            }
                                        },
                                        Some(DirectorySelector::Take) => {
                                            // Return the entry. Do not traverse further.
                                            return Some((
                                                DirectoryIteratorPathAccessor {
                                                    leaf: Some(name),
                                                    stack: self,
                                                },
                                                Ok(entry),
                                            ));
                                        }
                                        None => {
                                            // Ignore this node entirely.
                                            continue;
                                        }
                                    }
                                }
                            }
                        };

                        // We've exhausted this iterator. Go back to the previous stack frame.
                        self.stack.pop();
                    }
                }
            }

            impl<'a, 'b, L, H> DirectoryIteratorPathStack for $search_ty<'a, 'b, L, H> {
                fn for_each_path<'this, F>(&'this self, mut f: F)
                where
                    F: FnMut(&'this FileName),
                {
                    let it = self.stack.iter().filter_map(|frame| match frame {
                        SearchFrame::ReturnRoot { .. } => None,
                        SearchFrame::Search { name, .. } => name.as_deref(),
                    });

                    for path in it {
                        f(path);
                    }
                }
            }
        }

        impl DirectorySelector {
            pub fn $selector_search_method<'a, 'b, L, H, D: $dir_ty<L, H>>(
                &'a self,
                dir: &'b D,
            ) -> $search_ty<'a, 'b, L, H> {
                $search_ty::new(self, dir)
            }
        }

        pub use $mod::$search_ty;
    };
}

impl_directory_search!(
    UnorderedDirectorySearch,
    Directory,
    DirectoryEntries,
    entries,
    unordered_directory_search,
    unordered_search,
);

impl_directory_search!(
    OrderedDirectorySearch,
    Directory,
    OrderedDirectoryEntries,
    entries,
    ordered_directory_search,
    ordered_search,
);

impl_directory_search!(
    FingerprintedUnorderedDirectorySearch,
    FingerprintedDirectory,
    FingerprintedDirectoryEntries,
    fingerprinted_entries,
    fingerprinted_unordered_directory_search,
    fingerprinted_unordered_search,
);

impl_directory_search!(
    FingerprintedOrderedDirectorySearch,
    FingerprintedDirectory,
    FingerprintedOrderedDirectoryEntries,
    fingerprinted_entries,
    fingerprinted_ordered_directory_search,
    fingerprinted_ordered_search,
);

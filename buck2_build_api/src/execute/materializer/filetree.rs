/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Data structure akin to a map, but where the key is a sequence.
//!
//! Restrictions:
//! - Storing both a key A and a key B which is a prefix of A is not possible.
//!
//! Special operations:
//! - Using key A to search for a value at a key B when B is a prefix of A.
//!
//! This is useful when artifacts are directories and we need to query the map
//! to figure out which artifact a path belongs to. E.g. we have an artifact at
//! "foo/bar", and we need to find out which artifact "foo/bar/c" belongs to.

use std::{
    borrow::Borrow,
    collections::{
        hash_map::{Entry, OccupiedEntry, VacantEntry},
        HashMap,
    },
    hash::Hash,
};

use buck2_core::fs::paths::FileNameBuf;

pub type FileTree<V> = DataTree<FileNameBuf, V>;

/// Tree that stores data in the leaves. Think of the key as the path to the
/// leaf containing the value. The data/value is of type `V`, and each edge
/// is of type `K` (making the key to a value a sequence of `K`).
///
/// # Example
/// ```
/// use buck2_core::fs::paths::{FileNameBuf, ForwardRelativePathBuf};
/// use buck2_build_api::execute::materializer::filetree::DataTree;
///
/// let path = ForwardRelativePathBuf::unchecked_new("foo/bar".to_owned());
/// let contents = "contents_of_foobar".to_owned();
///
/// let mut file_path_to_contents: DataTree<FileNameBuf, String> = DataTree::new();
/// file_path_to_contents.insert(
///     path.iter().map(|f| f.to_owned()),
///     contents.clone(),
/// );
///
/// assert_eq!(file_path_to_contents.get(path.iter()), Some(&contents));
/// ```
#[derive(Debug)]
pub enum DataTree<K, V> {
    /// Stores data of type `V` with key of type `Iterator<Item = K>`.
    Tree(HashMap<K, DataTree<K, V>>),
    Data(V),
}

impl<K: Eq + Hash, V> DataTree<K, V> {
    pub fn new() -> Self {
        Self::Tree(HashMap::new())
    }

    /// Gets the value at `key` or one of its prefixes, and returns it.
    ///
    /// When a value is found and [`Some`] is returned, it's guaranteed that
    /// only enough of `key` to find the returned value was consumed.
    /// E.g. if `key` is (A, B, C, D) and there's a value present at (A, B),
    /// then after this method returns (C, D) can still be consumed from `key`.
    ///
    /// There are no guarantees on how much is consumed from `key` when
    /// [`None`] is returned.
    pub fn prefix_get<'a, I, Q>(&self, key: &mut I) -> Option<&V>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        if let Self::Data(data) = self {
            // return early so we don't consume from key_iter unnecessarily
            return Some(data);
        }
        let mut node = self;
        for k in key {
            node = match node.children().unwrap().get(k) {
                None => return None,
                Some(node) => match node {
                    Self::Tree(_) => node,
                    Self::Data(data) => return Some(data),
                },
            };
        }
        None
    }

    /// Similar to `prefix_get`, but takes and returns `&mut`.
    pub fn prefix_get_mut<'a, I, Q>(&mut self, key: &mut I) -> Option<&mut V>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        if let Self::Data(data) = self {
            // return early so we don't consume from key_iter unnecessarily
            return Some(data);
        }
        let mut node = self;
        for k in key {
            node = match node.children_mut().unwrap().get_mut(k) {
                None => return None,
                Some(node) => match node {
                    Self::Tree(_) => node,
                    Self::Data(data) => return Some(data),
                },
            };
        }
        None
    }

    /// Returns a reference to the value corresponding to the `key`.
    pub fn get<'a, I, Q>(&self, key: I) -> Option<&V>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        let mut node = self;
        for k in key {
            node = match node {
                Self::Tree(children) => match children.get(k) {
                    None => return None,
                    Some(node) => node,
                },
                Self::Data(_) => return None,
            };
        }
        match node {
            Self::Tree(_) => None,
            Self::Data(data) => Some(data),
        }
    }

    /// Returns a reference to the value corresponding to the `key`.
    /// NOTE: This will create the path to this entry.
    pub fn entry<'a, I>(&'a mut self, mut key: I) -> Option<DataTreeEntry<'a, K, V>>
    where
        I: Iterator<Item = K>,
    {
        let mut entry = match self {
            Self::Tree(t) => t.entry(key.next()?),
            Self::Data(..) => return None,
        };

        for k in key {
            entry = match entry {
                Entry::Occupied(entry) => match entry.into_mut() {
                    Self::Tree(t) => t.entry(k),
                    Self::Data(..) => return None,
                },
                Entry::Vacant(v) => match v.insert(Self::new()) {
                    Self::Tree(t) => t.entry(k),
                    Self::Data(..) => unreachable!("We just inserted a Tree here"),
                },
            }
        }

        DataTreeEntry::new(entry)
    }

    /// Returns a reference to the value corresponding to the `key`.
    pub fn get_mut<'a, I, Q>(&mut self, key: I) -> Option<&mut V>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        let mut node = self;
        for k in key {
            node = match node {
                Self::Tree(children) => match children.get_mut(k) {
                    None => return None,
                    Some(node) => node,
                },
                Self::Data(_) => return None,
            };
        }
        match node {
            Self::Tree(_) => None,
            Self::Data(data) => Some(data),
        }
    }

    /// Inserts a key-value pair into the tree.
    ///
    /// If there is already a key in the map that is a prefix of the inserted
    /// key, that key is removed.
    ///
    /// If the inserted key is a prefix of one or more keys in the map, all
    /// those keys are removed.
    pub fn insert<I: Iterator<Item = K>>(&mut self, mut key: I, value: V) {
        if let Some(k) = key.next() {
            if matches!(self, Self::Data(_)) {
                *self = Self::new();
            }
            let child = match self.children_mut().unwrap().entry(k) {
                Entry::Occupied(e) => e.into_mut(),
                Entry::Vacant(e) => e.insert(Self::new()),
            };
            child.insert(key, value);
        } else {
            *self = Self::Data(value);
        }
    }

    /// Removes a key from the tree, returning the value at the key if the key
    /// was previously in the tree.
    pub fn remove<'a, I, Q>(&mut self, mut key: I) -> Option<V>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        if matches!(self, Self::Data(_)) {
            return match std::mem::replace(self, Self::new()) {
                Self::Data(data) => Some(data),
                _ => unreachable!(),
            };
        }
        if let Some(k) = key.next() {
            if let Some(node) = self.children_mut().unwrap().get_mut(k) {
                let data = node.remove(key);
                let remove_node = match node {
                    Self::Tree(children) => children.is_empty(),
                    Self::Data(_) => true,
                };
                if remove_node {
                    self.children_mut().unwrap().remove(k);
                }
                return data;
            }
        }
        None
    }

    fn children(&self) -> Option<&HashMap<K, DataTree<K, V>>> {
        match self {
            Self::Tree(children) => Some(children),
            Self::Data(_) => None,
        }
    }

    fn children_mut(&mut self) -> Option<&mut HashMap<K, DataTree<K, V>>> {
        match self {
            Self::Tree(children) => Some(children),
            Self::Data(_) => None,
        }
    }
}

/// A Entry of the DataTree that is checked to contain Data, not Tree.
#[derive(Debug)]
pub enum DataTreeEntry<'a, K, V> {
    Vacant(VacantDataTreeEntry<'a, K, V>),
    Occupied(OccupiedDataTreeEntry<'a, K, V>),
}

#[derive(Debug)]
pub struct VacantDataTreeEntry<'a, K, V> {
    inner: VacantEntry<'a, K, DataTree<K, V>>,
}

#[derive(Debug)]
pub struct OccupiedDataTreeEntry<'a, K, V> {
    inner: OccupiedEntry<'a, K, DataTree<K, V>>,
}

impl<'a, K, V> DataTreeEntry<'a, K, V> {
    fn new(inner: Entry<'a, K, DataTree<K, V>>) -> Option<Self> {
        match inner {
            Entry::Vacant(inner) => Some(Self::Vacant(VacantDataTreeEntry { inner })),
            Entry::Occupied(inner) => match inner.get() {
                DataTree::Data(..) => Some(Self::Occupied(OccupiedDataTreeEntry { inner })),
                DataTree::Tree(..) => None,
            },
        }
    }
}

impl<'a, K, V> VacantDataTreeEntry<'a, K, V> {
    pub fn insert(self, value: V) {
        self.inner.insert(DataTree::Data(value));
    }
}

impl<'a, K, V> OccupiedDataTreeEntry<'a, K, V> {
    pub fn get(&self) -> &V {
        match self.inner.get() {
            DataTree::Data(ref d) => d,
            DataTree::Tree(..) => unreachable!("This is checked at construction time"),
        }
    }

    pub fn get_mut(&mut self) -> &mut V {
        match self.inner.get_mut() {
            DataTree::Data(ref mut d) => d,
            DataTree::Tree(..) => unreachable!("This is checked at construction time"),
        }
    }

    pub fn remove(self) -> V {
        match self.inner.remove() {
            DataTree::Data(d) => d,
            DataTree::Tree(..) => unreachable!("This is checked at construction time"),
        }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_entry() {
        let mut t = DataTree::<u64, &str>::new();
        t.insert(vec![1, 2, 3].into_iter(), "123");

        assert_matches!(t.entry(vec![1].into_iter()), None);

        assert_matches!(t.entry(vec![1, 2, 3, 4].into_iter()), None);

        assert_matches!(t.entry(vec![1, 2, 3].into_iter()), Some(e) => {
            assert_matches!(e, DataTreeEntry::Occupied(e) => {
                assert_eq!(*e.get(), "123");
            });
        });

        assert_matches!(t.entry(vec![2, 3, 4].into_iter()), Some(e) => {
            assert_matches!(e, DataTreeEntry::Vacant(v) => {
                v.insert("234");
            });
        });
    }
}

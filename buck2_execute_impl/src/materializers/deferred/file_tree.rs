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

use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::hash::Hash;

use buck2_core::fs::paths::FileNameBuf;

pub type FileTree<V> = DataTree<FileNameBuf, V>;

/// Tree that stores data in the leaves. Think of the key as the path to the
/// leaf containing the value. The data/value is of type `V`, and each edge
/// is of type `K` (making the key to a value a sequence of `K`).
/// TODO(scottcao): This trie is not implemented properly. It should be merged into the directory trie
/// we have at buck2_core/src/directory.
#[derive(Debug)]
pub enum DataTree<K, V> {
    /// Stores data of type `V` with key of type `Iterator<Item = K>`.
    Tree(HashMap<K, DataTree<K, V>>),
    Data(V),
}

impl<K: 'static + Eq + Hash + Clone, V: 'static> DataTree<K, V> {
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

    /// Removes a key from the tree, returning the subtree at the key if the key
    /// was previously in the tree.
    /// If the prefix of `key` exists as a leaf on the tree, that leaf is removed
    /// and the value of that leaf is returned. We guarantee that only enough of
    /// `key` to find the returned value was consumed.
    pub fn remove<'a, I, Q>(&mut self, mut key: I) -> Option<DataTree<K, V>>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        if matches!(self, Self::Data(_)) {
            return Some(std::mem::replace(self, Self::new()));
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
                data
            } else {
                None
            }
        } else {
            Some(std::mem::replace(self, Self::new()))
        }
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

    /// Returns an iterator over DataTree<K, V>, where each element is a tuple consisting of iterator
    /// of K (as a VecDeque) and V.
    #[allow(dead_code)]
    pub fn iter(&self) -> DataTreeIterator<'_, K, V> {
        fn iter_helper<K, V>(
            tree: &DataTree<K, V>,
            depth: usize, // Used to allocate VecDeque capacity
        ) -> Box<dyn Iterator<Item = (VecDeque<&K>, &V)> + '_> {
            match tree {
                DataTree::Tree(children) => box children.iter().flat_map(move |(k, data_tree)| {
                    iter_helper(data_tree, depth + 1)
                        .into_iter()
                        .map(move |(mut key_iter, v)| {
                            // Need to return a VecDeque and not Vec because keys are pushed from the front
                            key_iter.push_front(k);
                            (key_iter, v)
                        })
                }),
                DataTree::Data(v) => box std::iter::once((VecDeque::with_capacity(depth), v)),
            }
        }

        iter_helper(self, 0)
    }
}

pub type DataTreeIterator<'a, K, V> = Box<dyn Iterator<Item = (VecDeque<&'a K>, &'a V)> + 'a>;
pub type DataTreeIntoIterator<K, V> = Box<dyn Iterator<Item = (VecDeque<K>, V)>>;

impl<K: 'static + Eq + Hash + Clone, V: 'static> IntoIterator for DataTree<K, V> {
    type Item = (VecDeque<K>, V);
    type IntoIter = DataTreeIntoIterator<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        fn iter_helper<K: 'static + Clone, V: 'static>(
            tree: DataTree<K, V>,
            depth: usize, // Used to allocate VecDeque capacity
        ) -> Box<dyn Iterator<Item = (VecDeque<K>, V)> + 'static> {
            match tree {
                DataTree::Tree(children) => {
                    box children.into_iter().flat_map(move |(k, data_tree)| {
                        iter_helper(data_tree, depth + 1).into_iter().map(
                            move |(mut key_iter, v)| {
                                // Need to return a VecDeque and not Vec because keys are pushed from the front
                                key_iter.push_front(k.clone());
                                (key_iter, v)
                            },
                        )
                    })
                }
                DataTree::Data(v) => box std::iter::once((VecDeque::with_capacity(depth), v)),
            }
        }

        iter_helper(self, 0)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use assert_matches::assert_matches;
    use buck2_core::fs::paths::ForwardRelativePathBuf;

    use super::*;

    #[test]
    fn test_iter() {
        let expected: BTreeMap<Vec<i32>, String> = [
            (vec![1, 2, 3], "123".to_owned()),
            (vec![1, 2, 4], "124".to_owned()),
            (vec![1, 2, 5, 6], "1256".to_owned()),
            (vec![1, 3, 4], "134".to_owned()),
            (vec![1, 3, 5], "135".to_owned()),
        ]
        .into_iter()
        .collect();

        let mut tree = DataTree::<i32, String>::new();
        for (k, v) in expected.iter() {
            tree.insert(k.clone().into_iter(), v.clone());
        }
        let actual = tree
            .iter()
            .into_iter()
            .map(|(k, v)| (k.into_iter().copied().collect::<Vec<_>>(), v.to_owned()))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_into_iter() {
        let expected: BTreeMap<Vec<i32>, String> = [
            (vec![1, 2, 3], "123".to_owned()),
            (vec![1, 2, 4], "124".to_owned()),
            (vec![1, 2, 5, 6], "1256".to_owned()),
            (vec![1, 3, 4], "134".to_owned()),
            (vec![1, 3, 5], "135".to_owned()),
        ]
        .into_iter()
        .collect();

        let mut tree = DataTree::<i32, String>::new();
        for (k, v) in expected.iter() {
            tree.insert(k.clone().into_iter(), v.clone());
        }
        let actual = tree
            .into_iter()
            .map(|(k, v)| (k.into_iter().collect::<Vec<_>>(), v))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_get() {
        let path = ForwardRelativePathBuf::unchecked_new("foo/bar".to_owned());
        let contents = "contents_of_foobar".to_owned();

        let mut file_path_to_contents: DataTree<FileNameBuf, String> = DataTree::new();
        file_path_to_contents.insert(path.iter().map(|f| f.to_owned()), contents.clone());

        assert_eq!(
            file_path_to_contents
                .prefix_get_mut(&mut path.iter())
                .as_deref(),
            Some(&contents)
        );
    }

    #[test]
    fn test_prefix_remove() {
        // We need remove any artifacts at leaves when we invalidate
        // and declare artifacts at a subdirectory of that leaf path.
        let mut tree = DataTree::<i32, String>::new();
        tree.insert(vec![1, 2, 3].into_iter(), "123".to_owned());
        let key = vec![1, 2, 3, 4];
        let mut key_iter = key.iter();
        let removed = tree.remove(&mut key_iter);
        assert_matches!(removed, Some(DataTree::Data(val)) if val == *"123");
        // Test that only enough of key_iter to find the returned value was consumed.
        assert_eq!(key_iter.next(), Some(&4));
        assert_eq!(key_iter.next(), None);

        // Check tree is empty
        assert_eq!(tree.iter().next(), None);
    }

    #[test]
    fn test_suffix_remove() {
        let mut tree = DataTree::<i32, String>::new();
        tree.insert(vec![1, 2, 3].into_iter(), "123".to_owned());
        tree.remove(vec![1, 2].iter());

        assert_eq!(tree.iter().next(), None);
    }
}

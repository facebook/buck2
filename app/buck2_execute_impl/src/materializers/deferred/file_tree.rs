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
use std::collections::hash_map::IntoIter;
use std::collections::hash_map::Iter;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;

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

    /// Get the subtree at `key`, if the entry exists and is a tree.
    /// Return an error if there is an entry but it is not a tree.
    pub fn get_subtree<'a, I, Q>(&self, key: &mut I) -> anyhow::Result<Option<&HashMap<K, Self>>>
    where
        K: 'a + Borrow<Q>,
        Q: 'a + Hash + Eq + ?Sized,
        I: Iterator<Item = &'a Q>,
    {
        let mut entries = match self {
            Self::Tree(ref t) => t,
            Self::Data(..) => {
                return Err(anyhow::anyhow!("Data found where tree expected"));
            }
        };

        for k in key {
            let node = match entries.get(k) {
                None => return Ok(None),
                Some(v) => v,
            };

            entries = match node {
                Self::Tree(ref t) => t,
                Self::Data(..) => {
                    return Err(anyhow::anyhow!("Data found where tree expected"));
                }
            };
        }

        Ok(Some(entries))
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

    pub fn children(&self) -> Option<&HashMap<K, DataTree<K, V>>> {
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

    /// Returns an iterator over DataTree<K, V>.
    pub fn iter<T>(&self) -> DataTreeIterator<'_, K, V, T> {
        match self {
            Self::Tree(t) => DataTreeIterator::Stack(vec![(None, t.iter())], PhantomData),
            Self::Data(v) => DataTreeIterator::Entry(Some(v)),
        }
    }

    /// Take ownership of the values in DataTree<K, V> and iterate.
    pub fn into_iter<T>(self) -> DataTreeIntoIterator<K, V, T> {
        match self {
            Self::Tree(t) => DataTreeIntoIterator::Stack(vec![(None, t.into_iter())], PhantomData),
            Self::Data(v) => DataTreeIntoIterator::Entry(Some(v)),
        }
    }
}

struct NoopCollector;

impl<'a> FromIterator<&'a FileNameBuf> for NoopCollector {
    fn from_iter<I>(_iter: I) -> Self
    where
        I: IntoIterator<Item = &'a FileNameBuf>,
    {
        NoopCollector
    }
}

impl<V: 'static> FileTree<V> {
    pub fn iter_with_paths(&self) -> impl Iterator<Item = (ForwardRelativePathBuf, &V)> {
        self.iter::<Option<ForwardRelativePathBuf>>()
            .map(|(k, v)| (k.unwrap_or_else(ForwardRelativePathBuf::empty), v))
    }

    pub fn iter_without_paths(&self) -> impl Iterator<Item = &V> {
        self.iter::<NoopCollector>().map(|(NoopCollector, v)| v)
    }

    pub fn into_iter_with_paths(self) -> impl Iterator<Item = (ForwardRelativePathBuf, V)> {
        self.into_iter::<Option<ForwardRelativePathBuf>>()
            .map(|(k, v)| (k.unwrap_or_else(ForwardRelativePathBuf::empty), v))
    }

    #[allow(unused)]
    pub fn into_iter_without_paths(self) -> impl Iterator<Item = V> {
        self.into_iter::<NoopCollector>()
            .map(|(NoopCollector, v)| v)
    }
}

pub enum DataTreeIterator<'a, K, V, T> {
    Stack(
        Vec<(Option<&'a K>, Iter<'a, K, DataTree<K, V>>)>,
        PhantomData<T>,
    ),
    Entry(Option<&'a V>),
}

impl<'a, K, V, T> Iterator for DataTreeIterator<'a, K, V, T>
where
    T: for<'x> FromIterator<&'x K>,
{
    type Item = (T, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Stack(ref mut stack, _) => loop {
                let (_, ref mut last) = stack.last_mut()?;

                match last.next() {
                    Some((k, DataTree::Tree(t))) => {
                        stack.push((Some(k), t.iter()));
                    }
                    Some((k, DataTree::Data(v))) => {
                        let it = stack
                            .iter()
                            .filter_map(|(k, _)| k.as_deref())
                            .chain(std::iter::once(k));
                        return Some((it.collect(), v));
                    }
                    None => {
                        stack.pop();
                    }
                }
            },
            Self::Entry(v) => v.take().map(|v| (std::iter::empty().collect(), v)),
        }
    }
}

pub enum DataTreeIntoIterator<K, V, T> {
    Stack(
        Vec<(Option<K>, IntoIter<K, DataTree<K, V>>)>,
        PhantomData<T>,
    ),
    Entry(Option<V>),
}

impl<K, V, T> Iterator for DataTreeIntoIterator<K, V, T>
where
    T: for<'x> FromIterator<&'x K>,
{
    type Item = (T, V);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Stack(ref mut stack, _) => loop {
                let (_, ref mut last) = stack.last_mut()?;

                match last.next() {
                    Some((k, DataTree::Tree(t))) => {
                        stack.push((Some(k), t.into_iter()));
                    }
                    Some((k, DataTree::Data(v))) => {
                        let it = stack
                            .iter()
                            .filter_map(|(k, _)| k.as_ref())
                            .chain(std::iter::once(&k));
                        return Some((it.collect(), v));
                    }
                    None => {
                        stack.pop();
                    }
                }
            },
            Self::Entry(v) => v.take().map(|v| (std::iter::empty().collect(), v)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use assert_matches::assert_matches;

    use super::*;

    #[derive(Debug)]
    struct CopyCollector<T>(Vec<T>);

    impl<'a, T> FromIterator<&'a T> for CopyCollector<T>
    where
        T: Copy + 'static,
    {
        fn from_iter<I>(iter: I) -> Self
        where
            I: IntoIterator<Item = &'a T>,
        {
            Self(iter.into_iter().copied().collect())
        }
    }

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
            .iter::<CopyCollector<_>>()
            .map(|(k, v)| (k.0.into_iter().collect::<Vec<_>>(), v.to_owned()))
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
            .into_iter::<CopyCollector<_>>()
            .map(|(k, v)| (k.0.into_iter().collect::<Vec<_>>(), v))
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
        assert_matches!(tree.iter::<CopyCollector<_>>().next(), None);
    }

    #[test]
    fn test_suffix_remove() {
        let mut tree = DataTree::<i32, String>::new();
        tree.insert(vec![1, 2, 3].into_iter(), "123".to_owned());
        tree.remove(vec![1, 2].iter());

        assert_matches!(tree.iter::<CopyCollector<_>>().next(), None);
    }

    #[test]
    fn test_remove_empty_dirs() {
        let mut tree = DataTree::<i32, String>::new();
        tree.insert(vec![1, 2, 3].into_iter(), "123".to_owned());
        tree.remove(vec![1, 2, 3].iter());
        assert_matches!(tree, DataTree::Tree(m) if m.is_empty());
    }
}

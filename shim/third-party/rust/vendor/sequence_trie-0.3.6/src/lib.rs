//! Sequence Trie - a trie-like data-structure for storing sequences of values.
//!
//! See the `SequenceTrie` type for documentation.

#[cfg(not(feature = "btreemap"))]
use std::hash::Hash;
#[cfg(not(feature = "btreemap"))]
use std::collections::hash_map::{self, HashMap};

#[cfg(feature = "btreemap")]
use std::collections::{btree_map, BTreeMap};

use std::hash::BuildHasher;
use std::collections::hash_map::RandomState;

use std::iter::IntoIterator;
use std::default::Default;
use std::borrow::{Borrow, ToOwned};
use std::mem;
use std::marker::PhantomData;

#[cfg(feature = "serde")]
use serde::{Serialize, Deserialize};

#[cfg(feature = "serde")]
#[macro_use]
extern crate serde;

#[cfg(test)]
mod tests;
#[cfg(all(test, feature = "serde"))]
mod serde_tests;

/// A `SequenceTrie` is recursively defined as a value and a map containing child Tries.
///
/// Typically, Tries are used to store strings, which can be thought of as lists of `char`s.
/// Generalising this to any key type, a Trie is a data structure storing values for keys
/// which are themselves lists. Let the parts of such a list-key be called "key fragments".
/// In our representation of a Trie, `K` denotes the type of the key fragments.
///
/// The nesting of child Tries creates a tree structure which can be traversed by mapping
/// key fragments onto nodes. The structure is similar to a k-ary tree, except that the children
/// are stored in `HashMap`s, and there is no bound on the number of children a single node may
/// have (effectively k = ∞). In a `SequenceTrie` with `char` key fragments, the key
/// `['a', 'b', 'c']` might correspond to something like this:
///
/// ```text
/// SequenceTrie {
///     value: Some(0),
///     children: 'a' => SequenceTrie {
///         value: Some(1),
///         children: 'b' => SequenceTrie {
///             value: None,
///             children: 'c' => SequenceTrie {
///                 value: Some(3),
///                 children: Nil
///             }
///         }
///     }
/// }
/// ```
///
/// Values are stored optionally at each node because inserting a value for a list-key only inserts
/// a value for the last fragment of the key. The intermediate prefix nodes are created with value
/// `None` if they do not exist already.
///
/// The above `SequenceTrie` could be created using the following sequence of operations:
///
/// ```
/// # use sequence_trie::SequenceTrie;
/// let mut trie: SequenceTrie<char, i32> = SequenceTrie::new();
/// trie.insert(&['a', 'b', 'c'], 3);
/// trie.insert(&[], 0);
/// trie.insert(&['a'], 1);
/// ```
///
/// The order of insertion is never important.
///
/// One interesting thing about Tries is that every key is a *descendant* of the root, which itself
/// has no key fragment. Although this is a rather trivial observation, it means that every key
/// corresponds to a non-empty sequence of prefix nodes in the tree. This observation is the
/// motivation for the `get_prefix_nodes` method, which returns the nodes corresponding to the longest
/// prefix of a given key.
///
/// The empty list key, `[]`, always corresponds to the root node of the Trie.
///
/// # The Sequence Trie Invariant
/// All leaf nodes have non-trivial values (not equal to `None`). This invariant is maintained by
/// the insertion and removal methods and can be relied upon.
#[derive(Debug, Clone)]
#[cfg(not(feature = "btreemap"))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound(serialize = "K: Serialize, V: Serialize")))]
#[cfg_attr(feature = "serde",
    serde(bound(deserialize = "K: Deserialize<'de>, V: Deserialize<'de>")))]
pub struct SequenceTrie<K, V, S = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default,
{
    /// Node value.
    value: Option<V>,

    /// Node children as a hashmap keyed by key fragments.
    children: HashMap<K, SequenceTrie<K, V, S>, S>,
}

#[derive(Debug, Clone)]
#[cfg(feature = "btreemap")]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(bound(serialize = "K: Serialize, V: Serialize")))]
#[cfg_attr(feature = "serde",
    serde(bound(deserialize = "K: Deserialize<'de>, V: Deserialize<'de>")))]
pub struct SequenceTrie<K, V, S = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default,
{
    /// Node value.
    value: Option<V>,

    /// Node children as a btreemap keyed by key fragments.
    children: BTreeMap<K, SequenceTrie<K, V, S>>,

    /// Fake hasher for compatibility.
    #[cfg_attr(feature = "serde", serde(skip))]
    _phantom: PhantomData<S>,
}

/// Aggregate trait for types which can be used to key a `SequenceTrie`.
///
/// This trait is automatically implemented for all types implementing
/// the supertraits.
#[cfg(not(feature = "btreemap"))]
pub trait TrieKey: Eq + Hash {}
#[cfg(not(feature = "btreemap"))]
impl<K> TrieKey for K where K: Eq + Hash + ?Sized {}

#[cfg(feature = "btreemap")]
pub trait TrieKey: Ord {}
#[cfg(feature = "btreemap")]
impl<K> TrieKey for K where K: Ord + ?Sized {}

#[cfg(not(feature = "btreemap"))]
impl<K, V> SequenceTrie<K, V>
    where K: TrieKey,
{
    /// Creates a new `SequenceTrie` node with no value and an empty child map.
    pub fn new() -> SequenceTrie<K, V> {
        SequenceTrie::with_hasher(RandomState::new())
    }
}

#[cfg(not(feature = "btreemap"))]
impl<K, V, S> SequenceTrie<K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default + Clone,
{
    pub fn with_hasher(hash_builder: S) -> SequenceTrie<K, V, S> {
        SequenceTrie {
            value: None,
            children: HashMap::with_hasher(hash_builder),
        }
    }
}

#[cfg(feature = "btreemap")]
impl<K, V> SequenceTrie<K, V>
    where K: TrieKey,
{
    /// Creates a new `SequenceTrie` node with no value and an empty child map.
    pub fn new() -> SequenceTrie<K, V> {
        Self::new_generic()
    }
}

#[cfg(feature = "btreemap")]
impl<K, V, S> SequenceTrie<K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default + Clone,
{
    /// Creates a new `SequenceTrie` node with no value and an empty child map.
    pub fn new_generic() -> SequenceTrie<K, V, S> {
        SequenceTrie {
            value: None,
            children: BTreeMap::new(),
            _phantom: PhantomData,
        }
    }
}

impl<K, V, S> SequenceTrie<K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default + Clone,
{
    /// Retrieve the value stored at this node.
    pub fn value(&self) -> Option<&V> {
        self.value.as_ref()
    }

    /// Retrieve a mutable reference to the value stored at this node.
    pub fn value_mut(&mut self) -> Option<&mut V> {
        self.value.as_mut()
    }

    /// Checks if this node is empty.
    ///
    /// A node is considered empty when it has no value and no children.
    pub fn is_empty(&self) -> bool {
        self.is_leaf() && self.value.is_none()
    }

    /// Checks if this node has no descendants.
    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    /// Inserts a key and value into the SequenceTrie.
    ///
    /// Returns `None` if the key did not already correspond to a value, otherwise the old value is
    /// returned.
    pub fn insert<'key, I, Q: 'key + ?Sized>(&mut self, key: I, value: V) -> Option<V>
        where I: IntoIterator<Item = &'key Q>,
              Q: ToOwned<Owned = K>,
              K: Borrow<Q>
    {
        self.insert_owned(key.into_iter().map(ToOwned::to_owned), value)
    }

    /// Version of `insert` that takes an owned sequence of key fragments.
    ///
    /// This function is used internally by `insert`.
    #[cfg(not(feature = "btreemap"))]
    pub fn insert_owned<I>(&mut self, key: I, value: V) -> Option<V>
        where I: IntoIterator<Item = K>
    {
        let key_node = key.into_iter().fold(self, |current_node, fragment| {
            let hash_builder = current_node.children.hasher().clone();
            current_node.children
                .entry(fragment)
                .or_insert_with(|| Self::with_hasher(hash_builder))
        });

        mem::replace(&mut key_node.value, Some(value))
    }

    #[cfg(feature = "btreemap")]
    pub fn insert_owned<I>(&mut self, key: I, value: V) -> Option<V>
        where I: IntoIterator<Item = K>
    {
        let key_node = key.into_iter().fold(self, |current_node, fragment| {
            current_node.children
                .entry(fragment)
                .or_insert_with(Self::new_generic)
        });

        mem::replace(&mut key_node.value, Some(value))
    }

    /// Finds a reference to a key's value, if it has one.
    pub fn get<'key, I, Q: ?Sized>(&self, key: I) -> Option<&V>
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.get_node(key).and_then(|node| node.value.as_ref())
    }

    /// Finds a reference to a key's node, if it has one.
    pub fn get_node<'key, I, Q: ?Sized>(&self, key: I) -> Option<&SequenceTrie<K, V, S>>
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        let mut current_node = self;

        for fragment in key {
            match current_node.children.get(fragment) {
                Some(node) => current_node = node,
                None => return None,
            }
        }

        Some(current_node)
    }

    /// Finds a mutable reference to a key's value, if it has one.
    pub fn get_mut<'key, I, Q: ?Sized>(&mut self, key: I) -> Option<&mut V>
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.get_node_mut(key).and_then(|node| node.value.as_mut())
    }

    /// Finds a mutable reference to a key's node, if it has one.
    pub fn get_node_mut<'key, I, Q: ?Sized>(&mut self, key: I) -> Option<&mut SequenceTrie<K, V, S>>
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        let mut current_node = Some(self);

        for fragment in key {
            match current_node.and_then(|node| node.children.get_mut(fragment)) {
                Some(node) => current_node = Some(node),
                None => return None,
            }
        }

        current_node
    }

    /// Finds the longest prefix of nodes which match the given key.
    pub fn get_prefix_nodes<'key, I, Q: ?Sized>(&self, key: I) -> Vec<&SequenceTrie<K, V, S>>
        where I: 'key + IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.prefix_iter(key).collect()
    }

    /// Finds the value of the nearest ancestor with a non-empty value, if one exists.
    ///
    /// If all ancestors have empty (`None`) values, `None` is returned.
    pub fn get_ancestor<'key, I, Q: ?Sized>(&self, key: I) -> Option<&V>
        where I: 'key + IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.get_ancestor_node(key).and_then(|node| node.value.as_ref())
    }

    /// Finds the nearest ancestor with a non-empty value, if one exists.
    ///
    /// If all ancestors have empty (`None`) values, `None` is returned.
    pub fn get_ancestor_node<'key, I, Q: ?Sized>(&self, key: I) -> Option<&SequenceTrie<K, V, S>>
        where I: 'key + IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.prefix_iter(key)
            .filter(|node| node.value.is_some())
            .last()
    }

    /// Removes the node corresponding to the given key.
    ///
    /// This operation is like the reverse of `insert` in that
    /// it also deletes extraneous nodes on the path from the root.
    ///
    /// If the key node has children, its value is set to `None` and no further
    /// action is taken. If the key node is a leaf, then it and its ancestors with
    /// empty values and no other children are deleted. Deletion proceeds up the tree
    /// from the key node until a node with a non-empty value or children is reached.
    ///
    /// If the key doesn't match a node in the Trie, no action is taken.
    pub fn remove<'key, I, Q: ?Sized>(&mut self, key: I)
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        self.remove_recursive(key);
    }

    /// Recursive remove method that uses the call stack to safely and
    /// efficiently remove a node and its extraneous ancestors.
    ///
    /// Return `true` if the node should be deleted.
    ///
    /// See `remove` above.
    fn remove_recursive<'key, I, Q: ?Sized>(&mut self, key: I) -> bool
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        let mut fragments = key.into_iter();
        match fragments.next() {
            // Base case: Leaf node, no key left to recurse on.
            None => {
                self.value = None;
            }

            // Recursive case: Inner node, delete children.
            Some(fragment) => {
                let delete_child = match self.children.get_mut(fragment) {
                    Some(child) => child.remove_recursive(fragments),
                    None => false,
                };

                if delete_child {
                    self.children.remove(fragment);
                }
                // NB: If the child isn't found, false will be returned.
                // The `self` node is either a leaf, with a non-trivial value, or an
                // inner node (with children).
            }
        }

        // If the node is childless and valueless, mark it for deletion.
        self.is_empty()
    }

    /// Recursively apply a function to every node in the trie.
    ///
    /// Nodes are visited "bottom-up" (children before parent).
    /// If `f` returns a value, it replaces the value at that node.
    /// Otherwise, the node's value remains unchanged.
    pub fn map<F>(&mut self, f: F) where F: Fn(&Self) -> Option<V> {
        self.map_rec(&f)
    }

    /// Internal version of map that takes the closure by reference.
    fn map_rec<F>(&mut self, f: &F) where F: Fn(&Self) -> Option<V> {
        for child in self.children.values_mut() {
            child.map_rec(f);
        }

        if let Some(v) = f(&*self) {
            self.value = Some(v);
        }
    }

    /// Returns an iterator over all the key-value pairs in the collection.
    pub fn iter(&self) -> Iter<K, V, S> {
        Iter {
            root: self,
            root_visited: false,
            key: vec![],
            stack: vec![],
        }
    }

    /// Returns an iterator over all the keys in the trie.
    pub fn keys(&self) -> Keys<K, V, S> {
        Keys { inner: self.iter() }
    }

    /// Returns an iterator over all the values stored in the trie.
    pub fn values(&self) -> Values<K, V, S> {
        Values { inner: self.iter() }
    }

    /// Returns an iterator over the longest prefix of nodes which match the given key.
    pub fn prefix_iter<'trie, 'key, I, Q: ?Sized>(&'trie self,
                                                  key: I)
                                                  -> PrefixIter<'trie, 'key, K, V, Q, I::IntoIter, S>
        where I: IntoIterator<Item = &'key Q>,
              K: Borrow<Q>,
              Q: TrieKey + 'key
    {
        PrefixIter {
            next_node: Some(self),
            fragments: key.into_iter(),
            _phantom: PhantomData,
        }
    }

    /// Return all the children of this node, in an arbitrary order.
    pub fn children(&self) -> Vec<&Self> {
        self.children.values().collect()
    }

    /// Children of this node, with their associated keys in arbitrary order.
    pub fn children_with_keys<'a>(&'a self) -> Vec<(&'a K, &'a Self)> {
        self.children.iter().collect()
    }
}

/// Iterator over the keys and values of a `SequenceTrie`.
pub struct Iter<'a, K: 'a, V: 'a, S: 'a = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default
{
    root: &'a SequenceTrie<K, V, S>,
    root_visited: bool,
    key: Vec<&'a K>,
    stack: Vec<StackItem<'a, K, V, S>>,
}

/// Vector of key fragment references and values, yielded during iteration.
pub type KeyValuePair<'a, K, V> = (Vec<&'a K>, &'a V);

/// Iterator over the keys of a `SequenceTrie`.
pub struct Keys<'a, K: 'a, V: 'a, S: 'a = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default
{
    inner: Iter<'a, K, V, S>,
}

/// Iterator over the values of a `SequenceTrie`.
pub struct Values<'a, K: 'a, V: 'a, S: 'a = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default
{
    inner: Iter<'a, K, V, S>,
}

/// Information stored on the iteration stack whilst exploring.
struct StackItem<'a, K: 'a, V: 'a, S: 'a = RandomState>
    where K: TrieKey,
          S: BuildHasher + Default
{
    #[cfg(not(feature = "btreemap"))]
    child_iter: hash_map::Iter<'a, K, SequenceTrie<K, V, S>>,
    #[cfg(feature = "btreemap")]
    child_iter: btree_map::Iter<'a, K, SequenceTrie<K, V, S>>,
}

/// Delayed action type for iteration stack manipulation.
enum IterAction<'a, K: 'a, V: 'a, S: 'a>
    where K: TrieKey,
          S: BuildHasher + Default
{
    Push(&'a K, &'a SequenceTrie<K, V, S>),
    Pop,
}

impl<'a, K, V, S> Iterator for Iter<'a, K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default
{
    type Item = KeyValuePair<'a, K, V>;

    fn next(&mut self) -> Option<KeyValuePair<'a, K, V>> {
        use IterAction::*;

        // Special handling for the root.
        if !self.root_visited {
            self.root_visited = true;
            self.stack.push(StackItem { child_iter: self.root.children.iter() });
            if let Some(ref root_val) = self.root.value {
                return Some((vec![], root_val));
            }
        }

        loop {
            let action = match self.stack.last_mut() {
                Some(stack_top) => {
                    match stack_top.child_iter.next() {
                        Some((fragment, child_node)) => Push(fragment, child_node),
                        None => Pop,
                    }
                }
                None => return None,
            };

            match action {
                Push(fragment, node) => {
                    self.stack.push(StackItem { child_iter: node.children.iter() });
                    self.key.push(fragment);
                    if let Some(ref value) = node.value {
                        return Some((self.key.clone(), value));
                    }
                }
                Pop => {
                    self.key.pop();
                    self.stack.pop();
                }
            }
        }
    }
}

impl<'a, K, V, S> Iterator for Keys<'a, K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default,
{
    type Item = Vec<&'a K>;

    fn next(&mut self) -> Option<Vec<&'a K>> {
        self.inner.next().map(|(k, _)| k)
    }
}

impl<'a, K, V, S> Iterator for Values<'a, K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default
{
    type Item = &'a V;

    fn next(&mut self) -> Option<&'a V> {
        self.inner.next().map(|(_, v)| v)
    }
}

impl<K, V, S> PartialEq for SequenceTrie<K, V, S>
    where K: TrieKey,
          V: PartialEq,
          S: BuildHasher + Default
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.children == other.children
    }
}

impl<K, V, S> Eq for SequenceTrie<K, V, S>
    where K: TrieKey,
          V: Eq,
          S: BuildHasher + Default
{}

impl<K, V, S> Default for SequenceTrie<K, V, S>
    where K: TrieKey,
          S: BuildHasher + Default + Clone
{
    fn default() -> Self {
        #[cfg(not(feature = "btreemap"))]
        { SequenceTrie::with_hasher(S::default()) }
        #[cfg(feature = "btreemap")]
        { SequenceTrie::new_generic() }
    }
}

/// Iterator over the longest prefix of nodes which matches a key.
pub struct PrefixIter<'trie, 'key, K, V, Q: ?Sized, I, S = RandomState>
    where K: 'trie + TrieKey,
          V: 'trie,
          I: 'key + Iterator<Item = &'key Q>,
          K: Borrow<Q>,
          Q: TrieKey + 'key,
          S: 'trie + BuildHasher + Default
{
    next_node: Option<&'trie SequenceTrie<K, V, S>>,
    fragments: I,
    _phantom: PhantomData<&'key I>,
}

impl<'trie, 'key, K, V, Q: ?Sized, I, S> Iterator for PrefixIter<'trie, 'key, K, V, Q, I, S>
    where K: 'trie + TrieKey,
          V: 'trie,
          I: 'key + Iterator<Item = &'key Q>,
          K: Borrow<Q>,
          Q: TrieKey + 'key,
          S: BuildHasher + Default
{
    type Item = &'trie SequenceTrie<K, V, S>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current_node) = self.next_node.take() {
            if let Some(fragment) = self.fragments.next() {
                self.next_node = current_node.children.get(fragment)
            }

            return Some(current_node);
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let lower = if self.next_node.is_some() {
            1
        } else {
            0
        };

        (lower, self.fragments.size_hint().1)
    }
}

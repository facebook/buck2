//! The raw form of a `Map`, allowing untyped access.
//!
//! All relevant details are in the `RawMap` struct.

use std::any::TypeId;
use std::borrow::Borrow;
use std::collections::hash_map::{self, HashMap};
use std::hash::Hash;
use std::hash::{Hasher, BuildHasherDefault};
#[cfg(test)]
use std::mem;
use std::ops::{Index, IndexMut};
use std::ptr;

use any::{Any, UncheckedAnyExt};

#[derive(Default)]
struct TypeIdHasher {
    value: u64,
}

impl Hasher for TypeIdHasher {
    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        // This expects to receive one and exactly one 64-bit value
        debug_assert!(bytes.len() == 8);
        unsafe {
            ptr::copy_nonoverlapping(&bytes[0] as *const u8 as *const u64, &mut self.value, 1)
        }
    }

    #[inline]
    fn finish(&self) -> u64 { self.value }
}

#[test]
fn type_id_hasher() {
    fn verify_hashing_with(type_id: TypeId) {
        let mut hasher = TypeIdHasher::default();
        type_id.hash(&mut hasher);
        assert_eq!(hasher.finish(), unsafe { mem::transmute::<TypeId, u64>(type_id) });
    }
    // Pick a variety of types, just to demonstrate it’s all sane. Normal, zero-sized, unsized, &c.
    verify_hashing_with(TypeId::of::<usize>());
    verify_hashing_with(TypeId::of::<()>());
    verify_hashing_with(TypeId::of::<str>());
    verify_hashing_with(TypeId::of::<&str>());
    verify_hashing_with(TypeId::of::<Vec<u8>>());
}

/// The raw, underlying form of a `Map`.
///
/// At its essence, this is a wrapper around `HashMap<TypeId, Box<Any>>`, with the portions that
/// would be memory-unsafe removed or marked unsafe. Normal people are expected to use the safe
/// `Map` interface instead, but there is the occasional use for this such as iteration over the
/// contents of an `Map`. However, because you will then be dealing with `Any` trait objects, it
/// doesn’t tend to be so very useful. Still, if you need it, it’s here.
#[derive(Debug)]
pub struct RawMap<A: ?Sized + UncheckedAnyExt = Any> {
    inner: HashMap<TypeId, Box<A>, BuildHasherDefault<TypeIdHasher>>,
}

// #[derive(Clone)] would want A to implement Clone, but in reality it’s only Box<A> that can.
impl<A: ?Sized + UncheckedAnyExt> Clone for RawMap<A> where Box<A>: Clone {
    #[inline]
    fn clone(&self) -> RawMap<A> {
        RawMap {
            inner: self.inner.clone(),
        }
    }
}

impl<A: ?Sized + UncheckedAnyExt> Default for RawMap<A> {
    #[inline]
    fn default() -> RawMap<A> {
        RawMap::new()
    }
}

impl_common_methods! {
    field: RawMap.inner;
    new() => HashMap::with_hasher(Default::default());
    with_capacity(capacity) => HashMap::with_capacity_and_hasher(capacity, Default::default());
}

/// `RawMap` iterator.
#[derive(Clone)]
pub struct Iter<'a, A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::Iter<'a, TypeId, Box<A>>,
}
impl<'a, A: ?Sized + UncheckedAnyExt> Iterator for Iter<'a, A> {
    type Item = &'a A;
    #[inline] fn next(&mut self) -> Option<&'a A> { self.inner.next().map(|x| &**x.1) }
    #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.inner.size_hint() }
}
impl<'a, A: ?Sized + UncheckedAnyExt> ExactSizeIterator for Iter<'a, A> {
    #[inline] fn len(&self) -> usize { self.inner.len() }
}

/// `RawMap` mutable iterator.
pub struct IterMut<'a, A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::IterMut<'a, TypeId, Box<A>>,
}
impl<'a, A: ?Sized + UncheckedAnyExt> Iterator for IterMut<'a, A> {
    type Item = &'a mut A;
    #[inline] fn next(&mut self) -> Option<&'a mut A> { self.inner.next().map(|x| &mut **x.1) }
    #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.inner.size_hint() }
}
impl<'a, A: ?Sized + UncheckedAnyExt> ExactSizeIterator for IterMut<'a, A> {
    #[inline] fn len(&self) -> usize { self.inner.len() }
}

/// `RawMap` move iterator.
pub struct IntoIter<A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::IntoIter<TypeId, Box<A>>,
}
impl<A: ?Sized + UncheckedAnyExt> Iterator for IntoIter<A> {
    type Item = Box<A>;
    #[inline] fn next(&mut self) -> Option<Box<A>> { self.inner.next().map(|x| x.1) }
    #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.inner.size_hint() }
}
impl<A: ?Sized + UncheckedAnyExt> ExactSizeIterator for IntoIter<A> {
    #[inline] fn len(&self) -> usize { self.inner.len() }
}

/// `RawMap` drain iterator.
pub struct Drain<'a, A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::Drain<'a, TypeId, Box<A>>,
}
impl<'a, A: ?Sized + UncheckedAnyExt> Iterator for Drain<'a, A> {
    type Item = Box<A>;
    #[inline] fn next(&mut self) -> Option<Box<A>> { self.inner.next().map(|x| x.1) }
    #[inline] fn size_hint(&self) -> (usize, Option<usize>) { self.inner.size_hint() }
}
impl<'a, A: ?Sized + UncheckedAnyExt> ExactSizeIterator for Drain<'a, A> {
    #[inline] fn len(&self) -> usize { self.inner.len() }
}

impl<A: ?Sized + UncheckedAnyExt> RawMap<A> {
    /// An iterator visiting all entries in arbitrary order.
    ///
    /// Iterator element type is `&Any`.
    #[inline]
    pub fn iter(&self) -> Iter<A> {
        Iter {
            inner: self.inner.iter(),
        }
    }

    /// An iterator visiting all entries in arbitrary order.
    ///
    /// Iterator element type is `&mut Any`.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<A> {
        IterMut {
            inner: self.inner.iter_mut(),
        }
    }

    /// Clears the map, returning all items as an iterator.
    ///
    /// Iterator element type is `Box<Any>`.
    ///
    /// Keeps the allocated memory for reuse.
    #[inline]
    pub fn drain(&mut self) -> Drain<A> {
        Drain {
            inner: self.inner.drain(),
        }
    }

    /// Gets the entry for the given type in the collection for in-place manipulation.
    #[inline]
    pub fn entry(&mut self, key: TypeId) -> Entry<A> {
        match self.inner.entry(key) {
            hash_map::Entry::Occupied(e) => Entry::Occupied(OccupiedEntry {
                inner: e,
            }),
            hash_map::Entry::Vacant(e) => Entry::Vacant(VacantEntry {
                inner: e,
            }),
        }
    }

    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but `Hash` and `Eq` on the borrowed
    /// form *must* match those for the key type.
    #[inline]
    pub fn get<Q: ?Sized>(&self, k: &Q) -> Option<&A>
    where TypeId: Borrow<Q>, Q: Hash + Eq {
        self.inner.get(k).map(|x| &**x)
    }

    /// Returns true if the map contains a value for the specified key.
    ///
    /// The key may be any borrowed form of the map's key type, but `Hash` and `Eq` on the borrowed
    /// form *must* match those for the key type.
    #[inline]
    pub fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
    where TypeId: Borrow<Q>, Q: Hash + Eq {
        self.inner.contains_key(k)
    }

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but `Hash` and `Eq` on the borrowed
    /// form *must* match those for the key type.
    #[inline]
    pub fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut A>
    where TypeId: Borrow<Q>, Q: Hash + Eq {
        self.inner.get_mut(k).map(|x| &mut **x)
    }

    /// Inserts a key-value pair from the map. If the key already had a value present in the map,
    /// that value is returned. Otherwise, None is returned.
    ///
    /// It is the caller’s responsibility to ensure that the key corresponds with the type ID of
    /// the value. If they do not, memory safety may be violated.
    #[inline]
    pub unsafe fn insert(&mut self, key: TypeId, value: Box<A>) -> Option<Box<A>> {
        self.inner.insert(key, value)
    }

    /// Removes a key from the map, returning the value at the key if the key was previously in the
    /// map.
    ///
    /// The key may be any borrowed form of the map's key type, but `Hash` and `Eq` on the borrowed
    /// form *must* match those for the key type.
    #[inline]
    pub fn remove<Q: ?Sized>(&mut self, k: &Q) -> Option<Box<A>>
    where TypeId: Borrow<Q>, Q: Hash + Eq {
        self.inner.remove(k)
    }

}

impl<A: ?Sized + UncheckedAnyExt, Q> Index<Q> for RawMap<A> where TypeId: Borrow<Q>, Q: Eq + Hash {
    type Output = A;

    #[inline]
    fn index(&self, index: Q) -> &A {
        self.get(&index).expect("no entry found for key")
    }
}

impl<A: ?Sized + UncheckedAnyExt, Q> IndexMut<Q> for RawMap<A> where TypeId: Borrow<Q>, Q: Eq + Hash {
    #[inline]
    fn index_mut(&mut self, index: Q) -> &mut A {
        self.get_mut(&index).expect("no entry found for key")
    }
}

impl<A: ?Sized + UncheckedAnyExt> IntoIterator for RawMap<A> {
    type Item = Box<A>;
    type IntoIter = IntoIter<A>;

    #[inline]
    fn into_iter(self) -> IntoIter<A> {
        IntoIter {
            inner: self.inner.into_iter(),
        }
    }
}

/// A view into a single occupied location in a `RawMap`.
pub struct OccupiedEntry<'a, A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::OccupiedEntry<'a, TypeId, Box<A>>,
}

/// A view into a single empty location in a `RawMap`.
pub struct VacantEntry<'a, A: ?Sized + UncheckedAnyExt> {
    inner: hash_map::VacantEntry<'a, TypeId, Box<A>>,
}

/// A view into a single location in a `RawMap`, which may be vacant or occupied.
pub enum Entry<'a, A: ?Sized + UncheckedAnyExt> {
    /// An occupied Entry
    Occupied(OccupiedEntry<'a, A>),
    /// A vacant Entry
    Vacant(VacantEntry<'a, A>),
}

impl<'a, A: ?Sized + UncheckedAnyExt> Entry<'a, A> {
    /// Ensures a value is in the entry by inserting the default if empty, and returns
    /// a mutable reference to the value in the entry.
    ///
    /// It is the caller’s responsibility to ensure that the key of the entry corresponds with
    /// the type ID of `value`. If they do not, memory safety may be violated.
    #[inline]
    pub unsafe fn or_insert(self, default: Box<A>) -> &'a mut A {
        match self {
            Entry::Occupied(inner) => inner.into_mut(),
            Entry::Vacant(inner) => inner.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the default function if empty,
    /// and returns a mutable reference to the value in the entry.
    ///
    /// It is the caller’s responsibility to ensure that the key of the entry corresponds with
    /// the type ID of `value`. If they do not, memory safety may be violated.
    #[inline]
    pub unsafe fn or_insert_with<F: FnOnce() -> Box<A>>(self, default: F) -> &'a mut A {
        match self {
            Entry::Occupied(inner) => inner.into_mut(),
            Entry::Vacant(inner) => inner.insert(default()),
        }
    }
}

impl<'a, A: ?Sized + UncheckedAnyExt> OccupiedEntry<'a, A> {
    /// Gets a reference to the value in the entry.
    #[inline]
    pub fn get(&self) -> &A {
        &**self.inner.get() 
    }

    /// Gets a mutable reference to the value in the entry.
    #[inline]
    pub fn get_mut(&mut self) -> &mut A {
        &mut **self.inner.get_mut()
    }

    /// Converts the OccupiedEntry into a mutable reference to the value in the entry
    /// with a lifetime bound to the collection itself.
    #[inline]
    pub fn into_mut(self) -> &'a mut A {
        &mut **self.inner.into_mut()
    }

    /// Sets the value of the entry, and returns the entry's old value.
    ///
    /// It is the caller’s responsibility to ensure that the key of the entry corresponds with
    /// the type ID of `value`. If they do not, memory safety may be violated.
    #[inline]
    pub unsafe fn insert(&mut self, value: Box<A>) -> Box<A> {
        self.inner.insert(value)
    }

    /// Takes the value out of the entry, and returns it.
    #[inline]
    pub fn remove(self) -> Box<A> {
        self.inner.remove()
    }
}

impl<'a, A: ?Sized + UncheckedAnyExt> VacantEntry<'a, A> {
    /// Sets the value of the entry with the VacantEntry's key,
    /// and returns a mutable reference to it
    ///
    /// It is the caller’s responsibility to ensure that the key of the entry corresponds with
    /// the type ID of `value`. If they do not, memory safety may be violated.
    #[inline]
    pub unsafe fn insert(self, value: Box<A>) -> &'a mut A {
        &mut **self.inner.insert(value)
    }
}

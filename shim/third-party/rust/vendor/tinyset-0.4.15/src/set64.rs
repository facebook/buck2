// Copyright 2017-2019 David Roundy
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A set that is compact in size.

#[cfg(test)]
use proptest::prelude::*;
use std;
use std::marker::PhantomData;

/// This describes a type which can be stored in 64 bits without loss.
/// It is defined for all signed and unsigned integer types, as well
/// as `char`.  In each case, we store sets consisting exclusively of
/// "small" integers efficiently.
pub trait Fits64: Copy {
    /// Convert back *from* a u64.  This is unsafe, since it is only
    /// infallible (and lossless) if the `u64` originally came from
    /// type `Self`.
    unsafe fn from_u64(x: u64) -> Self;
    /// Convert to a `u64`.  This should be infallible.
    fn to_u64(self) -> u64;
}
/// A utility function that is useful for testing your Fits64
/// implentation.
pub fn test_fits64<T: Fits64 + Eq + std::fmt::Debug>(x: T) {
    let x64 = x.to_u64();
    let y = unsafe { T::from_u64(x64) };
    let y64 = y.to_u64();
    assert_eq!(x, y);
    assert_eq!(x64, y64);
}

macro_rules! define_fits {
    ($ty: ty, $test_name: ident) => {
        impl Fits64 for $ty {
            #[inline]
            unsafe fn from_u64(x: u64) -> Self {
                x as $ty
            }
            #[inline]
            fn to_u64(self) -> u64 {
                self as u64
            }
        }
        #[cfg(test)]
        proptest! {
            #[test]
            fn $test_name(x: $ty) {
                test_fits64(x);
            }
        }
    };
}
define_fits!(u64, fits_u64);
define_fits!(u32, fits_u32);
define_fits!(u16, fits_u16);
define_fits!(u8, fits_u8);
define_fits!(usize, fits_usize);
impl Fits64 for char {
    #[inline]
    unsafe fn from_u64(x: u64) -> Self {
        std::char::from_u32(x as u32).unwrap()
    }
    #[inline]
    fn to_u64(self) -> u64 {
        self as u64
    }
}
// The following constant allows me to check whether it is faster to
// handle negative numbers with an if expression or by doing bit
// manipulation more directly.
const USE_BRANCHES: bool = false;
macro_rules! define_ifits {
    ($ty: ty, $uty: ty, $test_name: ident) => {
        impl Fits64 for $ty {
            #[inline]
            unsafe fn from_u64(x: u64) -> Self {
                let pos_val = (x >> 1) as $ty;
                let neg_val = !(x >> 1) as $ty;
                if USE_BRANCHES {
                    if x & 1 == 1 {
                        neg_val
                    } else {
                        pos_val
                    }
                } else {
                    let pos_mask = (x & 1) as $ty - 1;
                    (pos_val & pos_mask) | (neg_val & !pos_mask)
                }
            }
            #[inline]
            fn to_u64(self) -> u64 {
                let neg_rep = ((!self as u64) << 1) | 1;
                let pos_rep = (self as u64) << 1;
                if USE_BRANCHES {
                    if self < 0 {
                        neg_rep
                    } else {
                        pos_rep
                    }
                } else {
                    let neg_mask = ((self >= 0) as i64 - 1) as u64;
                    (neg_rep & neg_mask) | (pos_rep & !neg_mask)
                }
            }
        }
        #[cfg(test)]
        proptest! {
            #[test]
            fn $test_name(x: $ty) {
                println!("\ntesting {}", x);
                test_fits64(x);
            }
        }
    };
}
define_ifits!(i8, u8, fits_i8);
define_ifits!(i16, u16, fits_i16);
define_ifits!(i32, u32, fits_i32);
define_ifits!(i64, u64, fits_i64);
define_ifits!(isize, usize, fits_isize);

/// A set type that can store any type that fits in a `u64`.
///
/// This set
/// type is very space-efficient in storing small or closely spaced
/// integers, while not being bad at storing large integers.  The
/// internal implementation for a `Set64` is a [`SetU64`](crate::SetU64), and you
/// can read about the format and size in its documentation.
///
/// **Major caveat** The `Set64` type defines iterators (`drain()` and
/// `iter()`) that iterate over `T` rather than `&T`.  This is a break
/// with standard libray convention, and can be annoying if you are
/// translating code from `HashSet` to `Set64`.  The motivation for
/// this is several-fold:
///
/// 1. `Set64` does not store `T` directly in its data structures
/// (which would waste space), so there is no reference to the data to
/// take.  This does not make it impossible, but does mean we would
/// have to fabricate a `T` and return a reference to it, which is
/// awkward and ugly.
///
/// 2. There is no inefficiency involved in returning `T`, since it is
/// necessarily no larger than a pointer.
///
/// # Examples
///
/// ```
/// use tinyset::Set64;
///
/// let a: Set64<char> = "Hello world".chars().collect();
///
/// for x in "Hello world".chars() {
///     assert!(a.contains(&x));
/// }
/// for x in a {
///     assert!("Hello world".contains(x));
/// }
/// ```
///
/// # Storage details
///
/// Internally a `Set64` is identical to a [SetU64](crate::SetU64), so read there for
/// details.  In short, small sets are the size of a pointer with no
/// heap storage.  Densely packed sets are around a bit per member.
/// Intermediate sets have intermediate storage.  The worst case
/// scenario is large integers widely spaced apart, in which case the
/// storage is similar to a [`std::collections::HashSet`].
#[derive(Debug, Clone)]
pub struct Set64<T: Fits64>(crate::setu64::SetU64, PhantomData<T>);

impl<T: Fits64> Default for Set64<T> {
    /// Creates an empty set..
    fn default() -> Self {
        Set64(crate::setu64::SetU64::new(), PhantomData)
    }
}

impl<T: Fits64> Set64<T> {
    /// Creates an empty set..
    pub fn new() -> Self {
        Self::default()
    }
    /// Creates an empty set with the specified capacity.
    pub fn with_capacity(_cap: usize) -> Self {
        Self::new()
    }
    /// Adds a value to the set.
    ///
    /// If the set did not have this value present, `true` is returned.
    ///
    /// If the set did have this value present, `false` is returned.
    pub fn insert(&mut self, elem: T) -> bool {
        self.0.insert(elem.to_u64())
    }
    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Returns true if the set contains a value.
    pub fn contains<R: std::borrow::Borrow<T>>(&self, value: R) -> bool {
        let x = value.borrow().clone().to_u64();
        self.0.contains(x)
    }
    /// Removes an element, and returns true if that element was present.
    pub fn remove(&mut self, value: &T) -> bool {
        let x = value.clone().to_u64();
        self.0.remove(x)
    }
    /// Iterate
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        self.0.iter().map(|x| unsafe { T::from_u64(x) })
    }
    /// Drain
    pub fn drain<'a>(&'a mut self) -> impl Iterator<Item = T> + 'a {
        self.0.drain().map(|x| unsafe { T::from_u64(x) })
    }
}

impl<T: Fits64> PartialEq for Set64<T> {
    fn eq(&self, other: &Set64<T>) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for k in other.0.iter() {
            if !self.0.contains(k) {
                return false;
            }
        }
        true
    }
}
impl<T: Fits64> Eq for Set64<T> {}

impl<T: Fits64> std::hash::Hash for Set64<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let mut membs: Vec<u64> = self.iter().map(|i| i.to_u64()).collect();
        membs.sort();
        for memb in membs {
            memb.hash(state);
        }
    }
}

impl<T: Fits64> std::iter::FromIterator<T> for Set64<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (sz, _) = iter.size_hint();
        let mut c = Set64::with_capacity(sz);
        for i in iter {
            c.insert(i);
        }
        c
    }
}

/// An iterator.
pub struct IntoIter<T: Fits64>(crate::setu64::IntoIter, PhantomData<T>);

impl<T: Fits64> Iterator for IntoIter<T> {
    type Item = T;
    #[inline]
    fn next(&mut self) -> Option<T> {
        self.0.next().map(|x| unsafe { T::from_u64(x) })
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn count(self) -> usize {
        self.0.count()
    }
    #[inline]
    fn last(self) -> Option<T> {
        self.0.last().map(|x| unsafe { T::from_u64(x) })
    }
}

impl<T: Fits64> IntoIterator for Set64<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> IntoIter<T> {
        IntoIter(self.0.into_iter(), PhantomData)
    }
}

impl<'a, 'b, T: Fits64> std::ops::Sub<&'b Set64<T>> for &'a Set64<T> {
    type Output = Set64<T>;

    /// Returns the difference of `self` and `rhs` as a new `Set64<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use tinyset::Set64;
    ///
    /// let a: Set64<u32> = vec![1, 2, 3].into_iter().collect();
    /// let b: Set64<u32> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let set = &a - &b;
    ///
    /// let mut i = 0;
    /// let expected = [1, 2];
    /// for x in set {
    ///     assert!(expected.contains(&x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn sub(self, rhs: &Set64<T>) -> Set64<T> {
        let mut s = Set64::with_capacity(self.len());
        for v in self.iter() {
            if !rhs.contains(&v) {
                s.insert(v);
            }
        }
        s
    }
}

impl<T: Fits64> Extend<T> for Set64<T> {
    /// Adds a bunch of elements to the set
    ///
    /// # Examples
    ///
    /// ```
    /// use tinyset::Set64;
    ///
    /// let mut a: Set64<u32> = vec![1, 2, 3].into_iter().collect();
    /// a.extend(vec![3, 4, 5]);
    ///
    /// let mut i = 0;
    /// let expected = [1, 2, 3, 4, 5];
    /// for x in a {
    ///     assert!(expected.contains(&x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        for i in iter {
            self.insert(i);
        }
    }
}

#[cfg(feature = "serde")]
mod serde {
    use crate::{Fits64, Set64};
    use serde::de::{Deserialize, Deserializer, SeqAccess, Visitor};
    use serde::ser::{Serialize, SerializeSeq, Serializer};
    use std::marker::PhantomData;

    impl<T: Fits64 + Serialize> Serialize for Set64<T> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut seq = serializer.serialize_seq(Some(self.len()))?;
            for e in self.iter() {
                seq.serialize_element(&e)?;
            }
            seq.end()
        }
    }

    // A Visitor is a type that holds methods that a Deserializer can drive
    // depending on what is contained in the input data.
    //
    // In the case of a map we need generic type parameters K and V to be
    // able to set the output type correctly, but don't require any state.
    // This is an example of a "zero sized type" in Rust. The PhantomData
    // keeps the compiler from complaining about unused generic type
    // parameters.
    struct SetVisitor<T> {
        phantom: PhantomData<T>,
    }

    impl<T> SetVisitor<T> {
        fn new() -> Self {
            SetVisitor {
                phantom: PhantomData,
            }
        }
    }

    // This is the trait that Deserializers are going to be driving. There
    // is one method for each type of data that our type knows how to
    // deserialize from. There are many other methods that are not
    // implemented here, for example deserializing from integers or strings.
    // By default those methods will return an error, which makes sense
    // because we cannot deserialize a MyMap from an integer or string.
    impl<'de, T: Fits64 + Deserialize<'de>> Visitor<'de> for SetVisitor<T> {
        // The type that our Visitor is going to produce.
        type Value = Set64<T>;

        // Format a message stating what data this Visitor expects to receive.
        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a set of usize")
        }

        fn visit_seq<M>(self, mut access: M) -> Result<Self::Value, M::Error>
        where
            M: SeqAccess<'de>,
        {
            let mut set = Set64::new();

            // While there are entries remaining in the input, add them
            // into our map.
            while let Some(elem) = access.next_element()? {
                set.insert(elem);
            }

            Ok(set)
        }
    }

    // This is the trait that informs Serde how to deserialize MyMap.
    impl<'de, T: Fits64 + Deserialize<'de>> Deserialize<'de> for Set64<T> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            // Instantiate our Visitor and ask the Deserializer to drive
            // it over the input data, resulting in an instance of MyMap.
            deserializer.deserialize_seq(SetVisitor::new())
        }
    }

    #[test]
    fn serialize_deserialize() {
        use std::iter::FromIterator;

        let set = Set64::<usize>::from_iter([0]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = Set64::<usize>::from_iter([]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = Set64::<usize>::from_iter([usize::MAX, usize::MAX - 100]);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());

        let set = Set64::<usize>::from_iter(0..10000);
        let s = serde_json::to_string(&set).unwrap();
        assert_eq!(set, serde_json::from_str(&s).unwrap());
    }
}

impl<'a, 'b, T: Fits64> std::ops::BitOr<&'b Set64<T>> for &'a Set64<T> {
    type Output = Set64<T>;

    /// Returns the union of `self` and `rhs` as a new `Set64<T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use tinyset::Set64;
    ///
    /// let a: Set64<u32> = vec![1, 2, 3].into_iter().collect();
    /// let b: Set64<u32> = vec![3, 4, 5].into_iter().collect();
    ///
    /// let set = &a | &b;
    ///
    /// let mut i = 0;
    /// let expected = [1, 2, 3, 4, 5];
    /// for x in set {
    ///     assert!(expected.contains(&x));
    ///     i += 1;
    /// }
    /// assert_eq!(i, expected.len());
    /// ```
    fn bitor(self, rhs: &Set64<T>) -> Set64<T> {
        let mut s: Set64<T> = Set64::with_capacity(self.len() + rhs.len());
        for x in self.iter() {
            s.insert(x);
        }
        for x in rhs.iter() {
            s.insert(x);
        }
        s
    }
}

impl<T: Fits64 + Eq + Ord + std::fmt::Debug + std::fmt::Display> crate::copyset::CopySet
    for Set64<T>
{
    type Item = T;
    type Iter = IntoIter<T>;
    fn ins(&mut self, e: Self::Item) -> bool {
        self.insert(e)
    }
    fn rem(&mut self, e: Self::Item) -> bool {
        self.remove(&e)
    }
    fn con(&self, e: Self::Item) -> bool {
        self.contains(&e)
    }
    fn vec(&self) -> Vec<Self::Item> {
        self.iter().collect()
    }
    fn ln(&self) -> usize {
        self.len()
    }
    fn it(self) -> Self::Iter {
        self.into_iter()
    }
}

#[cfg(test)]
proptest! {
    #[test]
    fn copycheck_random_sets(slice in prop::collection::vec(1u64..5, 1usize..10)) {
        crate::copyset::check_set::<Set64<u64>>(&slice);
    }
    #[test]
    fn copycheck_medium_sets(slice in prop::collection::vec(1u64..255, 1usize..100)) {
        crate::copyset::check_set::<Set64<u64>>(&slice);
    }
    #[test]
    fn copycheck_big_sets(slice: Vec<u64>) {
        crate::copyset::check_set::<Set64<u64>>(&slice);
    }
    #[test]
    fn copycheck_u8_sets(slice: Vec<u8>) {
        crate::copyset::check_set::<Set64<u8>>(&slice);
    }
}

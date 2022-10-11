use crate::{Enum, EnumMap};
use core::iter::{Enumerate, FusedIterator};
use core::marker::PhantomData;
use core::mem::ManuallyDrop;
use core::ptr;
use core::slice;

/// Immutable enum map iterator
///
/// This struct is created by `iter` method or `into_iter` on a reference
/// to `EnumMap`.
///
/// # Examples
///
/// ```
/// # extern crate enum_map;
/// use enum_map::{enum_map, Enum};
///
/// #[derive(Enum)]
/// enum Example {
///     A,
///     B,
///     C,
/// }
///
/// let mut map = enum_map! { Example::A => 3, _ => 0 };
/// assert_eq!(map[Example::A], 3);
/// for (key, &value) in &map {
///     assert_eq!(value, match key {
///         Example::A => 3,
///         _ => 0,
///     });
/// }
/// ```
#[derive(Debug)]
pub struct Iter<'a, K, V: 'a> {
    _phantom: PhantomData<fn() -> K>,
    iterator: Enumerate<slice::Iter<'a, V>>,
}

impl<'a, K: Enum<V>, V> Clone for Iter<'a, K, V> {
    fn clone(&self) -> Self {
        Iter {
            _phantom: PhantomData,
            iterator: self.iterator.clone(),
        }
    }
}

impl<'a, K: Enum<V>, V> Iterator for Iter<'a, K, V> {
    type Item = (K, &'a V);
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator
            .next()
            .map(|(index, item)| (K::from_usize(index), item))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iterator.size_hint()
    }

    fn fold<B, F>(self, init: B, f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        self.iterator
            .map(|(index, item)| (K::from_usize(index), item))
            .fold(init, f)
    }
}

impl<'a, K: Enum<V>, V> DoubleEndedIterator for Iter<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iterator
            .next_back()
            .map(|(index, item)| (K::from_usize(index), item))
    }
}

impl<'a, K: Enum<V>, V> ExactSizeIterator for Iter<'a, K, V> {}

impl<'a, K: Enum<V>, V> FusedIterator for Iter<'a, K, V> {}

impl<'a, K: Enum<V>, V> IntoIterator for &'a EnumMap<K, V> {
    type Item = (K, &'a V);
    type IntoIter = Iter<'a, K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            _phantom: PhantomData,
            iterator: self.as_slice().iter().enumerate(),
        }
    }
}

/// Mutable map iterator
///
/// This struct is created by `iter_mut` method or `into_iter` on a mutable
/// reference to `EnumMap`.
///
/// # Examples
///
/// ```
/// # extern crate enum_map;
/// use enum_map::{enum_map, Enum};
///
/// #[derive(Debug, Enum)]
/// enum Example {
///     A,
///     B,
///     C,
/// }
///
/// let mut map = enum_map! { Example::A => 3, _ => 0 };
/// for (_, value) in &mut map {
///     *value += 1;
/// }
/// assert_eq!(map, enum_map! { Example::A => 4, _ => 1 });
/// ```
#[derive(Debug)]
pub struct IterMut<'a, K, V: 'a> {
    _phantom: PhantomData<fn() -> K>,
    iterator: Enumerate<slice::IterMut<'a, V>>,
}

impl<'a, K: Enum<V>, V> Iterator for IterMut<'a, K, V> {
    type Item = (K, &'a mut V);
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iterator
            .next()
            .map(|(index, item)| (K::from_usize(index), item))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iterator.size_hint()
    }

    fn fold<B, F>(self, init: B, f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        self.iterator
            .map(|(index, item)| (K::from_usize(index), item))
            .fold(init, f)
    }
}

impl<'a, K: Enum<V>, V> DoubleEndedIterator for IterMut<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iterator
            .next_back()
            .map(|(index, item)| (K::from_usize(index), item))
    }
}

impl<'a, K: Enum<V>, V> ExactSizeIterator for IterMut<'a, K, V> {}

impl<'a, K: Enum<V>, V> FusedIterator for IterMut<'a, K, V> {}

impl<'a, K: Enum<V>, V> IntoIterator for &'a mut EnumMap<K, V> {
    type Item = (K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            _phantom: PhantomData,
            iterator: self.as_mut_slice().iter_mut().enumerate(),
        }
    }
}

/// A map iterator that moves out of map.
///
/// This struct is created by `into_iter` on `EnumMap`.
///
/// # Examples
///
/// ```
/// # extern crate enum_map;
/// use enum_map::{enum_map, Enum};
///
/// #[derive(Debug, Enum)]
/// enum Example {
///     A,
///     B,
/// }
///
/// let map = enum_map! { Example::A | Example::B => String::from("123") };
/// for (_, value) in map {
///     assert_eq!(value + "4", "1234");
/// }
/// ```
pub struct IntoIter<K: Enum<V>, V> {
    map: ManuallyDrop<EnumMap<K, V>>,
    position: usize,
}

impl<K: Enum<V>, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);
    fn next(&mut self) -> Option<(K, V)> {
        let slice = self.map.as_slice();
        if self.position < slice.len() {
            let key = K::from_usize(self.position);
            let result = Some((key, unsafe { ptr::read(&slice[self.position]) }));
            self.position += 1;
            result
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let slice = self.map.as_slice();
        let diff = slice.len() - self.position;
        (diff, Some(diff))
    }
}

impl<K: Enum<V>, V> ExactSizeIterator for IntoIter<K, V> {}

impl<K: Enum<V>, V> FusedIterator for IntoIter<K, V> {}

impl<K: Enum<V>, V> Drop for IntoIter<K, V> {
    #[inline]
    fn drop(&mut self) {
        for _item in self {}
    }
}

impl<K: Enum<V>, V> IntoIterator for EnumMap<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            map: ManuallyDrop::new(self),
            position: 0,
        }
    }
}

impl<K: Enum<V>, V> EnumMap<K, V> {
    /// An iterator visiting all values. The iterator type is `&V`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::enum_map;
    ///
    /// let map = enum_map! { false => 3, true => 4 };
    /// let mut values = map.values();
    /// assert_eq!(values.next(), Some(&3));
    /// assert_eq!(values.next(), Some(&4));
    /// assert_eq!(values.next(), None);
    /// ```
    #[inline]
    pub fn values(&self) -> Values<V> {
        Values(self.as_slice().iter())
    }

    /// An iterator visiting all values mutably. The iterator type is `&mut V`.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::enum_map;
    ///
    /// let mut map = enum_map! { _ => 2 };
    /// for value in map.values_mut() {
    ///     *value += 2;
    /// }
    /// assert_eq!(map[false], 4);
    /// assert_eq!(map[true], 4);
    /// ```
    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<V> {
        ValuesMut(self.as_mut_slice().iter_mut())
    }
}

/// An iterator over the values of `EnumMap`.
///
/// This `struct` is created by the `values` method of `EnumMap`.
/// See its documentation for more.
pub struct Values<'a, V: 'a>(slice::Iter<'a, V>);

impl<'a, V> Clone for Values<'a, V> {
    fn clone(&self) -> Self {
        Values(self.0.clone())
    }
}

impl<'a, V: 'a> Iterator for Values<'a, V> {
    type Item = &'a V;
    #[inline]
    fn next(&mut self) -> Option<&'a V> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, V: 'a> DoubleEndedIterator for Values<'a, V> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a V> {
        self.0.next_back()
    }
}

impl<'a, V: 'a> ExactSizeIterator for Values<'a, V> {}

impl<'a, V: 'a> FusedIterator for Values<'a, V> {}

/// A mutable iterator over the values of `EnumMap`.
///
/// This `struct` is created by the `values_mut` method of `EnumMap`.
/// See its documentation for more.
pub struct ValuesMut<'a, V: 'a>(slice::IterMut<'a, V>);

impl<'a, V: 'a> Iterator for ValuesMut<'a, V> {
    type Item = &'a mut V;
    #[inline]
    fn next(&mut self) -> Option<&'a mut V> {
        self.0.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, V: 'a> DoubleEndedIterator for ValuesMut<'a, V> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut V> {
        self.0.next_back()
    }
}

impl<'a, V: 'a> ExactSizeIterator for ValuesMut<'a, V> {}

impl<'a, V: 'a> FusedIterator for ValuesMut<'a, V> {}

//! An enum mapping type.
//!
//! It is implemented using an array type, so using it is as fast as using Rust
//! arrays.
//!
//! # Examples
//!
//! ```
//! use enum_map::{enum_map, Enum, EnumMap};
//!
//! #[derive(Debug, Enum)]
//! enum Example {
//!     A,
//!     B,
//!     C,
//! }
//!
//! let mut map = enum_map! {
//!     Example::A => 1,
//!     Example::B => 2,
//!     Example::C => 3,
//! };
//! map[Example::C] = 4;
//!
//! assert_eq!(map[Example::A], 1);
//!
//! for (key, &value) in &map {
//!     println!("{:?} has {} as value.", key, value);
//! }
//! ```

#![no_std]
#![deny(missing_docs)]

mod enum_map_impls;
mod internal;
mod iter;
mod serde;

pub use enum_map_derive::Enum;
pub use internal::Enum;
pub use iter::{IntoIter, Iter, IterMut, Values, ValuesMut};

/// Enum map constructor.
///
/// This macro allows to create a new enum map in a type safe way. It takes
/// a list of `,` separated pairs separated by `=>`. Left side is `|`
/// separated list of enum keys, or `_` to match all unmatched enum keys,
/// while right side is a value.
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
///     D,
/// }
///
/// let enum_map = enum_map! {
///     Example::A | Example::B => 1,
///     Example::C => 2,
///     _ => 3,
/// };
/// assert_eq!(enum_map[Example::A], 1);
/// assert_eq!(enum_map[Example::B], 1);
/// assert_eq!(enum_map[Example::C], 2);
/// assert_eq!(enum_map[Example::D], 3);
/// ```
#[macro_export]
macro_rules! enum_map {
    {$($t:tt)*} => {
        $crate::__from_fn(|k| match k { $($t)* })
    };
}

/// An enum mapping.
///
/// This internally uses an array which stores a value for each possible
/// enum value. To work, it requires implementation of internal (private,
/// although public due to macro limitations) trait which allows extracting
/// information about an enum, which can be automatically generated using
/// `#[derive(Enum)]` macro.
///
/// Additionally, `bool` and `u8` automatically derives from `Enum`. While
/// `u8` is not technically an enum, it's convenient to consider it like one.
/// In particular, [reverse-complement in benchmark game] could be using `u8`
/// as an enum.
///
/// # Examples
///
/// ```
/// # extern crate enum_map;
/// use enum_map::{enum_map, Enum, EnumMap};
///
/// #[derive(Enum)]
/// enum Example {
///     A,
///     B,
///     C,
/// }
///
/// let mut map = EnumMap::default();
/// // new initializes map with default values
/// assert_eq!(map[Example::A], 0);
/// map[Example::A] = 3;
/// assert_eq!(map[Example::A], 3);
/// ```
///
/// [reverse-complement in benchmark game]:
///     http://benchmarksgame.alioth.debian.org/u64q/program.php?test=revcomp&lang=rust&id=2
pub struct EnumMap<K: Enum<V>, V> {
    array: K::Array,
}

impl<K: Enum<V>, V: Default> EnumMap<K, V> {
    /// Creates an enum map with default values.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::{Enum, EnumMap};
    ///
    /// #[derive(Enum)]
    /// enum Example {
    ///     A,
    /// }
    ///
    /// let enum_map = EnumMap::<_, i32>::default();
    /// assert_eq!(enum_map[Example::A], 0);
    /// ```
    #[inline]
    #[must_use]
    #[deprecated(since = "0.6.5", note = "Please use EnumMap::default instead")]
    pub fn new() -> Self {
        EnumMap::default()
    }

    /// Clear enum map with default values.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::{Enum, EnumMap};
    ///
    /// #[derive(Enum)]
    /// enum Example {
    ///     A,
    ///     B,
    /// }
    ///
    /// let mut enum_map = EnumMap::<_, String>::default();
    /// enum_map[Example::B] = "foo".into();
    /// enum_map.clear();
    /// assert_eq!(enum_map[Example::A], "");
    /// assert_eq!(enum_map[Example::B], "");
    /// ```
    #[inline]
    pub fn clear(&mut self) {
        for v in self.as_mut_slice() {
            *v = V::default();
        }
    }
}

impl<K: Enum<V>, V> EnumMap<K, V> {
    /// Returns an iterator over enum map.
    #[inline]
    pub fn iter(&self) -> Iter<K, V> {
        self.into_iter()
    }

    /// Returns a mutable iterator over enum map.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        self.into_iter()
    }

    /// Returns number of elements in enum map.
    #[inline]
    pub fn len(&self) -> usize {
        self.as_slice().len()
    }

    /// Returns whether the enum variant set is empty.
    ///
    /// This isn't particularly useful, as there is no real reason to use
    /// enum map for enums without variants.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::{Enum, EnumMap};
    ///
    /// #[derive(Enum)]
    /// enum Void {}
    ///
    /// #[derive(Enum)]
    /// enum SingleVariant {
    ///     Variant,
    /// }
    ///
    /// assert!(EnumMap::<Void, ()>::default().is_empty());
    /// assert!(!EnumMap::<SingleVariant, ()>::default().is_empty());
    /// ```
    #[deprecated(since = "0.6.5")]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }

    /// Swaps two indexes.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::enum_map;
    ///
    /// let mut map = enum_map! { false => 0, true => 1 };
    /// map.swap(false, true);
    /// assert_eq!(map[false], 1);
    /// assert_eq!(map[true], 0);
    /// ```
    #[inline]
    pub fn swap(&mut self, a: K, b: K) {
        self.as_mut_slice().swap(a.to_usize(), b.to_usize())
    }

    /// Converts an enum map to a slice representing values.
    #[inline]
    pub fn as_slice(&self) -> &[V] {
        K::slice(&self.array)
    }

    /// Converts a mutable enum map to a mutable slice representing values.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [V] {
        K::slice_mut(&mut self.array)
    }

    /// Returns a raw pointer to the enum map's slice.
    ///
    /// The caller must ensure that the slice outlives the pointer this
    /// function returns, or else it will end up pointing to garbage.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::{enum_map, EnumMap};
    ///
    /// let map = enum_map! { 5 => 42, _ => 0 };
    /// assert_eq!(unsafe { *map.as_ptr().offset(5) }, 42);
    /// ```
    #[deprecated(since = "0.6.4", note = "Please use .as_slice().as_ptr() instead")]
    #[inline]
    pub fn as_ptr(&self) -> *const V {
        self.as_slice().as_ptr()
    }

    /// Returns an unsafe mutable pointer to the enum map's slice.
    ///
    /// The caller must ensure that the slice outlives the pointer this
    /// function returns, or else it will end up pointing to garbage.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate enum_map;
    /// use enum_map::{enum_map, EnumMap};
    ///
    /// let mut map = enum_map! { _ => 0 };
    /// unsafe {
    ///     *map.as_mut_ptr().offset(11) = 23
    /// };
    /// assert_eq!(map[11], 23);
    /// ```
    #[deprecated(
        since = "0.6.4",
        note = "Please use .as_mut_slice().as_mut_ptr() instead"
    )]
    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut V {
        self.as_mut_slice().as_mut_ptr()
    }
}

impl<F: FnMut(K) -> V, K: Enum<V>, V> From<F> for EnumMap<K, V> {
    #[inline]
    fn from(f: F) -> Self {
        EnumMap {
            array: K::from_function(f),
        }
    }
}

#[doc(hidden)]
pub fn __from_fn<K, V>(f: impl FnMut(K) -> V) -> EnumMap<K, V>
where
    K: Enum<V>,
{
    f.into()
}

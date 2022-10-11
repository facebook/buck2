//! This crate provides the `AnyMap` type, a safe and convenient store for one value of each type.

#![cfg_attr(all(feature = "bench", test), feature(test))]
#![warn(missing_docs, unused_results)]

#[cfg(all(feature = "bench", test))]
extern crate test;

use std::any::TypeId;
use std::marker::PhantomData;

use raw::RawMap;
use any::{UncheckedAnyExt, IntoBox, Any};

macro_rules! impl_common_methods {
    (
        field: $t:ident.$field:ident;
        new() => $new:expr;
        with_capacity($with_capacity_arg:ident) => $with_capacity:expr;
    ) => {
        impl<A: ?Sized + UncheckedAnyExt> $t<A> {
            /// Create an empty collection.
            #[inline]
            pub fn new() -> $t<A> {
                $t {
                    $field: $new,
                }
            }

            /// Creates an empty collection with the given initial capacity.
            #[inline]
            pub fn with_capacity($with_capacity_arg: usize) -> $t<A> {
                $t {
                    $field: $with_capacity,
                }
            }

            /// Returns the number of elements the collection can hold without reallocating.
            #[inline]
            pub fn capacity(&self) -> usize {
                self.$field.capacity()
            }

            /// Reserves capacity for at least `additional` more elements to be inserted
            /// in the collection. The collection may reserve more space to avoid
            /// frequent reallocations.
            ///
            /// # Panics
            ///
            /// Panics if the new allocation size overflows `usize`.
            #[inline]
            pub fn reserve(&mut self, additional: usize) {
                self.$field.reserve(additional)
            }

            /// Shrinks the capacity of the collection as much as possible. It will drop
            /// down as much as possible while maintaining the internal rules
            /// and possibly leaving some space in accordance with the resize policy.
            #[inline]
            pub fn shrink_to_fit(&mut self) {
                self.$field.shrink_to_fit()
            }

            /// Returns the number of items in the collection.
            #[inline]
            pub fn len(&self) -> usize {
                self.$field.len()
            }

            /// Returns true if there are no items in the collection.
            #[inline]
            pub fn is_empty(&self) -> bool {
                self.$field.is_empty()
            }

            /// Removes all items from the collection. Keeps the allocated memory for reuse.
            #[inline]
            pub fn clear(&mut self) {
                self.$field.clear()
            }
        }
    }
}

pub mod any;
pub mod raw;

/// A collection containing zero or one values for any given type and allowing convenient,
/// type-safe access to those values.
///
/// The type parameter `A` allows you to use a different value type; normally you will want it to
/// be `anymap::any::Any`, but there are other choices:
///
/// - If you want the entire map to be cloneable, use `CloneAny` instead of `Any`.
/// - You can add on `+ Send` and/or `+ Sync` (e.g. `Map<Any + Send>`) to add those bounds.
///
/// ```rust
/// # use anymap::AnyMap;
/// let mut data = AnyMap::new();
/// assert_eq!(data.get(), None::<&i32>);
/// data.insert(42i32);
/// assert_eq!(data.get(), Some(&42i32));
/// data.remove::<i32>();
/// assert_eq!(data.get::<i32>(), None);
///
/// #[derive(Clone, PartialEq, Debug)]
/// struct Foo {
///     str: String,
/// }
///
/// assert_eq!(data.get::<Foo>(), None);
/// data.insert(Foo { str: format!("foo") });
/// assert_eq!(data.get(), Some(&Foo { str: format!("foo") }));
/// data.get_mut::<Foo>().map(|foo| foo.str.push('t'));
/// assert_eq!(&*data.get::<Foo>().unwrap().str, "foot");
/// ```
///
/// Values containing non-static references are not permitted.
#[derive(Debug)]
pub struct Map<A: ?Sized + UncheckedAnyExt = Any> {
    raw: RawMap<A>,
}

// #[derive(Clone)] would want A to implement Clone, but in reality it’s only Box<A> that can.
impl<A: ?Sized + UncheckedAnyExt> Clone for Map<A> where Box<A>: Clone {
    #[inline]
    fn clone(&self) -> Map<A> {
        Map {
            raw: self.raw.clone(),
        }
    }
}

/// The most common type of `Map`: just using `Any`.
///
/// Why is this a separate type alias rather than a default value for `Map<A>`? `Map::new()`
/// doesn’t seem to be happy to infer that it should go with the default value.
/// It’s a bit sad, really. Ah well, I guess this approach will do.
pub type AnyMap = Map<Any>;

impl_common_methods! {
    field: Map.raw;
    new() => RawMap::new();
    with_capacity(capacity) => RawMap::with_capacity(capacity);
}

impl<A: ?Sized + UncheckedAnyExt> Map<A> {
    /// Returns a reference to the value stored in the collection for the type `T`, if it exists.
    #[inline]
    pub fn get<T: IntoBox<A>>(&self) -> Option<&T> {
        self.raw.get(&TypeId::of::<T>())
            .map(|any| unsafe { any.downcast_ref_unchecked::<T>() })
    }

    /// Returns a mutable reference to the value stored in the collection for the type `T`,
    /// if it exists.
    #[inline]
    pub fn get_mut<T: IntoBox<A>>(&mut self) -> Option<&mut T> {
        self.raw.get_mut(&TypeId::of::<T>())
            .map(|any| unsafe { any.downcast_mut_unchecked::<T>() })
    }

    /// Sets the value stored in the collection for the type `T`.
    /// If the collection already had a value of type `T`, that value is returned.
    /// Otherwise, `None` is returned.
    #[inline]
    pub fn insert<T: IntoBox<A>>(&mut self, value: T) -> Option<T> {
        unsafe {
            self.raw.insert(TypeId::of::<T>(), value.into_box())
                .map(|any| *any.downcast_unchecked::<T>())
        }
    }

    /// Removes the `T` value from the collection,
    /// returning it if there was one or `None` if there was not.
    #[inline]
    pub fn remove<T: IntoBox<A>>(&mut self) -> Option<T> {
        self.raw.remove(&TypeId::of::<T>())
            .map(|any| *unsafe { any.downcast_unchecked::<T>() })
    }

    /// Returns true if the collection contains a value of type `T`.
    #[inline]
    pub fn contains<T: IntoBox<A>>(&self) -> bool {
        self.raw.contains_key(&TypeId::of::<T>())
    }

    /// Gets the entry for the given type in the collection for in-place manipulation
    #[inline]
    pub fn entry<T: IntoBox<A>>(&mut self) -> Entry<A, T> {
        match self.raw.entry(TypeId::of::<T>()) {
            raw::Entry::Occupied(e) => Entry::Occupied(OccupiedEntry {
                inner: e,
                type_: PhantomData,
            }),
            raw::Entry::Vacant(e) => Entry::Vacant(VacantEntry {
                inner: e,
                type_: PhantomData,
            }),
        }
    }
}

impl<A: ?Sized + UncheckedAnyExt> AsRef<RawMap<A>> for Map<A> {
    #[inline]
    fn as_ref(&self) -> &RawMap<A> {
        &self.raw
    }
}

impl<A: ?Sized + UncheckedAnyExt> AsMut<RawMap<A>> for Map<A> {
    #[inline]
    fn as_mut(&mut self) -> &mut RawMap<A> {
        &mut self.raw
    }
}

impl<A: ?Sized + UncheckedAnyExt> Into<RawMap<A>> for Map<A> {
    #[inline]
    fn into(self) -> RawMap<A> {
        self.raw
    }
}

/// A view into a single occupied location in an `Map`.
pub struct OccupiedEntry<'a, A: ?Sized + UncheckedAnyExt, V: 'a> {
    inner: raw::OccupiedEntry<'a, A>,
    type_: PhantomData<V>,
}

/// A view into a single empty location in an `Map`.
pub struct VacantEntry<'a, A: ?Sized + UncheckedAnyExt, V: 'a> {
    inner: raw::VacantEntry<'a, A>,
    type_: PhantomData<V>,
}

/// A view into a single location in an `Map`, which may be vacant or occupied.
pub enum Entry<'a, A: ?Sized + UncheckedAnyExt, V: 'a> {
    /// An occupied Entry
    Occupied(OccupiedEntry<'a, A, V>),
    /// A vacant Entry
    Vacant(VacantEntry<'a, A, V>),
}

impl<'a, A: ?Sized + UncheckedAnyExt, V: IntoBox<A>> Entry<'a, A, V> {
    /// Ensures a value is in the entry by inserting the default if empty, and returns
    /// a mutable reference to the value in the entry.
    #[inline]
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Entry::Occupied(inner) => inner.into_mut(),
            Entry::Vacant(inner) => inner.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the default function if empty,
    /// and returns a mutable reference to the value in the entry.
    #[inline]
    pub fn or_insert_with<F: FnOnce() -> V>(self, default: F) -> &'a mut V {
        match self {
            Entry::Occupied(inner) => inner.into_mut(),
            Entry::Vacant(inner) => inner.insert(default()),
        }
    }
}

impl<'a, A: ?Sized + UncheckedAnyExt, V: IntoBox<A>> OccupiedEntry<'a, A, V> {
    /// Gets a reference to the value in the entry
    #[inline]
    pub fn get(&self) -> &V {
        unsafe { self.inner.get().downcast_ref_unchecked() }
    }

    /// Gets a mutable reference to the value in the entry
    #[inline]
    pub fn get_mut(&mut self) -> &mut V {
        unsafe { self.inner.get_mut().downcast_mut_unchecked() }
    }

    /// Converts the OccupiedEntry into a mutable reference to the value in the entry
    /// with a lifetime bound to the collection itself
    #[inline]
    pub fn into_mut(self) -> &'a mut V {
        unsafe { self.inner.into_mut().downcast_mut_unchecked() }
    }

    /// Sets the value of the entry, and returns the entry's old value
    #[inline]
    pub fn insert(&mut self, value: V) -> V {
        unsafe { *self.inner.insert(value.into_box()).downcast_unchecked() }
    }

    /// Takes the value out of the entry, and returns it
    #[inline]
    pub fn remove(self) -> V {
        unsafe { *self.inner.remove().downcast_unchecked() }
    }
}

impl<'a, A: ?Sized + UncheckedAnyExt, V: IntoBox<A>> VacantEntry<'a, A, V> {
    /// Sets the value of the entry with the VacantEntry's key,
    /// and returns a mutable reference to it
    #[inline]
    pub fn insert(self, value: V) -> &'a mut V {
        unsafe { self.inner.insert(value.into_box()).downcast_mut_unchecked() }
    }
}

#[cfg(all(feature = "bench", test))]
mod bench {
    use AnyMap;
    use test::Bencher;
    use test::black_box;

    #[bench]
    fn insertion(b: &mut Bencher) {
        b.iter(|| {
            let mut data = AnyMap::new();
            for _ in 0..100 {
                let _ = data.insert(42);
            }
        })
    }

    #[bench]
    fn get_missing(b: &mut Bencher) {
        b.iter(|| {
            let data = AnyMap::new();
            for _ in 0..100 {
                assert_eq!(data.get(), None::<&i32>);
            }
        })
    }

    #[bench]
    fn get_present(b: &mut Bencher) {
        b.iter(|| {
            let mut data = AnyMap::new();
            let _ = data.insert(42);
            // These inner loops are a feeble attempt to drown the other factors.
            for _ in 0..100 {
                assert_eq!(data.get(), Some(&42));
            }
        })
    }

    macro_rules! big_benchmarks {
        ($name:ident, $($T:ident)*) => (
            #[bench]
            fn $name(b: &mut Bencher) {
                $(
                    struct $T(&'static str);
                )*

                b.iter(|| {
                    let mut data = AnyMap::new();
                    $(
                        let _ = black_box(data.insert($T(stringify!($T))));
                    )*
                    $(
                        let _ = black_box(data.get::<$T>());
                    )*
                })
            }
        );
    }

    // Caution: if the macro does too much (e.g. assertions) this goes from being slow to being
    // *really* slow (like add a minute for each assertion on it) and memory-hungry (like, adding
    // several hundred megabytes to the peak for each assertion).
    big_benchmarks! {
        insert_and_get_on_260_types,
        A0 B0 C0 D0 E0 F0 G0 H0 I0 J0 K0 L0 M0 N0 O0 P0 Q0 R0 S0 T0 U0 V0 W0 X0 Y0 Z0
        A1 B1 C1 D1 E1 F1 G1 H1 I1 J1 K1 L1 M1 N1 O1 P1 Q1 R1 S1 T1 U1 V1 W1 X1 Y1 Z1
        A2 B2 C2 D2 E2 F2 G2 H2 I2 J2 K2 L2 M2 N2 O2 P2 Q2 R2 S2 T2 U2 V2 W2 X2 Y2 Z2
        A3 B3 C3 D3 E3 F3 G3 H3 I3 J3 K3 L3 M3 N3 O3 P3 Q3 R3 S3 T3 U3 V3 W3 X3 Y3 Z3
        A4 B4 C4 D4 E4 F4 G4 H4 I4 J4 K4 L4 M4 N4 O4 P4 Q4 R4 S4 T4 U4 V4 W4 X4 Y4 Z4
        A5 B5 C5 D5 E5 F5 G5 H5 I5 J5 K5 L5 M5 N5 O5 P5 Q5 R5 S5 T5 U5 V5 W5 X5 Y5 Z5
        A6 B6 C6 D6 E6 F6 G6 H6 I6 J6 K6 L6 M6 N6 O6 P6 Q6 R6 S6 T6 U6 V6 W6 X6 Y6 Z6
        A7 B7 C7 D7 E7 F7 G7 H7 I7 J7 K7 L7 M7 N7 O7 P7 Q7 R7 S7 T7 U7 V7 W7 X7 Y7 Z7
        A8 B8 C8 D8 E8 F8 G8 H8 I8 J8 K8 L8 M8 N8 O8 P8 Q8 R8 S8 T8 U8 V8 W8 X8 Y8 Z8
        A9 B9 C9 D9 E9 F9 G9 H9 I9 J9 K9 L9 M9 N9 O9 P9 Q9 R9 S9 T9 U9 V9 W9 X9 Y9 Z9
    }

    big_benchmarks! {
        insert_and_get_on_26_types,
        A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    }
}

#[cfg(test)]
mod tests {
    use {Map, AnyMap, Entry};
    use any::{Any, CloneAny};

    #[derive(Clone, Debug, PartialEq)] struct A(i32);
    #[derive(Clone, Debug, PartialEq)] struct B(i32);
    #[derive(Clone, Debug, PartialEq)] struct C(i32);
    #[derive(Clone, Debug, PartialEq)] struct D(i32);
    #[derive(Clone, Debug, PartialEq)] struct E(i32);
    #[derive(Clone, Debug, PartialEq)] struct F(i32);
    #[derive(Clone, Debug, PartialEq)] struct J(i32);

    macro_rules! test_entry {
        ($name:ident, $init:ty) => {
            #[test]
            fn $name() {
                let mut map = <$init>::new();
                assert_eq!(map.insert(A(10)), None);
                assert_eq!(map.insert(B(20)), None);
                assert_eq!(map.insert(C(30)), None);
                assert_eq!(map.insert(D(40)), None);
                assert_eq!(map.insert(E(50)), None);
                assert_eq!(map.insert(F(60)), None);

                // Existing key (insert)
                match map.entry::<A>() {
                    Entry::Vacant(_) => unreachable!(),
                    Entry::Occupied(mut view) => {
                        assert_eq!(view.get(), &A(10));
                        assert_eq!(view.insert(A(100)), A(10));
                    }
                }
                assert_eq!(map.get::<A>().unwrap(), &A(100));
                assert_eq!(map.len(), 6);


                // Existing key (update)
                match map.entry::<B>() {
                    Entry::Vacant(_) => unreachable!(),
                    Entry::Occupied(mut view) => {
                        let v = view.get_mut();
                        let new_v = B(v.0 * 10);
                        *v = new_v;
                    }
                }
                assert_eq!(map.get::<B>().unwrap(), &B(200));
                assert_eq!(map.len(), 6);


                // Existing key (remove)
                match map.entry::<C>() {
                    Entry::Vacant(_) => unreachable!(),
                    Entry::Occupied(view) => {
                        assert_eq!(view.remove(), C(30));
                    }
                }
                assert_eq!(map.get::<C>(), None);
                assert_eq!(map.len(), 5);


                // Inexistent key (insert)
                match map.entry::<J>() {
                    Entry::Occupied(_) => unreachable!(),
                    Entry::Vacant(view) => {
                        assert_eq!(*view.insert(J(1000)), J(1000));
                    }
                }
                assert_eq!(map.get::<J>().unwrap(), &J(1000));
                assert_eq!(map.len(), 6);

                // Entry.or_insert on existing key
                map.entry::<B>().or_insert(B(71)).0 += 1;
                assert_eq!(map.get::<B>().unwrap(), &B(201));
                assert_eq!(map.len(), 6);

                // Entry.or_insert on nonexisting key
                map.entry::<C>().or_insert(C(300)).0 += 1;
                assert_eq!(map.get::<C>().unwrap(), &C(301));
                assert_eq!(map.len(), 7);
            }
        }
    }

    test_entry!(test_entry_any, AnyMap);
    test_entry!(test_entry_cloneany, Map<CloneAny>);

    #[test]
    fn test_clone() {
        let mut map: Map<CloneAny> = Map::new();
        let _ = map.insert(A(1));
        let _ = map.insert(B(2));
        let _ = map.insert(D(3));
        let _ = map.insert(E(4));
        let _ = map.insert(F(5));
        let _ = map.insert(J(6));
        let map2 = map.clone();
        assert_eq!(map2.len(), 6);
        assert_eq!(map2.get::<A>(), Some(&A(1)));
        assert_eq!(map2.get::<B>(), Some(&B(2)));
        assert_eq!(map2.get::<C>(), None);
        assert_eq!(map2.get::<D>(), Some(&D(3)));
        assert_eq!(map2.get::<E>(), Some(&E(4)));
        assert_eq!(map2.get::<F>(), Some(&F(5)));
        assert_eq!(map2.get::<J>(), Some(&J(6)));
    }

    #[test]
    fn test_varieties() {
        fn assert_send<T: Send>() { }
        fn assert_sync<T: Sync>() { }
        fn assert_clone<T: Clone>() { }
        fn assert_debug<T: ::std::fmt::Debug>() { }
        assert_send::<Map<Any + Send>>();
        assert_send::<Map<Any + Send + Sync>>();
        assert_sync::<Map<Any + Sync>>();
        assert_sync::<Map<Any + Send + Sync>>();
        assert_debug::<Map<Any>>();
        assert_debug::<Map<Any + Send>>();
        assert_debug::<Map<Any + Sync>>();
        assert_debug::<Map<Any + Send + Sync>>();
        assert_send::<Map<CloneAny + Send>>();
        assert_send::<Map<CloneAny + Send + Sync>>();
        assert_sync::<Map<CloneAny + Sync>>();
        assert_sync::<Map<CloneAny + Send + Sync>>();
        assert_clone::<Map<CloneAny + Send>>();
        assert_clone::<Map<CloneAny + Send + Sync>>();
        assert_clone::<Map<CloneAny + Sync>>();
        assert_clone::<Map<CloneAny + Send + Sync>>();
        assert_debug::<Map<CloneAny>>();
        assert_debug::<Map<CloneAny + Send>>();
        assert_debug::<Map<CloneAny + Sync>>();
        assert_debug::<Map<CloneAny + Send + Sync>>();
    }
}

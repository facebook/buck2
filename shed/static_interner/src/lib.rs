/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Intern objects in memory.
//!
//! This is similar to [`internment` crate](https://github.com/droundy/internment)
//! but with changes for performance and flexibility.

use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ptr;

use allocative::Allocative;
use allocative::Visitor;
use dupe::Dupe;
pub use equivalent::Equivalent;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::arc_erase::ArcErase;
use pagable::arc_erase::ArcEraseType;
use pagable::arc_erase::StdArcEraseType;
use pagable::arc_erase::deserialize_arc;
use strong_hash::StrongHash;

pub struct Interner<T: 'static, H = DefaultHasher> {
    table: ShardedLockFreeRawTable<Box<InternedData<T>>, 64>,
    _marker: PhantomData<H>,
}

/// This structure is similar to `Hashed<T>`, but it is not parameterized by hash function.
#[derive(Debug)]
struct InternedData<T: 'static> {
    data: T,
    hash: u64,
}

/// An interned pointer.
///
/// Equality of this type is a pointer comparison.
/// But note, this works correctly only if `Intern` pointers created
/// from the same instance of `Interner`.
#[derive(Debug)]
pub struct Intern<T: 'static> {
    pointer: &'static InternedData<T>,
}

pub trait Internable {
    type Hasher;

    fn interner() -> &'static Interner<Self, Self::Hasher>
    where
        Self: Sized;
}

impl<T: StrongHash> StrongHash for Intern<T> {
    fn strong_hash<H: Hasher>(&self, hasher: &mut H) {
        self.pointer.data.strong_hash(hasher);
    }
}

impl<
    T: PagableSerialize
        + for<'de> PagableDeserialize<'de>
        + std::fmt::Debug
        + Hash
        + Send
        + Sync
        + Internable<Hasher = H>
        + Eq
        + 'static,
    H: Hasher + Default + 'static,
> ArcErase for Intern<T>
{
    type Weak = ();
    fn dupe_strong(&self) -> Self {
        *self
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        self.pointer as *const _ as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        // TODO(ctolliday): Since we never drop interned things, we could have Self::Weak = Self and return a value here
        None
    }

    fn serialize_inner(&self, ser: &mut dyn PagableSerializer) -> pagable::Result<()> {
        T::pagable_serialize(&self, ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de> + ?Sized>(
        deser: &mut D,
    ) -> pagable::Result<Self> {
        let interner = T::interner();
        let val = T::pagable_deserialize(deser)?;
        Ok(interner.intern(val))
    }
}

impl<
    T: PagableSerialize
        + for<'de> PagableDeserialize<'de>
        + std::fmt::Debug
        + Send
        + Sync
        + Internable<Hasher = H>
        + Eq
        + Hash
        + 'static,
    H: Hasher + Default + 'static,
> PagableSerialize for Intern<T>
{
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        serializer.serialize_arc(self)
    }
}

impl<
    'de,
    T: PagableSerialize
        + for<'a> PagableDeserialize<'a>
        + std::fmt::Debug
        + Hash
        + Send
        + Sync
        + Internable<Hasher = H>
        + Eq
        + std::any::Any,
    H: Hasher + Default + 'static,
> PagableDeserialize<'de> for Intern<T>
{
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        deserialize_arc::<Self, _>(deserializer)
    }
}

// TODO(nga): derive.
impl<T: Allocative> Allocative for Intern<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if mem::size_of::<T>() > 0 {
            let visitor = visitor.enter_shared(
                allocative::Key::new("pointer"),
                mem::size_of::<*const T>(),
                &**self as &T as *const T as *const (),
            );
            if let Some(mut visitor) = visitor {
                (**self).visit(&mut visitor);
                visitor.exit();
            }
        }
    }
}

impl<T: 'static> Copy for Intern<T> {}

impl<T: 'static> Clone for Intern<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: 'static> Dupe for Intern<T> {
    #[inline]
    fn dupe(&self) -> Self {
        *self
    }
}

impl<T: 'static> Deref for Intern<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.pointer.data
    }
}

impl<T: 'static> Intern<T> {
    #[inline]
    pub const fn deref_static(&self) -> &'static T {
        &self.pointer.data
    }

    /// SAFETY: This may only be called with pointers returned from [`Self::deref_static`]
    #[inline]
    pub const unsafe fn from_ptr(p: *const T) -> Self {
        // SAFETY: `p` is a pointer to the `data` field of an `InternedData<T>`
        unsafe {
            let p = p
                .cast::<u8>()
                .sub(std::mem::offset_of!(InternedData<T>, data))
                .cast::<InternedData<T>>();
            Self { pointer: &*p }
        }
    }
}

impl<T> Hash for Intern<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We could hash only the pointer, since we only compare the pointers,
        // but users may expect hashing to be stable between runs.
        self.pointer.hash.hash(state);
    }
}

impl<T> PartialEq for Intern<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.pointer, other.pointer)
    }
}

impl<T> Eq for Intern<T> {}

impl<T: PartialOrd> PartialOrd for Intern<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.pointer.data.partial_cmp(&other.pointer.data)
    }
}

impl<T: Ord> Ord for Intern<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.pointer.data.cmp(&other.pointer.data)
    }
}

impl<T: Display> Display for Intern<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.pointer.data, f)
    }
}

/// Hash the value before acquiring the lock.
struct Hashed<T, H> {
    hash: u64,
    value: T,
    _marker: PhantomData<H>,
}

impl<T: Hash, H: Hasher + Default> Hashed<T, H> {
    /// Compute the hash.
    fn hash(value: &T) -> u64 {
        let mut hasher = H::default();
        value.hash(&mut hasher);
        hasher.finish()
    }

    fn new(value: T) -> Self {
        let hash = Self::hash(&value);
        Hashed {
            hash,
            value,
            _marker: PhantomData,
        }
    }
}

impl<T: 'static, H> Interner<T, H> {
    /// Create a new interner for given type.
    pub const fn new() -> Interner<T, H> {
        Interner {
            table: ShardedLockFreeRawTable::new(),
            _marker: PhantomData,
        }
    }
}

pub enum InternDisposition {
    /// The value was already interned.
    Interned,
    /// The value was computed and interned.
    Computed,
}

impl<T: 'static, H: Hasher + Default> Interner<T, H> {
    /// Allocate a value, or return previously allocated one as well as whether that value
    /// was computed or already interned.
    pub fn observed_intern<Q>(&'static self, value: Q) -> (Intern<T>, InternDisposition)
    where
        Q: Hash + Equivalent<T> + Into<T>,
        T: Eq + Hash,
    {
        let hashed = Hashed::<_, H>::new(value);
        if let Some(pointer) = self
            .table
            .lookup(hashed.hash, |t| hashed.value.equivalent(&t.data))
        {
            return (Intern { pointer }, InternDisposition::Interned);
        }

        self.intern_slow(hashed)
    }

    /// Allocate a value, or return previously allocated one.
    pub fn intern<Q>(&'static self, value: Q) -> Intern<T>
    where
        Q: Hash + Equivalent<T> + Into<T>,
        T: Eq + Hash,
    {
        let (intern, _disposition) = self.observed_intern(value);
        intern
    }

    #[cold]
    fn intern_slow<Q>(&'static self, hashed_value: Hashed<Q, H>) -> (Intern<T>, InternDisposition)
    where
        Q: Hash + Equivalent<T> + Into<T>,
        T: Eq + Hash,
    {
        let pointer = Box::new(InternedData {
            data: hashed_value.value.into(),
            hash: hashed_value.hash,
        });
        let pointer = self
            .table
            .insert(
                hashed_value.hash,
                pointer,
                |a, b| a.hash == b.hash && a.data == b.data,
                |t| t.hash,
            )
            .0;
        (Intern { pointer }, InternDisposition::Computed)
    }

    /// Get a value if it has been interned.
    pub fn get<Q>(&'static self, key: Q) -> Option<Intern<T>>
    where
        Q: Hash + Equivalent<T>,
        T: Eq + Hash,
    {
        let hashed = Hashed::<_, H>::new(key);
        self.table
            .lookup(hashed.hash, |t| hashed.value.equivalent(&t.data))
            .map(|pointer| Intern { pointer })
    }

    /// Iterate over the interned values.
    #[inline]
    pub fn iter(&'static self) -> Iter<T, H> {
        Iter {
            iter: self.table.iter(),
            _marker: PhantomData,
        }
    }
}

pub struct Iter<T: 'static, H: 'static> {
    iter: lock_free_hashtable::sharded::Iter<'static, Box<InternedData<T>>, 64>,
    _marker: PhantomData<H>,
}

impl<T: 'static, H: 'static> Iterator for Iter<T, H> {
    type Item = Intern<T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|pointer| Intern { pointer })
    }
}

/// Define a static interner and implement Internable to associate it with an interned type.
///
/// Without additional arguments, `interner!(STRING_INTERNER, DefaultHasher, InternString)`
/// creates a `static STRING_INTERNER: Interner<InternString, DefaultHasher>` and implements Internable for InternString.
///
/// Additional arguments are used to implement convenience traits when the interned type wraps an existing type.
/// `interner!(STRING_INTERNER, DefaultHasher, InternString, String, str, StrRef)`
/// also implements
///     ``Equivalent<InternString>`` for `String`
///     ``From<String>``for `InternString`
///     `Deref<Target = str>` for `InternString`
/// and creates a new `StrRef<'a>(&'a str)` type that implements ``Equivalent<InternString>`` and ``From<StrRef>`` for InternString.
#[macro_export]
macro_rules! interner {
    ($interner_name:ident, $hasher:ty, $type:ty) => {
        static $interner_name: $crate::Interner<$type, $hasher> = $crate::Interner::new();

        impl $crate::Internable for $type {
            type Hasher = $hasher;

            fn interner() -> &'static $crate::Interner<Self, $hasher> {
                &$interner_name
            }
        }
    };
    ($interner_name:ident, $hasher:ty, $newtype:ident, $basetype:ty) => {
        impl $crate::Equivalent<$newtype> for $basetype {
            fn equivalent(&self, key: &$newtype) -> bool {
                self == &key.0
            }
        }

        impl From<$basetype> for $newtype {
            fn from(value: $basetype) -> Self {
                $newtype(value)
            }
        }

        $crate::interner!($interner_name, $hasher, $newtype);
    };
    ($interner_name:ident, $hasher:ty, $newtype:ident, $basetype:ty, $basereftype:ty) => {
        impl std::ops::Deref for $newtype {
            type Target = $basereftype;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        $crate::interner!($interner_name, $hasher, $newtype, $basetype);
    };
    ($interner_name:ident, $hasher:ty, $newtype:ident, $basetype:ty, $basereftype:ty, $newreftype:ident) => {
        #[derive(Hash, Eq, PartialEq)]
        pub struct $newreftype<'a>(&'a $basereftype);

        impl $crate::Equivalent<$newtype> for $newreftype<'_> {
            fn equivalent(&self, key: &$newtype) -> bool {
                self.0 == key.0
            }
        }

        impl From<$newreftype<'_>> for $newtype {
            fn from(value: $newreftype<'_>) -> Self {
                $newtype(value.0.into())
            }
        }

        $crate::interner!($interner_name, $hasher, $newtype, $basetype, $basereftype);
    };
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::collections::hash_map::DefaultHasher;

    use crate::Intern;
    use crate::InternDisposition;
    use crate::interner;

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct StringValue(String);
    interner!(
        STRING_INTERNER,
        DefaultHasher,
        StringValue,
        String,
        str,
        StrRef
    );

    #[test]
    fn test_intern() {
        assert_eq!(
            STRING_INTERNER.intern("hello".to_owned()),
            STRING_INTERNER.intern("hello".to_owned())
        );
        assert_eq!(
            STRING_INTERNER.intern("hello".to_owned()),
            STRING_INTERNER.intern(StrRef("hello")),
        );
        assert_ne!(
            STRING_INTERNER.intern("hello".to_owned()),
            STRING_INTERNER.intern("world".to_owned())
        );
    }

    // Make sure things work with reallocation.
    #[test]
    fn test_resize() {
        let mut interned_strings = Vec::new();
        for i in 0..100000 {
            let s = i.to_string();
            let interned = STRING_INTERNER.intern(s.clone());
            assert_eq!(&s, &*interned.0);
            interned_strings.push(interned);
        }

        for s in &interned_strings {
            let interned = STRING_INTERNER.intern(String::clone(&s.0));
            assert_eq!(*s, interned);
        }
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestDispositionValue(String);
    interner!(
        TEST_DISPOSITION_INTERNER,
        DefaultHasher,
        TestDispositionValue,
        String
    );
    #[test]
    fn test_disposition() {
        let (val, disposition) = TEST_DISPOSITION_INTERNER.observed_intern("hello".to_owned());
        assert_eq!(val.0.to_string(), "hello".to_owned());
        assert!(std::matches!(disposition, InternDisposition::Computed));

        let (val, disposition) = TEST_DISPOSITION_INTERNER.observed_intern("hello".to_owned());
        assert_eq!(val.0.to_string(), "hello".to_owned());
        assert!(std::matches!(disposition, InternDisposition::Interned));
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestGetValue(String);
    interner!(
        TEST_GET_INTERNER,
        DefaultHasher,
        TestGetValue,
        String,
        str,
        TestGetRef
    );
    #[test]
    fn test_get() {
        let interner = &TEST_GET_INTERNER;
        assert_eq!(interner.get(TestGetRef("hello")), None);
        assert_eq!(interner.get("hello".to_owned()), None);

        let interned = interner.intern("hello".to_owned());
        assert_eq!(interner.get(TestGetRef("hello")), Some(interned));
        assert_eq!(interner.get("hello".to_owned()), Some(interned));
        assert_eq!(interner.get(TestGetRef("world")), None);
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestIterValue(&'static str);
    interner!(
        TEST_ITER_INTERNER,
        DefaultHasher,
        TestIterValue,
        &'static str
    );
    #[test]
    fn test_iter() {
        let interner = &TEST_ITER_INTERNER;
        assert_eq!(
            interner
                .iter()
                .map(|v| v.0)
                .collect::<BTreeSet<&'static str>>(),
            BTreeSet::from([])
        );
        interner.intern("hello");
        interner.intern("cat");
        interner.intern("world");

        assert_eq!(
            interner
                .iter()
                .map(|v| v.0)
                .collect::<BTreeSet<&'static str>>(),
            BTreeSet::from(["hello", "cat", "world"])
        );
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct TestPointerValue(&'static str);
    interner!(
        TEST_POINTER_INTERNER,
        DefaultHasher,
        TestPointerValue,
        &'static str
    );
    #[test]
    fn test_pointer_roundtrip() {
        let one = TEST_POINTER_INTERNER.intern("one");
        let one_p = one.deref_static() as *const _;
        assert_eq!(one, unsafe { Intern::from_ptr(one_p) });
    }
}

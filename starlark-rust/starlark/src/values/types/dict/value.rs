/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::any::TypeId;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use display_container::display_keyed_container;
use gazebo::cell::ARef;
use serde::Serialize;
use starlark_map::small_map;
use starlark_map::Equivalent;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::coerce;
use crate::coerce::Coerce;
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::environment::Methods;
use crate::environment::MethodsStatic;
use crate::hint::unlikely;
use crate::values::comparison::equals_small_map;
use crate::values::dict::DictOf;
use crate::values::dict::DictRef;
use crate::values::error::ValueError;
use crate::values::layout::avalue::VALUE_EMPTY_FROZEN_DICT;
use crate::values::string::hash_string_value;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::StringValue;
use crate::values::Trace;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(
    Clone,
    Default,
    Trace,
    Debug,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(builtin = "standard")]
pub(crate) struct DictGen<T>(pub(crate) T);

impl<'v, T: DictLike<'v>> Display for DictGen<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(f, "{", "}", ": ", self.0.content().iter())
    }
}

impl<'v> Display for Dict<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(f, "{", "}", ": ", self.iter())
    }
}

/// Define the dict type.
#[derive(Clone, Default, Trace, Debug, ProvidesStaticType, Allocative)]
#[repr(transparent)]
pub struct Dict<'v> {
    /// The data stored by the dictionary. The keys must all be hashable values.
    content: SmallMap<Value<'v>, Value<'v>>,
}

impl<'v> StarlarkTypeRepr for Dict<'v> {
    fn starlark_type_repr() -> String {
        DictOf::<Value<'v>, Value<'v>>::starlark_type_repr()
    }
}

#[derive(Clone, Default, Debug, ProvidesStaticType, Allocative)]
#[repr(transparent)]
pub(crate) struct FrozenDictData {
    /// The data stored by the dictionary. The keys must all be hashable values.
    pub(crate) content: SmallMap<FrozenValue, FrozenValue>,
}

/// Alias is used in `StarlarkDocs` derive.
type FrozenDict = DictGen<FrozenDictData>;

unsafe impl<'v> Coerce<Dict<'v>> for FrozenDictData {}

impl<'v> AllocValue<'v> for Dict<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(DictGen(RefCell::new(self)))
    }
}

impl AllocFrozenValue for FrozenDictData {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        if self.content.is_empty() {
            FrozenValue::new_repr(&VALUE_EMPTY_FROZEN_DICT)
        } else {
            heap.alloc_simple(DictGen(self))
        }
    }
}

impl<'v> Dict<'v> {
    pub(crate) fn is_dict_type(x: TypeId) -> bool {
        x == TypeId::of::<DictGen<FrozenDictData>>()
            || x == TypeId::of::<DictGen<RefCell<Dict<'static>>>>()
    }

    pub(crate) unsafe fn from_value_unchecked_mut(x: Value<'v>) -> RefMut<'v, Self> {
        let dict = &x.downcast_ref_unchecked::<DictGen<RefCell<Dict<'v>>>>().0;
        dict.borrow_mut()
    }
}

/// Helper type for lookups, not useful.
#[derive(Eq, PartialEq)]
pub struct ValueStr<'a>(pub(crate) &'a str);

impl<'a> Hash for ValueStr<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_string_value(self.0, state)
    }
}

impl<'v> Equivalent<Value<'v>> for ValueStr<'_> {
    fn equivalent(&self, key: &Value<'v>) -> bool {
        key.unpack_str() == Some(self.0)
    }
}

impl Equivalent<FrozenValue> for ValueStr<'_> {
    fn equivalent(&self, key: &FrozenValue) -> bool {
        key.unpack_str() == Some(self.0)
    }
}

impl<'v> Dict<'v> {
    /// The result of calling `type()` on dictionaries.
    pub const TYPE: &'static str = "dict";

    /// Dict type string as Starlark frozen string value.
    pub fn get_type_value_static() -> FrozenStringValue {
        DictGen::<FrozenDictData>::get_type_value_static()
    }

    /// Create a new [`Dict`].
    pub fn new(content: SmallMap<Value<'v>, Value<'v>>) -> Self {
        Self { content }
    }

    /// Number of elements in the dict.
    pub fn len(&self) -> usize {
        self.content.len()
    }

    /// Is the dict empty?
    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }

    /// Iterate through the key/value pairs in the dictionary.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = (Value<'v>, Value<'v>)> + 'a {
        self.content.iter().map(|(l, r)| (*l, *r))
    }

    /// Iterate through the key/value pairs in the dictionary, but retaining the hash of the keys.
    pub fn iter_hashed<'a>(&'a self) -> impl Iterator<Item = (Hashed<Value<'v>>, Value<'v>)> + 'a
    where
        'v: 'a,
    {
        self.content.iter_hashed().map(|(l, r)| (l.copied(), *r))
    }

    /// Iterator over keys.
    pub fn keys<'a>(&'a self) -> impl Iterator<Item = Value<'v>> + 'a {
        self.content.keys().copied()
    }

    /// Iterator over keys.
    pub fn values<'a>(&'a self) -> impl Iterator<Item = Value<'v>> + 'a {
        self.content.values().copied()
    }

    /// Get the value associated with a particular key. Will be [`Err`] if the key is not hashable,
    /// and otherwise [`Some`] if the key exists in the dictionary and [`None`] otherwise.
    pub fn get(&self, key: Value<'v>) -> anyhow::Result<Option<Value<'v>>> {
        Ok(self.get_hashed(key.get_hashed()?))
    }

    /// Lookup the value by the given prehashed key.
    pub fn get_hashed(&self, key: Hashed<Value<'v>>) -> Option<Value<'v>> {
        self.content.get_hashed_by_value(key).copied()
    }

    /// Get the value associated with a particular string. Equivalent to allocating the
    /// string on the heap, turning it into a value, and looking up using that.
    pub fn get_str(&self, key: &str) -> Option<Value<'v>> {
        self.content.get(&ValueStr(key)).copied()
    }

    /// Like [`Dict::get_str`], but where you already have the hash.
    pub fn get_str_hashed(&self, key: Hashed<&str>) -> Option<Value<'v>> {
        self.content
            .get_hashed_by_value(Hashed::new_unchecked(key.hash(), ValueStr(key.key())))
            .copied()
    }

    /// Try to coerce all keys to strings.
    pub(crate) fn downcast_ref_key_string(&self) -> Option<&SmallMap<StringValue<'v>, Value<'v>>> {
        for &key in self.content.keys() {
            if unlikely(!key.is_str()) {
                return None;
            }
        }

        // Scary part: `SmallMap` has the same repr for `Value` and `StringValue`,
        // and we just checked above that all keys are strings.

        fn _assert_coerce<'v>(
            s: SmallMap<StringValue<'v>, Value<'v>>,
        ) -> SmallMap<Value<'v>, Value<'v>> {
            coerce(s)
        }

        Some(unsafe {
            transmute!(&SmallMap<Value, Value>, &SmallMap<StringValue, Value>, &self.content)
        })
    }

    /// Reserve capacity to insert `additional` elements without reallocating.
    pub fn reserve(&mut self, additional: usize) {
        self.content.reserve(additional);
    }

    /// Insert a key/value pair into the dictionary.
    pub fn insert_hashed(&mut self, key: Hashed<Value<'v>>, value: Value<'v>) {
        self.content.insert_hashed(key, value);
    }

    /// Remove given key from the dictionary.
    pub fn remove_hashed(&mut self, key: Hashed<Value<'v>>) -> Option<Value<'v>> {
        self.content.remove_hashed(key.as_ref())
    }

    /// Remove all elements from the dictionary.
    pub fn clear(&mut self) {
        self.content.clear();
    }
}

impl FrozenDictData {
    /// Iterate through the key/value pairs in the dictionary.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = (FrozenValue, FrozenValue)> + 'a {
        self.content.iter().map(|(l, r)| (*l, *r))
    }

    /// Get the value associated with a particular string. Equivalent to allocating the
    /// string on the heap, turning it into a value, and looking up using that.
    pub fn get_str(&self, key: &str) -> Option<FrozenValue> {
        self.content.get(&ValueStr(key)).copied()
    }
}

impl<'v> Freeze for DictGen<RefCell<Dict<'v>>> {
    type Frozen = DictGen<FrozenDictData>;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let content = self.0.into_inner().content.freeze(freezer)?;
        Ok(DictGen(FrozenDictData { content }))
    }
}

trait DictLike<'v>: Debug + Allocative {
    fn content(&self) -> ARef<SmallMap<Value<'v>, Value<'v>>>;
    fn content_iter<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a>;
    fn set_at(&self, index: Hashed<Value<'v>>, value: Value<'v>) -> anyhow::Result<()>;
}

impl<'v> DictLike<'v> for RefCell<Dict<'v>> {
    fn content(&self) -> ARef<SmallMap<Value<'v>, Value<'v>>> {
        ARef::new_ref(Ref::map(self.borrow(), |x| &x.content))
    }

    fn content_iter<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a> {
        struct IterImpl<'a, 'v> {
            /// Keep the dict borrowed so that it won't be modified while we iterate.
            _dict: Ref<'a, Dict<'v>>,
            iter: small_map::Keys<'a, Value<'v>, Value<'v>>,
        }

        impl<'a, 'v> Iterator for IterImpl<'a, 'v> {
            type Item = Value<'v>;

            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next().copied()
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }
        }

        let dict = self.borrow();
        // Drop the lifetime: we need to return the iterator while borrowing the dict.
        let iter = unsafe { &*(&dict.content as *const SmallMap<Value, Value>) }.keys();
        Box::new(IterImpl { _dict: dict, iter })
    }

    fn set_at(&self, index: Hashed<Value<'v>>, alloc_value: Value<'v>) -> anyhow::Result<()> {
        match self.try_borrow_mut() {
            Ok(mut xs) => {
                xs.content.insert_hashed(index, alloc_value);
                Ok(())
            }
            Err(_) => Err(ValueError::MutationDuringIteration.into()),
        }
    }
}

impl<'v> DictLike<'v> for FrozenDictData {
    fn content(&self) -> ARef<SmallMap<Value<'v>, Value<'v>>> {
        ARef::new_ptr(coerce(&self.content))
    }

    fn content_iter<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a> {
        Box::new(self.content.keys().copied().map(|v| v.to_value()))
    }

    fn set_at(&self, _index: Hashed<Value<'v>>, _value: Value<'v>) -> anyhow::Result<()> {
        Err(ValueError::CannotMutateImmutableValue.into())
    }
}

pub(crate) fn dict_methods() -> Option<&'static Methods> {
    static RES: MethodsStatic = MethodsStatic::new();
    RES.methods(crate::stdlib::dict::dict_methods)
}

impl<'v, T: DictLike<'v> + 'v> StarlarkValue<'v> for DictGen<T>
where
    Self: ProvidesStaticType,
{
    starlark_type!(Dict::TYPE);

    fn get_methods() -> Option<&'static Methods> {
        dict_methods()
    }

    fn collect_repr(&self, r: &mut String) {
        // Fast path as repr() for dicts is quite hot
        r.push('{');
        for (i, (name, value)) in self.0.content().iter().enumerate() {
            if i != 0 {
                r.push_str(", ");
            }
            name.collect_repr(r);
            r.push_str(": ");
            value.collect_repr(r);
        }
        r.push('}');
    }

    fn collect_repr_cycle(&self, collector: &mut String) {
        collector.push_str("{...}");
    }

    fn to_bool(&self) -> bool {
        !self.0.content().is_empty()
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match DictRef::from_value(other) {
            None => Ok(false),
            Some(other) => {
                equals_small_map(&*self.0.content(), &other.content, |x, y| x.equals(*y))
            }
        }
    }

    fn at(&self, index: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self.0.content().get_hashed_by_value(index.get_hashed()?) {
            Some(v) => Ok(v.to_value()),
            None => Err(ValueError::KeyNotFound(index.to_repr()).into()),
        }
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.0.content().len() as i32)
    }

    fn is_in(&self, other: Value<'v>) -> anyhow::Result<bool> {
        Ok(self
            .0
            .content()
            .contains_key_hashed_by_value(other.get_hashed()?))
    }

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(self.0.content_iter())
    }

    fn with_iterator(
        &self,
        _heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut self.0.content().keys().copied())
    }

    fn set_at(&self, index: Value<'v>, alloc_value: Value<'v>) -> anyhow::Result<()> {
        let index = index.get_hashed()?;
        self.0.set_at(index, alloc_value)
    }

    fn bit_or(&self, rhs: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = DictRef::from_value(rhs)
            .map_or_else(|| ValueError::unsupported_with(self, "|", rhs), Ok)?;
        if self.0.content().is_empty() {
            return Ok(heap.alloc(rhs.clone()));
        }
        // Might be faster if we preallocate the capacity, but then copying in the LHS
        // is more expensive and might oversize given the behaviour on duplicates.
        // If this becomes a bottleneck, benchmark.
        let mut items = self.0.content().clone();
        for (k, v) in rhs.iter_hashed() {
            items.insert_hashed(k, v);
        }
        Ok(heap.alloc(Dict::new(items)))
    }
}

impl<'v, T: DictLike<'v>> Serialize for DictGen<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_map(self.0.content().iter())
    }
}

impl<'v, K: UnpackValue<'v> + Hash + Eq, V: UnpackValue<'v>> StarlarkTypeRepr for SmallMap<K, V> {
    fn starlark_type_repr() -> String {
        DictOf::<K, V>::starlark_type_repr()
    }
}

impl<'v, K: UnpackValue<'v> + Hash + Eq, V: UnpackValue<'v>> UnpackValue<'v> for SmallMap<K, V> {
    fn expected() -> String {
        format!("dict mapping {} to {}", K::expected(), V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let dict = DictRef::from_value(value)?;
        let mut r = SmallMap::new();
        for (k, v) in dict.content.iter() {
            r.insert(K::unpack_value(*k)?, V::unpack_value(*v)?);
        }
        Some(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert;
    use crate::collections::SmallMap;
    use crate::values::Heap;

    #[test]
    fn test_mutate_dict() {
        assert::is_true(
            r#"
x = {1: 2, 2: 4}
b1 = str(x) == "{1: 2, 2: 4}"
x[2] = 3
b2 = str(x) == "{1: 2, 2: 3}"
x[(3,4)] = 5
b3 = str(x) == "{1: 2, 2: 3, (3, 4): 5}"
b1 and b2 and b3
"#,
        );
    }

    #[test]
    fn test_get_str() -> anyhow::Result<()> {
        let heap = Heap::new();
        let k1 = heap.alloc_str("hello").get_hashed();
        let k2 = heap.alloc_str("world").get_hashed();
        let mut sm = SmallMap::new();
        sm.insert_hashed(k1, Value::new_int(12));
        sm.insert_hashed(k2, Value::new_int(56));
        let d = Dict::new(coerce(sm));

        assert_eq!(d.get(heap.alloc("hello"))?.unwrap().unpack_int(), Some(12));
        assert_eq!(d.get(heap.alloc("foo"))?, None);
        assert_eq!(d.get_str("hello").unwrap().unpack_int(), Some(12));
        assert_eq!(d.get_str("foo"), None);
        Ok(())
    }

    #[test]
    fn test_repr_cycle() {
        assert::eq("d = {}; d[17] = d; repr(d)", "'{17: {...}}'");
        assert::eq("d = {}; d[17] = d; str(d)", "'{17: {...}}'");
    }
}

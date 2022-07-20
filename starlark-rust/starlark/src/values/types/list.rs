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

//! The list type, a mutable sequence of values.

use std::any::TypeId;
use std::cell::Cell;
use std::cmp;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::intrinsics::likely;
use std::intrinsics::unlikely;
use std::marker::PhantomData;
use std::ops::Deref;
use std::slice;

use gazebo::any::ProvidesStaticType;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::display::display_container;
use gazebo::prelude::*;
use serde::Serialize;

use crate::environment::Methods;
use crate::environment::MethodsStatic;
use crate::private::Private;
use crate::values::array::Array;
use crate::values::comparison::compare_slice;
use crate::values::comparison::equals_slice;
use crate::values::error::ValueError;
use crate::values::index::apply_slice;
use crate::values::index::convert_index;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::ValueTyped;
use crate::{self as starlark};

#[derive(Clone, Default, Trace, Debug, ProvidesStaticType)]
#[repr(transparent)]
pub(crate) struct ListGen<T>(pub(crate) T);

/// Define the list type. See [`List`] and [`FrozenList`] as the two possible representations.
#[derive(Trace, Debug, ProvidesStaticType)]
pub struct List<'v> {
    /// The data stored by the list.
    pub(crate) content: Cell<ValueTyped<'v, Array<'v>>>,
}

/// Define the list type. See [`List`] and [`FrozenList`] as the two possible representations.
#[derive(ProvidesStaticType)]
#[repr(C)]
pub struct FrozenList {
    len: usize,
    /// The data stored by the tuple.
    content: [FrozenValue; 0],
}

impl Debug for FrozenList {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("FrozenList")
            .field("content", &self.content())
            .finish()
    }
}

impl ListGen<FrozenList> {
    pub(crate) fn offset_of_content() -> usize {
        memoffset::offset_of!(FrozenList, content)
    }
}

/// Reference to list content.
#[repr(transparent)]
#[derive(Coerce)]
pub struct ListRef<'v> {
    content: [Value<'v>],
}

impl<'v> ListRef<'v> {
    fn new<'a>(slice: &'a [Value<'v>]) -> &'a ListRef<'v> {
        coerce(slice)
    }

    /// List elements.
    pub fn content(&self) -> &[Value<'v>] {
        &self.content
    }

    /// Iterate over the elements in the list.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.content.iter().copied()
    }
}

impl<'v> List<'v> {
    /// Downcast the list.
    pub fn from_value(x: Value<'v>) -> Option<&'v ListRef<'v>> {
        if x.unpack_frozen().is_some() {
            x.downcast_ref::<ListGen<FrozenList>>()
                .map(|x| ListRef::new(coerce(x.0.content())))
        } else {
            let ptr = x.downcast_ref::<ListGen<List>>()?;
            Some(ListRef::new(ptr.0.content()))
        }
    }

    #[inline]
    pub(crate) fn from_value_mut(x: Value<'v>) -> anyhow::Result<&'v Self> {
        #[derive(thiserror::Error, Debug)]
        #[error("Value is not list, value type: `{0}`")]
        struct NotListError(&'static str);

        #[cold]
        #[inline(never)]
        fn error<'v>(x: Value<'v>) -> anyhow::Error {
            if x.downcast_ref::<ListGen<FrozenList>>().is_some() {
                ValueError::CannotMutateImmutableValue.into()
            } else {
                NotListError(x.get_type()).into()
            }
        }

        if let Some(x) = x.downcast_ref::<ListGen<List<'v>>>() {
            x.0.check_can_mutate()?;
            Ok(&x.0)
        } else {
            Err(error(x))
        }
    }

    pub(crate) unsafe fn from_value_unchecked_mut(x: Value<'v>) -> &'v Self {
        let list = x.downcast_ref_unchecked::<ListGen<List<'v>>>();
        debug_assert!(list.0.check_can_mutate().is_ok());
        &list.0
    }

    pub(crate) fn is_list_type(x: TypeId) -> bool {
        x == TypeId::of::<ListGen<List>>() || x == TypeId::of::<ListGen<FrozenList>>()
    }

    /// Return an error if there's at least one iterator over the list.
    fn check_can_mutate(&self) -> anyhow::Result<()> {
        if unlikely(self.content.get().as_ref().iter_count_is_non_zero()) {
            return Err(ValueError::MutationDuringIteration.into());
        }
        Ok(())
    }

    #[cold]
    #[inline(never)]
    fn reserve_additional_slow(&self, additional: usize, heap: &'v Heap) {
        let new_cap = cmp::max(self.len() + additional, self.len() * 2);
        // Size of `Array` is 2 words and size of `List` is one word,
        // so allocating at least 4 words would not be too large waste.
        // Note `Vec` allocates 4 by default.
        // Also note `Array` removes extra capacity on GC.
        let new_cap = cmp::max(new_cap, 4);

        let new_array = heap.alloc_array(new_cap);
        new_array.extend_from_slice(self.content());
        self.content.set(new_array);
    }

    #[inline(always)]
    fn reserve_additional(&self, additional: usize, heap: &'v Heap) {
        if likely(self.content.get().as_ref().remaining_capacity() >= additional) {
            return;
        }

        self.reserve_additional_slow(additional, heap);
    }

    pub(crate) fn double(&self, heap: &'v Heap) {
        self.reserve_additional(self.len(), heap);
        self.content.get().double();
    }

    #[inline]
    pub(crate) fn extend<I: IntoIterator<Item = Value<'v>>>(&self, iter: I, heap: &'v Heap) {
        let iter = iter.into_iter();
        let (lo, hi) = iter.size_hint();
        match hi {
            Some(hi) if lo == hi => {
                // Exact size iterator.
                self.reserve_additional(lo, heap);
                // Extend will panic if upper bound is provided incorrectly.
                self.content.get().extend(iter);
            }
            Some(hi) if self.content.get().remaining_capacity() >= hi => {
                // Enough capacity for upper bound.
                // Extend will panic if upper bound is provided incorrectly.
                self.content.get().extend(iter);
            }
            _ => {
                // Default slow version.
                self.reserve_additional(iter.size_hint().0, heap);
                for item in iter {
                    self.push(item, heap);
                }
            }
        }
    }

    pub(crate) fn push(&self, value: Value<'v>, heap: &'v Heap) {
        self.reserve_additional(1, heap);
        self.content.get().push(value);
    }

    pub(crate) fn clear(&self) {
        self.content.get().clear();
    }

    pub(crate) fn insert(&self, index: usize, value: Value<'v>, heap: &'v Heap) {
        self.reserve_additional(1, heap);
        self.content.get().insert(index, value);
    }

    pub(crate) fn remove(&self, index: usize) -> Value<'v> {
        self.content.get().remove(index)
    }
}

impl<'v> Deref for ListRef<'v> {
    type Target = [Value<'v>];

    fn deref(&self) -> &[Value<'v>] {
        &self.content
    }
}

impl<'v, V: AllocValue<'v>> AllocValue<'v> for Vec<V> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_list_iter(self.into_map(|x| x.alloc_value(heap)))
    }
}

impl<V: AllocFrozenValue> AllocFrozenValue for Vec<V> {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_list(&self.into_map(|x| x.alloc_frozen_value(heap)))
    }
}

impl<'a, 'v, V: 'a> StarlarkTypeRepr for &'a [V]
where
    &'a V: AllocValue<'v> + StarlarkTypeRepr,
{
    fn starlark_type_repr() -> String {
        Vec::<&'a V>::starlark_type_repr()
    }
}

impl<'a, 'v, V: 'a> AllocValue<'v> for &'a [V]
where
    &'a V: AllocValue<'v>,
{
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_list_iter(self.iter().map(|x| x.alloc_value(heap)))
    }
}

impl<'a, V: 'a> AllocFrozenValue for &'a [V]
where
    &'a V: AllocFrozenValue,
{
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_list(&self.map(|x| x.alloc_frozen_value(heap)))
    }
}

impl FrozenList {
    /// Utility to allocate an empty frozen list.
    pub fn empty() -> impl AllocFrozenValue {
        struct EmptyFrozenList;

        impl AllocFrozenValue for EmptyFrozenList {
            fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
                heap.alloc_list(&[])
            }
        }

        EmptyFrozenList
    }

    pub(crate) const unsafe fn new(len: usize) -> FrozenList {
        FrozenList { len, content: [] }
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(crate) fn content(&self) -> &[FrozenValue] {
        unsafe { slice::from_raw_parts(self.content.as_ptr(), self.len) }
    }

    /// Obtain the [`FrozenList`] pointed at by a [`FrozenValue`].
    #[allow(clippy::trivially_copy_pass_by_ref)]
    // We need a lifetime because FrozenValue doesn't contain the right lifetime
    pub fn from_frozen_value(x: &FrozenValue) -> Option<&FrozenList> {
        x.downcast_ref::<ListGen<FrozenList>>().map(|x| &x.0)
    }
}

impl<'v> List<'v> {
    /// The result of calling `type()` on lists.
    pub const TYPE: &'static str = "list";

    /// Type of list as frozen string value.
    pub fn get_type_value_static() -> FrozenStringValue {
        ListGen::<FrozenList>::get_type_value_static()
    }

    pub(crate) fn new(content: ValueTyped<'v, Array<'v>>) -> Self {
        List {
            content: Cell::new(content),
        }
    }

    /// Obtain the length of the list.
    pub fn len(&self) -> usize {
        self.content.get().len()
    }

    /// List content.
    ///
    /// Note this operation does not prevent mutation of this list while
    /// holding the slice. But such mutation does not violate memory-safety.
    pub fn content(&self) -> &[Value<'v>] {
        self.content.get().as_ref().content()
    }

    /// Iterate over the elements in the list.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.content.get().as_ref().iter()
    }
}

impl<'v> Display for List<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_list(self.content.get().content(), f)
    }
}

impl Display for FrozenList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_list(coerce(&self.content()), f)
    }
}

impl<'v> Display for ListRef<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_list(&self.content, f)
    }
}

impl FrozenList {
    /// Iterate over the elements in the list.
    pub fn iter<'a, 'v>(&'a self) -> impl ExactSizeIterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.content().iter().map(|e| e.to_value())
    }
}

// This trait need to be `pub(crate)` because `ListGen<T>` is.
pub(crate) trait ListLike<'v>: Debug {
    fn content(&self) -> &[Value<'v>];
    fn set_at(&self, i: usize, v: Value<'v>) -> anyhow::Result<()>;
    fn iterate<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a>
    where
        'v: 'a;
    fn with_iterator(
        &self,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()>;
}

impl<'v> ListLike<'v> for List<'v> {
    fn content(&self) -> &[Value<'v>] {
        self.content.get().as_ref().content()
    }

    fn set_at(&self, i: usize, v: Value<'v>) -> anyhow::Result<()> {
        self.check_can_mutate()?;
        self.content.get().set_at(i, v);
        Ok(())
    }

    fn iterate<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a>
    where
        'v: 'a,
    {
        box self.content.get().as_ref().iter()
    }

    fn with_iterator(
        &self,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut self.content.get().iter())
    }
}

impl<'v> ListLike<'v> for FrozenList {
    fn content(&self) -> &[Value<'v>] {
        coerce(self.content())
    }

    fn set_at(&self, _i: usize, _v: Value<'v>) -> anyhow::Result<()> {
        Err(ValueError::CannotMutateImmutableValue.into())
    }

    fn iterate<'a>(&'a self) -> Box<dyn Iterator<Item = Value<'v>> + 'a>
    where
        'v: 'a,
    {
        box coerce(self.content()).iter().copied()
    }

    fn with_iterator(
        &self,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut coerce(self.content()).iter().copied())
    }
}

impl<T: Display> Display for ListGen<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

pub(crate) fn display_list(xs: &[Value], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    display_container(f, "[", "]", xs.iter())
}

pub(crate) fn list_methods() -> Option<&'static Methods> {
    static RES: MethodsStatic = MethodsStatic::new();
    RES.methods(crate::stdlib::list::list_methods)
}

impl<'v, T: ListLike<'v> + 'v> StarlarkValue<'v> for ListGen<T>
where
    Self: ProvidesStaticType + Display,
{
    starlark_type!(List::TYPE);

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn get_methods() -> Option<&'static Methods> {
        list_methods()
    }

    fn collect_repr(&self, s: &mut String) {
        // Fast path as repr() for lists is quite hot
        s.push('[');
        for (i, v) in self.0.content().iter().enumerate() {
            if i != 0 {
                s.push_str(", ");
            }
            v.collect_repr(s);
        }
        s.push(']');
    }

    fn collect_repr_cycle(&self, collector: &mut String) {
        collector.push_str("[...]");
    }

    fn to_bool(&self) -> bool {
        !self.0.content().is_empty()
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match List::from_value(other) {
            None => Ok(false),
            Some(other) => equals_slice(self.0.content(), &other.content, |x, y| x.equals(*y)),
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match List::from_value(other) {
            None => ValueError::unsupported_with(self, "cmp()", other),
            Some(other) => compare_slice(self.0.content(), &other.content, |x, y| x.compare(*y)),
        }
    }

    fn at(&self, index: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = convert_index(index, self.0.content().len() as i32)? as usize;
        Ok(self.0.content()[i])
    }

    fn extra_memory(&self) -> usize {
        0
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.0.content().len() as i32)
    }

    fn is_in(&self, other: Value<'v>) -> anyhow::Result<bool> {
        for x in self.0.content().iter() {
            if x.equals(other)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn slice(
        &self,
        start: Option<Value>,
        stop: Option<Value>,
        stride: Option<Value>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let xs = self.0.content();
        let res = apply_slice(xs, start, stop, stride)?;
        Ok(heap.alloc_list(&res))
    }

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(self.0.iterate())
    }

    fn with_iterator(
        &self,
        _heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        self.0.with_iterator(f)
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        List::from_value(other)
            .map(|other| Ok(heap.alloc_list_concat(self.0.content(), other.content())))
    }

    fn mul(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let l = i32::unpack_param(other)?;
        let mut result = Vec::with_capacity(self.0.content().len() * cmp::max(0, l) as usize);
        for _ in 0..l {
            result.extend(self.0.content().iter());
        }
        Ok(heap.alloc_list(&result))
    }

    fn set_at(&self, index: Value<'v>, alloc_value: Value<'v>) -> anyhow::Result<()> {
        let i = convert_index(index, self.0.content().len() as i32)? as usize;
        self.0.set_at(i, alloc_value)
    }
}

impl<'v, T: ListLike<'v>> Serialize for ListGen<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(self.0.content().iter())
    }
}

/// Like `ValueOf`, but only validates item types; does not construct or store a
/// vec. Use `to_vec` to get a Vec.
pub struct ListOf<'v, V: UnpackValue<'v>> {
    value: Value<'v>,
    phantom: PhantomData<V>,
}

impl<'v, V: UnpackValue<'v>> ListOf<'v, V> {
    /// Collect the list elements into a `Vec`.
    pub fn to_vec(&self) -> Vec<V> {
        List::from_value(self.value)
            .expect("already validated as a list")
            .iter()
            .map(|v| V::unpack_value(v).expect("already validated value"))
            .collect()
    }
}

impl<'v> StarlarkTypeRepr for &'v ListRef<'v> {
    fn starlark_type_repr() -> String {
        Vec::<Value<'v>>::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v ListRef<'v> {
    fn expected() -> String {
        "list".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        List::from_value(value)
    }
}

impl<'v, V: UnpackValue<'v>> StarlarkTypeRepr for ListOf<'v, V> {
    fn starlark_type_repr() -> String {
        Vec::<V>::starlark_type_repr()
    }
}

impl<'v, V: UnpackValue<'v>> UnpackValue<'v> for ListOf<'v, V> {
    fn expected() -> String {
        format!("list of {}", V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let list = List::from_value(value)?;
        if list.iter().all(|v| V::unpack_value(v).is_some()) {
            Some(ListOf {
                value,
                phantom: PhantomData,
            })
        } else {
            None
        }
    }
}

impl<'v, V: UnpackValue<'v>> Deref for ListOf<'v, V> {
    type Target = Value<'v>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::assert::Assert;

    #[test]
    fn test_to_str() {
        assert::all_true(
            r#"
str([1, 2, 3]) == "[1, 2, 3]"
str([1, [2, 3]]) == "[1, [2, 3]]"
str([]) == "[]"
"#,
        );
    }

    #[test]
    fn test_repr_cycle() {
        assert::eq("l = []; l.append(l); repr(l)", "'[[...]]'");
        assert::eq("l = []; l.append(l); str(l)", "'[[...]]'");
    }

    #[test]
    fn test_mutate_list() {
        assert::is_true(
            r#"
v = [1, 2, 3]
v[1] = 1
v[2] = [2, 3]
v == [1, 1, [2, 3]]
"#,
        );
    }

    #[test]
    fn test_arithmetic_on_list() {
        assert::all_true(
            r#"
[1, 2, 3] + [2, 3] == [1, 2, 3, 2, 3]
[1, 2, 3] * 3 == [1, 2, 3, 1, 2, 3, 1, 2, 3]
"#,
        );
    }

    #[test]
    fn test_value_alias() {
        assert::is_true(
            r#"
v1 = [1, 2, 3]
v2 = v1
v2[2] = 4
v1 == [1, 2, 4] and v2 == [1, 2, 4]
"#,
        );
    }

    #[test]
    fn test_mutating_imports() {
        let mut a = Assert::new();
        a.module(
            "x",
            r#"
frozen_list = [1, 2]
frozen_list += [4]
def frozen_list_result():
    return frozen_list
def list_result():
    return [1, 2, 4]
"#,
        );
        a.fail("load('x','frozen_list')\nfrozen_list += [1]", "Immutable");
        a.fail(
            "load('x','frozen_list_result')\nx = frozen_list_result()\nx += [1]",
            "Immutable",
        );
        a.is_true("load('x','list_result')\nx = list_result()\nx += [8]\nx == [1, 2, 4, 8]");
    }
}

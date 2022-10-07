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

//! The list type, an immutable sequence of values.

#![allow(clippy::extra_unused_lifetimes)] // FIXME?

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::slice;

use gazebo::any::ProvidesStaticType;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::display::display_container;
use serde::ser::SerializeTuple;
use serde::Serialize;

use crate as starlark;
use crate::collections::StarlarkHasher;
use crate::private::Private;
use crate::values::comparison::compare_slice;
use crate::values::comparison::equals_slice;
use crate::values::index::apply_slice;
use crate::values::index::convert_index;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueError;
use crate::values::ValueLike;

/// Define the tuple type. See [`Tuple`] and [`FrozenTuple`] as the two aliases.
#[repr(C)]
#[derive(ProvidesStaticType, StarlarkDocs)]
#[starlark_docs_attrs(builtin = "standard")]
pub struct TupleGen<V> {
    len: usize,
    /// The data stored by the tuple.
    content: [V; 0],
}

impl<'v, V: ValueLike<'v>> Display for TupleGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // For single-item tuples we need to add a trailing ',' and easier to just handle that ourself than configure display_container correctly
        if self.len() == 1 {
            if f.alternate() {
                write!(f, "( {:#}, )", self.content()[0])
            } else {
                write!(f, "({},)", self.content()[0])
            }
        } else {
            display_container(f, "(", ")", self.content().iter())
        }
    }
}

impl<'v, V: ValueLike<'v>> Debug for TupleGen<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("TupleGen")
            .field("content", &self.content())
            .finish()
    }
}

impl<V> TupleGen<V> {
    /// `type(())`.
    pub const TYPE: &'static str = "tuple";

    pub(crate) const unsafe fn new(len: usize) -> TupleGen<V> {
        TupleGen { len, content: [] }
    }

    pub(crate) fn offset_of_content() -> usize {
        memoffset::offset_of!(Self, content)
    }
}

/// Runtime type of unfrozen tuple.
pub type Tuple<'v> = TupleGen<Value<'v>>;
/// Runtime type of frozen tuple.
pub type FrozenTuple = TupleGen<FrozenValue>;

unsafe impl<'v> Coerce<Tuple<'v>> for FrozenTuple {}

impl<'v> Tuple<'v> {
    /// Downcast a value to a tuple.
    pub fn from_value(value: Value<'v>) -> Option<&'v Self> {
        if value.unpack_frozen().is_some() {
            value.downcast_ref::<FrozenTuple>().map(coerce)
        } else {
            value.downcast_ref::<Tuple<'v>>()
        }
    }
}

impl<'v, V: ValueLike<'v>> TupleGen<V> {
    /// Get the length of the tuple.
    pub fn len(&self) -> usize {
        self.content().len()
    }

    /// Tuple elements.
    pub fn content(&self) -> &[V] {
        unsafe { slice::from_raw_parts(self.content.as_ptr(), self.len) }
    }

    pub(crate) fn content_mut(&mut self) -> &mut [V] {
        unsafe { slice::from_raw_parts_mut(self.content.as_mut_ptr(), self.len) }
    }

    /// Iterate over the elements of the tuple.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Value<'v>> + 'a
    where
        'v: 'a,
    {
        self.content().iter().map(|e| e.to_value())
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TupleGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!(Tuple::TYPE);

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn to_bool(&self) -> bool {
        self.len() != 0
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        for v in self.content() {
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match Tuple::from_value(other) {
            None => Ok(false),
            Some(other) => equals_slice(self.content(), other.content(), |x, y| x.equals(*y)),
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match Tuple::from_value(other) {
            None => ValueError::unsupported_with(self, "cmp()", other),
            Some(other) => compare_slice(self.content(), other.content(), |x, y| x.compare(*y)),
        }
    }

    fn at(&self, index: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let i = convert_index(index, self.len() as i32)? as usize;
        Ok(self.content()[i].to_value())
    }

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.len() as i32)
    }

    fn is_in(&self, other: Value<'v>) -> anyhow::Result<bool> {
        for x in self.content() {
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
        Ok(heap.alloc_tuple(&apply_slice(coerce(self.content()), start, stop, stride)?))
    }

    fn iterate<'a>(
        &'a self,
        _heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        Ok(box self.iter())
    }

    fn with_iterator(
        &self,
        _heap: &'v Heap,
        f: &mut dyn FnMut(&mut dyn Iterator<Item = Value<'v>>) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        f(&mut self.iter())
    }

    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        if let Some(other) = Tuple::from_value(other) {
            let mut result = Vec::with_capacity(self.len() + other.len());
            for x in self.iter() {
                result.push(x);
            }
            for x in other.iter() {
                result.push(x);
            }
            Some(Ok(heap.alloc_tuple(&result)))
        } else {
            None
        }
    }

    fn mul(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let l = i32::unpack_param(other)?;
        let mut result = Vec::new();
        for _i in 0..l {
            result.extend(self.content().iter().map(|e| e.to_value()));
        }
        Ok(heap.alloc_tuple(&result))
    }

    fn collect_repr_cycle(&self, collector: &mut String) {
        collector.push_str("(...)");
    }
}

impl<'v, V: ValueLike<'v>> Serialize for TupleGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut tuple_serializer = serializer.serialize_tuple(self.len)?;

        for e in self.content().iter() {
            tuple_serializer.serialize_element(e)?;
        }

        tuple_serializer.end()
    }
}

impl<'v, T1: AllocValue<'v>> AllocValue<'v> for (T1,) {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[self.0.alloc_value(heap)])
    }
}

impl<'v, T1: AllocValue<'v>, T2: AllocValue<'v>> AllocValue<'v> for (T1, T2) {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[self.0.alloc_value(heap), self.1.alloc_value(heap)])
    }
}

impl<'v, T1: AllocValue<'v>, T2: AllocValue<'v>, T3: AllocValue<'v>> AllocValue<'v>
    for (T1, T2, T3)
{
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_tuple(&[
            self.0.alloc_value(heap),
            self.1.alloc_value(heap),
            self.2.alloc_value(heap),
        ])
    }
}

impl<T1: StarlarkTypeRepr, T2: StarlarkTypeRepr> StarlarkTypeRepr for (T1, T2) {
    fn starlark_type_repr() -> String {
        format!(
            "({}, {})",
            T1::starlark_type_repr(),
            T2::starlark_type_repr()
        )
    }
}

impl<'v, T1: StarlarkTypeRepr> StarlarkTypeRepr for (T1,) {
    fn starlark_type_repr() -> String {
        format!("({},)", T1::starlark_type_repr())
    }
}

impl<T1: StarlarkTypeRepr, T2: StarlarkTypeRepr, T3: StarlarkTypeRepr> StarlarkTypeRepr
    for (T1, T2, T3)
{
    fn starlark_type_repr() -> String {
        format!(
            "({}, {}, {})",
            T1::starlark_type_repr(),
            T2::starlark_type_repr(),
            T3::starlark_type_repr()
        )
    }
}

impl<'v, T1: UnpackValue<'v>, T2: UnpackValue<'v>> UnpackValue<'v> for (T1, T2) {
    fn expected() -> String {
        format!("tuple ({}, {})", T1::expected(), T2::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let t = Tuple::from_value(value)?;
        if t.len() != 2 {
            return None;
        }
        Some((
            T1::unpack_value(t.content()[0])?,
            T2::unpack_value(t.content()[1])?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_to_str() {
        assert::all_true(
            r#"
str((1, 2, 3)) == "(1, 2, 3)"
str((1, (2, 3))) == "(1, (2, 3))"
str((1,)) == "(1,)"
"#,
        );
    }

    #[test]
    fn test_repr_cycle() {
        assert::eq("l = []; t = (l,); l.append(t); repr(t)", "'([(...)],)'");
        assert::eq("l = []; t = (l,); l.append(t); str(t)", "'([(...)],)'");
    }
}

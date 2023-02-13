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

use crate as starlark;
use crate::coerce::coerce;
use crate::coerce::Coerce;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::tuple::value::FrozenTuple;
use crate::values::types::tuple::value::Tuple;
use crate::values::FrozenValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

/// Reference to tuple data in Starlark heap.
#[derive(Coerce, Debug)]
#[repr(transparent)]
pub struct TupleRef<'v> {
    contents: [Value<'v>],
}

/// Reference to tuple data in frozen Starlark heap.
#[repr(transparent)]
#[derive(Coerce, Debug)]
pub struct FrozenTupleRef {
    contents: [FrozenValue],
}

impl<'v> TupleRef<'v> {
    /// `type(())`, which is `"tuple"`.
    pub const TYPE: &'static str = FrozenTupleRef::TYPE;

    /// Downcast a value to a tuple.
    pub fn from_value(value: Value<'v>) -> Option<&'v TupleRef<'v>> {
        Some(coerce(Tuple::from_value(value)?.content()))
    }

    /// Downcast a value to a tuple.
    pub fn from_frozen_value(value: FrozenValue) -> Option<&'v TupleRef<'v>> {
        Self::from_value(value.to_value())
    }

    /// Number of elements.
    pub fn len(&self) -> usize {
        self.contents.len()
    }

    /// Tuple elements.
    pub fn content(&self) -> &[Value<'v>] {
        &self.contents
    }

    /// Iterate over the contents.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = Value<'v>> + '_ {
        self.content().iter().copied()
    }
}

impl FrozenTupleRef {
    /// `type(())`, which is `"tuple"`.
    pub const TYPE: &'static str = FrozenTuple::TYPE;

    /// Downcast a value to a tuple.
    pub fn from_frozen_value(value: FrozenValue) -> Option<&'static FrozenTupleRef> {
        Some(coerce(value.downcast_ref::<FrozenTuple>()?.content()))
    }

    /// Number of elements.
    pub fn len(&self) -> usize {
        self.contents.len()
    }

    /// Tuple elements.
    pub fn content(&self) -> &[FrozenValue] {
        &self.contents
    }

    /// Iterate over contents.
    pub fn iter(&self) -> impl ExactSizeIterator<Item = FrozenValue> + '_ {
        self.content().iter().copied()
    }
}

impl<'v> StarlarkTypeRepr for &'v TupleRef<'v> {
    fn starlark_type_repr() -> String {
        TupleRef::TYPE.to_owned()
    }
}

impl<'a> StarlarkTypeRepr for &'a FrozenTupleRef {
    fn starlark_type_repr() -> String {
        FrozenTupleRef::TYPE.to_owned()
    }
}

impl<'v> UnpackValue<'v> for &'v TupleRef<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        TupleRef::from_value(value)
    }
}

impl<'v> UnpackValue<'v> for &'v FrozenTupleRef {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        FrozenTupleRef::from_frozen_value(value.unpack_frozen()?)
    }
}

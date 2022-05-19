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

//! This mod defines utilities to easily create Rust values as Starlark values.

use crate::values::{FrozenHeap, FrozenValue, Heap, UnpackValue, Value, ValueOf};

/// Trait for things that can be created on a [`Heap`] producing a [`Value`].
///
/// Note, this trait does not represent Starlark types.
/// For example, this trait is implemented for `char`,
/// but there's no Starlark type for `char`, this trait
/// is implemented for `char` to construct Starlark `str`.
pub trait AllocValue<'v> {
    /// Allocate the value on a heap and return a reference to the allocated value.
    ///
    /// Note, for certain values (e.g. empty strings) no allocation is actually performed,
    /// and a reference to the statically allocated object is returned.
    fn alloc_value(self, heap: &'v Heap) -> Value<'v>;
}

impl<'v> AllocValue<'v> for FrozenValue {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.to_value()
    }
}

impl<'v> AllocValue<'v> for Value<'v> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self
    }
}

impl<'v, T> AllocValue<'v> for Option<T>
where
    T: AllocValue<'v>,
{
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match self {
            Some(v) => v.alloc_value(heap),
            None => Value::new_none(),
        }
    }
}

/// Trait for things that can be allocated on a [`FrozenHeap`] producing a [`FrozenValue`].
pub trait AllocFrozenValue {
    /// Allocate a value in the frozen heap and return a reference to the allocated value.
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue;
}

impl AllocFrozenValue for FrozenValue {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        self
    }
}

impl FrozenHeap {
    /// Allocate a new value on a [`FrozenHeap`].
    pub fn alloc<T: AllocFrozenValue>(&self, val: T) -> FrozenValue {
        val.alloc_frozen_value(self)
    }
}

impl Heap {
    /// Allocate a new value on a [`Heap`].
    pub fn alloc<'v, T: AllocValue<'v>>(&'v self, x: T) -> Value<'v> {
        x.alloc_value(self)
    }

    /// Allocate a value and return [`ValueOf`] of it.
    pub fn alloc_value_of<'v, T>(&'v self, x: T) -> ValueOf<'v, &'v T>
    where
        T: AllocValue<'v>,
        &'v T: UnpackValue<'v>,
    {
        let value = self.alloc(x);
        ValueOf::unpack_value(value)
            .expect("just allocate value must be unpackable to the type of value")
    }
}

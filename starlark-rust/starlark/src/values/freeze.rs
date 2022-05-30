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

use std::{cell::RefCell, marker, marker::PhantomData};

use gazebo::prelude::*;

use crate::{
    collections::{
        vec_map::{Bucket, VecMap},
        SmallMap,
    },
    values::{Freezer, FrozenStringValue, FrozenValue, StringValue, Value},
};

/// Need to be implemented for non-simple `StarlarkValue`.
///
/// This is called on freeze of the heap. Must produce a replacement object to place
/// in the frozen heap.
///
/// For relatively simple cases it can be implemented with `#[derive(Freeze)]`:
///
/// ```
/// # struct AdditionalData;
///
/// use starlark::values::Freeze;
///
/// #[derive(Freeze)]
/// struct MyType<V> {
///     value: V,
///     // This field does not implement `Freeze`, but we can use it as is for freeze.
///     #[freeze(identity)]
///     data: AdditionalData,
/// }
/// ```
pub trait Freeze {
    /// When type is frozen, it is frozen into this type.
    type Frozen;

    /// Freeze a value. The frozen value _must_ be equal to the original,
    /// and produce the same hash.
    ///
    /// Note during freeze, `Value` objects in `Self` might be already special forward-objects,
    /// trying to unpack these objects will crash the process.
    /// So the function is only allowed to access `Value` objects after it froze them.
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen>;
}

impl Freeze for String {
    type Frozen = String;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<String> {
        Ok(self)
    }
}

impl Freeze for i32 {
    type Frozen = i32;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<i32> {
        Ok(self)
    }
}

impl Freeze for usize {
    type Frozen = usize;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<usize> {
        Ok(self)
    }
}

impl Freeze for bool {
    type Frozen = bool;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<bool> {
        Ok(self)
    }
}

impl<'v, T: 'static> Freeze for marker::PhantomData<&'v T> {
    type Frozen = PhantomData<&'static T>;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<PhantomData<&'static T>> {
        Ok(marker::PhantomData)
    }
}

impl<T> Freeze for Vec<T>
where
    T: Freeze,
{
    type Frozen = Vec<T::Frozen>;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Vec<T::Frozen>> {
        self.into_try_map(|v| v.freeze(freezer))
    }
}

impl<T> Freeze for RefCell<T>
where
    T: Freeze,
{
    type Frozen = T::Frozen;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<T::Frozen> {
        self.into_inner().freeze(freezer)
    }
}

impl<T> Freeze for Box<T>
where
    T: Freeze,
{
    type Frozen = Box<T::Frozen>;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(box (*self).freeze(freezer)?)
    }
}

impl<T> Freeze for Option<T>
where
    T: Freeze,
{
    type Frozen = Option<T::Frozen>;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Option<T::Frozen>> {
        self.into_try_map(|v| v.freeze(freezer))
    }
}

impl<K, V> Freeze for VecMap<K, V>
where
    K: Freeze,
    V: Freeze,
{
    type Frozen = VecMap<K::Frozen, V::Frozen>;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let buckets = self.buckets.into_try_map(|Bucket { hash, key, value }| {
            let key = key.freeze(freezer)?;
            let value = value.freeze(freezer)?;
            // `freeze` must not change hash.
            anyhow::Ok(Bucket { hash, key, value })
        })?;
        Ok(VecMap { buckets })
    }
}

impl<K, V> Freeze for SmallMap<K, V>
where
    K: Freeze,
    V: Freeze,
{
    type Frozen = SmallMap<K::Frozen, V::Frozen>;

    fn freeze(mut self, freezer: &Freezer) -> anyhow::Result<SmallMap<K::Frozen, V::Frozen>> {
        self.maybe_drop_index();
        let (entries, index) = self.into_raw_parts();
        let entries = entries.freeze(freezer)?;
        unsafe { Ok(SmallMap::from_raw_parts(entries, index)) }
    }
}

impl<'v> Freeze for Value<'v> {
    type Frozen = FrozenValue;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenValue> {
        freezer.freeze(self)
    }
}

impl Freeze for FrozenValue {
    type Frozen = FrozenValue;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<FrozenValue> {
        Ok(self)
    }
}

impl<'v> Freeze for StringValue<'v> {
    type Frozen = FrozenStringValue;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenStringValue> {
        self.freeze(freezer)
    }
}

impl Freeze for () {
    type Frozen = ();

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<()> {
        Ok(())
    }
}

impl<A: Freeze> Freeze for (A,) {
    type Frozen = (A::Frozen,);

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<(A::Frozen,)> {
        Ok((self.0.freeze(freezer)?,))
    }
}

impl<A: Freeze, B: Freeze> Freeze for (A, B) {
    type Frozen = (A::Frozen, B::Frozen);

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<(A::Frozen, B::Frozen)> {
        Ok((self.0.freeze(freezer)?, self.1.freeze(freezer)?))
    }
}

impl<A: Freeze, B: Freeze, C: Freeze> Freeze for (A, B, C) {
    type Frozen = (A::Frozen, B::Frozen, C::Frozen);

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<(A::Frozen, B::Frozen, C::Frozen)> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
        ))
    }
}

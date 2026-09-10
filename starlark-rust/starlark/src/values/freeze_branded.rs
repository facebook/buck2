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

use std::cell::OnceCell;
use std::cell::RefCell;
use std::cell::UnsafeCell;
use std::marker;
use std::marker::PhantomData;

use starlark_map::Hashed;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_syntax::slice_vec_ext::VecExt;

use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::Value;

/// Need to be implemented for non-simple `StarlarkValue`.
///
/// This is called on freeze of the heap. Must produce a replacement object to place
/// in the frozen heap.
///
/// `'v` is the brand of the heap the value is frozen out of, like the `'v` of
/// [`Trace<'v>`](crate::values::Trace): a type branded by `'v` implements `FreezeBranded<'v>`, a
/// type that holds no values implements it for every `'v`.
///
/// For relatively simple cases it can be implemented with `#[derive(FreezeBranded)]`:
///
/// ```
/// # struct AdditionalData;
///
/// use starlark::values::FreezeBranded;
/// use starlark::values::Value;
///
/// #[derive(FreezeBranded)]
/// struct MyType<'v> {
///     value: Value<'v>,
///     // This field does not implement `FreezeBranded`, but we can use it as is for freeze.
///     #[freeze_branded(identity)]
///     data: AdditionalData,
/// }
/// ```
pub trait FreezeBranded<'v> {
    /// When type is frozen, it is frozen into this type.
    type Frozen<'fv>;

    /// Freeze a value. The frozen value _must_ be equal to the original,
    /// and produce the same hash.
    ///
    /// Note during freeze, `Value` objects in `Self` might be already special forward-objects,
    /// trying to unpack these objects will crash the process.
    /// So the function is only allowed to access `Value` objects after it froze them.
    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>>;
}

macro_rules! impl_freeze_branded_identity {
    ($($t:ty),*) => {
        $(
            impl<'v> FreezeBranded<'v> for $t {
                type Frozen<'fv> = Self;

                fn freeze<'fv>(self, _freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
                    Ok(self)
                }
            }
        )*
    }
}

impl_freeze_branded_identity!(String, i32, u32, i64, u64, usize, bool, ());

impl<'v, T: 'static> FreezeBranded<'v> for marker::PhantomData<&'v T> {
    type Frozen<'fv> = PhantomData<&'fv T>;

    fn freeze<'fv>(self, _freezer: &Freezer<'v, 'fv>) -> FreezeResult<PhantomData<&'fv T>> {
        Ok(marker::PhantomData)
    }
}

impl<'v, T> FreezeBranded<'v> for Vec<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = Vec<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Vec<T::Frozen<'fv>>> {
        self.into_try_map(|v| v.freeze(freezer))
    }
}

impl<'v, T> FreezeBranded<'v> for RefCell<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = T::Frozen<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<T::Frozen<'fv>> {
        self.into_inner().freeze(freezer)
    }
}

impl<'v, T> FreezeBranded<'v> for UnsafeCell<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = UnsafeCell<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(UnsafeCell::new(self.into_inner().freeze(freezer)?))
    }
}

impl<'v, T> FreezeBranded<'v> for OnceCell<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = Option<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        self.into_inner().freeze(freezer)
    }
}

impl<'v, T> FreezeBranded<'v> for Box<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = Box<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(Box::new((*self).freeze(freezer)?))
    }
}

impl<'v, T> FreezeBranded<'v> for Box<[T]>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = Box<[T::Frozen<'fv>]>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        self.into_vec()
            .into_try_map(|v| v.freeze(freezer))
            .map(|v| v.into_boxed_slice())
    }
}

impl<'v, T> FreezeBranded<'v> for Option<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = Option<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Option<T::Frozen<'fv>>> {
        self.map(|v| v.freeze(freezer)).transpose()
    }
}

impl<'v, K: FreezeBranded<'v>> FreezeBranded<'v> for Hashed<K> {
    type Frozen<'fv> = Hashed<K::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        // `freeze` must not change hash.
        Ok(Hashed::new_unchecked(
            self.hash(),
            self.into_key().freeze(freezer)?,
        ))
    }
}

impl<'v, K, V> FreezeBranded<'v> for SmallMap<K, V>
where
    K: FreezeBranded<'v>,
    V: FreezeBranded<'v>,
{
    type Frozen<'fv> = SmallMap<K::Frozen<'fv>, V::Frozen<'fv>>;

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<SmallMap<K::Frozen<'fv>, V::Frozen<'fv>>> {
        let mut new = SmallMap::with_capacity(self.len());
        for (key, value) in self.into_iter_hashed() {
            let hash = key.hash();
            let key = key.into_key().freeze(freezer)?;
            // TODO(nga): verify hash unchanged after freeze.
            let key = Hashed::new_unchecked(hash, key);
            let value = value.freeze(freezer)?;
            new.insert_hashed_unique_unchecked(key, value);
        }
        Ok(new)
    }
}

impl<'v, T> FreezeBranded<'v> for SmallSet<T>
where
    T: FreezeBranded<'v>,
{
    type Frozen<'fv> = SmallSet<T::Frozen<'fv>>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        let mut new = SmallSet::with_capacity(self.len());
        for value in self.into_iter_hashed() {
            let value = value.freeze(freezer)?;
            // TODO(nga): verify hash unchanged after freeze.
            new.insert_hashed_unique_unchecked(value);
        }
        Ok(new)
    }
}

impl<'v> FreezeBranded<'v> for Value<'v> {
    type Frozen<'fv> = Value<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Value<'fv>> {
        freezer.freeze(self)
    }
}

impl<'v, A: FreezeBranded<'v>> FreezeBranded<'v> for (A,) {
    type Frozen<'fv> = (A::Frozen<'fv>,);

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<(A::Frozen<'fv>,)> {
        Ok((self.0.freeze(freezer)?,))
    }
}

impl<'v, A: FreezeBranded<'v>, B: FreezeBranded<'v>> FreezeBranded<'v> for (A, B) {
    type Frozen<'fv> = (A::Frozen<'fv>, B::Frozen<'fv>);

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(A::Frozen<'fv>, B::Frozen<'fv>)> {
        Ok((self.0.freeze(freezer)?, self.1.freeze(freezer)?))
    }
}

impl<'v, A: FreezeBranded<'v>, B: FreezeBranded<'v>, C: FreezeBranded<'v>> FreezeBranded<'v>
    for (A, B, C)
{
    type Frozen<'fv> = (A::Frozen<'fv>, B::Frozen<'fv>, C::Frozen<'fv>);

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(A::Frozen<'fv>, B::Frozen<'fv>, C::Frozen<'fv>)> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
        ))
    }
}

impl<'v, A: FreezeBranded<'v>, B: FreezeBranded<'v>, C: FreezeBranded<'v>, D: FreezeBranded<'v>>
    FreezeBranded<'v> for (A, B, C, D)
{
    type Frozen<'fv> = (
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
    );

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
    )> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
            self.3.freeze(freezer)?,
        ))
    }
}

impl<
    'v,
    A: FreezeBranded<'v>,
    B: FreezeBranded<'v>,
    C: FreezeBranded<'v>,
    D: FreezeBranded<'v>,
    E: FreezeBranded<'v>,
> FreezeBranded<'v> for (A, B, C, D, E)
{
    type Frozen<'fv> = (
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
        E::Frozen<'fv>,
    );

    fn freeze<'fv>(
        self,
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<(
        A::Frozen<'fv>,
        B::Frozen<'fv>,
        C::Frozen<'fv>,
        D::Frozen<'fv>,
        E::Frozen<'fv>,
    )> {
        Ok((
            self.0.freeze(freezer)?,
            self.1.freeze(freezer)?,
            self.2.freeze(freezer)?,
            self.3.freeze(freezer)?,
            self.4.freeze(freezer)?,
        ))
    }
}

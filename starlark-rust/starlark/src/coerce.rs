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

//! Zero-cost re-typing of a `SmallMap`'s keys.
//!
//! The one use is handing a `SmallMap<StringValue<'v>, Value<'v>>` (how named arguments are
//! collected) to `Dict::new`, which takes `SmallMap<Value<'v>, Value<'v>>`, without rehashing.

use std::alloc::Layout;
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::ptr;

use starlark_map::small_map::SmallMap;

/// A marker trait such that the existence of `From: Coerce<To>` implies
/// that `From` can be treated as `To` without any data manipulation: the two have the same
/// layout, `To` has no invariant `From` might violate, and a `From` can be dropped as a `To`.
/// [`coerce`] performs the conversion.
pub(crate) unsafe trait Coerce<To: ?Sized> {}

/// [`Coerce`], plus: the [`Hash`](std::hash::Hash), [`Eq`], [`PartialEq`], [`Ord`] and
/// [`PartialOrd`] impls of `From` and `To` give identical results on a value, which is what
/// re-typing the keys of a map needs.
pub(crate) unsafe trait CoerceKey<To: ?Sized>: Coerce<To> {}

unsafe impl<FromK, FromV, ToK, ToV> Coerce<SmallMap<ToK, ToV>> for SmallMap<FromK, FromV>
where
    FromK: CoerceKey<ToK>,
    FromV: Coerce<ToV>,
{
}

/// Convert between types which have a `Coerce` relationship.
/// Often the second type argument will need to be given explicitly,
/// e.g. `coerce::<_, ToType>(x)`.
#[inline]
pub(crate) fn coerce<From, To>(x: From) -> To
where
    From: Coerce<To>,
{
    assert_eq!(Layout::new::<From>(), Layout::new::<To>());
    let x = ManuallyDrop::new(x);
    unsafe { ptr::read(x.deref() as *const From as *const To) }
}

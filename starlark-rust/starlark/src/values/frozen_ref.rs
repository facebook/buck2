/*
 * Copyright 2019 The Starlark in Rust Authors.
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

use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::ptr;
use std::sync::atomic;

use allocative::Allocative;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;

use crate::values::Freeze;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::Trace;
use crate::values::Tracer;

/// A [`FrozenRef`] is essentially a [`FrozenValue`](crate::values::FrozenValue),
/// and has the same memory and access guarantees as it.
/// However, this keeps the type of the type `T` of the actual
/// [`FrozenValue`](crate::values::FrozenValue) as a
/// reference, allowing manipulation of the actual typed data.
#[derive(Clone_, Dupe_, Copy_, Debug, Allocative)]
#[allocative(skip)] // Data is owned by heap.
pub struct FrozenRef<'fv, T: 'fv + ?Sized> {
    pub(crate) value: &'fv T,
}

impl<'fv, T> Default for FrozenRef<'fv, [T]> {
    fn default() -> FrozenRef<'fv, [T]> {
        FrozenRef { value: &[] }
    }
}

unsafe impl<'v, 'fv, T: 'fv + ?Sized> Trace<'v> for FrozenRef<'fv, T> {
    fn trace(&mut self, _: &Tracer<'v>) {
        // Do nothing, because `FrozenRef` can only point to frozen value.
    }
}

impl<'fv, T: 'fv + ?Sized> FrozenRef<'fv, T> {
    pub(crate) const fn new(value: &'fv T) -> FrozenRef<'fv, T> {
        FrozenRef { value }
    }

    /// Returns a reference to the underlying value.
    pub fn as_ref(self) -> &'fv T {
        self.value
    }

    /// Converts `self` into a new reference that points at something reachable from the previous.
    pub fn map<F, U: 'fv + ?Sized>(self, f: F) -> FrozenRef<'fv, U>
    where
        for<'v> F: FnOnce(&'v T) -> &'v U,
    {
        FrozenRef {
            value: f(self.value),
        }
    }

    /// Fallible map the reference to another one.
    pub fn try_map_result<F, U: 'fv + ?Sized, E>(self, f: F) -> Result<FrozenRef<'fv, U>, E>
    where
        for<'v> F: FnOnce(&'v T) -> Result<&'v U, E>,
    {
        Ok(FrozenRef {
            value: f(self.value)?,
        })
    }

    /// Optionally map the reference to another one.
    pub fn try_map_option<F, U: 'fv + ?Sized>(self, f: F) -> Option<FrozenRef<'fv, U>>
    where
        for<'v> F: FnOnce(&'v T) -> Option<&'v U>,
    {
        Some(FrozenRef {
            value: f(self.value)?,
        })
    }
}

impl<'fv, T: ?Sized + Display> Display for FrozenRef<'fv, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<'fv, T: ?Sized> Deref for FrozenRef<'fv, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.value
    }
}

impl<'fv, T: 'fv + ?Sized> Borrow<T> for FrozenRef<'fv, T> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<'fv, T: 'fv + ?Sized> Borrow<T> for FrozenRef<'fv, Box<T>> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<'fv, T: 'fv + ?Sized> PartialEq for FrozenRef<'fv, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        (self as &T).eq(other as &T)
    }
}

impl<'fv, T: 'fv + ?Sized> Eq for FrozenRef<'fv, T> where T: Eq {}

impl<'fv, T: 'fv + ?Sized> PartialOrd for FrozenRef<'fv, T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self as &T).partial_cmp(other as &T)
    }
}

impl<'fv, T: 'fv + ?Sized> Ord for FrozenRef<'fv, T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        (self as &T).cmp(other as &T)
    }
}

impl<'fv, T: 'fv + ?Sized> Hash for FrozenRef<'fv, T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as &T).hash(state);
    }
}

impl<'fv, T: 'fv + ?Sized> Freeze for FrozenRef<'fv, T> {
    type Frozen = Self;

    fn freeze(self, _freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(self)
    }
}

/// `Atomic<Option<FrozenRef<T>>>`.
pub(crate) struct AtomicFrozenRefOption<T>(atomic::AtomicPtr<T>);

unsafe impl<'v, T> Trace<'v> for AtomicFrozenRefOption<T> {
    fn trace(&mut self, _: &Tracer<'v>) {
        // Do nothing, because `AtomicFrozenRefOption` holds `FrozenRef`.
    }
}

impl<T> AtomicFrozenRefOption<T> {
    pub(crate) fn new(module: Option<FrozenRef<T>>) -> AtomicFrozenRefOption<T> {
        AtomicFrozenRefOption(atomic::AtomicPtr::new(match module {
            Some(v) => v.as_ref() as *const T as *mut T,
            None => ptr::null_mut(),
        }))
    }

    pub(crate) fn load_relaxed(&self) -> Option<FrozenRef<'static, T>> {
        // Note this is relaxed load which is cheap.
        let ptr = self.0.load(atomic::Ordering::Relaxed);
        if ptr.is_null() {
            None
        } else {
            Some(FrozenRef::new(unsafe { &*ptr }))
        }
    }

    pub(crate) fn store_relaxed(&self, module: FrozenRef<T>) {
        self.0.store(
            module.as_ref() as *const T as *mut T,
            atomic::Ordering::Relaxed,
        );
    }
}

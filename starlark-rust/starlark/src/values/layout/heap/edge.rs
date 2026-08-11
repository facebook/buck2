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

use std::marker::PhantomData;

use dupe::Dupe;

use crate::any::IsStaticType;
use crate::any::ProvidesStaticType;
use crate::cast::transmute;

/// Witness that the heap identified by `'v` depends on the heap identified by `'dep`.
///
/// As described in the `branding` module, a `Value<'v>` may live either in the heap identified
/// by `'v` or in any frozen heap that that heap depends on. This type is a certificate of such a
/// dependency: it proves that the heap of `'dep` is kept alive by the heap of `'v`, so that
/// anything kept alive by the former is usable in the context of the latter. That conversion is
/// what [`rebrand`](HeapEdge::rebrand) provides.
#[derive(Copy, Clone, Dupe)]
pub struct HeapEdge<'v, 'dep> {
    _invariant: PhantomData<(fn(&'v ()) -> &'v (), fn(&'dep ()) -> &'dep ())>,
}

impl<'v, 'dep> HeapEdge<'v, 'dep> {
    /// Assert the existence of this heap dependency.
    ///
    /// # SAFETY
    ///
    /// The heap identified by `'v` must keep the heap identified by `'dep` alive. Additionally,
    /// `'dep` must be a brand: a lifetime at which only values kept alive by that heap (or
    /// `'static` data) can exist. An ordinary borrow lifetime is not a brand — stack data can be
    /// borrowed at it, and [`rebrand`](HeapEdge::rebrand) would extend such a borrow past its
    /// referent.
    pub unsafe fn unchecked_new() -> Self {
        Self {
            _invariant: PhantomData,
        }
    }

    /// Convert a value kept alive by the `'dep` heap for use in the context of the `'v` heap.
    pub fn rebrand<U>(self, v: U) -> <U::StaticType as IsStaticType>::Reinfect<'v>
    where
        U: ProvidesStaticType<'dep>,
        U::StaticType: IsStaticType + Sized,
        <U::StaticType as IsStaticType>::Reinfect<'v>: Sized,
    {
        // SAFETY: The input and output are the same type up to the brand (guaranteed by
        // `ProvidesStaticType`/`IsStaticType`), and everything that exists at the `'dep` brand
        // lives as long as the `'dep` heap, which lives at least as long as the `'v` heap (both
        // guaranteed by the construction contract of `self`)
        unsafe { transmute!(U, <U::StaticType as IsStaticType>::Reinfect<'v>, v) }
    }
}

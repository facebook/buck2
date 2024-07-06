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

use std::fmt::Debug;
use std::fmt::Formatter;
use std::marker::PhantomData;

use allocative::Allocative;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;

use crate::typing::Ty;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::Heap;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::UnpackValue;
use crate::values::Value;

/// Starlark value with type annotation.
///
/// Can be used in function signatures to provide types to the type checker.
///
/// Note this type does not actually check the type of the value.
/// Providing incorrect type annotation will result
/// in incorrect error reporting by the type checker.
#[derive(Clone_, Copy_, Dupe_, Allocative)]
#[allocative(bound = "")]
pub struct ValueOfUnchecked<'v, T: StarlarkTypeRepr>(Value<'v>, PhantomData<T>);

impl<'v, T: StarlarkTypeRepr> Debug for ValueOfUnchecked<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ValueOfUnchecked").field(&self.0).finish()
    }
}

impl<'v, T: StarlarkTypeRepr> ValueOfUnchecked<'v, T> {
    /// New.
    #[inline]
    pub fn new(value: Value<'v>) -> Self {
        Self(value, PhantomData)
    }

    /// Construct after checking the type.
    #[inline]
    pub fn new_checked(value: Value<'v>) -> anyhow::Result<Self>
    where
        T: UnpackValue<'v>,
    {
        T::unpack_value_err(value)?;
        Ok(Self::new(value))
    }

    /// Get the value.
    #[inline]
    pub fn get(self) -> Value<'v> {
        self.0
    }
}

impl<'v, T: StarlarkTypeRepr> StarlarkTypeRepr for ValueOfUnchecked<'v, T> {
    type Canonical = T::Canonical;

    fn starlark_type_repr() -> Ty {
        T::starlark_type_repr()
    }
}

impl<'v, T: StarlarkTypeRepr> AllocValue<'v> for ValueOfUnchecked<'v, T> {
    #[inline]
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.0
    }
}

impl<'v, T: StarlarkTypeRepr> UnpackValue<'v> for ValueOfUnchecked<'v, T> {
    #[inline]
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        Some(Self::new(value))
    }
}

unsafe impl<'v, T: StarlarkTypeRepr> Trace<'v> for ValueOfUnchecked<'v, T> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        // TODO(nga): should derive, but to do that we need to implement `#[trace(bound = "")]`
        let ValueOfUnchecked(value, phantom) = self;
        value.trace(tracer);
        phantom.trace(tracer);
    }
}

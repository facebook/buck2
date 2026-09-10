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

use std::convert::Infallible;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::marker::PhantomData;

use allocative::Allocative;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;
use starlark_derive::StarlarkPagable;

use crate as starlark;
use crate::typing::Ty;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FreezeBranded;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::Heap;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::type_repr::StarlarkTypeRepr;

/// Starlark value with type annotation.
///
/// Can be used in function signatures to provide types to the type checker.
///
/// Note this type does not actually check the type of the value.
/// Providing incorrect type annotation will result
/// in incorrect error reporting by the type checker.
#[derive(Clone_, Copy_, Dupe_, Allocative)]
#[allocative(bound = "")]
#[derive(pagable::PagablePanic, StarlarkPagable)]
pub struct ValueOfUnchecked<'v, T: StarlarkTypeRepr>(Value<'v>, PhantomData<fn() -> T>);

impl<'v, T: StarlarkTypeRepr> ValueOfUnchecked<'v, T> {
    /// New.
    #[inline]
    pub fn new(value: Value<'v>) -> Self {
        Self(value, PhantomData)
    }

    /// Construct after checking the type.
    #[inline]
    pub fn new_checked(value: Value<'v>) -> crate::Result<Self>
    where
        T: UnpackValue<'v>,
    {
        T::unpack_value_err(value)?;
        Ok(Self::new(value))
    }

    /// Cast to a different Rust type for the same Starlark type.
    #[inline]
    pub fn cast<U: StarlarkTypeRepr<Canonical = T::Canonical>>(self) -> ValueOfUnchecked<'v, U> {
        ValueOfUnchecked::new(self.0)
    }

    /// Get the value.
    #[inline]
    pub fn get(self) -> Value<'v> {
        self.0
    }

    /// Unpack the value.
    pub fn unpack(self) -> crate::Result<T>
    where
        T: UnpackValue<'v>,
    {
        T::unpack_value_err(self.0)
    }
}

impl<'v, T: StarlarkTypeRepr> Debug for ValueOfUnchecked<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ValueOfUnchecked").field(&self.0).finish()
    }
}

impl<'v, T: StarlarkTypeRepr> Display for ValueOfUnchecked<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'v, T: StarlarkTypeRepr> StarlarkTypeRepr for ValueOfUnchecked<'v, T> {
    type Canonical = T::Canonical;

    fn starlark_type_repr() -> Ty {
        <Self as StarlarkTypeRepr>::Canonical::starlark_type_repr()
    }
}

impl<'v, T: StarlarkTypeRepr> AllocValue<'v> for ValueOfUnchecked<'v, T> {
    fn alloc_value(self, _heap: Heap<'v>) -> Value<'v> {
        self.0
    }
}

impl<'fv, T: StarlarkTypeRepr> AllocFrozenValue<'fv> for ValueOfUnchecked<'fv, T> {
    fn alloc_frozen_value(self, _heap: FrozenHeap<'fv>) -> Value<'fv> {
        self.0
    }
}

unsafe impl<'v, T: StarlarkTypeRepr> Trace<'v> for ValueOfUnchecked<'v, T> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        self.0.trace(tracer)
    }
}

impl<'v, T: StarlarkTypeRepr> FreezeBranded<'v> for ValueOfUnchecked<'v, T> {
    type Frozen<'fv> = ValueOfUnchecked<'fv, T>;

    fn freeze<'fv>(self, freezer: &Freezer<'v, 'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(ValueOfUnchecked::new(self.0.freeze(freezer)?))
    }
}

impl<'v, T: StarlarkTypeRepr> UnpackValue<'v> for ValueOfUnchecked<'v, T> {
    type Error = Infallible;

    #[inline]
    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        Ok(Some(Self::new(value)))
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::const_frozen_string;
    use crate::typing::Ty;
    use crate::values::ValueOfUnchecked;
    use crate::values::type_repr::StarlarkTypeRepr;

    #[test]
    fn test_cast_example() {
        let a = ValueOfUnchecked::<String>::new_checked(const_frozen_string!("a").at().to_value())
            .unwrap();
        let _b: ValueOfUnchecked<&str> = a.cast();
    }

    #[test]
    fn test_value_of_unchecked_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}

        #[allow(dead_code)]
        struct ReprNotSendSync(Rc<String>);
        impl StarlarkTypeRepr for ReprNotSendSync {
            type Canonical = Self;
            fn starlark_type_repr() -> Ty {
                panic!("not needed in test")
            }
        }

        assert_send_sync::<ValueOfUnchecked<'static, ReprNotSendSync>>();
    }
}

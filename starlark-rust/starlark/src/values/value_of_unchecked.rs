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
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenValue;
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
pub struct ValueOfUnchecked<'v, T: StarlarkTypeRepr>(Value<'v>, PhantomData<fn() -> T>);

/// Frozen starlark value with type annotation.
#[derive(Clone_, Copy_, Dupe_, Allocative)]
#[allocative(bound = "")]
pub struct FrozenValueOfUnchecked<'f, T: StarlarkTypeRepr>(
    FrozenValue,
    PhantomData<(&'f (), fn() -> T)>,
);

impl<'v, T: StarlarkTypeRepr> Debug for ValueOfUnchecked<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ValueOfUnchecked").field(&self.0).finish()
    }
}

impl<'f, T: StarlarkTypeRepr> Debug for FrozenValueOfUnchecked<'f, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FrozenValueOfUnchecked")
            .field(&self.0)
            .finish()
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

    /// Cast to a different Rust type for the same Starlark type.
    pub fn cast<U: StarlarkTypeRepr<Canonical = T::Canonical>>(self) -> ValueOfUnchecked<'v, U> {
        ValueOfUnchecked::new(self.0)
    }

    /// Get the value.
    #[inline]
    pub fn get(self) -> Value<'v> {
        self.0
    }
}

impl<'f, T: StarlarkTypeRepr> FrozenValueOfUnchecked<'f, T> {
    /// New.
    #[inline]
    pub fn new(value: FrozenValue) -> Self {
        Self(value, PhantomData)
    }

    /// Construct after checking the type.
    #[inline]
    pub fn new_checked(value: FrozenValue) -> anyhow::Result<Self>
    where
        T: UnpackValue<'f>,
    {
        T::unpack_value_err(value.to_value())?;
        Ok(Self::new(value))
    }

    /// Cast to a different Rust type for the same Starlark type.
    pub fn cast<U: StarlarkTypeRepr<Canonical = T::Canonical>>(
        self,
    ) -> FrozenValueOfUnchecked<'f, U> {
        FrozenValueOfUnchecked::new(self.0)
    }

    /// Convert to a value.
    #[inline]
    pub fn to_value(self) -> ValueOfUnchecked<'f, T> {
        ValueOfUnchecked::new(self.0.to_value())
    }

    /// Get the value.
    #[inline]
    pub fn get(self) -> FrozenValue {
        self.0
    }
}

impl<'f, T: StarlarkTypeRepr> StarlarkTypeRepr for FrozenValueOfUnchecked<'f, T> {
    type Canonical = T::Canonical;

    fn starlark_type_repr() -> Ty {
        T::starlark_type_repr()
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
    fn unpack_value(value: Value<'v>) -> crate::Result<Option<Self>> {
        Ok(Some(Self::new(value)))
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

impl<'v, T: StarlarkTypeRepr> Freeze for ValueOfUnchecked<'v, T> {
    type Frozen = FrozenValueOfUnchecked<'static, T>;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(FrozenValueOfUnchecked::new(self.0.freeze(freezer)?))
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::const_frozen_string;
    use crate::typing::Ty;
    use crate::values::type_repr::StarlarkTypeRepr;
    use crate::values::FrozenValueOfUnchecked;
    use crate::values::ValueOfUnchecked;

    #[test]
    fn test_cast_example() {
        let a =
            ValueOfUnchecked::<String>::new_checked(const_frozen_string!("a").to_value()).unwrap();
        let _b: ValueOfUnchecked<&str> = a.cast();
    }

    #[test]
    fn test_frozen_value_of_unchecked_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}

        #[allow(dead_code)]
        struct ReprNotSendSync(Rc<String>);
        impl StarlarkTypeRepr for ReprNotSendSync {
            type Canonical = Self;
            fn starlark_type_repr() -> Ty {
                panic!("not needed in test")
            }
        }

        assert_send_sync::<FrozenValueOfUnchecked<ReprNotSendSync>>();
    }

    #[test]
    fn test_frozen_value_of_unchecked_covariant() {
        fn _assert_covariant<'a>(
            _value: FrozenValueOfUnchecked<'static, String>,
        ) -> FrozenValueOfUnchecked<'a, String> {
            panic!()
        }
    }
}

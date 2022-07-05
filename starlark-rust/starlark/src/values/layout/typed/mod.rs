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

pub(crate) mod string;

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::marker;
use std::ops::Deref;

use gazebo::cast;
use gazebo::coerce::Coerce;
use gazebo::coerce::CoerceKey;
use gazebo::prelude::*;
use serde::Serialize;

use crate::gazebo::any::AnyLifetime;
use crate::gazebo::any::ProvidesStaticType;
use crate::values::int::PointerI32;
use crate::values::layout::avalue::AValue;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::string::StarlarkStr;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenRef;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

/// [`Value`] wrapper which asserts contained value is of type `<T>`.
#[derive(Copy_, Clone_, Dupe_, ProvidesStaticType)]
pub struct ValueTyped<'v, T: StarlarkValue<'v>>(Value<'v>, marker::PhantomData<&'v T>);
/// [`FrozenValue`] wrapper which asserts contained value is of type `<T>`.
#[derive(Copy_, Clone_, Dupe_, ProvidesStaticType)]
pub struct FrozenValueTyped<'v, T: StarlarkValue<'v>>(FrozenValue, marker::PhantomData<&'v T>);

unsafe impl<'v, T: StarlarkValue<'v>> Coerce<ValueTyped<'v, T>> for ValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> CoerceKey<ValueTyped<'v, T>> for ValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> Coerce<Value<'v>> for ValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> CoerceKey<Value<'v>> for ValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> Coerce<FrozenValueTyped<'v, T>> for FrozenValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> CoerceKey<FrozenValueTyped<'v, T>>
    for FrozenValueTyped<'v, T>
{
}
unsafe impl<'v, T: StarlarkValue<'v>> Coerce<Value<'v>> for FrozenValueTyped<'v, T> {}
unsafe impl<'v, T: StarlarkValue<'v>> CoerceKey<Value<'v>> for FrozenValueTyped<'v, T> {}

unsafe impl<'v, 'f, T: StarlarkValue<'f>> Trace<'v> for FrozenValueTyped<'f, T> {
    fn trace(&mut self, _tracer: &Tracer<'v>) {}
}

impl<T: StarlarkValue<'static>> Freeze for FrozenValueTyped<'static, T> {
    type Frozen = Self;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(self)
    }
}

impl<'v, T: StarlarkValue<'v>> Debug for ValueTyped<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ValueTyped").field(&self.0).finish()
    }
}

impl<'v, T: StarlarkValue<'v>> Debug for FrozenValueTyped<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("FrozenValueTyped").field(&self.0).finish()
    }
}

impl<'v, T: StarlarkValue<'v>> Display for ValueTyped<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'v, T: StarlarkValue<'v>> Display for FrozenValueTyped<'v, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'v, T: StarlarkValue<'v>> Serialize for ValueTyped<'v, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'v, T: StarlarkValue<'v>> Serialize for FrozenValueTyped<'v, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'v, T: StarlarkValue<'v>> ValueTyped<'v, T> {
    /// Downcast.
    pub fn new(value: Value<'v>) -> Option<ValueTyped<'v, T>> {
        value.downcast_ref::<T>()?;
        Some(ValueTyped(value, marker::PhantomData))
    }

    /// Construct typed value without checking the value is of type `<T>`.
    pub unsafe fn new_unchecked(value: Value<'v>) -> ValueTyped<'v, T> {
        debug_assert!(value.downcast_ref::<T>().is_some());
        ValueTyped(value, marker::PhantomData)
    }

    pub(crate) fn new_repr<A: AValue<'v, StarlarkValue = T>>(
        repr: &'v AValueRepr<A>,
    ) -> ValueTyped<'v, T> {
        ValueTyped(Value::new_repr(repr), marker::PhantomData)
    }

    /// Erase the type.
    pub fn to_value(self) -> Value<'v> {
        self.0
    }

    /// Get the reference to the pointed value.
    pub fn as_ref(self) -> &'v T {
        if PointerI32::type_is_pointer_i32::<T>() {
            unsafe {
                transmute!(
                    &PointerI32,
                    &T,
                    PointerI32::new(self.0.0.unpack_int_unchecked())
                )
            }
        } else {
            unsafe { &*(self.0.0.unpack_ptr_no_int_unchecked().payload_ptr() as *const T) }
        }
    }
}

impl<'v, T: StarlarkValue<'v>> FrozenValueTyped<'v, T> {
    /// Construct `FrozenValueTyped` without checking that the value is of correct type.
    pub unsafe fn new_unchecked(value: FrozenValue) -> FrozenValueTyped<'v, T> {
        debug_assert!(value.downcast_ref::<T>().is_some());
        FrozenValueTyped(value, marker::PhantomData)
    }

    /// Downcast.
    pub fn new(value: FrozenValue) -> Option<FrozenValueTyped<'v, T>> {
        value.downcast_ref::<T>()?;
        Some(FrozenValueTyped(value, marker::PhantomData))
    }

    pub(crate) fn new_repr<A: AValue<'v, StarlarkValue = T>>(
        repr: &'v AValueRepr<A>,
    ) -> FrozenValueTyped<'v, T> {
        // drop lifetime: `FrozenValue` is not (yet) parameterized with lifetime.
        let header = unsafe { cast::ptr_lifetime(&repr.header) };
        FrozenValueTyped(FrozenValue::new_ptr(header, A::IS_STR), marker::PhantomData)
    }

    /// Erase the type.
    pub fn to_frozen_value(self) -> FrozenValue {
        self.0
    }

    /// Convert to the value.
    #[inline]
    pub fn to_value(self) -> Value<'v> {
        self.0.to_value()
    }

    /// Convert to the value.
    pub fn to_value_typed(self) -> ValueTyped<'v, T> {
        unsafe { ValueTyped::new_unchecked(self.0.to_value()) }
    }

    /// Get the reference to the pointed value.
    #[inline]
    pub fn as_ref(self) -> &'v T {
        if PointerI32::type_is_pointer_i32::<T>() {
            unsafe {
                transmute!(
                    &PointerI32,
                    &T,
                    PointerI32::new(self.0.0.unpack_int_unchecked())
                )
            }
        } else if T::static_type_id() == StarlarkStr::static_type_id() {
            unsafe { &*(self.0.0.unpack_ptr_no_int_unchecked().payload_ptr() as *const T) }
        } else {
            // When a frozen pointer is not str and not int,
            // unpack is does not need untagging.
            // This generates slightly more efficient machine code.
            unsafe { &*(self.0.0.unpack_ptr_no_int_no_str_unchecked().payload_ptr() as *const T) }
        }
    }

    pub(crate) fn as_frozen_ref(self) -> FrozenRef<'v, T> {
        FrozenRef::new(self.as_ref())
    }
}

impl<'v> ValueTyped<'v, StarlarkStr> {
    /// Get the Rust string reference.
    pub fn as_str(self) -> &'v str {
        self.as_ref().as_str()
    }
}

impl<'v> FrozenValueTyped<'v, StarlarkStr> {
    /// Get the Rust string reference.
    pub fn as_str(self) -> &'v str {
        self.as_ref().as_str()
    }
}

unsafe impl<'v, T: StarlarkValue<'v>> Trace<'v> for ValueTyped<'v, T> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        tracer.trace(&mut self.0);
        // If type of value changed, dereference will produce the wrong object type.
        debug_assert!(self.0.downcast_ref::<T>().is_some());
    }
}

impl<'v, T: StarlarkValue<'v>> Deref for FrozenValueTyped<'v, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<'v, T: StarlarkValue<'v>> Deref for ValueTyped<'v, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<'v, T: StarlarkValue<'v>> StarlarkTypeRepr for ValueTyped<'v, T> {
    fn starlark_type_repr() -> String {
        T::starlark_type_repr()
    }
}

impl<'v, T: StarlarkValue<'v>> UnpackValue<'v> for ValueTyped<'v, T> {
    fn expected() -> String {
        T::get_type_value_static().as_str().to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        ValueTyped::new(value)
    }
}

impl<'v, T: StarlarkValue<'v>> AllocValue<'v> for ValueTyped<'v, T> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.0
    }
}

impl<'v, T: StarlarkValue<'v>> StarlarkTypeRepr for FrozenValueTyped<'v, T> {
    fn starlark_type_repr() -> String {
        T::starlark_type_repr()
    }
}

impl<'v, T: StarlarkValue<'v>> AllocValue<'v> for FrozenValueTyped<'v, T> {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        self.0.to_value()
    }
}

impl<'v, T: StarlarkValue<'v>> AllocFrozenValue for FrozenValueTyped<'v, T> {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::values::int::PointerI32;
    use crate::values::FrozenValue;
    use crate::values::FrozenValueTyped;
    use crate::values::StarlarkValue;

    #[test]
    fn int() {
        let v = FrozenValueTyped::<PointerI32>::new(FrozenValue::new_int(17)).unwrap();
        assert_eq!(17, v.as_ref().to_int().unwrap());
    }
}

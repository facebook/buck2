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

use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::StarlarkValue;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::vtable::AValueVTable;

pub(crate) const fn alloc_static<'v, A>(value: A::StarlarkValue) -> AValueRepr<AValueImpl<'v, A>>
where
    A: AValue<'v>,
{
    let payload = AValueImpl::<A>::new(value);
    AValueRepr::with_metadata(AValueVTable::new::<A>(), payload)
}

/// For types which are only allocated statically (never in heap).
/// Technically we can use `AValueSimple` for these, but this is more explicit and safe.
pub(crate) struct AValueBasic<T>(PhantomData<T>);

impl<'v, T: StarlarkValue<'v>> AValue<'v> for AValueBasic<T> {
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(_value: &T) -> usize {
        unreachable!("Basic types don't appear in the heap")
    }

    fn offset_of_extra() -> usize {
        unreachable!("Basic types don't appear in the heap")
    }

    unsafe fn heap_freeze(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        unreachable!("Basic types don't appear in the heap")
    }
    unsafe fn heap_copy(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unreachable!("Basic types don't appear in the heap")
    }

    fn total_memory_for_profile(_value: &Self::StarlarkValue) -> usize {
        // This avalue is always statically allocated so don't charge anyone for the memory.
        //
        // The fact that we need this at all is a bit weird - it comes about only because of the way
        // we do retained heap profiling. We first freeze the heap and then walk the *unfrozen* heap
        // looking for all the forwards. Since some non-statically allocated values freeze into
        // statically allocated ones (list, dict), that might point here
        0
    }
}

/// Allocate simple value statically.
pub struct AllocStaticSimple<T: StarlarkValue<'static>>(
    AValueRepr<AValueImpl<'static, AValueBasic<T>>>,
);

impl<T: StarlarkValue<'static>> AllocStaticSimple<T> {
    /// Allocate a value statically.
    pub const fn alloc(value: T) -> Self {
        AllocStaticSimple(alloc_static::<AValueBasic<T>>(value))
    }

    /// Get the value.
    pub fn unpack(&'static self) -> FrozenValueTyped<'static, T> {
        FrozenValueTyped::new_repr(&self.0)
    }

    /// Get the value.
    pub fn to_frozen_value(&'static self) -> FrozenValue {
        self.unpack().to_frozen_value()
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use starlark_derive::NoSerialize;
    use starlark_derive::ProvidesStaticType;
    use starlark_derive::starlark_value;

    use crate as starlark;
    use crate::values::AllocStaticSimple;
    use crate::values::StarlarkValue;

    #[test]
    fn test_alloc_static_simple() {
        #[derive(
            Debug,
            derive_more::Display,
            ProvidesStaticType,
            NoSerialize,
            Allocative
        )]
        #[display("MySimpleValue")]
        struct MySimpleValue(u32);

        #[starlark_value(type = "MySimpleValue")]
        impl<'v> StarlarkValue<'v> for MySimpleValue {}

        static VALUE: AllocStaticSimple<MySimpleValue> =
            AllocStaticSimple::alloc(MySimpleValue(17));
        assert_eq!(17, VALUE.unpack().as_ref().0);
    }
}

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
}

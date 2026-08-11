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

use std::any::TypeId;
use std::marker::PhantomData;
use std::mem;

use super::simple::AValueSimple;
use crate::eval::compiler::def::FrozenDef;
use crate::private::Private;
use crate::values::FreezeBranded;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::FrozenValueTyped;
use crate::values::Heap;
use crate::values::HeapSendable;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::AValueSimpleBound;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::avalue::try_freeze_directly;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;

struct AValueComplexBranded<T>(PhantomData<T>);

impl<'v, T> AValue<'v> for AValueComplexBranded<T>
where
    T: StarlarkValue<'v> + Trace<'v> + FreezeBranded,
    for<'fv> <T as FreezeBranded>::Frozen<'fv>: AValueSimpleBound<'fv>,
{
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(_value: &T) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self>()
    }

    unsafe fn heap_freeze<'fv>(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer<'fv>,
    ) -> FreezeResult<FrozenValue> {
        unsafe {
            if let Some(f) = try_freeze_directly::<Self>(me, freezer) {
                return f;
            }

            let (fv, r) = freezer.reserve::<AValueSimple<T::Frozen<'fv>>>();
            let x = AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_frozen(fv),
            );
            let res = x.freeze(freezer)?;
            r.fill(res);
            if TypeId::of::<T::Frozen<'static>>() == TypeId::of::<FrozenDef>() {
                let frozen_def = FrozenValueTyped::new(fv).unwrap();
                freezer.frozen_defs.borrow_mut().push(frozen_def);
            }
            Ok(fv)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe { heap_copy_impl::<Self>(me, tracer, Trace::trace) }
    }
}

impl<'v> Heap<'v> {
    /// Allocate a [`ComplexValue`] on the [`Heap`].
    pub fn alloc_complex_branded<T>(self, x: T) -> Value<'v>
    where
        T: StarlarkValue<'v> + HeapSendable<'v> + Trace<'v> + FreezeBranded,
        for<'fv> <T as FreezeBranded>::Frozen<'fv>: AValueSimpleBound<'fv>,
    {
        assert!(!T::is_special(Private));
        self.alloc_raw(AValueImpl::<AValueComplexBranded<T>>::new(x))
            .to_value()
    }
}

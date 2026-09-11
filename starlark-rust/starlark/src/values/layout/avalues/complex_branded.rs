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
use std::mem;

use crate::private::Private;
use crate::values::ComplexValue;
use crate::values::FreezePlan;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::Heap;
use crate::values::HeapSendable;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::freeze::FreezeDestination;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;

struct AValueComplexBranded<T>(PhantomData<T>);

impl<'v, T> AValue<'v> for AValueComplexBranded<T>
where
    T: ComplexValue<'v>,
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
        freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<Value<'fv>> {
        unsafe {
            let plan = (*me).payload.prepare_freeze(freezer)?;
            let destination = plan.target().reserve(freezer);
            let slot = match destination {
                FreezeDestination::Direct(frozen_value) => {
                    // The destination already exists, so the source payload is
                    // discarded here rather than consumed by `freeze_into`.
                    drop(AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                        me,
                        ForwardPtr::new_frozen(frozen_value),
                    ));
                    return Ok(frozen_value);
                }
                FreezeDestination::Slot(slot) => slot,
            };
            let value =
                AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(me, slot.forward_ptr());
            let fv = plan.freeze_into(value, freezer, slot)?.publish();
            if let Some(frozen_def) = ValueTyped::new(fv) {
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
    /// Allocate a value which can be traced (garbage collected) and frozen on the [`Heap`].
    pub fn alloc_complex_branded<T>(self, x: T) -> Value<'v>
    where
        T: ComplexValue<'v>,
        T: HeapSendable<'v>,
    {
        assert!(!T::is_special(Private));
        self.alloc_raw(AValueImpl::<AValueComplexBranded<T>>::new(x))
            .to_value()
    }
}

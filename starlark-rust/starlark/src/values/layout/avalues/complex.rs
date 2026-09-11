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

use std::any::type_name;
use std::marker::PhantomData;
use std::mem;

use crate::private::Private;
use crate::values::FreezeError;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::Heap;
use crate::values::HeapSendable;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::heap_copy_impl;
use crate::values::layout::heap::repr::AValueRepr;

#[derive(Debug, thiserror::Error)]
enum AValueError {
    #[error("Value of type `{0}` cannot be frozen")]
    CannotBeFrozen(&'static str),
}

pub(crate) struct AValueComplexNoFreeze<T>(PhantomData<T>);

impl<'v, T> AValue<'v> for AValueComplexNoFreeze<T>
where
    T: StarlarkValue<'v> + Trace<'v>,
{
    type StarlarkValue = T;

    type ExtraElem = ();

    fn extra_len(_value: &T) -> usize {
        0
    }

    fn offset_of_extra() -> usize {
        mem::size_of::<Self::StarlarkValue>()
    }

    unsafe fn heap_freeze<'fv>(
        _me: *mut AValueRepr<Self::StarlarkValue>,
        _freezer: &Freezer<'v, 'fv>,
    ) -> FreezeResult<Value<'fv>> {
        Err(FreezeError::new(
            AValueError::CannotBeFrozen(type_name::<T>()).to_string(),
        ))
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe { heap_copy_impl::<Self>(me, tracer, Trace::trace) }
    }
}

impl<'v> Heap<'v> {
    /// Allocate a value which can be traced (garbage collected), but cannot be frozen.
    pub fn alloc_complex_no_freeze<T>(self, x: T) -> Value<'v>
    where
        T: StarlarkValue<'v> + Trace<'v>,
        T: HeapSendable<'v>,
    {
        assert!(!T::is_special(Private));
        self.alloc_raw(AValueImpl::<AValueComplexNoFreeze<T>>::new(x))
            .to_value()
    }
}

// The fixtures deliberately model one Starlark-visible type with multiple
// frozen Rust representations sharing a single canonical type, the shape
// runtime-selected freezing exists for (see buck2's `cmd_args`).
#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use derive_more::Display;
    use starlark_derive::NoSerialize;
    use starlark_derive::StarlarkPagable;
    use starlark_derive::starlark_value;

    use crate as starlark;
    use crate::any::ProvidesStaticType;
    use crate::values::FreezeDynamic;
    use crate::values::FreezeError;
    use crate::values::FreezePlan;
    use crate::values::FreezeResult;
    use crate::values::FreezeSlot;
    use crate::values::FreezeTarget;
    use crate::values::Freezer;
    use crate::values::InitializedFreezeSlot;
    use crate::values::StarlarkValue;
    use crate::values::Trace;
    use crate::values::ValueLike;

    #[derive(
        Debug,
        Display,
        ProvidesStaticType,
        NoSerialize,
        Allocative,
        StarlarkPagable
    )]
    #[display("small dynamic freeze target")]
    struct SmallTarget(u32);

    #[starlark_value(type = "dynamic_freeze_target")]
    impl<'v> StarlarkValue<'v> for SmallTarget {
        type Canonical = Self;
    }

    #[derive(
        Debug,
        Display,
        ProvidesStaticType,
        NoSerialize,
        Allocative,
        StarlarkPagable
    )]
    #[display("large dynamic freeze target")]
    struct LargeTarget([u64; 4]);

    #[starlark_value(type = "dynamic_freeze_target")]
    impl<'v> StarlarkValue<'v> for LargeTarget {
        type Canonical = SmallTarget;
    }

    #[derive(Debug, Display, ProvidesStaticType, NoSerialize, Allocative, Trace)]
    #[display("dynamic freeze source")]
    struct FreezeDynamicValue {
        large: bool,
        fail: bool,
        panics: bool,
        wrong_target: bool,
    }

    impl FreezeDynamicValue {
        fn small() -> FreezeDynamicValue {
            FreezeDynamicValue {
                large: false,
                fail: false,
                panics: false,
                wrong_target: false,
            }
        }

        fn large() -> FreezeDynamicValue {
            FreezeDynamicValue {
                large: true,
                ..Self::small()
            }
        }

        fn failing() -> FreezeDynamicValue {
            FreezeDynamicValue {
                fail: true,
                ..Self::small()
            }
        }

        // Only referenced by the `cfg(panic = "unwind")` test below.
        #[cfg(panic = "unwind")]
        fn panicking() -> FreezeDynamicValue {
            FreezeDynamicValue {
                panics: true,
                ..Self::small()
            }
        }

        fn wrong_target() -> FreezeDynamicValue {
            FreezeDynamicValue {
                wrong_target: true,
                ..Self::small()
            }
        }
    }

    #[starlark_value(type = "dynamic_freeze_target", skip_vtable)]
    impl<'v> StarlarkValue<'v> for FreezeDynamicValue {
        type Canonical = SmallTarget;
    }

    enum DynamicPlan {
        Small,
        Large,
        Fail,
        Panic,
        WrongTarget,
    }

    impl<'v> FreezeDynamic<'v> for FreezeDynamicValue {
        type Plan<'fv> = DynamicPlan;

        fn prepare_freeze<'fv>(
            &self,
            _freezer: &Freezer<'v, 'fv>,
        ) -> FreezeResult<Self::Plan<'fv>> {
            Ok(if self.fail {
                DynamicPlan::Fail
            } else if self.panics {
                DynamicPlan::Panic
            } else if self.wrong_target {
                DynamicPlan::WrongTarget
            } else if self.large {
                DynamicPlan::Large
            } else {
                DynamicPlan::Small
            })
        }
    }

    impl<'v, 'fv> FreezePlan<'v, 'fv, FreezeDynamicValue> for DynamicPlan {
        fn target(&self) -> FreezeTarget<'fv> {
            match self {
                Self::Small | Self::Fail | Self::Panic | Self::WrongTarget => {
                    FreezeTarget::simple::<SmallTarget>()
                }
                Self::Large => FreezeTarget::simple::<LargeTarget>(),
            }
        }

        fn freeze_into(
            self,
            _value: FreezeDynamicValue,
            _freezer: &Freezer<'v, 'fv>,
            slot: FreezeSlot<'fv>,
        ) -> FreezeResult<InitializedFreezeSlot<'fv>> {
            match self {
                Self::Small => slot.write(SmallTarget(1)),
                Self::Large => slot.write(LargeTarget([2, 3, 5, 7])),
                Self::Fail => Err(FreezeError::new(
                    "intentional dynamic freeze failure".to_owned(),
                )),
                Self::Panic => panic!("intentional dynamic freeze panic"),
                Self::WrongTarget => slot.write(LargeTarget([11, 13, 17, 19])),
            }
        }
    }

    #[test]
    fn selects_different_runtime_frozen_types() {
        Freezer::testing_temp(|heap, freezer| {
            let small = heap.alloc_complex_branded(FreezeDynamicValue::small());
            let large = heap.alloc_complex_branded(FreezeDynamicValue::large());

            let small = freezer.freeze(small).expect("small value should freeze");
            let large = freezer.freeze(large).expect("large value should freeze");
            assert_eq!(
                1,
                small.downcast_ref::<SmallTarget>().expect("small target").0
            );
            assert_eq!(
                [2, 3, 5, 7],
                large.downcast_ref::<LargeTarget>().expect("large target").0,
            );
        });
    }

    #[test]
    fn failed_dynamic_freeze_leaves_frozen_heap_traversable() {
        Freezer::testing_temp(|heap, freezer| {
            let value = heap.alloc_complex_branded(FreezeDynamicValue::failing());

            freezer
                .freeze(value)
                .expect_err("the dynamic freeze plan should fail");
            let following = freezer
                .frozen_heap()
                .alloc_str("after failed dynamic freeze");
            assert_eq!("after failed dynamic freeze", following.as_str());
        });
    }

    // Buck builds tests with panic=abort, where unwinding does not exist;
    // this test is meaningful only in unwind builds such as Cargo's.
    #[cfg(panic = "unwind")]
    #[test]
    fn panicking_dynamic_freeze_leaves_heaps_droppable() {
        Freezer::testing_temp(|heap, freezer| {
            let value = heap.alloc_complex_branded(FreezeDynamicValue::panicking());

            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| freezer.freeze(value)))
                .expect_err("the dynamic freeze plan should panic");
            // The abandoned slot leaves a reservation header behind; the frozen
            // heap must remain allocatable, walkable, and droppable.
            let following = freezer
                .frozen_heap()
                .alloc_str("after panicking dynamic freeze");
            assert_eq!("after panicking dynamic freeze", following.as_str());
        });
    }

    // Buck builds tests with panic=abort, where unwinding does not exist;
    // this test is meaningful only in unwind builds such as Cargo's.
    #[cfg(all(debug_assertions, panic = "unwind"))]
    #[test]
    fn freezing_after_a_freeze_error_panics() {
        Freezer::testing_temp(|heap, freezer| {
            let value = heap.alloc_complex_branded(FreezeDynamicValue::failing());
            freezer
                .freeze(value)
                .expect_err("the dynamic freeze plan should fail");

            let following = heap.alloc_str("after the failed freeze").to_value();
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| freezer.freeze(following)))
                .expect_err("freezing through an abandoned freezer should panic");
        });
    }

    #[test]
    fn mismatched_freeze_target_is_an_error_not_a_panic() {
        Freezer::testing_temp(|heap, freezer| {
            let value = heap.alloc_complex_branded(FreezeDynamicValue::wrong_target());

            let error = freezer
                .freeze(value)
                .expect_err("writing a non-selected target should fail");
            assert!(
                error.err_msg.contains("different from its selected target"),
                "unexpected error: {}",
                error.err_msg
            );
            let following = freezer
                .frozen_heap()
                .alloc_str("after mismatched freeze target");
            assert_eq!("after mismatched freeze target", following.as_str());
        });
    }
}

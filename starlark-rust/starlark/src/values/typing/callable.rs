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

use allocative::Allocative;
use starlark_derive::starlark_value;
use starlark_derive::NoSerialize;
use starlark_derive::ProvidesStaticType;

use crate as starlark;
use crate::typing::Ty;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::AllocFrozenValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;

#[derive(
    Debug,
    derive_more::Display,
    Allocative,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "{}", Self::TYPE)]
pub(crate) struct TypingCallable;

#[starlark_value(type = "typing.Callable")]
impl<'v> StarlarkValue<'v> for TypingCallable {
    fn eval_type(&self) -> Option<Ty> {
        Some(Ty::any_function())
    }
}

impl AllocFrozenValue for TypingCallable {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        static CALLABLE: AValueRepr<AValueImpl<Basic, TypingCallable>> =
            alloc_static(Basic, TypingCallable);

        FrozenValue::new_repr(&CALLABLE)
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_callable_runtime() {
        assert::is_true("isinstance(lambda: None, typing.Callable)");
        assert::is_true("isinstance(len, typing.Callable)");
        assert::is_true("Rec = record(); isinstance(Rec, typing.Callable)");
        assert::is_false("isinstance(37, typing.Callable)");
    }

    #[test]
    fn test_callable_pass_compile_time() {
        assert::pass(
            r#"
Rec = record()

def foo(x: typing.Callable):
    pass

def bar():
    foo(len)
    foo(lambda x: 1)
    foo(Rec)
"#,
        );
    }

    #[test]
    fn test_callable_fail_compile_time() {
        assert::fail(
            r#"
def foo(x: typing.Callable):
    pass

def bar():
    foo(1)
"#,
            "Expected type",
        );
    }
}

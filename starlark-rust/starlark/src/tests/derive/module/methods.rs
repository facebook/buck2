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

use gazebo::any::ProvidesStaticType;

use crate as starlark;
use crate::assert::Assert;
use crate::environment::Methods;
use crate::environment::MethodsBuilder;
use crate::environment::MethodsStatic;
use crate::values::AllocFrozenValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::StarlarkValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, derive_more::Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "{:?}", self)]
struct Applaud {
    value: i32,
}

#[starlark_module]
fn methods(builder: &mut MethodsBuilder) {
    fn test_method(#[starlark(this)] receiver: Value, this: i32) -> anyhow::Result<i32> {
        let applauld = receiver.downcast_ref::<Applaud>().unwrap();
        Ok(applauld.value + this)
    }
}

impl<'v> StarlarkValue<'v> for Applaud {
    starlark_type!("applaud");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(methods)
    }
}

impl AllocFrozenValue for Applaud {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc_simple(self)
    }
}

#[test]
fn test_receiver_can_be_named_anything() {
    let mut a = Assert::new();
    a.globals_add(|g| g.set("x", g.alloc(Applaud { value: 10 })));
    a.eq("13", "x.test_method(this=3)");
}

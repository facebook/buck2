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

use std::sync::LazyLock;

use dupe::Dupe;

use crate::environment::Globals;
use crate::values::HeapEdge;
use crate::values::OwnedFrozen;
use crate::values::Value;
use crate::values::ValueTyped;
use crate::values::namespace::Namespace;

#[derive(Clone, Dupe, Debug)]
pub(crate) struct BuiltinFn(OwnedFrozen<Value<'static>>);

impl BuiltinFn {
    /// The function, for use with any heap.
    ///
    /// [`Constants`] is a process-wide static, so the heap it holds is immortal and the value can
    /// be brought to any brand, see [`HeapEdge::immortal`]; `&'static self` is the proof.
    pub(crate) fn at<'v>(&'static self) -> Value<'v> {
        HeapEdge::immortal().rebrand(self.0.as_ref().value())
    }

    /// Whether `v` is this function.
    pub(crate) fn is(&self, v: Value) -> bool {
        // Pointer equality works because `#[starlark_module]` proc macro
        // generates a singleton which allocates the function only once
        // even if builder function is called multiple times.
        self.0.by_ref(|f| f.ptr_eq(v))
    }
}

pub(crate) struct Constants {
    pub(crate) fn_len: BuiltinFn,
    pub(crate) fn_type: BuiltinFn,
    pub(crate) fn_list: BuiltinFn,
    pub(crate) fn_dict: BuiltinFn,
    pub(crate) fn_tuple: BuiltinFn,
    pub(crate) fn_isinstance: BuiltinFn,
    // Technically, this is not a function.
    pub(crate) typing_callable: BuiltinFn,
}

impl Constants {
    pub fn get() -> &'static Constants {
        static RES: LazyLock<Constants> = LazyLock::new(|| {
            let g = Globals::extended_internal();
            let builtin = |name| BuiltinFn(g.get_owned(name).unwrap());
            Constants {
                fn_len: builtin("len"),
                fn_type: builtin("type"),
                fn_list: builtin("list"),
                fn_dict: builtin("dict"),
                fn_tuple: builtin("tuple"),
                fn_isinstance: builtin("isinstance"),
                typing_callable: BuiltinFn(
                    g.get_owned("typing")
                        .unwrap()
                        .by_ref_with_reconstructor(|typing, r| {
                            let typing = ValueTyped::<Namespace>::new(*typing).unwrap();
                            r.reconstruct(typing.as_ref().get("Callable").unwrap())
                        }),
                ),
            }
        });
        LazyLock::force(&RES)
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Globals;
    use crate::eval::compiler::constants::Constants;

    #[test]
    fn test_constants() {
        for globals in [Globals::standard(), Globals::extended_internal()] {
            let len = globals.get_owned("len").unwrap();
            assert!(len.by_ref(|len| Constants::get().fn_len.is(*len)));
        }
    }
}

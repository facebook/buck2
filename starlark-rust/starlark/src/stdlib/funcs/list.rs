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

use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::values::function::SpecialBuiltinFunction;
use crate::values::list::value::FrozenList;
use crate::values::list::AllocList;
use crate::values::list::ListRef;
use crate::values::Heap;
use crate::values::StarlarkIter;
use crate::values::Value;
use crate::values::ValueOfUnchecked;

#[starlark_module]
pub(crate) fn register_list(globals: &mut GlobalsBuilder) {
    /// [list](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#list
    /// ): construct a list.
    ///
    /// `list(x)` returns a new list containing the elements of the
    /// iterable sequence x.
    ///
    /// With no argument, `list()` returns a new empty list.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// list()        == []
    /// list((1,2,3)) == [1, 2, 3]
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// list("strings are not iterable") # error: not supported
    /// # "#, r#"Operation `(iter)` not supported on type `string`"#);
    /// ```
    #[starlark(
    as_type = FrozenList,
    speculative_exec_safe,
    special_builtin_function = SpecialBuiltinFunction::List,
    )]
    fn list<'v>(
        #[starlark(require = pos)] a: Option<ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueOfUnchecked<'v, &'v ListRef<'v>>> {
        Ok(ValueOfUnchecked::new(if let Some(a) = a {
            if let Some(xs) = ListRef::from_value(a.get()) {
                heap.alloc_list(xs.content())
            } else {
                let it = a.get().iterate(heap)?;
                heap.alloc(AllocList(it))
            }
        } else {
            heap.alloc(AllocList::EMPTY)
        }))
    }
}

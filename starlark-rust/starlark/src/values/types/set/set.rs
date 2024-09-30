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
use crate::values::set::refs::SetRef;
use crate::values::set::value::SetData;
use crate::values::typing::StarlarkIter;
use crate::values::Heap;
use crate::values::Value;
use crate::values::ValueOfUnchecked;

#[starlark_module]
pub(crate) fn register_set(globals: &mut GlobalsBuilder) {
    #[starlark(speculative_exec_safe)]
    fn set<'v>(
        #[starlark(require = pos)] arg: Option<ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>>,
        heap: &'v Heap,
    ) -> starlark::Result<SetData<'v>> {
        let set = match arg {
            Some(pos) => match SetRef::from_value(pos.get()) {
                Some(set) => (*set).clone(),
                None => {
                    let it = pos.get().iterate(heap)?;
                    let mut data = SetData::default();
                    for el in it {
                        let el = el.get_hashed()?;
                        data.content.insert_hashed(el);
                    }
                    data
                }
            },
            None => SetData::default(),
        };
        Ok(set)
    }
}

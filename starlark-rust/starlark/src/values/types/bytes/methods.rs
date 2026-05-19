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
use crate::environment::MethodsBuilder;
use crate::values::Heap;
use crate::values::ValueOfUnchecked;
use crate::values::types::bytes::value::StarlarkBytes;
use crate::values::typing::StarlarkIter;

#[starlark_module]
pub(crate) fn bytes_methods(builder: &mut MethodsBuilder) {
    /// Returns an iterable over the individual bytes as 1-byte bytes objects.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// list(b"abc".elems()) == [b"a", b"b", b"c"]
    /// # "#);
    /// ```
    fn elems<'v>(
        this: &'v StarlarkBytes,
        heap: Heap<'v>,
    ) -> anyhow::Result<ValueOfUnchecked<'v, StarlarkIter<StarlarkBytes>>> {
        let it = this
            .as_bytes()
            .iter()
            .map(|&b| heap.alloc(StarlarkBytes::new(&[b])));
        Ok(ValueOfUnchecked::new(heap.alloc_tuple_iter(it)))
    }
}

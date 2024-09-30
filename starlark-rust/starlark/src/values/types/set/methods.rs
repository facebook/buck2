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

//! Methods for the `set` type.

use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::MethodsBuilder;
use crate::values::none::NoneType;
use crate::values::set::refs::SetMut;
use crate::values::Value;

#[starlark_module]
pub(crate) fn set_methods(builder: &mut MethodsBuilder) {
    fn clear(this: Value) -> anyhow::Result<NoneType> {
        let mut this = SetMut::from_value(this)?;
        this.clear();
        Ok(NoneType)
    }
}
#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_empty() {
        assert::is_true("s = set(); len(s) == 0")
    }

    #[test]
    fn test_single() {
        assert::is_true("s = set([0, 1]); len(s) == 2")
    }

    #[test]
    fn test_eq() {
        assert::is_true("set([1, 2, 3]) == set([3, 2, 1])")
    }

    #[test]
    fn test_clear() {
        assert::is_true("s = set([1, 2, 3]); s.clear(); s == set()")
    }

    #[test]
    fn test_type() {
        assert::eq("type(set([1, 2, 3]))", "'set'")
    }

    #[test]
    fn test_iter() {
        assert::is_true("list([elem for elem in set([1, 2, 3])]) ==  [1, 2, 3]")
    }

    #[test]
    fn test_bool_true() {
        assert::is_true("bool(set([1, 2, 3]))")
    }

    #[test]
    fn test_bool_false() {
        assert::is_false("bool(set())")
    }
}

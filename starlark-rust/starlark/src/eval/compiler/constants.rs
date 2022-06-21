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

use gazebo::dupe::Dupe;
use once_cell::sync::Lazy;

use crate::environment::Globals;
use crate::values::FrozenValue;

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct BuiltinFn(pub(crate) FrozenValue);

impl PartialEq<FrozenValue> for BuiltinFn {
    fn eq(&self, other: &FrozenValue) -> bool {
        self.0.to_value().ptr_eq(other.to_value())
    }
}

impl PartialEq<BuiltinFn> for FrozenValue {
    fn eq(&self, other: &BuiltinFn) -> bool {
        other == self
    }
}

pub(crate) struct Constants {
    pub(crate) fn_len: BuiltinFn,
    pub(crate) fn_type: BuiltinFn,
}

impl Constants {
    pub fn get() -> &'static Constants {
        static RES: Lazy<Constants> = Lazy::new(|| {
            let g = Globals::standard();
            Constants {
                fn_len: BuiltinFn(g.get_frozen("len").unwrap()),
                fn_type: BuiltinFn(g.get_frozen("type").unwrap()),
            }
        });
        Lazy::force(&RES)
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::Globals;
    use crate::eval::compiler::constants::Constants;

    #[test]
    fn test_constants() {
        assert_eq!(
            Globals::standard().get_frozen("len").unwrap(),
            Constants::get().fn_len
        );
        assert_eq!(
            Globals::extended().get_frozen("len").unwrap(),
            Constants::get().fn_len
        );
    }
}

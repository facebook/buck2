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

use crate as starlark;
use crate::{environment::GlobalsBuilder, values::Value};

pub(crate) fn json(globals: &mut GlobalsBuilder) {
    #[starlark_module]
    fn json_members(globals: &mut GlobalsBuilder) {
        fn encode(ref x: Value) -> anyhow::Result<String> {
            x.to_json()
        }
    }

    // Copying Bazel's json module: https://bazel.build/rules/lib/json
    // or starlark-go json module:
    // https://github.com/google/starlark-go/blob/d1966c6b9fcd6631f48f5155f47afcd7adcc78c2/lib/json/json.go#L28
    globals.struct_("json", json_members);
}

#[cfg(test)]
mod tests {
    use crate::assert::Assert;

    #[test]
    fn test_json_encode() {
        let a = Assert::new();
        a.eq("'[10]'", "json.encode([10])");
    }
}

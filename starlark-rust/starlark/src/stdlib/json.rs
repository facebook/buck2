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

use std::str::FromStr;

use gazebo::prelude::*;
use num_bigint::BigInt;
use thiserror::Error;

use crate as starlark;
use crate::collections::SmallMap;
use crate::environment::GlobalsBuilder;
use crate::values::dict::Dict;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::Heap;
use crate::values::Value;

#[derive(Debug, Error)]
enum JsonError {
    #[error("Number can't be represented, perhaps a float value that is too precise, got `{0}")]
    UnrepresentableNumber(String),
}

fn serde_to_starlark<'v>(x: serde_json::Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
    match x {
        serde_json::Value::Null => Ok(Value::new_none()),
        serde_json::Value::Bool(x) => Ok(Value::new_bool(x)),
        serde_json::Value::Number(x) => {
            if let Some(x) = x.as_u64() {
                Ok(heap.alloc(x))
            } else if let Some(x) = x.as_f64() {
                Ok(heap.alloc(x))
            } else if let Ok(x) = BigInt::from_str(&x.to_string()) {
                Ok(StarlarkBigInt::alloc_bigint(x, heap))
            } else {
                Err(JsonError::UnrepresentableNumber(x.to_string()).into())
            }
        }
        serde_json::Value::String(x) => Ok(heap.alloc(x)),
        serde_json::Value::Array(x) => {
            Ok(heap.alloc_list_iter(x.into_try_map(|v| serde_to_starlark(v, heap))?))
        }
        serde_json::Value::Object(x) => {
            let mut mp = SmallMap::with_capacity(x.len());
            for (k, v) in x {
                let k = heap.alloc_str(&k).get_hashed_value();
                let v = serde_to_starlark(v, heap)?;
                mp.insert_hashed(k, v);
            }
            Ok(heap.alloc(Dict::new(mp)))
        }
    }
}

pub(crate) fn json(globals: &mut GlobalsBuilder) {
    #[starlark_module]
    fn json_members(globals: &mut GlobalsBuilder) {
        fn encode(#[starlark(require = pos)] x: Value) -> anyhow::Result<String> {
            x.to_json()
        }

        fn decode<'v>(
            #[starlark(require = pos)] x: &str,
            heap: &'v Heap,
        ) -> anyhow::Result<Value<'v>> {
            serde_to_starlark(serde_json::from_str(x)?, heap)
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

    #[test]
    fn test_json_decode() {
        let a = Assert::new();
        a.eq(
            "[10, None, False, {'k': 'v'}]",
            "json.decode('[10, null, false, {\"k\": \"v\"}]')",
        );

        a.eq("3.142", "json.decode('3.142')");
        a.eq(
            "123456789123456789123456789",
            "json.decode('123456789123456789123456789')",
        );
    }
}

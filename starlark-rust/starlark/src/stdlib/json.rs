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

// https://github.com/rust-lang/rust-clippy/issues/11142
#![allow(clippy::needless_borrow)]

use std::str::FromStr;

use either::Either;
use num_bigint::BigInt;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::collections::SmallMap;
use crate::environment::GlobalsBuilder;
use crate::typing::Ty;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::Value;
use crate::values::dict::AllocDict;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::int::int_or_big::StarlarkInt;

impl StarlarkTypeRepr for serde_json::Number {
    type Canonical = Either<i32, f64>;

    fn starlark_type_repr() -> Ty {
        Either::<i32, f64>::starlark_type_repr()
    }
}

impl<'a> StarlarkTypeRepr for &'a serde_json::Number {
    type Canonical = serde_json::Number;

    fn starlark_type_repr() -> Ty {
        serde_json::Number::starlark_type_repr()
    }
}

impl<'v, 'a> AllocValue<'v> for &'a serde_json::Number {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        if let Some(x) = self.as_u64() {
            heap.alloc(x)
        } else if let Some(x) = self.as_i64() {
            heap.alloc(x)
        } else if let Some(x) = self.as_f64() {
            heap.alloc(x)
        } else if let Ok(x) = BigInt::from_str(&self.to_string()) {
            heap.alloc(StarlarkInt::from(x))
        } else {
            panic!("Unrepresentable number: {self:?}")
        }
    }
}

impl<'v> AllocValue<'v> for serde_json::Number {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
    }
}

impl<'a> AllocFrozenValue for &'a serde_json::Number {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        if let Some(x) = self.as_u64() {
            heap.alloc(x)
        } else if let Some(x) = self.as_i64() {
            heap.alloc(x)
        } else if let Some(x) = self.as_f64() {
            heap.alloc(x)
        } else if let Ok(x) = BigInt::from_str(&self.to_string()) {
            heap.alloc(StarlarkInt::from(x))
        } else {
            panic!("Unrepresentable number: {self:?}")
        }
    }
}

impl AllocFrozenValue for serde_json::Number {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
    }
}

impl<'a, K: StarlarkTypeRepr, V: StarlarkTypeRepr> StarlarkTypeRepr for &'a serde_json::Map<K, V> {
    type Canonical = <serde_json::Map<K, V> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        AllocDict::<SmallMap<K, V>>::starlark_type_repr()
    }
}

impl<K: StarlarkTypeRepr, V: StarlarkTypeRepr> StarlarkTypeRepr for serde_json::Map<K, V> {
    type Canonical = <SmallMap<K, V> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        AllocDict::<SmallMap<K, V>>::starlark_type_repr()
    }
}

impl<'a, 'v> AllocValue<'v> for &'a serde_json::Map<String, serde_json::Value> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        // For some reason `IntoIter` is implemented only for `String, Value` parameters,
        // so this implementation is limited.
        heap.alloc(AllocDict(self))
    }
}

impl<'v> AllocValue<'v> for serde_json::Map<String, serde_json::Value> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
    }
}

impl<'a> AllocFrozenValue for &'a serde_json::Map<String, serde_json::Value> {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc(AllocDict(self.iter().map(|(k, v)| (k.as_str(), v))))
    }
}

impl AllocFrozenValue for serde_json::Map<String, serde_json::Value> {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
    }
}

impl<'a> StarlarkTypeRepr for &'a serde_json::Value {
    type Canonical = <serde_json::Value as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        // Any.
        Value::starlark_type_repr()
    }
}

impl StarlarkTypeRepr for serde_json::Value {
    type Canonical = <FrozenValue as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        // Any.
        Value::starlark_type_repr()
    }
}

impl<'v, 'a> AllocValue<'v> for &'a serde_json::Value {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        match self {
            serde_json::Value::Null => Value::new_none(),
            serde_json::Value::Bool(x) => Value::new_bool(*x),
            serde_json::Value::Number(x) => heap.alloc(x),
            serde_json::Value::String(x) => heap.alloc(x.as_str()),
            serde_json::Value::Array(x) => heap.alloc(x.as_slice()),
            serde_json::Value::Object(x) => heap.alloc(x),
        }
    }
}

impl<'v> AllocValue<'v> for serde_json::Value {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
    }
}

impl<'a> AllocFrozenValue for &'a serde_json::Value {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match self {
            serde_json::Value::Null => FrozenValue::new_none(),
            serde_json::Value::Bool(x) => FrozenValue::new_bool(*x),
            serde_json::Value::Number(x) => heap.alloc(x),
            serde_json::Value::String(x) => heap.alloc(x.as_str()),
            serde_json::Value::Array(x) => heap.alloc(x.as_slice()),
            serde_json::Value::Object(x) => heap.alloc(x),
        }
    }
}

impl AllocFrozenValue for serde_json::Value {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        // If you follow this hint, it becomes infinite recursion
        #[allow(clippy::needless_borrows_for_generic_args)]
        heap.alloc(&self)
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
            heap: Heap<'v>,
        ) -> anyhow::Result<Value<'v>> {
            Ok(heap.alloc(serde_json::from_str::<serde_json::Value>(x)?))
        }
    }

    // Copying Bazel's json module: https://bazel.build/rules/lib/json
    // or starlark-go json module:
    // https://github.com/google/starlark-go/blob/d1966c6b9fcd6631f48f5155f47afcd7adcc78c2/lib/json/json.go#L28
    globals.namespace("json", json_members);
}

#[cfg(test)]
mod tests {
    use crate::assert::Assert;

    #[test]
    fn test_json_encode() {
        let a = Assert::new();
        a.eq("'[10]'", "json.encode([10])");

        // Test small integer
        a.eq("'1'", "json.encode(1)");

        // Test 63-bit integer (largest 63-bit signed integer)
        a.eq("'9223372036854775807'", "json.encode(9223372036854775807)");
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

    #[test]
    fn test_json_very_large_int() {
        let a = Assert::new();

        // Test edge cases: numbers at the boundary of different ranges

        // u64::MAX - should be encoded as unquoted number
        a.eq(
            "'18446744073709551615'",
            "json.encode(18446744073709551615)",
        );
        a.eq(
            "18446744073709551615",
            "json.decode(json.encode(18446744073709551615))",
        );

        // i64::MIN - should be encoded as unquoted number
        a.eq(
            "'-9223372036854775808'",
            "json.encode(-9223372036854775808)",
        );
        a.eq(
            "-9223372036854775808",
            "json.decode(json.encode(-9223372036854775808))",
        );

        // Test decoding of very large numbers from JSON strings
        a.eq(
            "18446744073709551616",
            "json.decode('18446744073709551616')",
        );
        a.eq(
            "-9223372036854775809",
            "json.decode('-9223372036854775809')",
        );
    }

    #[test]
    fn test_json_128bit_and_beyond() {
        let a = Assert::new();

        // Test 128-bit boundary cases

        // 2^64 (u64::MAX + 1) - first number requiring more than 64 bits
        a.eq(
            "18446744073709551616",
            "json.decode('18446744073709551616')",
        );
        a.eq(
            "18446744073709551616",
            "json.decode(json.encode(18446744073709551616))",
        );

        // 2^100 - large 128-bit number
        a.eq(
            "1267650600228229401496703205376",
            "json.decode('1267650600228229401496703205376')",
        );
        a.eq(
            "1267650600228229401496703205376",
            "json.decode(json.encode(1267650600228229401496703205376))",
        );

        // 2^128 - 1 (largest 128-bit unsigned integer)
        a.eq(
            "340282366920938463463374607431768211455",
            "json.decode('340282366920938463463374607431768211455')",
        );
        a.eq(
            "340282366920938463463374607431768211455",
            "json.decode(json.encode(340282366920938463463374607431768211455))",
        );

        // 2^128 (first number requiring more than 128 bits)
        a.eq(
            "340282366920938463463374607431768211456",
            "json.decode('340282366920938463463374607431768211456')",
        );
        a.eq(
            "340282366920938463463374607431768211456",
            "json.decode(json.encode(340282366920938463463374607431768211456))",
        );

        // Beyond 128-bit: 2^200
        a.eq(
            "1606938044258990275541962092341162602522202993782792835301376",
            "json.decode('1606938044258990275541962092341162602522202993782792835301376')",
        );
        a.eq(
            "1606938044258990275541962092341162602522202993782792835301376",
            "json.decode(json.encode(1606938044258990275541962092341162602522202993782792835301376))"
        );

        // 2^256 - extremely large number
        a.eq(
            "115792089237316195423570985008687907853269984665640564039457584007913129639936",
            "json.decode('115792089237316195423570985008687907853269984665640564039457584007913129639936')"
        );
        a.eq(
            "115792089237316195423570985008687907853269984665640564039457584007913129639936",
            "json.decode(json.encode(115792089237316195423570985008687907853269984665640564039457584007913129639936))"
        );

        // Test negative 128-bit and beyond numbers

        // -(2^100)
        a.eq(
            "-1267650600228229401496703205376",
            "json.decode('-1267650600228229401496703205376')",
        );
        a.eq(
            "-1267650600228229401496703205376",
            "json.decode(json.encode(-1267650600228229401496703205376))",
        );

        // -(2^128)
        a.eq(
            "-340282366920938463463374607431768211456",
            "json.decode('-340282366920938463463374607431768211456')",
        );
        a.eq(
            "-340282366920938463463374607431768211456",
            "json.decode(json.encode(-340282366920938463463374607431768211456))",
        );

        // -(2^200)
        a.eq(
            "-1606938044258990275541962092341162602522202993782792835301376",
            "json.decode('-1606938044258990275541962092341162602522202993782792835301376')",
        );
        a.eq(
            "-1606938044258990275541962092341162602522202993782792835301376",
            "json.decode(json.encode(-1606938044258990275541962092341162602522202993782792835301376))"
        );

        // -(2^256)
        a.eq(
            "-115792089237316195423570985008687907853269984665640564039457584007913129639936",
            "json.decode('-115792089237316195423570985008687907853269984665640564039457584007913129639936')"
        );
        a.eq(
            "-115792089237316195423570985008687907853269984665640564039457584007913129639936",
            "json.decode(json.encode(-115792089237316195423570985008687907853269984665640564039457584007913129639936))"
        );

        // Test extremely large numbers beyond typical use cases

        // 2^512 - cryptocurrency/cryptographic scale number
        let large_512bit = "13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096";
        a.eq(large_512bit, &format!("json.decode('{large_512bit}')"));
        a.eq(
            large_512bit,
            &format!("json.decode(json.encode({large_512bit}))"),
        );

        // Test large decimal with many digits (308 digits - close to f64 limit but still manageable)
        let large_number = "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        a.eq(large_number, &format!("json.decode('{large_number}')"));
        a.eq(
            large_number,
            &format!("json.decode(json.encode({large_number}))"),
        );
    }
}

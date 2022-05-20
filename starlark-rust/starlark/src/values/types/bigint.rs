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

//! Outside of `i32` range int.

use std::{cmp::Ordering, hash::Hash, ops::Not};

use gazebo::any::AnyLifetime;
use num_bigint::{BigInt, Sign};
use num_traits::{cast::ToPrimitive, Signed, Zero};
use serde::Serialize;

use crate::{
    collections::StarlarkHasher,
    values::{
        float::StarlarkFloat, num::Num, AllocFrozenValue, AllocValue, FrozenHeap, FrozenValue,
        Heap, StarlarkValue, Value, ValueError,
    },
};

/// `int` implementation for larger integers.
#[derive(
    Clone,
    Debug,
    Default,
    derive_more::Display,
    AnyLifetime,
    Ord,
    PartialOrd,
    Eq,
    PartialEq
)]
#[display(fmt = "{}", value)]
pub struct StarlarkBigInt {
    /// `value` is strictly either smaller than `i32::MIN` or larger than `i32::MAX`.
    /// Many operation implementations depend on this fact.
    /// For example, `non_zero_int << positive_big_int` is considered to be overflow
    /// without checking the actual value of `positive_big_int`.
    value: BigInt,
}

impl StarlarkBigInt {
    fn unchecked_new(value: BigInt) -> Self {
        debug_assert!(
            value.to_i32().is_none(),
            "BigInt must be outside of i32 range"
        );
        Self { value }
    }

    pub(crate) fn try_from_bigint(value: BigInt) -> Result<StarlarkBigInt, i32> {
        match value.to_i32() {
            Some(i) => Err(i),
            None => Ok(StarlarkBigInt::unchecked_new(value)),
        }
    }

    pub(crate) fn get(&self) -> &BigInt {
        &self.value
    }

    pub(crate) fn to_f64(&self) -> f64 {
        // `to_f64` is infallible.
        self.value.to_f64().unwrap()
    }

    pub(crate) fn alloc_bigint<'v>(value: BigInt, heap: &'v Heap) -> Value<'v> {
        match Self::try_from_bigint(value) {
            Ok(bigint) => heap.alloc_simple(bigint),
            Err(i) => Value::new_int(i),
        }
    }

    pub(crate) fn alloc_bigint_frozen(value: BigInt, heap: &FrozenHeap) -> FrozenValue {
        match Self::try_from_bigint(value) {
            Ok(bigint) => heap.alloc_simple(bigint),
            Err(i) => FrozenValue::new_int(i),
        }
    }

    pub(crate) fn cmp_small_big(a: i32, b: &StarlarkBigInt) -> Ordering {
        let a_sign = a.signum();
        let b_sign = match b.value.sign() {
            Sign::Plus => 2,
            Sign::Minus => -2,
            Sign::NoSign => 0,
        };
        // Sign comparison is enough because `StarlarkBigInt` is out of range of `i32`.
        a_sign.cmp(&b_sign)
    }

    pub(crate) fn cmp_big_small(a: &StarlarkBigInt, b: i32) -> Ordering {
        Self::cmp_small_big(b, a).reverse()
    }

    fn signum(b: &BigInt) -> i32 {
        match b.sign() {
            Sign::Plus => 1,
            Sign::Minus => -1,
            Sign::NoSign => 0,
        }
    }

    pub(crate) fn floor_div_big<'v>(
        a: &BigInt,
        b: &BigInt,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if b.is_zero() {
            return Err(ValueError::DivisionByZero.into());
        }
        let sig = Self::signum(b) * Self::signum(a);
        // TODO(nga): optimize.
        let offset = if sig < 0 && (a % b).is_zero().not() {
            1
        } else {
            0
        };
        Ok(StarlarkBigInt::alloc_bigint((a / b) - offset, heap))
    }

    pub(crate) fn percent_big<'v>(
        a: &BigInt,
        b: &BigInt,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if b.is_zero() {
            return Err(ValueError::DivisionByZero.into());
        }
        let r = a % b;
        if r.is_zero() {
            Ok(Value::new_int(0))
        } else {
            Ok(StarlarkBigInt::alloc_bigint(
                if b.sign() != r.sign() { r + b } else { r },
                heap,
            ))
        }
    }
}

impl PartialEq<i32> for StarlarkBigInt {
    fn eq(&self, _other: &i32) -> bool {
        false
    }
}

impl Serialize for StarlarkBigInt {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.value.to_string())
    }
}

impl<'v> StarlarkValue<'v> for StarlarkBigInt {
    starlark_type!("int");

    fn to_bool(&self) -> bool {
        // `StarlarkBigInt` is non-zero.
        true
    }

    fn minus(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(StarlarkBigInt::alloc_bigint(-&self.value, heap))
    }

    fn plus(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // This unnecessarily allocates, could return `self`.
        // But practically people rarely write `+NNN` except in constants,
        // and in constants we fold `+NNN` into `NNN`.
        Ok(StarlarkBigInt::alloc_bigint(self.value.clone(), heap))
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match other.unpack_num() {
            None => Ok(false),
            Some(Num::Int(_)) => {
                // `StarlarkBigInt` is out of range of `i32`.
                Ok(false)
            }
            Some(Num::BigInt(other)) => Ok(self == other),
            Some(Num::Float(f)) => Ok(self.to_f64() == f),
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match other.unpack_num() {
            None => ValueError::unsupported_with(self, "compare", other),
            Some(Num::BigInt(b)) => Ok(self.value.cmp(&b.value)),
            Some(Num::Int(i)) => Ok(StarlarkBigInt::cmp_big_small(self, i)),
            Some(Num::Float(f)) => Ok(StarlarkFloat::compare_impl(self.to_f64(), f)),
        }
    }

    fn add(&self, rhs: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        match rhs.unpack_num()? {
            Num::Int(i) => Some(Ok(StarlarkBigInt::alloc_bigint(&self.value + i, heap))),
            Num::BigInt(b) => Some(Ok(StarlarkBigInt::alloc_bigint(
                &self.value + &b.value,
                heap,
            ))),
            Num::Float(f) => Some(Ok(heap.alloc_float(StarlarkFloat(self.to_f64() + f)))),
        }
    }

    fn sub(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_num() {
            Some(rhs) => rhs,
            None => return ValueError::unsupported_with(self, "-", other),
        };
        match rhs {
            Num::Int(i) => Ok(StarlarkBigInt::alloc_bigint(&self.value - i, heap)),
            Num::BigInt(b) => Ok(StarlarkBigInt::alloc_bigint(&self.value - &b.value, heap)),
            Num::Float(f) => Ok(heap.alloc_float(StarlarkFloat(self.to_f64() - f))),
        }
    }

    fn mul(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_num() {
            Some(rhs) => rhs,
            None => return ValueError::unsupported_with(self, "*", other),
        };
        match rhs {
            Num::Int(i) => Ok(StarlarkBigInt::alloc_bigint(&self.value * i, heap)),
            Num::BigInt(b) => Ok(StarlarkBigInt::alloc_bigint(&self.value * &b.value, heap)),
            Num::Float(f) => Ok(heap.alloc_float(StarlarkFloat(self.to_f64() * f))),
        }
    }

    fn div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        if other.unpack_num().is_some() {
            StarlarkFloat(self.to_f64()).div(other, heap)
        } else {
            ValueError::unsupported_with(self, "/", other)
        }
    }

    fn floor_div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_num() {
            Some(rhs) => rhs,
            None => return ValueError::unsupported_with(self, "//", other),
        };
        let b;
        let b = match rhs {
            Num::Float(f) => {
                return Ok(
                    heap.alloc_float(StarlarkFloat(StarlarkFloat::floor_div_impl(
                        self.to_f64(),
                        f,
                    )?)),
                );
            }
            Num::Int(i) => {
                if i == 0 {
                    return Err(ValueError::DivisionByZero.into());
                }
                // TODO(nga): do not allocate
                b = BigInt::from(i);
                &b
            }
            Num::BigInt(b) => &b.value,
        };
        StarlarkBigInt::floor_div_big(&self.value, b, heap)
    }

    fn percent(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_num() {
            Some(rhs) => rhs,
            None => return ValueError::unsupported_with(self, "%", other),
        };
        let b;
        let b = match rhs {
            Num::Float(f) => {
                return Ok(heap.alloc_float(StarlarkFloat(StarlarkFloat::percent_impl(
                    self.to_f64(),
                    f,
                )?)));
            }
            Num::Int(i) => {
                // TODO(nga): do not allocate.
                b = BigInt::from(i);
                &b
            }
            Num::BigInt(b) => &b.value,
        };
        StarlarkBigInt::percent_big(&self.value, b, heap)
    }

    fn to_int(&self) -> anyhow::Result<i32> {
        Err(ValueError::IntegerOverflow.into())
    }

    fn bit_and(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_int_or_big() {
            Some(rhs) => rhs,
            None => return ValueError::unsupported_with(self, "&", other),
        };
        Ok(StarlarkBigInt::alloc_bigint(&self.value & &*rhs, heap))
    }

    fn bit_xor(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_int_or_big() {
            None => return ValueError::unsupported_with(self, "^", other),
            Some(rhs) => rhs,
        };
        Ok(StarlarkBigInt::alloc_bigint(&self.value ^ &*rhs, heap))
    }

    fn bit_or(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_int_or_big() {
            None => return ValueError::unsupported_with(self, "|", other),
            Some(rhs) => rhs,
        };
        Ok(StarlarkBigInt::alloc_bigint(&self.value | &*rhs, heap))
    }

    fn bit_not(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(StarlarkBigInt::alloc_bigint(!&self.value, heap))
    }

    fn left_shift(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, "<<", other),
            Some(Num::Int(i)) => {
                if i < 0 {
                    Err(ValueError::NegativeShiftCount.into())
                } else {
                    Ok(StarlarkBigInt::alloc_bigint(&self.value << i, heap))
                }
            }
            Some(Num::BigInt(b)) => {
                if b.value.is_negative() {
                    Err(ValueError::NegativeShiftCount.into())
                } else {
                    Err(ValueError::IntegerOverflow.into())
                }
            }
        }
    }

    #[allow(clippy::collapsible_else_if)]
    fn right_shift(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, ">>", other),
            Some(Num::Int(i)) => {
                if i < 0 {
                    Err(ValueError::NegativeShiftCount.into())
                } else {
                    Ok(StarlarkBigInt::alloc_bigint(&self.value >> i, heap))
                }
            }
            Some(Num::BigInt(b)) => {
                if b.value.is_negative() {
                    Err(ValueError::NegativeShiftCount.into())
                } else {
                    if self.value.is_negative() {
                        Ok(Value::new_int(-1))
                    } else {
                        Ok(Value::new_int(0))
                    }
                }
            }
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        Num::BigInt(self).get_hash_64().hash(hasher);
        Ok(())
    }

    fn extra_memory(&self) -> usize {
        // We don't know the capacity, but this is a good approximation.
        self.value.magnitude().iter_u64_digits().len() * 8
    }
}

impl<'v> AllocValue<'v> for u64 {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for u64 {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

impl<'v> AllocValue<'v> for i64 {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match i32::try_from(self) {
            Ok(x) => Value::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint(self.into(), heap),
        }
    }
}

impl AllocFrozenValue for i64 {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match i32::try_from(self) {
            Ok(x) => FrozenValue::new_int(x),
            Err(_) => StarlarkBigInt::alloc_bigint_frozen(self.into(), heap),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::hash::Hasher;

    use num_bigint::BigInt;

    use crate::{
        assert,
        collections::StarlarkHasher,
        values::{float::StarlarkFloat, types::bigint::StarlarkBigInt, StarlarkValue},
    };

    #[test]
    fn test_parse() {
        assert::eq(
            "'1234567890112233445566778899'",
            "str(1234567890112233445566778899)",
        );
        assert::eq(
            "'1234567890112233445566778899'",
            "str(0x3fd35eb6d519aff76f50e13)",
        );
        assert::eq(
            "'1234567890112233445566778899'",
            "str(0o776465726665214657756675207023)",
        );
        assert::eq(
            "'1234567890112233445566778899'",
            "str(0b11111111010011010111101011011011010101000\
                1100110101111111101110110111101010000111000010011)",
        );
    }

    #[test]
    fn test_str() {
        assert::eq(
            "'1234567890112233445566778899'",
            "str(1234567890112233445566778899)",
        );
    }

    #[test]
    fn test_repr() {
        assert::eq(
            "'1234567890112233445566778899'",
            "repr(1234567890112233445566778899)",
        );
    }

    #[test]
    fn test_equals() {
        assert::eq("10000000000000000000000", "10000000000000000000000");
        assert::eq("10000000000000000000000", "10000000000000000000000.0");
        assert::eq("10000000000000000000000.0", "10000000000000000000000");
    }

    #[test]
    fn test_plus() {
        assert::eq("10000000000000000000000", "+10000000000000000000000");
    }

    #[test]
    fn test_compare_big_big() {
        assert::is_true("10000000000000000000000 < 20000000000000000000000");
        assert::is_true("-20000000000000000000000 < -10000000000000000000000");
        assert::is_true("20000000000000000000000 > 10000000000000000000000");
        assert::is_true("-10000000000000000000000 > -20000000000000000000000");
    }

    #[test]
    fn test_compare_big_small() {
        assert::is_true("1 < 10000000000000000000000");
        assert::is_true("-1 < 10000000000000000000000");
        assert::is_true("1 > -10000000000000000000000");
        assert::is_true("-1 > -10000000000000000000000");
        assert::is_true("10000000000000000000000 > 1");
        assert::is_true("10000000000000000000000 > -1");
        assert::is_true("-10000000000000000000000 < 1");
        assert::is_true("-10000000000000000000000 < -1");
    }

    #[test]
    fn test_compare_big_float() {
        assert::is_true("1.0 < 10000000000000000000000");
        assert::is_true("-1.0 < 10000000000000000000000");
        assert::is_true("1.0 > -10000000000000000000000");
        assert::is_true("-1.0 > -10000000000000000000000");
        assert::is_true("10000000000000000000000 > 1.0");
        assert::is_true("10000000000000000000000 > -1.0");
        assert::is_true("-10000000000000000000000 < 1.0");
        assert::is_true("-10000000000000000000000 < -1.0");
    }

    #[test]
    fn test_add_big() {
        assert::eq(
            "300000000000000000009",
            "100000000000000000004 + 200000000000000000005",
        );
        assert::eq("7", "100000000000000000007 + -100000000000000000000");
        assert::eq(
            "200000000000000000005",
            "300000000000000000009 - 100000000000000000004",
        );
        assert::eq("7", "100000000000000000007 - 100000000000000000000");
    }

    #[test]
    fn test_add_big_small() {
        assert::eq("100000000000000000017", "100000000000000000000 + 17");
        assert::eq("100000000000000000017", "17 + 100000000000000000000");
        assert::eq("100000000000000000000", "100000000000000000017 - 17");
        assert::eq("-100000000000000000017", "17 - 100000000000000000034");
    }

    #[test]
    fn test_add_big_float() {
        assert::eq("2e20", "100000000000000000000 + 1e20");
        assert::eq("2e20", "1e20 + 100000000000000000000");
        assert::eq("2e20", "300000000000000000000 - 1e20");
        assert::eq("2e20", "3e20 - 100000000000000000000");
    }

    #[test]
    fn test_mul_big() {
        assert::eq(
            "60000000000000000000000000000000000000000",
            "200000000000000000000 * 300000000000000000000",
        );
    }

    #[test]
    fn test_mul_big_small() {
        assert::eq("600000000000000000000", "200000000000000000000 * 3");
        assert::eq("600000000000000000000", "3 * 200000000000000000000");
    }

    #[test]
    fn test_mul_big_float() {
        assert::eq("6e20", "200000000000000000000 * 3.0");
        assert::eq("6e20", "3.0 * 200000000000000000000");
    }

    #[test]
    fn test_div_big() {
        assert::eq(
            "2e20",
            "60000000000000000000000000000000000000000 / 300000000000000000000",
        );
    }

    #[test]
    fn test_div_big_small() {
        assert::eq("2e20", "600000000000000000000 / 3");
        assert::eq("2e-20", "6 / 300000000000000000000");
    }

    #[test]
    fn test_div_big_float() {
        assert::eq("2e20", "600000000000000000000 / 3.0");
        assert::eq("2e-20", "6.0 / 300000000000000000000");
    }

    #[test]
    fn test_floor_div_big() {
        assert::eq("2", "600000000000000000000 // 300000000000000000000");
    }

    #[test]
    fn test_floor_div_big_small() {
        assert::eq("200000000000000000000", "600000000000000000000 // 3");
        assert::eq("0", "3 // 600000000000000000000");
    }

    #[test]
    fn test_floor_div_big_float() {
        assert::eq("2e20", "600000000000000000000 / 3.0");
        assert::eq("2e-20", "6.0 / 300000000000000000000");
    }

    #[test]
    fn test_percent_big() {
        assert::eq("7", "600000000000000000007 % 200000000000000000000");
    }

    #[test]
    fn test_percent_big_small() {
        assert::eq("7", "600000000000000000007 % 20");
        assert::eq("3", "3 % 600000000000000000000");
    }

    #[test]
    fn test_percent_big_float() {
        assert::eq("1e20", "100000000000000000000 % 1e50");
        assert::eq("10.0", "10.0 % 100000000000000000000");
    }

    #[test]
    fn test_bit_and_big() {
        assert::eq(
            "0x10000000000000000000000",
            "0x30000000000000000000000 & 0x90000000000000000000000",
        );
    }

    #[test]
    fn test_bit_and_big_small() {
        assert::eq("1", "0x60000000000000000000003 & 0x9");
        assert::eq("1", "0x9 & 0x60000000000000000000003");
    }

    #[test]
    fn test_bit_and_float() {
        assert::fail("0x60000000000000000000000 & 1.0", "not supported");
        assert::fail("1.0 & 0x60000000000000000000000", "not supported");
    }

    #[test]
    fn test_bit_or_big() {
        assert::eq(
            "0x70000000000000000000000",
            "0x30000000000000000000000 | 0x50000000000000000000000",
        );
    }

    #[test]
    fn test_bit_or_big_small() {
        assert::eq(
            "0x60000000000000000000009",
            "0x60000000000000000000000 | 0x9",
        );
        assert::eq(
            "0x60000000000000000000009",
            "0x9 | 0x60000000000000000000000",
        );
    }

    #[test]
    fn test_bit_or_float() {
        assert::fail("0x60000000000000000000000 | 1.0", "not supported");
        assert::fail("1.0 | 0x60000000000000000000000", "not supported");
    }

    #[test]
    fn test_bit_xor_big() {
        assert::eq(
            "0x60000000000000000000000",
            "0x30000000000000000000000 ^ 0x50000000000000000000000",
        );
    }

    #[test]
    fn test_bit_xor_big_small() {
        assert::eq(
            "0x60000000000000000000000",
            "0x60000000000000000000009 ^ 0x9",
        );
        assert::eq(
            "0x60000000000000000000000",
            "0x9 ^ 0x60000000000000000000009",
        );
    }

    #[test]
    fn test_bit_xor_float() {
        assert::fail("0x60000000000000000000000 ^ 1.0", "not supported");
        assert::fail("1.0 ^ 0x60000000000000000000000", "not supported");
    }

    #[test]
    fn test_bit_not() {
        assert::eq(
            "-0x10000000000000000000000000000001",
            "~0x10000000000000000000000000000000",
        );
    }

    #[test]
    fn test_left_shift() {
        assert::fail(
            "0x10000000000000000000000000000000 << 0x10000000000000000000000000000000",
            "Integer overflow",
        );
        assert::fail(
            "0x10000000000000000000000000000000 << -0x10000000000000000000000000000000",
            "Negative shift count",
        );
    }

    #[test]
    fn test_left_shift_small() {
        assert::eq(
            "0x20000000000000000000000000000000",
            "0x10000000000000000000000000000000 << 1",
        );
        assert::fail(
            "0x10000000000000000000000000000000 << -1",
            "Negative shift count",
        );
        assert::fail(
            "1 << 0x10000000000000000000000000000000",
            "Integer overflow",
        );
        assert::fail(
            "1 << -0x10000000000000000000000000000000",
            "Negative shift count",
        );
        assert::eq("0", "0 << 0x10000000000000000000000000000000");
    }

    #[test]
    fn test_left_shift_float() {
        assert::fail("0x10000000000000000000000000000000 << 1.0", "not supported");
        assert::fail("1.0 << 0x10000000000000000000000000000000", "not supported");
    }

    #[test]
    fn test_right_shift() {
        assert::eq(
            "0",
            "0x20000000000000000000000000000000 >> 0x20000000000000000000000000000000",
        );
        assert::eq(
            "-1",
            "-0x20000000000000000000000000000000 >> 0x20000000000000000000000000000000",
        );
        assert::fail(
            "0x20000000000000000000000000000000 >> -0x20000000000000000000000000000000",
            "Negative shift count",
        );
    }

    #[test]
    fn test_right_shift_small() {
        assert::eq(
            "0x10000000000000000000000000000000",
            "0x20000000000000000000000000000000 >> 1",
        );
        assert::fail(
            "0x20000000000000000000000000000000 >> -1",
            "Negative shift count",
        );
        assert::eq("0", "1 >> 0x20000000000000000000000000000000");
        assert::eq("-1", "-1 >> 0x20000000000000000000000000000000");
        assert::fail(
            "1 >> -0x10000000000000000000000000000000",
            "Negative shift count",
        );
    }

    #[test]
    fn test_right_shift_float() {
        assert::fail("0x20000000000000000000000000000000 >> 1.0", "not supported");
        assert::fail("1.0 >> 0x20000000000000000000000000000000", "not supported");
    }

    #[test]
    fn test_int_function() {
        assert::eq(
            "123456789012345678901234567890",
            "int(123456789012345678901234567890)",
        );
    }

    #[test]
    fn test_hash() {
        let mut hash1 = StarlarkHasher::new();
        let mut hash2 = StarlarkHasher::new();
        StarlarkFloat(1e20).write_hash(&mut hash1).unwrap();
        StarlarkBigInt::unchecked_new(BigInt::from(10).pow(20))
            .write_hash(&mut hash2)
            .unwrap();
        assert_eq!(hash1.finish(), hash2.finish());
    }
}

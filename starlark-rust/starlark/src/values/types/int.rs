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

//! The integer type. Currently limited to 32 bit.
//!
//! Can be created with [`new_int`](Value::new_int) and unwrapped with [`unpack_int`](Value::unpack_int).
//! Unlike most Starlark values, these aren't actually represented on the [`Heap`], but as special values.
//! At some point in the future we plan to support arbitrary sized integers (as required by the
//! [Starlark spec](https://github.com/bazelbuild/starlark/blob/master/spec.md#integers)), and those larger
//! integer values will be stored on the heap.

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hasher;
use std::ptr;

use gazebo::any::AnyLifetime;
use gazebo::any::ProvidesStaticType;
use gazebo::cast;
use num_bigint::BigInt;
use num_traits::Signed;
use serde::Serialize;
use serde::Serializer;

use crate::collections::StarlarkHashValue;
use crate::collections::StarlarkHasher;
use crate::private::Private;
use crate::values::basic::StarlarkValueBasic;
use crate::values::error::ValueError;
use crate::values::float::StarlarkFloat;
use crate::values::num::Num;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::UnpackValue;
use crate::values::Value;

/// The result of calling `type()` on integers.
pub const INT_TYPE: &str = "int";

impl<'v> AllocValue<'v> for i32 {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        Value::new_int(self)
    }
}
impl AllocFrozenValue for i32 {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        FrozenValue::new_int(self)
    }
}

impl StarlarkTypeRepr for i32 {
    fn starlark_type_repr() -> String {
        PointerI32::starlark_type_repr()
    }
}

impl UnpackValue<'_> for i32 {
    fn unpack_value(value: Value) -> Option<Self> {
        value.unpack_int()
    }
}

// WARNING: This type isn't a real type, a pointer to this is secretly an i32.
// Therefore, don't derive stuff on it, since it will be wrong.
// However, `ProvidesStaticType` promises not to peek at its value, so that's fine.
#[derive(ProvidesStaticType)]
#[repr(C)]
pub(crate) struct PointerI32 {
    _private: (),
}

impl PartialEq for PointerI32 {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

impl Eq for PointerI32 {}

impl Debug for PointerI32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.get(), f)
    }
}

impl PointerI32 {
    pub(crate) fn new(x: i32) -> &'static Self {
        // UB if the pointer isn't aligned, or it is zero
        // Alignment is 1, so that's not an issue.
        // And the pointer is not zero because it has `TAG_INT` bit set.
        unsafe { cast::usize_to_ptr(FrozenValue::new_int(x).ptr_value()) }
    }

    pub(crate) fn get(&self) -> i32 {
        unsafe { FrozenValue::new_ptr_value(cast::ptr_to_usize(self)).unpack_int_unchecked() }
    }

    /// This operation is expensive, use only if you have to.
    fn to_bigint(&self) -> BigInt {
        BigInt::from(self.get())
    }

    pub(crate) fn type_is_pointer_i32<'v, T: StarlarkValue<'v>>() -> bool {
        T::static_type_id() == PointerI32::static_type_id()
    }
}

impl Display for PointerI32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.get())
    }
}

/// Define the int type
impl<'v> StarlarkValue<'v> for PointerI32 {
    starlark_type!(INT_TYPE);

    fn get_type_starlark_repr() -> String {
        "int.type".to_owned()
    }

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        Ok(match other.unpack_num() {
            Some(Num::Int(other)) => self.get() == other,
            Some(Num::Float(other)) => self.get() as f64 == other,
            Some(Num::BigInt(b)) => *b == self.get(),
            None => false,
        })
    }

    fn to_int(&self) -> anyhow::Result<i32> {
        Ok(self.get())
    }
    fn to_bool(&self) -> bool {
        self.get() != 0
    }
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        hasher.write_u64(Num::from(self.get()).get_hash_64());
        Ok(())
    }
    fn plus(&self, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_int(self.get()))
    }
    fn minus(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(self.get().checked_neg().map_or_else(
            || StarlarkBigInt::alloc_bigint(-BigInt::from(self.get()), heap),
            Value::new_int,
        ))
    }
    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        match other.unpack_num() {
            Some(Num::Int(other)) => Some(Ok(self.get().checked_add(other).map_or_else(
                || StarlarkBigInt::alloc_bigint(self.to_bigint() + other, heap),
                Value::new_int,
            ))),
            Some(Num::Float(_)) => StarlarkFloat(self.get() as f64).add(other, heap),
            Some(Num::BigInt(other)) => Some(Ok(StarlarkBigInt::alloc_bigint(
                self.get() + other.get(),
                heap,
            ))),
            None => None,
        }
    }
    fn sub(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(Num::Int(other)) => Ok(self.get().checked_sub(other).map_or_else(
                || StarlarkBigInt::alloc_bigint(self.to_bigint() - other, heap),
                Value::new_int,
            )),
            Some(Num::Float(_)) => StarlarkFloat(self.get() as f64).sub(other, heap),
            Some(Num::BigInt(other)) => {
                Ok(StarlarkBigInt::alloc_bigint(self.get() - other.get(), heap))
            }
            None => ValueError::unsupported_with(self, "-", other),
        }
    }
    fn mul(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        if let Some(other) = other.unpack_int() {
            Ok(self.get().checked_mul(other).map_or_else(
                || StarlarkBigInt::alloc_bigint(self.to_bigint() * other, heap),
                Value::new_int,
            ))
        } else {
            other.mul(Value::new_int(self.get()), heap)
        }
    }
    fn div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        if other.unpack_num().is_some() {
            StarlarkFloat(self.get() as f64).div(other, heap)
        } else {
            ValueError::unsupported_with(self, "/", other)
        }
    }
    fn percent(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None => ValueError::unsupported_with(self, "%", other),
            Some(Num::Float(_)) => return StarlarkFloat(self.get() as f64).percent(other, heap),
            Some(Num::BigInt(other)) => {
                return StarlarkBigInt::percent_big(&BigInt::from(self.get()), other.get(), heap);
            }
            Some(Num::Int(b)) => {
                let a = self.get();
                if b == 0 {
                    return Err(ValueError::DivisionByZero.into());
                }
                // In Rust `i32::min_value() % -1` is overflow, but we should eval it to zero.
                if self.get() == i32::min_value() && b == -1 {
                    return Ok(Value::new_int(0));
                }
                let r = a % b;
                if r == 0 {
                    Ok(Value::new_int(0))
                } else {
                    Ok(Value::new_int(if b.signum() != r.signum() {
                        r + b
                    } else {
                        r
                    }))
                }
            }
        }
    }
    fn floor_div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let rhs = match other.unpack_num() {
            None => return ValueError::unsupported_with(self, "//", other),
            Some(rhs) => rhs,
        };
        match rhs {
            Num::Float(_) => StarlarkFloat(self.get() as f64).floor_div(other, heap),
            Num::Int(b) => {
                let a = self.get();
                if b == 0 {
                    return Err(ValueError::DivisionByZero.into());
                }
                let sig = b.signum() * a.signum();
                let offset = if sig < 0 && a % b != 0 { 1 } else { 0 };
                match a.checked_div(b) {
                    Some(div) => Ok(Value::new_int(div - offset)),
                    None => StarlarkBigInt::floor_div_big(&BigInt::from(a), &BigInt::from(b), heap),
                }
            }
            Num::BigInt(b) => {
                StarlarkBigInt::floor_div_big(&BigInt::from(self.get()), b.get(), heap)
            }
        }
    }

    fn compare(&self, other: Value) -> anyhow::Result<Ordering> {
        match other.unpack_num() {
            Some(Num::Int(other)) => Ok(self.get().cmp(&other)),
            Some(Num::Float(_)) => StarlarkFloat(self.get() as f64).compare(other),
            Some(Num::BigInt(b)) => Ok(StarlarkBigInt::cmp_small_big(self.get(), b)),
            None => ValueError::unsupported_with(self, "==", other),
        }
    }

    fn bit_and(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, "&", other),
            Some(Num::Int(i)) => Ok(Value::new_int(self.get() & i)),
            Some(Num::BigInt(b)) => Ok(StarlarkBigInt::alloc_bigint(
                &self.to_bigint() & b.get(),
                heap,
            )),
        }
    }

    fn bit_or(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, "|", other),
            Some(Num::Int(i)) => Ok(Value::new_int(self.get() | i)),
            Some(Num::BigInt(b)) => Ok(StarlarkBigInt::alloc_bigint(
                &self.to_bigint() | b.get(),
                heap,
            )),
        }
    }

    fn bit_xor(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, "^", other),
            Some(Num::Int(i)) => Ok(Value::new_int(self.get() ^ i)),
            Some(Num::BigInt(b)) => Ok(StarlarkBigInt::alloc_bigint(
                &self.to_bigint() ^ b.get(),
                heap,
            )),
        }
    }

    fn bit_not(&self, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_int(!self.get()))
    }

    fn left_shift(&self, other: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, "<<", other),
            Some(Num::Int(other)) => {
                if let Ok(other) = other.try_into() {
                    if let Some(r) = self.get().checked_shl(other) {
                        Ok(Value::new_int(r))
                    } else {
                        Err(ValueError::IntegerOverflow.into())
                    }
                } else {
                    Err(ValueError::NegativeShiftCount.into())
                }
            }
            Some(Num::BigInt(b)) => {
                if b.get().is_negative() {
                    Err(ValueError::NegativeShiftCount.into())
                } else if self.get() == 0 {
                    Ok(Value::new_int(0))
                } else {
                    Err(ValueError::IntegerOverflow.into())
                }
            }
        }
    }

    #[allow(clippy::collapsible_else_if)]
    fn right_shift(&self, other: Value, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            None | Some(Num::Float(_)) => ValueError::unsupported_with(self, ">>", other),
            Some(Num::Int(other)) => {
                if let Ok(other) = other.try_into() {
                    if let Some(r) = self.get().checked_shr(other) {
                        Ok(Value::new_int(r))
                    } else {
                        Err(ValueError::IntegerOverflow.into())
                    }
                } else {
                    Err(ValueError::NegativeShiftCount.into())
                }
            }
            Some(Num::BigInt(b)) => {
                if b.get().is_negative() {
                    Err(ValueError::NegativeShiftCount.into())
                } else {
                    if self.get() >= 0 {
                        Ok(Value::new_int(0))
                    } else {
                        Ok(Value::new_int(-1))
                    }
                }
            }
        }
    }
}

impl<'v> StarlarkValueBasic<'v> for PointerI32 {
    fn get_hash(&self) -> StarlarkHashValue {
        Num::from(self.get()).get_hash()
    }
}

impl Serialize for PointerI32 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_i32(self.get())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert;

    #[test]
    fn test_arithmetic_operators() {
        assert::all_true(
            r#"
+1 == 1
-1 == 0 - 1
1 + 2 == 3
1 + 2.0 == 3.0
1 - 2 == -1
1 - 2.0 == -1.0
2 * 3 == 6
2 * 3.0 == 6.0
4 / 2 == 2.0
5 % 3 == 2
4 // 2 == 2
"#,
        );
    }

    #[test]
    fn test_minus() {
        // `-i32::MIN` should overflow to `StarlarkBigInt`.
        assert::eq("2147483648", "-(-2147483647 - 1)")
    }

    #[test]
    fn test_int_tag() {
        fn check(x: i32) {
            assert_eq!(x, PointerI32::new(x).get())
        }

        for x in -10..10 {
            check(x)
        }
        check(i32::MAX);
        check(i32::MIN);
    }

    #[test]
    fn test_alignment_int_pointer() {
        assert_eq!(1, std::mem::align_of::<PointerI32>());
    }
}

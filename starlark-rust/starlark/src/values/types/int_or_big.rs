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

use std::cmp::Ordering;
use std::ops::Add;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Sub;

use dupe::Dupe;
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use num_traits::Num;
use num_traits::ToPrimitive;

use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::AllocFrozenValue;
use crate::values::AllocValue;
use crate::values::FrozenHeap;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, thiserror::Error)]
enum StarlarkIntError {
    #[error("Cannot parse `{0}` as an integer in base {1}")]
    CannotParse(String, u32),
    #[error("Float `{0}` cannot be represented as exact integer")]
    CannotRepresentAsExact(f64),
}

#[derive(Debug, Clone, Eq, PartialEq, derive_more::Display)]
pub(crate) enum StarlarkInt {
    Small(i32),
    Big(StarlarkBigInt),
}

#[derive(Eq, PartialEq, Copy, Clone, Dupe, Debug)]
pub(crate) enum StarlarkIntRef<'v> {
    Small(i32),
    Big(&'v StarlarkBigInt),
}

impl StarlarkInt {
    pub(crate) fn from_str_radix(s: &str, base: u32) -> anyhow::Result<StarlarkInt> {
        if let Ok(i) = i32::from_str_radix(s, base) {
            Ok(StarlarkInt::Small(i))
        } else {
            match BigInt::from_str_radix(s, base) {
                Ok(i) => Ok(StarlarkBigInt::try_from_bigint(i)),
                Err(_) => Err(StarlarkIntError::CannotParse(s.to_owned(), base).into()),
            }
        }
    }

    pub(crate) fn from_f64_exact(f: f64) -> anyhow::Result<StarlarkInt> {
        let i = f as i32;
        if i as f64 == f {
            Ok(StarlarkInt::Small(i))
        } else {
            if let Some(i) = BigInt::from_f64(f) {
                if i.to_f64() == Some(f) {
                    return Ok(StarlarkBigInt::try_from_bigint(i));
                }
            }
            Err(StarlarkIntError::CannotRepresentAsExact(f).into())
        }
    }
}

impl<'v> StarlarkIntRef<'v> {
    fn to_big(self) -> BigInt {
        match self {
            StarlarkIntRef::Small(i) => BigInt::from(i),
            StarlarkIntRef::Big(i) => i.get().clone(),
        }
    }

    pub(crate) fn to_f64(self) -> f64 {
        match self {
            StarlarkIntRef::Small(i) => i as f64,
            StarlarkIntRef::Big(i) => i.to_f64(),
        }
    }

    pub(crate) fn to_i32(self) -> Option<i32> {
        match self {
            StarlarkIntRef::Small(i) => Some(i),
            StarlarkIntRef::Big(_) => {
                // `StarlarkBigInt` is out of range of `i32`.
                None
            }
        }
    }
}

impl StarlarkTypeRepr for StarlarkInt {
    fn starlark_type_repr() -> String {
        StarlarkBigInt::starlark_type_repr()
    }
}

impl<'v> AllocValue<'v> for StarlarkInt {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        match self {
            StarlarkInt::Small(i) => heap.alloc(i),
            StarlarkInt::Big(i) => heap.alloc(i),
        }
    }
}

impl AllocFrozenValue for StarlarkInt {
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        match self {
            StarlarkInt::Small(i) => heap.alloc(i),
            StarlarkInt::Big(i) => heap.alloc(i),
        }
    }
}

impl<'v> StarlarkTypeRepr for StarlarkIntRef<'v> {
    fn starlark_type_repr() -> String {
        StarlarkInt::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for StarlarkIntRef<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(i) = i32::unpack_value(value) {
            Some(StarlarkIntRef::Small(i))
        } else {
            value.downcast_ref().map(StarlarkIntRef::Big)
        }
    }
}

impl<'v> BitAnd for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn bitand(self, other: Self) -> StarlarkInt {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => StarlarkInt::Small(a & b),
            (a, b) => StarlarkBigInt::try_from_bigint(a.to_big() & b.to_big()),
        }
    }
}

impl<'v> BitOr for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn bitor(self, other: Self) -> StarlarkInt {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => StarlarkInt::Small(a | b),
            (a, b) => StarlarkBigInt::try_from_bigint(a.to_big() | b.to_big()),
        }
    }
}

impl<'v> BitXor for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn bitxor(self, other: Self) -> StarlarkInt {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => StarlarkInt::Small(a ^ b),
            (a, b) => StarlarkBigInt::try_from_bigint(a.to_big() ^ b.to_big()),
        }
    }
}

impl<'v> Not for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn not(self) -> StarlarkInt {
        match self {
            StarlarkIntRef::Small(a) => StarlarkInt::Small(!a),
            a => StarlarkBigInt::try_from_bigint(!a.to_big()),
        }
    }
}

impl<'v> Neg for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn neg(self) -> StarlarkInt {
        if let StarlarkIntRef::Small(i) = self {
            if let Some(n) = i.checked_neg() {
                return StarlarkInt::Small(n);
            }
        }
        StarlarkBigInt::try_from_bigint(-self.to_big())
    }
}

impl<'v> Add for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn add(self, other: Self) -> StarlarkInt {
        if let (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) = (self, other) {
            if let Some(c) = a.checked_add(b) {
                return StarlarkInt::Small(c);
            }
        }
        StarlarkBigInt::try_from_bigint(self.to_big() + other.to_big())
    }
}

impl<'v> Sub for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn sub(self, other: Self) -> StarlarkInt {
        if let (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) = (self, other) {
            if let Some(c) = a.checked_sub(b) {
                return StarlarkInt::Small(c);
            }
        }
        StarlarkBigInt::try_from_bigint(self.to_big() - other.to_big())
    }
}

impl<'v> Mul<i32> for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn mul(self, rhs: i32) -> Self::Output {
        match self {
            StarlarkIntRef::Small(a) => {
                if let Some(c) = a.checked_mul(rhs) {
                    return StarlarkInt::Small(c);
                }
                StarlarkBigInt::try_from_bigint(BigInt::from(a) * rhs)
            }
            StarlarkIntRef::Big(b) => StarlarkBigInt::try_from_bigint(b.get() * rhs),
        }
    }
}

impl<'v> Mul<StarlarkIntRef<'v>> for i32 {
    type Output = StarlarkInt;

    fn mul(self, rhs: StarlarkIntRef<'v>) -> Self::Output {
        rhs * self
    }
}

impl<'v> Mul for StarlarkIntRef<'v> {
    type Output = StarlarkInt;

    fn mul(self, other: Self) -> StarlarkInt {
        match (self, other) {
            (StarlarkIntRef::Small(a), b) => a * b,
            (a, StarlarkIntRef::Small(b)) => a * b,
            (StarlarkIntRef::Big(a), StarlarkIntRef::Big(b)) => {
                StarlarkBigInt::try_from_bigint(a.get() * b.get())
            }
        }
    }
}

impl<'v> PartialOrd for StarlarkIntRef<'v> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'v> Ord for StarlarkIntRef<'v> {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => a.cmp(b),
            (StarlarkIntRef::Big(a), StarlarkIntRef::Big(b)) => a.cmp(b),
            (StarlarkIntRef::Small(a), StarlarkIntRef::Big(b)) => {
                StarlarkBigInt::cmp_small_big(*a, b)
            }
            (StarlarkIntRef::Big(a), StarlarkIntRef::Small(b)) => {
                StarlarkBigInt::cmp_big_small(a, *b)
            }
        }
    }
}

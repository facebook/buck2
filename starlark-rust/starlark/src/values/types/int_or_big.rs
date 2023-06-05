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

use anyhow::Context;
use dupe::Dupe;
use num_bigint::BigInt;
use num_bigint::Sign;
use num_traits::FromPrimitive;
use num_traits::Num;
use num_traits::Signed;
use num_traits::ToPrimitive;
use num_traits::Zero;

use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::types::inline_int::InlineInt;
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
    #[error("Floor division by zero: {0} // {1}")]
    FloorDivisionByZero(StarlarkInt, StarlarkInt),
    #[error("Modulo by zero: {0} % {1}")]
    ModuloByZero(StarlarkInt, StarlarkInt),
    #[error("Integer overflow computing left shift")]
    LeftShiftOverflow,
    #[error("Negative left shift")]
    LeftShiftNegative,
    #[error("Negative right shift")]
    RightShiftNegative,
}

#[derive(Debug, Clone, Eq, PartialEq, derive_more::Display)]
pub(crate) enum StarlarkInt {
    Small(InlineInt),
    Big(StarlarkBigInt),
}

#[derive(Eq, PartialEq, Copy, Clone, Dupe, Debug)]
pub(crate) enum StarlarkIntRef<'v> {
    Small(InlineInt),
    Big(&'v StarlarkBigInt),
}

impl StarlarkInt {
    pub(crate) fn from_str_radix(s: &str, base: u32) -> anyhow::Result<StarlarkInt> {
        if let Ok(i) = InlineInt::from_str_radix(s, base) {
            Ok(StarlarkInt::Small(i))
        } else {
            match BigInt::from_str_radix(s, base) {
                Ok(i) => Ok(StarlarkBigInt::try_from_bigint(i)),
                Err(_) => Err(StarlarkIntError::CannotParse(s.to_owned(), base).into()),
            }
        }
    }

    pub(crate) fn from_f64_exact(f: f64) -> anyhow::Result<StarlarkInt> {
        let i = InlineInt::try_from(f as i32).unwrap_or(InlineInt::ZERO);
        if i.to_f64() == f {
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

    pub(crate) fn as_ref(&self) -> StarlarkIntRef {
        match self {
            StarlarkInt::Small(i) => StarlarkIntRef::Small(*i),
            StarlarkInt::Big(i) => StarlarkIntRef::Big(i),
        }
    }
}

impl<'v> StarlarkIntRef<'v> {
    pub(crate) fn to_owned(self) -> StarlarkInt {
        match self {
            StarlarkIntRef::Small(i) => StarlarkInt::Small(i),
            StarlarkIntRef::Big(i) => StarlarkInt::Big(i.clone()),
        }
    }

    fn to_big(self) -> BigInt {
        match self {
            StarlarkIntRef::Small(i) => i.to_bigint(),
            StarlarkIntRef::Big(i) => i.get().clone(),
        }
    }

    pub(crate) fn to_f64(self) -> f64 {
        match self {
            StarlarkIntRef::Small(i) => i.to_f64(),
            StarlarkIntRef::Big(i) => i.to_f64(),
        }
    }

    pub(crate) fn to_i32(self) -> Option<i32> {
        match self {
            StarlarkIntRef::Small(i) => Some(i.to_i32()),
            StarlarkIntRef::Big(i) => i.to_i32(),
        }
    }

    pub(crate) fn to_u64(self) -> Option<u64> {
        match self {
            StarlarkIntRef::Small(i) => i.to_u64(),
            StarlarkIntRef::Big(i) => i.get().to_u64(),
        }
    }

    fn floor_div_small_small(a: InlineInt, b: InlineInt) -> anyhow::Result<StarlarkInt> {
        if b == 0 {
            return Err(StarlarkIntError::FloorDivisionByZero(
                StarlarkInt::Small(a),
                StarlarkInt::Small(b),
            )
            .into());
        }
        let sig = b.signum() * a.signum();
        let offset = if sig < 0 && a % b != 0 { 1 } else { 0 };
        match a.checked_div(b) {
            Some(div) => Ok(StarlarkInt::Small(
                div.checked_sub_i32(offset).context("unreachable")?,
            )),
            None => Self::floor_div_big_big(&a.to_bigint(), &b.to_bigint()),
        }
    }

    fn is_negative(self) -> bool {
        match self {
            StarlarkIntRef::Small(i) => i < 0,
            StarlarkIntRef::Big(i) => i.get().is_negative(),
        }
    }

    fn is_zero(self) -> bool {
        match self {
            StarlarkIntRef::Small(i) => i == 0,
            StarlarkIntRef::Big(_) => false,
        }
    }

    fn signum_big(b: &BigInt) -> i32 {
        match b.sign() {
            Sign::Plus => 1,
            Sign::Minus => -1,
            Sign::NoSign => 0,
        }
    }

    fn floor_div_big_big(a: &BigInt, b: &BigInt) -> anyhow::Result<StarlarkInt> {
        if b.is_zero() {
            return Err(StarlarkIntError::FloorDivisionByZero(
                StarlarkBigInt::try_from_bigint(a.clone()),
                StarlarkBigInt::try_from_bigint(b.clone()),
            )
            .into());
        }
        let sig = Self::signum_big(b) * Self::signum_big(a);
        // TODO(nga): optimize.
        let offset = if sig < 0 && (a % b).is_zero().not() {
            1
        } else {
            0
        };
        Ok(StarlarkBigInt::try_from_bigint((a / b) - offset))
    }

    /// `//`.
    pub(crate) fn floor_div(self, other: StarlarkIntRef) -> anyhow::Result<StarlarkInt> {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => {
                Self::floor_div_small_small(a, b)
            }
            (StarlarkIntRef::Small(a), StarlarkIntRef::Big(b)) => {
                Self::floor_div_big_big(&a.to_bigint(), b.get())
            }
            (StarlarkIntRef::Big(a), StarlarkIntRef::Small(b)) => {
                Self::floor_div_big_big(a.get(), &b.to_bigint())
            }
            (StarlarkIntRef::Big(a), StarlarkIntRef::Big(b)) => {
                Self::floor_div_big_big(a.get(), b.get())
            }
        }
    }

    fn percent_small(a: InlineInt, b: InlineInt) -> anyhow::Result<InlineInt> {
        if b == 0 {
            return Err(StarlarkIntError::ModuloByZero(
                StarlarkInt::Small(a),
                StarlarkInt::Small(b),
            )
            .into());
        }
        // In Rust `i32::min_value() % -1` is overflow, but we should eval it to zero.
        if a == i32::min_value() && b == -1 {
            return Ok(InlineInt::ZERO);
        }
        let r = a % b;
        if r == 0 {
            Ok(InlineInt::ZERO)
        } else {
            Ok(if b.signum() != r.signum() {
                r.checked_add(b).context("unreachable")?
            } else {
                r
            })
        }
    }

    fn percent_big(a: &BigInt, b: &BigInt) -> anyhow::Result<StarlarkInt> {
        if b.is_zero() {
            return Err(StarlarkIntError::ModuloByZero(
                StarlarkBigInt::try_from_bigint(a.clone()),
                StarlarkBigInt::try_from_bigint(b.clone()),
            )
            .into());
        }
        let r = a % b;
        if r.is_zero() {
            Ok(StarlarkInt::Small(InlineInt::ZERO))
        } else {
            Ok(StarlarkBigInt::try_from_bigint(if b.sign() != r.sign() {
                r + b
            } else {
                r
            }))
        }
    }

    /// `%`.
    pub(crate) fn percent(self, other: StarlarkIntRef) -> anyhow::Result<StarlarkInt> {
        match (self, other) {
            (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) => {
                Ok(StarlarkInt::Small(Self::percent_small(a, b)?))
            }
            (StarlarkIntRef::Small(a), StarlarkIntRef::Big(b)) => {
                Self::percent_big(&a.to_bigint(), b.get())
            }
            (StarlarkIntRef::Big(a), StarlarkIntRef::Small(b)) => {
                Self::percent_big(a.get(), &b.to_bigint())
            }
            (StarlarkIntRef::Big(a), StarlarkIntRef::Big(b)) => Self::percent_big(a.get(), b.get()),
        }
    }

    /// `<<`.
    pub(crate) fn left_shift(self, other: StarlarkIntRef) -> anyhow::Result<StarlarkInt> {
        // Handle the most common case first.
        if let (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) = (self, other) {
            if let Some(b) = b.to_u32() {
                if let Some(r) = a.checked_shl(b) {
                    return Ok(StarlarkInt::Small(r));
                }
            }
        }

        if other.is_negative() {
            return Err(StarlarkIntError::LeftShiftNegative.into());
        }
        if self.is_zero() || other.is_zero() {
            return Ok(self.to_owned());
        }
        if other > 100_000 {
            // Limit the size of the BigInt to avoid accidentally consuming
            // too much memory. 100_000 is practically enough for most use cases.
            return Err(StarlarkIntError::LeftShiftOverflow.into());
        }

        match other {
            StarlarkIntRef::Big(_) => Err(StarlarkIntError::LeftShiftOverflow.into()),
            StarlarkIntRef::Small(b) => {
                // No overflow, checked above.
                let b = b.to_u64().unwrap();
                Ok(StarlarkBigInt::try_from_bigint(self.to_big() << b))
            }
        }
    }

    /// `>>`.
    pub(crate) fn right_shift(self, other: StarlarkIntRef) -> anyhow::Result<StarlarkInt> {
        // Handle the most common case first.
        if let (StarlarkIntRef::Small(a), StarlarkIntRef::Small(b)) = (self, other) {
            if let Some(b) = b.to_u32() {
                if let Some(r) = a.checked_shr(b) {
                    return Ok(StarlarkInt::Small(r));
                }
            }
        }

        if other.is_negative() {
            return Err(StarlarkIntError::RightShiftNegative.into());
        }
        if self.is_zero() || other.is_zero() {
            return Ok(self.to_owned());
        }
        let Some(other) = other.to_u64() else {
            return if self.is_negative() {
                Ok(StarlarkInt::Small(InlineInt::MINUS_ONE))
            } else {
                Ok(StarlarkInt::Small(InlineInt::ZERO))
            }
        };

        match self {
            StarlarkIntRef::Small(a) => {
                if a < 0 {
                    Ok(StarlarkInt::Small(InlineInt::MINUS_ONE))
                } else {
                    Ok(StarlarkInt::Small(InlineInt::ZERO))
                }
            }
            StarlarkIntRef::Big(a) => Ok(StarlarkBigInt::try_from_bigint(a.get() >> other)),
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
        if let Some(i) = InlineInt::unpack_value(value) {
            Some(StarlarkIntRef::Small(i))
        } else {
            value.downcast_ref().map(StarlarkIntRef::Big)
        }
    }
}

impl From<i32> for StarlarkInt {
    #[inline]
    fn from(value: i32) -> Self {
        match InlineInt::try_from(value) {
            Ok(i) => StarlarkInt::Small(i),
            Err(_) => StarlarkBigInt::try_from_bigint(value.into()),
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

impl Neg for StarlarkInt {
    type Output = StarlarkInt;

    fn neg(self) -> StarlarkInt {
        // TODO(nga): can negate without allocating in most cases.
        -self.as_ref()
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
                if let Some(c) = a.checked_mul_i32(rhs) {
                    return StarlarkInt::Small(c);
                }
                StarlarkBigInt::try_from_bigint(a.to_bigint() * rhs)
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
            (StarlarkIntRef::Small(a), b) => a.to_i32() * b,
            (a, StarlarkIntRef::Small(b)) => a * b.to_i32(),
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

impl<'v> PartialEq<i32> for StarlarkIntRef<'v> {
    fn eq(&self, other: &i32) -> bool {
        *self == StarlarkInt::from(*other).as_ref()
    }
}

impl<'v> PartialOrd<i32> for StarlarkIntRef<'v> {
    fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
        // TODO(nga): this is inefficient if `i32` cannot fit in `InlineInt`.
        self.partial_cmp(&StarlarkInt::from(*other).as_ref())
    }
}

impl<'v> PartialEq<StarlarkIntRef<'v>> for i32 {
    fn eq(&self, other: &StarlarkIntRef<'v>) -> bool {
        StarlarkInt::from(*self).as_ref() == *other
    }
}

impl<'v> PartialOrd<StarlarkIntRef<'v>> for i32 {
    // TODO(nga): this is inefficient if `i32` cannot fit in `InlineInt`.
    fn partial_cmp(&self, other: &StarlarkIntRef<'v>) -> Option<Ordering> {
        StarlarkInt::from(*self).as_ref().partial_cmp(other)
    }
}

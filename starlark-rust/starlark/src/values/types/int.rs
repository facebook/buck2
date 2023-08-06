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
//! Unlike most Starlark values, these aren't actually represented on the [`Heap`], but as special values.
//! At some point in the future we plan to support arbitrary sized integers (as required by the
//! [Starlark spec](https://github.com/bazelbuild/starlark/blob/master/spec.md#integers)), and those larger
//! integer values will be stored on the heap.

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hasher;
use std::mem;
use std::ptr;

use allocative::Allocative;
use num_bigint::BigInt;
use serde::Serialize;
use serde::Serializer;
use starlark_derive::starlark_value;
use starlark_derive::StarlarkDocs;

use crate as starlark;
use crate::any::AnyLifetime;
use crate::any::ProvidesStaticType;
use crate::cast;
use crate::collections::StarlarkHashValue;
use crate::collections::StarlarkHasher;
use crate::private::Private;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingBinOp;
use crate::values::error::ValueError;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::pointer::RawPointer;
use crate::values::layout::vtable::AValueDyn;
use crate::values::layout::vtable::AValueVTable;
use crate::values::layout::vtable::StarlarkValueRawPtr;
use crate::values::num::typecheck::typecheck_num_bin_op;
use crate::values::num::typecheck::NumTy;
use crate::values::num::value::NumRef;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::types::inline_int::InlineInt;
use crate::values::types::int_or_big::StarlarkInt;
use crate::values::types::int_or_big::StarlarkIntRef;
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
    #[inline]
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc(StarlarkInt::from(self))
    }
}
impl AllocFrozenValue for i32 {
    #[inline]
    fn alloc_frozen_value(self, heap: &FrozenHeap) -> FrozenValue {
        heap.alloc(StarlarkInt::from(self))
    }
}

impl StarlarkTypeRepr for i32 {
    fn starlark_type_repr() -> Ty {
        PointerI32::starlark_type_repr()
    }
}

impl UnpackValue<'_> for i32 {
    fn unpack_value(value: Value) -> Option<Self> {
        if InlineInt::smaller_than_i32() {
            StarlarkIntRef::unpack_value(value)?.to_i32()
        } else {
            Some(InlineInt::unpack_value(value)?.to_i32())
        }
    }
}

// WARNING: This type isn't a real type, a pointer to this is secretly an i32.
// Therefore, don't derive stuff on it, since it will be wrong.
// However, `ProvidesStaticType` promises not to peek at its value, so that's fine.
#[derive(ProvidesStaticType, StarlarkDocs, Allocative)]
#[starlark_docs(builtin = "standard")]
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
    const _ASSERTIONS: () = {
        assert!(mem::align_of::<Self>() == 1);
    };

    #[inline]
    pub(crate) fn new(x: InlineInt) -> &'static Self {
        // UB if the pointer isn't aligned, or it is zero
        // Alignment is 1, so that's not an issue.
        // And the pointer is not zero because it has `TAG_INT` bit set.
        unsafe { Self::from_raw_pointer_unchecked(FrozenValue::new_int(x).ptr_value()) }
    }

    #[inline]
    pub(crate) unsafe fn from_raw_pointer_unchecked(
        raw_pointer: RawPointer,
    ) -> &'static PointerI32 {
        debug_assert!(raw_pointer.is_int());
        cast::usize_to_ptr(raw_pointer.ptr_value())
    }

    #[inline]
    pub(crate) fn get(&self) -> InlineInt {
        unsafe { RawPointer::new_unchecked(self as *const Self as usize).unpack_int_unchecked() }
    }

    #[inline]
    pub(crate) fn as_avalue_dyn(&'static self) -> AValueDyn<'static> {
        unsafe { AValueDyn::new(StarlarkValueRawPtr::new_pointer_i32(self), Self::vtable()) }
    }

    #[inline]
    pub(crate) fn vtable() -> &'static AValueVTable {
        &AValueVTable::new::<AValueImpl<Basic, PointerI32>>()
    }

    /// This operation is expensive, use only if you have to.
    fn to_bigint(&self) -> BigInt {
        self.get().to_bigint()
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
#[starlark_value(type = INT_TYPE)]
impl<'v> StarlarkValue<'v> for PointerI32 {
    type Canonical = StarlarkBigInt;

    fn is_special(_: Private) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn equals(&self, other: Value) -> anyhow::Result<bool> {
        Ok(Some(NumRef::Int(StarlarkIntRef::Small(self.get()))) == other.unpack_num())
    }

    fn to_bool(&self) -> bool {
        self.get().to_i32() != 0
    }
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        hasher.write_u64(NumRef::Int(StarlarkIntRef::Small(self.get())).get_hash_64());
        Ok(())
    }

    fn get_hash(&self, _private: Private) -> anyhow::Result<StarlarkHashValue> {
        Ok(NumRef::Int(StarlarkIntRef::Small(self.get())).get_hash())
    }

    fn plus(&self, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_int(self.get()))
    }
    fn minus(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(-StarlarkIntRef::Small(self.get())))
    }
    fn add(&self, other: Value<'v>, heap: &'v Heap) -> Option<anyhow::Result<Value<'v>>> {
        Some(Ok(heap.alloc(
            NumRef::Int(StarlarkIntRef::Small(self.get())) + other.unpack_num()?,
        )))
    }
    fn sub(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(other) => Ok(heap.alloc(NumRef::Int(StarlarkIntRef::Small(self.get())) - other)),
            None => ValueError::unsupported_with(self, "-", other),
        }
    }
    fn mul(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(other) => Ok(heap.alloc(NumRef::Int(StarlarkIntRef::Small(self.get())) * other)),
            None => other.mul(Value::new_int(self.get()), heap),
        }
    }
    fn div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(other) => {
                Ok(heap.alloc(NumRef::Int(StarlarkIntRef::Small(self.get())).div(other)?))
            }
            None => ValueError::unsupported_with(self, "/", other),
        }
    }
    fn percent(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(other) => {
                Ok(heap.alloc(NumRef::Int(StarlarkIntRef::Small(self.get())).percent(other)?))
            }
            None => ValueError::unsupported_with(self, "%", other),
        }
    }
    fn floor_div(&self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match other.unpack_num() {
            Some(other) => {
                Ok(heap.alloc(NumRef::Int(StarlarkIntRef::Small(self.get())).floor_div(other)?))
            }
            None => ValueError::unsupported_with(self, "//", other),
        }
    }

    fn compare(&self, other: Value) -> anyhow::Result<Ordering> {
        match other.unpack_num() {
            None => ValueError::unsupported_with(self, "compare", other),
            Some(other) => Ok(NumRef::Int(StarlarkIntRef::Small(self.get())).cmp(&other)),
        }
    }

    fn bit_and(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match StarlarkIntRef::unpack_value(other) {
            None => ValueError::unsupported_with(self, "&", other),
            Some(StarlarkIntRef::Small(i)) => Ok(Value::new_int(self.get() & i)),
            Some(StarlarkIntRef::Big(b)) => {
                Ok(heap.alloc(StarlarkInt::from(&self.to_bigint() & b.get())))
            }
        }
    }

    fn bit_or(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match StarlarkIntRef::unpack_value(other) {
            None => ValueError::unsupported_with(self, "|", other),
            Some(StarlarkIntRef::Small(i)) => Ok(Value::new_int(self.get() | i)),
            Some(StarlarkIntRef::Big(b)) => {
                Ok(heap.alloc(StarlarkInt::from(&self.to_bigint() | b.get())))
            }
        }
    }

    fn bit_xor(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match StarlarkIntRef::unpack_value(other) {
            None => ValueError::unsupported_with(self, "^", other),
            Some(StarlarkIntRef::Small(i)) => Ok(Value::new_int(self.get() ^ i)),
            Some(StarlarkIntRef::Big(b)) => {
                Ok(heap.alloc(StarlarkInt::from(&self.to_bigint() ^ b.get())))
            }
        }
    }

    fn bit_not(&self, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_int(!self.get()))
    }

    fn left_shift(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match StarlarkIntRef::unpack_value(other) {
            None => ValueError::unsupported_with(self, "<<", other),
            Some(other) => Ok(heap.alloc(StarlarkIntRef::Small(self.get()).left_shift(other)?)),
        }
    }

    fn right_shift(&self, other: Value, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match StarlarkIntRef::unpack_value(other) {
            None => ValueError::unsupported_with(self, ">>", other),
            Some(other) => Ok(heap.alloc(StarlarkIntRef::Small(self.get()).right_shift(other)?)),
        }
    }

    fn bin_op_ty(op: TypingBinOp, rhs: &TyBasic) -> Option<Ty> {
        // This is dead code, because canonical int type is `StarlarkBigInt`,
        // but keep for consistency.
        typecheck_num_bin_op(NumTy::Int, op, rhs)
    }
}

impl Serialize for PointerI32 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.get().serialize(serializer)
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
        fn check(x: InlineInt) {
            assert_eq!(x, PointerI32::new(x).get())
        }

        for x in -10..10 {
            check(InlineInt::try_from(x).ok().unwrap())
        }
        check(InlineInt::MAX);
        check(InlineInt::MIN);
    }

    #[test]
    fn test_alignment_int_pointer() {
        assert_eq!(1, std::mem::align_of::<PointerI32>());
    }

    #[test]
    fn test_as_avalue_dyn() {
        // `get_type` calls `as_avalue_dyn` internally.
        assert_eq!("int", Value::new_int(InlineInt::MINUS_ONE).get_type());
    }
}

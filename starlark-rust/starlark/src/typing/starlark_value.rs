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
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;

use allocative::Allocative;
use dupe::Dupe;

use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingBinOp;
use crate::typing::TypingUnOp;
use crate::values::bool::StarlarkBool;
use crate::values::float::StarlarkFloat;
use crate::values::none::NoneType;
use crate::values::starlark_type_id::StarlarkTypeId;
use crate::values::string::StarlarkStr;
use crate::values::traits::StarlarkValueVTable;
use crate::values::traits::StarlarkValueVTableGet;
use crate::values::types::bigint::StarlarkBigInt;
use crate::values::types::not_type::NotType;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::StarlarkValue;
use crate::values::Value;

// This is a bit suboptimal for binary size:
// we have two vtable instances for each type: this one, and the one within `AValue` vtable.
struct TyStarlarkValueVTable {
    type_name: &'static str,
    // TODO(nga): put these into generated `StarlarkValueVTable`.
    vtable: StarlarkValueVTable,
    starlark_type_id: StarlarkTypeId,
    /// `starlark_type_id` is `TypeId` of `T::Canonical`.
    /// This is `TypeId` of `T::Canonical::Canonical`.
    starlark_type_id_check: StarlarkTypeId,
}

struct TyStarlarkValueVTableGet<'v, T: StarlarkValue<'v>>(PhantomData<&'v T>);

impl<'v, T: StarlarkValue<'v>> TyStarlarkValueVTableGet<'v, T> {
    const VTABLE: TyStarlarkValueVTable = TyStarlarkValueVTable {
        type_name: T::TYPE,
        vtable: StarlarkValueVTableGet::<T>::VTABLE,
        starlark_type_id: StarlarkTypeId::of_canonical::<T>(),
        starlark_type_id_check: StarlarkTypeId::of_canonical::<T::Canonical>(),
    };
}

/// Type implementation where typing is handled by the `StarlarkValue` trait implementation.
#[derive(Allocative, Clone, Copy, Dupe)]
pub struct TyStarlarkValue {
    #[allocative(skip)]
    vtable: &'static TyStarlarkValueVTable,
}

impl Debug for TyStarlarkValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("TyStarlarkValue")
            .field("type_name", &self.vtable.type_name)
            .finish_non_exhaustive()
    }
}

impl Display for TyStarlarkValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.vtable.type_name {
            "string" => write!(f, "str"),
            "NoneType" => write!(f, "None"),
            type_name => write!(f, "{}", type_name),
        }
    }
}

impl PartialEq for TyStarlarkValue {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.vtable.starlark_type_id == other.vtable.starlark_type_id
    }
}

impl Eq for TyStarlarkValue {}

impl Hash for TyStarlarkValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash type name because type id is not stable.
        // TODO(nga): store hash in vtable.
        self.vtable.type_name.hash(state);
    }
}

impl PartialOrd for TyStarlarkValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyStarlarkValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.vtable.type_name.cmp(other.vtable.type_name)
    }
}

impl TyStarlarkValue {
    pub(crate) const fn new<'v, T: StarlarkValue<'v>>() -> TyStarlarkValue {
        TyStarlarkValue {
            vtable: &TyStarlarkValueVTableGet::<T::Canonical>::VTABLE,
        }
    }

    // Cannot have this check in constructor where it belongs because `new` is `const`.
    #[inline]
    fn self_check(self) {
        debug_assert_ne!(
            self.vtable.starlark_type_id,
            StarlarkTypeId::of::<NotType>(),
            "`Canonical` for `{}` is `NotType",
            self.vtable.type_name
        );
        debug_assert_eq!(
            self.vtable.starlark_type_id, self.vtable.starlark_type_id_check,
            "`Canonical` for `{}` is not canonical",
            self.vtable.type_name
        );
    }

    pub(crate) fn as_name(self) -> &'static str {
        self.self_check();
        self.vtable.type_name
    }

    pub(crate) const fn int() -> TyStarlarkValue {
        TyStarlarkValue::new::<StarlarkBigInt>()
    }

    pub(crate) const fn float() -> TyStarlarkValue {
        TyStarlarkValue::new::<StarlarkFloat>()
    }

    /// Result of applying unary operator to this type.
    pub(crate) fn un_op(self, un_op: TypingUnOp) -> Result<TyStarlarkValue, ()> {
        let has = match un_op {
            TypingUnOp::Plus => self.vtable.vtable.HAS_plus,
            TypingUnOp::Minus => self.vtable.vtable.HAS_minus,
            TypingUnOp::BitNot => self.vtable.vtable.HAS_bit_not,
        };
        if has { Ok(self) } else { Err(()) }
    }

    pub(crate) fn bin_op(self, op: TypingBinOp, rhs: &TyBasic) -> Result<Ty, ()> {
        match (self.vtable.vtable.bin_op_ty)(op, rhs) {
            Some(ty) => Ok(ty),
            None => Err(()),
        }
    }

    pub(crate) fn rbin_op(self, op: TypingBinOp, lhs: &TyBasic) -> Result<Ty, ()> {
        match (self.vtable.vtable.rbin_op_ty)(lhs, op) {
            Some(ty) => Ok(ty),
            None => Err(()),
        }
    }

    pub(crate) fn index(self, _index: &TyBasic) -> Result<Ty, ()> {
        if self.vtable.vtable.HAS_at {
            Ok(Ty::any())
        } else {
            Err(())
        }
    }

    pub(crate) fn attr(self, name: &str) -> Result<Ty, ()> {
        if let Some(methods) = (self.vtable.vtable.get_methods)() {
            if let Some(method) = methods.get(name) {
                return Ok(Ty::of_value(method));
            }
        }
        if let Some(ty) = (self.vtable.vtable.attr_ty)(name) {
            return Ok(ty);
        }
        Err(())
    }

    #[inline]
    pub(crate) fn is_iterable(vtable: &StarlarkValueVTable) -> bool {
        vtable.HAS_iterate || vtable.HAS_iterate_collect
    }

    pub(crate) fn iter_item(self) -> Result<Ty, ()> {
        if Self::is_iterable(&self.vtable.vtable) {
            Ok(Ty::any())
        } else {
            Err(())
        }
    }

    /// Convert to runtime type matcher.
    pub(crate) fn type_compiled<'v>(
        self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        self.self_check();

        // First handle special cases that can match faster than default matcher.
        // These are optimizations.
        if self.vtable.starlark_type_id == StarlarkTypeId::of::<StarlarkBigInt>() {
            TypeCompiled::type_int()
        } else if self.vtable.starlark_type_id == StarlarkTypeId::of::<StarlarkBool>() {
            TypeCompiled::type_bool()
        } else if self.vtable.starlark_type_id == StarlarkTypeId::of::<NoneType>() {
            TypeCompiled::type_none()
        } else if self.vtable.starlark_type_id == StarlarkTypeId::of::<StarlarkStr>() {
            TypeCompiled::type_string()
        } else {
            #[derive(Hash, Eq, PartialEq, Allocative, Debug, Clone)]
            struct StarlarkTypeIdMatcher {
                starlark_type_id: StarlarkTypeId,
            }

            impl TypeCompiledImpl for StarlarkTypeIdMatcher {
                fn matches(&self, value: Value) -> bool {
                    value.starlark_type_id() == self.starlark_type_id
                }
            }

            type_compiled_factory.alloc(StarlarkTypeIdMatcher {
                starlark_type_id: self.vtable.starlark_type_id,
            })
        }
    }
}

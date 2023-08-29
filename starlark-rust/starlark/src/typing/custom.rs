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

use std::any;
use std::any::Any;
use std::any::TypeId;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use cmp_any::OrdAny;
use cmp_any::PartialEqAny;
use dupe::Dupe;
use starlark_map::StarlarkHasher;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::error::TypingOrInternalError;
use crate::typing::oracle::traits::TypingAttr;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingBinOp;
use crate::typing::TypingOracleCtx;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::Value;

/// Custom type implementation. [`Display`] must implement the representation of the type.
pub trait TyCustomImpl:
    Debug + Display + Clone + Hash + Ord + Allocative + Send + Sync + 'static
{
    fn as_name(&self) -> Option<&str>;
    fn validate_call(
        &self,
        span: Span,
        _args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        Err(oracle.msg_error(span, format!("Value of type `{}` is not callable", self)))
    }
    /// Must override if implementing `validate_call`.
    fn is_callable(&self) -> bool {
        false
    }
    fn bin_op(&self, bin_op: TypingBinOp, rhs: &TyBasic, ctx: &TypingOracleCtx) -> Result<Ty, ()> {
        let _unused = (bin_op, rhs, ctx);
        Err(())
    }
    fn iter_item(&self) -> Result<Ty, ()> {
        Err(())
    }
    fn attribute(&self, attr: TypingAttr) -> Result<Ty, ()>;
    fn union2(x: Arc<Self>, other: Arc<Self>) -> Result<Arc<Self>, (Arc<Self>, Arc<Self>)> {
        if x == other { Ok(x) } else { Err((x, other)) }
    }
    fn intersects(x: &Self, y: &Self) -> bool {
        let _ignore = (x, y);
        true
    }

    /// Create runtime type matcher for values.
    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>>;
}

pub(crate) trait TyCustomDyn: Debug + Display + Allocative + Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn hash_code(&self) -> u64;
    fn cmp_token(&self) -> (OrdAny, &'static str);
    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync>;
    fn as_any(&self) -> &dyn Any;

    fn as_name_dyn(&self) -> Option<&str>;
    fn validate_call_dyn(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError>;
    fn is_callable_dyn(&self) -> bool;
    fn iter_item_dyn(&self) -> Result<Ty, ()>;
    fn attribute_dyn(&self, attr: TypingAttr) -> Result<Ty, ()>;
    fn bin_op_dyn(
        &self,
        bin_op: TypingBinOp,
        rhs: &TyBasic,
        ctx: &TypingOracleCtx,
    ) -> Result<Ty, ()>;
    fn union2_dyn(
        self: Arc<Self>,
        other: Arc<dyn TyCustomDyn>,
    ) -> Result<Arc<dyn TyCustomDyn>, (Arc<dyn TyCustomDyn>, Arc<dyn TyCustomDyn>)>;
    fn intersects_dyn(&self, other: &dyn TyCustomDyn) -> bool;

    fn matcher_dyn<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>>;
}

impl<T: TyCustomImpl> TyCustomDyn for T {
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash_code(&self) -> u64 {
        let mut hasher = StarlarkHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

    fn cmp_token(&self) -> (OrdAny, &'static str) {
        (OrdAny::new(self), any::type_name::<Self>())
    }

    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync> {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_name_dyn(&self) -> Option<&str> {
        self.as_name()
    }

    fn validate_call_dyn(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        self.validate_call(span, args, oracle)
    }

    fn is_callable_dyn(&self) -> bool {
        self.is_callable()
    }

    fn attribute_dyn(&self, attr: TypingAttr) -> Result<Ty, ()> {
        self.attribute(attr)
    }

    fn iter_item_dyn(&self) -> Result<Ty, ()> {
        self.iter_item()
    }

    fn bin_op_dyn(
        &self,
        bin_op: TypingBinOp,
        rhs: &TyBasic,
        ctx: &TypingOracleCtx,
    ) -> Result<Ty, ()> {
        self.bin_op(bin_op, rhs, ctx)
    }

    fn union2_dyn(
        self: Arc<Self>,
        other: Arc<dyn TyCustomDyn>,
    ) -> Result<Arc<dyn TyCustomDyn>, (Arc<dyn TyCustomDyn>, Arc<dyn TyCustomDyn>)> {
        if TypeId::of::<Self>() == other.eq_token().type_id() {
            let other: Arc<Self> = Arc::downcast(other.into_any()).unwrap();
            T::union2(self, other)
                .map::<Arc<dyn TyCustomDyn>, _>(|x| x)
                .map_err::<(Arc<dyn TyCustomDyn>, Arc<dyn TyCustomDyn>), _>(|(x, y)| (x, y))
        } else {
            Err((self, other))
        }
    }

    fn intersects_dyn(&self, other: &dyn TyCustomDyn) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<Self>() {
            T::intersects(self, other)
        } else {
            false
        }
    }

    fn matcher_dyn<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        self.matcher(type_compiled_factory)
    }
}

#[derive(Debug, derive_more::Display, Allocative, Clone, Dupe)]
pub struct TyCustom(pub(crate) Arc<dyn TyCustomDyn>);

impl TyCustom {
    pub(crate) fn new<T: TyCustomImpl>(ty: T) -> Self {
        Self(Arc::new(ty))
    }

    pub(crate) fn as_name(&self) -> Option<&str> {
        self.0.as_name_dyn()
    }

    pub(crate) fn union2(x: TyCustom, y: TyCustom) -> Result<TyCustom, (TyCustom, TyCustom)> {
        x.0.union2_dyn(y.0)
            .map(TyCustom)
            .map_err(|(x, y)| (TyCustom(x), TyCustom(y)))
    }

    pub(crate) fn intersects(x: &TyCustom, y: &TyCustom) -> bool {
        x.0.intersects_dyn(&*y.0)
    }

    pub(crate) fn intersects_with(&self, other: &TyBasic) -> bool {
        match other {
            TyBasic::Custom(other) => Self::intersects(self, other),
            TyBasic::Name(name) => self.as_name() == Some(name.as_str()),
            TyBasic::Callable => self.0.is_callable_dyn(),
            _ => false,
        }
    }

    pub(crate) fn matcher<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        self.0.matcher_dyn(type_compiled_factory)
    }
}

impl PartialEq for TyCustom {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_token() == other.0.eq_token()
    }
}

impl Eq for TyCustom {}

impl Hash for TyCustom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash_code().hash(state)
    }
}

impl PartialOrd for TyCustom {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TyCustom {
    fn cmp(&self, other: &Self) -> Ordering {
        let (a_cmp, a_type_name) = self.0.cmp_token();
        let (b_cmp, b_type_name) = other.0.cmp_token();

        // Type ids are comparable, but we want comparison independent of hashing.
        if OrdAny::type_id(&a_cmp) != OrdAny::type_id(&b_cmp) {
            let type_name_cmp = a_type_name.cmp(b_type_name);
            if type_name_cmp != Ordering::Equal {
                return type_name_cmp;
            }

            // This is unreachable: if the type names are the same,
            // the type ids should be the same.
        }

        a_cmp.cmp(&b_cmp)
    }
}

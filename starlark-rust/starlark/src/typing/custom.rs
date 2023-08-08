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

use allocative::Allocative;
use cmp_any::OrdAny;
use cmp_any::PartialEqAny;
use starlark_map::StarlarkHasher;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::error::TypingOrInternalError;
use crate::typing::Arg;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingAttr;
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
    fn attribute(&self, attr: TypingAttr) -> Result<Ty, ()>;
    fn union2(x: Box<Self>, other: Box<Self>) -> Result<Box<Self>, (Box<Self>, Box<Self>)> {
        if x == other { Ok(x) } else { Err((x, other)) }
    }

    /// Create runtime type matcher for values.
    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>>;
}

pub(crate) trait TyCustomDyn: Debug + Display + Allocative + Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn hash_code(&self) -> u64;
    fn cmp_token(&self) -> (OrdAny, &'static str);
    fn into_any(self: Box<Self>) -> Box<dyn Any>;

    fn clone_box_dyn(&self) -> Box<dyn TyCustomDyn>;
    fn as_name_dyn(&self) -> Option<&str>;
    fn validate_call_dyn(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError>;
    fn attribute_dyn(&self, attr: TypingAttr) -> Result<Ty, ()>;
    fn union2_dyn(
        self: Box<Self>,
        other: Box<dyn TyCustomDyn>,
    ) -> Result<Box<dyn TyCustomDyn>, (Box<dyn TyCustomDyn>, Box<dyn TyCustomDyn>)>;

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

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }

    fn clone_box_dyn(&self) -> Box<dyn TyCustomDyn> {
        Box::new(self.clone())
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

    fn attribute_dyn(&self, attr: TypingAttr) -> Result<Ty, ()> {
        self.attribute(attr)
    }

    fn union2_dyn(
        self: Box<Self>,
        other: Box<dyn TyCustomDyn>,
    ) -> Result<Box<dyn TyCustomDyn>, (Box<dyn TyCustomDyn>, Box<dyn TyCustomDyn>)> {
        if TypeId::of::<Self>() == other.eq_token().type_id() {
            let other: Box<Self> = other.into_any().downcast().unwrap();
            T::union2(self, other)
                .map::<Box<dyn TyCustomDyn>, _>(|x| x)
                .map_err::<(Box<dyn TyCustomDyn>, Box<dyn TyCustomDyn>), _>(|(x, y)| (x, y))
        } else {
            Err((self, other))
        }
    }

    fn matcher_dyn<'v>(
        &self,
        type_compiled_factory: TypeCompiledFactory<'v>,
    ) -> TypeCompiled<Value<'v>> {
        self.matcher(type_compiled_factory)
    }
}

#[derive(Debug, derive_more::Display, Allocative)]
pub struct TyCustom(pub(crate) Box<dyn TyCustomDyn>);

impl TyCustom {
    pub(crate) fn as_name(&self) -> Option<&str> {
        self.0.as_name_dyn()
    }

    pub(crate) fn union2(x: TyCustom, y: TyCustom) -> Result<TyCustom, (TyCustom, TyCustom)> {
        x.0.union2_dyn(y.0)
            .map(TyCustom)
            .map_err(|(x, y)| (TyCustom(x), TyCustom(y)))
    }

    #[allow(clippy::if_same_then_else, clippy::needless_bool)]
    pub(crate) fn intersects(x: &TyCustom, y: &TyCustom) -> bool {
        if x.as_name() == Some("function") && y.as_name() == Some("function") {
            true
        } else if x.0.eq_token().type_id() == y.0.eq_token().type_id() {
            // FIXME: Can probably be a bit more precise here
            true
        } else {
            false
        }
    }

    pub(crate) fn intersects_with(&self, other: &TyBasic) -> bool {
        match other {
            TyBasic::Custom(other) => Self::intersects(self, other),
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

impl Clone for TyCustom {
    fn clone(&self) -> TyCustom {
        TyCustom(self.0.clone_box_dyn())
    }
}

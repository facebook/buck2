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

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;

use allocative::Allocative;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::callable_param::Param;
use crate::typing::callable_param::ParamMode;
use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::small_arc_vec_or_static::SmallArcVec1OrStatic;
use crate::typing::Ty;
use crate::typing::TyBasic;
use crate::typing::TypingBinOp;
use crate::typing::TypingOracleCtx;
use crate::values::typing::type_compiled::alloc::TypeMatcherAlloc;

/// An argument being passed to a function
#[derive(Debug)]
pub enum Arg<'a> {
    /// A positional argument.
    Pos(Ty),
    /// A named argument.
    Name(&'a str, Ty),
    /// A `*args`.
    Args(Ty),
    /// A `**kwargs`.
    Kwargs(Ty),
}

/// Custom function typechecker.
pub trait TyCustomFunctionImpl:
    Debug + Eq + Ord + Hash + Allocative + Send + Sync + 'static
{
    fn has_type_attr(&self) -> bool {
        false
    }

    fn validate_call(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError>;

    fn as_function(&self) -> Option<&TyFunction> {
        None
    }
}

#[derive(
    Allocative,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Debug,
    derive_more::Display
)]
#[display(fmt = "\"function\"")]
pub struct TyCustomFunction<F: TyCustomFunctionImpl>(pub F);

impl<F: TyCustomFunctionImpl> TyCustomImpl for TyCustomFunction<F> {
    fn as_name(&self) -> Option<&str> {
        Some("function")
    }

    fn validate_call(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        self.0.validate_call(span, args, oracle)
    }

    fn is_callable(&self) -> bool {
        true
    }

    fn as_function(&self) -> Option<&TyFunction> {
        self.0.as_function()
    }

    fn bin_op(
        &self,
        bin_op: TypingBinOp,
        _rhs: &TyBasic,
        _ctx: &TypingOracleCtx,
    ) -> Result<Ty, ()> {
        match bin_op {
            // `str | list`.
            TypingBinOp::BitOr if self.0.has_type_attr() => {
                // TODO(nga): result is type, but we don't have a type for type yet.
                Ok(Ty::any())
            }
            _ => Err(()),
        }
    }

    fn index(&self, _item: &TyBasic, _ctx: &TypingOracleCtx) -> Result<Ty, ()> {
        // TODO(nga): this is hack for `enum` (type) which pretends to be a function.
        //   Should be a custom type.
        Ok(Ty::any())
    }

    fn attribute(&self, attr: &str) -> Result<Ty, ()> {
        if attr == "type" && self.0.has_type_attr() {
            Ok(Ty::string())
        } else {
            Err(())
        }
    }

    fn matcher<T: TypeMatcherAlloc>(&self, factory: T) -> T::Result {
        factory.callable()
    }
}

/// A function.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct TyFunction {
    /// The `.type` property of the function, often `""`.
    pub(crate) type_attr: Option<Ty>,
    /// The parameters to the function.
    pub(crate) params: SmallArcVec1OrStatic<Param>,
    /// The result type of the function.
    pub(crate) result: Ty,
}

impl TyFunction {
    /// Constructor.
    pub fn new_with_type_attr(params: Vec<Param>, result: Ty, type_attr: Ty) -> Self {
        // TODO(nga): validate params are in correct order.
        TyFunction {
            type_attr: Some(type_attr),
            params: Self::maybe_intern_params(params),
            result,
        }
    }

    /// Constructor.
    pub fn new(params: Vec<Param>, result: Ty) -> Self {
        TyFunction {
            type_attr: None,
            params: Self::maybe_intern_params(params),
            result,
        }
    }

    fn maybe_intern_params(params: Vec<Param>) -> SmallArcVec1OrStatic<Param> {
        if params.as_slice() == Self::any_params() {
            SmallArcVec1OrStatic::new_static(Self::any_params())
        } else {
            SmallArcVec1OrStatic::clone_from_slice(&params)
        }
    }

    /// `*args`, `**kwargs` parameters.
    fn any_params() -> &'static [Param] {
        static ANY_PARAMS: [Param; 2] = [Param::args(Ty::any()), Param::kwargs(Ty::any())];
        &ANY_PARAMS
    }

    /// Function type that accepts any arguments and returns any result.
    pub(crate) fn _any() -> TyFunction {
        TyFunction {
            type_attr: None,
            params: SmallArcVec1OrStatic::new_static(Self::any_params()),
            result: Ty::any(),
        }
    }
}

impl Display for TyFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TyFunction { params, result, .. } = self;
        write!(f, "def(")?;
        let mut first = true;
        for param in params.iter() {
            if !first {
                write!(f, ", ")?;
                first = false;
            }
            let opt = if param.optional { "=.." } else { "" };
            match &param.mode {
                ParamMode::PosOnly => write!(f, "#: {}{}", param.ty, opt)?,
                ParamMode::PosOrName(name) => write!(f, "#{}: {}{}", name, param.ty, opt)?,
                ParamMode::NameOnly(name) => write!(f, "{}: {}{}", name, param.ty, opt)?,
                ParamMode::Args => write!(f, "*args: {}", param.ty)?,
                ParamMode::Kwargs => write!(f, "**kwargs: {}", param.ty)?,
            }
        }
        write!(f, ") -> {}", result)
    }
}

impl TyCustomFunctionImpl for TyFunction {
    fn has_type_attr(&self) -> bool {
        self.type_attr.is_some()
    }

    fn validate_call(
        &self,
        span: Span,
        args: &[Spanned<Arg>],
        oracle: TypingOracleCtx,
    ) -> Result<Ty, TypingOrInternalError> {
        oracle.validate_fn_call(span, self, args)
    }

    fn as_function(&self) -> Option<&TyFunction> {
        Some(self)
    }
}

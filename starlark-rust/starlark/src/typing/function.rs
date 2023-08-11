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
use dupe::Dupe;

use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::typing::custom::TyCustomImpl;
use crate::typing::error::TypingOrInternalError;
use crate::typing::Ty;
use crate::typing::TypingAttr;
use crate::typing::TypingBinOp;
use crate::typing::TypingOracleCtx;
use crate::values::typing::type_compiled::compiled::TypeCompiled;
use crate::values::typing::type_compiled::compiled::TypeCompiledImpl;
use crate::values::typing::type_compiled::factory::TypeCompiledFactory;
use crate::values::Value;

/// An argument being passed to a function
#[derive(Debug)]
pub enum Arg {
    /// A positional argument.
    Pos(Ty),
    /// A named argument.
    Name(String, Ty),
    /// A `*args`.
    Args(Ty),
    /// A `**kwargs`.
    Kwargs(Ty),
}

/// The type of a parameter - can be positional, by name, `*args` or `**kwargs`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub enum ParamMode {
    /// Parameter can only be passed by position.
    PosOnly,
    /// Parameter can be passed by position or name.
    PosOrName(String),
    /// Parameter can only be passed by name.
    NameOnly(String),
    /// Parameter is `*args`.
    Args,
    /// Parameter is `**kwargs`.
    Kwargs,
}

/// A parameter argument to a function
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct Param {
    /// The type of parameter
    pub mode: ParamMode,
    /// Whether the parameter have a default value or is otherwise optional
    pub optional: bool,
    /// The type of the parameter
    pub ty: Ty,
}

impl Param {
    /// Create a [`ParamMode::PosOnly`] parameter.
    pub fn pos_only(ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOnly,
            optional: false,
            ty,
        }
    }

    /// Create a [`ParamMode::NameOnly`] parameter.
    pub fn name_only(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::NameOnly(name.to_owned()),
            optional: false,
            ty,
        }
    }

    /// Create a [`ParamMode::PosOrName`] parameter.
    pub fn pos_or_name(name: &str, ty: Ty) -> Self {
        Self {
            mode: ParamMode::PosOrName(name.to_owned()),
            optional: false,
            ty,
        }
    }

    /// Make a parameter optional.
    pub fn optional(self) -> Self {
        Self {
            optional: true,
            ..self
        }
    }

    /// Create a [`ParamMode::Args`] parameter.
    ///
    /// `ty` is a tuple item type.
    pub fn args(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Args,
            optional: true,
            ty,
        }
    }

    /// Create a [`ParamMode::Kwargs`] parameter.
    ///
    /// `ty` is a dict value type.
    pub fn kwargs(ty: Ty) -> Self {
        Self {
            mode: ParamMode::Kwargs,
            optional: true,
            ty,
        }
    }

    pub(crate) fn allows_pos(&self) -> bool {
        match self.mode {
            ParamMode::PosOnly | ParamMode::PosOrName(_) | ParamMode::Args => true,
            ParamMode::NameOnly(_) | ParamMode::Kwargs => false,
        }
    }

    pub(crate) fn allows_many(&self) -> bool {
        match self.mode {
            ParamMode::Args | ParamMode::Kwargs => true,
            _ => false,
        }
    }

    /// Get a display name for this parameter.
    pub fn name(&self) -> &str {
        match &self.mode {
            ParamMode::PosOnly => "_",
            ParamMode::PosOrName(x) => x,
            ParamMode::NameOnly(x) => x,
            ParamMode::Args => "*args",
            ParamMode::Kwargs => "**kwargs",
        }
    }
}

/// Custom function typechecker.
pub trait TyCustomFunctionImpl:
    Clone + Debug + Eq + Ord + Hash + Allocative + Send + Sync + 'static
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
}

#[derive(
    Allocative,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Debug,
    Clone,
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

    fn attribute(&self, attr: TypingAttr) -> Result<Ty, ()> {
        if attr == TypingAttr::Regular("type") && self.0.has_type_attr() {
            Ok(Ty::string())
        } else if attr == TypingAttr::BinOp(TypingBinOp::BitOr) && self.0.has_type_attr() {
            // `str | list`.
            Ok(Ty::function(
                vec![Param::pos_only(Ty::any())],
                // TODO(nga): result is type, but we don't have a type for type yet.
                Ty::any(),
            ))
        } else {
            Err(())
        }
    }

    fn matcher<'v>(&self, factory: TypeCompiledFactory<'v>) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Eq, PartialEq, Hash, Clone, Copy, Dupe, Debug)]
        struct FunctionMatcher;

        impl TypeCompiledImpl for FunctionMatcher {
            fn matches(&self, value: Value) -> bool {
                value.vtable().starlark_value.HAS_invoke
            }
        }

        factory.alloc(FunctionMatcher)
    }
}

/// A function.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Allocative)]
pub struct TyFunction {
    /// The `.type` property of the function, often `""`.
    pub type_attr: Option<Ty>,
    /// The parameters to the function.
    pub params: Vec<Param>,
    /// The result type of the function.
    pub result: Box<Ty>,
}

impl TyFunction {
    /// Function type that accepts any arguments and returns any result.
    pub(crate) fn any() -> TyFunction {
        TyFunction {
            type_attr: None,
            params: vec![Param::args(Ty::any()), Param::kwargs(Ty::any())],
            result: Box::new(Ty::any()),
        }
    }
}

impl Display for TyFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TyFunction { params, result, .. } = self;
        write!(f, "def(")?;
        let mut first = true;
        for param in params {
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
}

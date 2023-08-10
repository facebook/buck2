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

// This makes for a better API.
#![allow(clippy::result_unit_err)]

use dupe::Dupe;

use crate::typing::basic::TyBasic;
use crate::typing::function::TyFunction;
use crate::typing::ty::Ty;
use crate::typing::ty::TyName;

/// Unary operator for [`TypingOracle::attribute`].
#[derive(Copy, Clone, Dupe, Eq, PartialEq, derive_more::Display, Debug)]
pub enum TypingUnOp {
    /// `+`.
    #[display(fmt = "+")]
    Plus,
    /// `+`.
    #[display(fmt = "-")]
    Minus,
    /// `~`.
    #[display(fmt = "~")]
    BitNot,
}

/// Binary operator for [`TypingOracle::attribute`].
#[derive(Copy, Clone, Dupe, Eq, PartialEq, derive_more::Display, Debug)]
pub enum TypingBinOp {
    /// `+`.
    #[display(fmt = "+")]
    Add,
    /// `-`.
    #[display(fmt = "-")]
    Sub,
    /// `/`.
    #[display(fmt = "/")]
    Div,
    /// `//`.
    #[display(fmt = "/")]
    FloorDiv,
    /// `*`.
    #[display(fmt = "*")]
    Mul,
    /// `%`.
    #[display(fmt = "%")]
    Percent,
    /// `y in x`.
    #[display(fmt = "in")]
    In,
    /// `|`.
    #[display(fmt = "|")]
    BitOr,
    /// `^`.
    #[display(fmt = "^")]
    BitXor,
    /// `&`.
    #[display(fmt = "&")]
    BitAnd,
    /// `<`.
    #[display(fmt = "<")]
    Less,
    /// `<<`.
    #[display(fmt = "<<")]
    LeftShift,
    /// `>>`.
    #[display(fmt = ">>")]
    RightShift,
}

impl TypingBinOp {
    /// Result type is always `bool`.
    pub(crate) fn always_bool(self) -> bool {
        matches!(self, TypingBinOp::In | TypingBinOp::Less)
    }
}

/// Attribute for [`TypingOracle::attribute`].
#[derive(Copy, Clone, Dupe, Eq, PartialEq, derive_more::Display)]
pub enum TypingAttr<'a> {
    /// Apply binary operator.
    #[display(fmt = "binary {}", _0)]
    BinOp(TypingBinOp),
    /// `x[a:b:c]`.
    #[display(fmt = "[::]")]
    Slice,
    /// Return iterable element type.
    #[display(fmt = "iter")]
    Iter,
    /// `x[a]`, return the function.
    #[display(fmt = "[]")]
    Index,
    /// Get a regular attribute.
    Regular(&'a str),
}

/// Callbacks which provide types when typechecking a module.
///
/// The instance for slices/`Vec` allow composing a series of oracles
/// which are tried in order until one succeeds.
#[allow(unused_variables)] // Otherwise the types are bad in completions
pub trait TypingOracle {
    /// Given a type and a `.` attribute, what is its type.
    /// Return [`Err`] to indicate we _know_ this isn't a valid attribute.
    /// Return [`None`] if we aren't sure.
    fn attribute(&self, _ty: &TyBasic, _attr: TypingAttr) -> Option<Result<Ty, ()>> {
        None
    }

    /// If type is callable, return it as function signature.
    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        None
    }

    /// If we require the first type, but got the second type, is that OK?
    /// Usually its OK if the requirement is a subtype of the one we got.
    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        false
    }
}

/// Declare that there are no attributes, usually used at the end of a [`Vec`].
#[cfg(test)]
pub(crate) struct OracleNoAttributes;

#[cfg(test)]
impl TypingOracle for OracleNoAttributes {
    fn attribute(&self, _ty: &TyBasic, _attr: TypingAttr) -> Option<Result<Ty, ()>> {
        Some(Err(()))
    }
}

/// Sequence of oracles, first one that returns [`Some`] wins.
pub struct OracleSeq<T>(pub Vec<T>)
where
    T: TypingOracle;

impl<T: TypingOracle> TypingOracle for OracleSeq<T> {
    fn attribute(&self, ty: &TyBasic, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        self.0.iter().find_map(|oracle| oracle.attribute(ty, attr))
    }

    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        self.0.iter().find_map(|oracle| oracle.as_function(ty))
    }

    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        self.0.iter().any(|oracle| oracle.subtype(require, got))
    }
}

pub(crate) struct OracleAny;

impl TypingOracle for OracleAny {}

// Forwarding traits

impl<'a, T: TypingOracle + ?Sized> TypingOracle for &'a T {
    fn attribute(&self, ty: &TyBasic, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        (*self).attribute(ty, attr)
    }
    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        (*self).as_function(ty)
    }
    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        (*self).subtype(require, got)
    }
}

impl<T: TypingOracle + ?Sized> TypingOracle for Box<T> {
    fn attribute(&self, ty: &TyBasic, attr: TypingAttr) -> Option<Result<Ty, ()>> {
        self.as_ref().attribute(ty, attr)
    }
    fn as_function(&self, ty: &TyName) -> Option<Result<TyFunction, ()>> {
        self.as_ref().as_function(ty)
    }
    fn subtype(&self, require: &TyName, got: &TyName) -> bool {
        self.as_ref().subtype(require, got)
    }
}

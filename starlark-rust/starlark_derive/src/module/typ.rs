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

use dupe::Dupe;
use proc_macro2::Ident;
use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Block;
use syn::Expr;
use syn::Type;
use syn::Visibility;

use crate::module::parse::ModuleKind;
use crate::module::util::is_type_name;
use crate::module::util::unpack_option;

#[derive(Debug)]
pub(crate) struct StarModule {
    pub module_kind: ModuleKind,
    pub visibility: Visibility,
    // We reuse the users globals_builder to make sure `use` statements etc
    // make sense
    pub globals_builder: Type,
    pub name: Ident,
    pub attrs: Vec<Attribute>,
    pub docstring: Option<String>,
    pub stmts: Vec<StarStmt>,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub(crate) enum StarStmt {
    Const(StarConst),
    Fun(StarFun),
    Attr(StarAttr),
}

#[derive(Debug)]
pub(crate) struct StarConst {
    pub name: Ident,
    pub ty: Type,
    pub value: Expr,
}

#[derive(Debug)]
pub(crate) struct SpecialParam {
    pub(crate) ident: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug)]
pub(crate) struct StarFun {
    pub name: Ident,
    pub as_type: Option<syn::Path>,
    pub attrs: Vec<Attribute>,
    pub args: Vec<StarArg>,
    /// Has `&Heap` parameter.
    pub heap: Option<SpecialParam>,
    /// Has `&mut Evaluator` parameter.
    pub eval: Option<SpecialParam>,
    /// `anyhow::Result<T>`.
    pub return_type: Type,
    pub starlark_ty_custom_function: Option<Expr>,
    pub special_builtin_function: Option<Expr>,
    pub speculative_exec_safe: bool,
    pub body: Block,
    pub source: StarFunSource,
    pub docstring: Option<String>,
}

impl StarFun {
    /// Is this function a method? (I. e. has `this` as first parameter).
    pub(crate) fn is_method(&self) -> bool {
        match self.args.first() {
            Some(first) => {
                assert!(first.source != StarArgSource::Unknown, "not yet resolved");
                first.source == StarArgSource::This
            }
            None => false,
        }
    }

    pub(crate) fn is_arguments(&self) -> bool {
        self.args
            .iter()
            .any(|arg| matches!(arg.pass_style, StarArgPassStyle::Arguments))
    }

    pub(crate) fn span(&self) -> Span {
        self.name
            .span()
            .join(self.body.span())
            .unwrap_or_else(|| self.name.span())
    }
}

#[derive(Debug)]
pub(crate) struct StarAttr {
    pub name: Ident,
    pub arg: Type,
    /// Has `&Heap` parameter.
    pub heap: Option<SpecialParam>,
    pub attrs: Vec<Attribute>,
    /// `anyhow::Result<T>`.
    pub return_type: Type,
    pub speculative_exec_safe: bool,
    pub body: Block,
    pub docstring: Option<String>,
}

#[derive(Debug, PartialEq, Copy, Clone, Dupe)]
pub(crate) enum StarArgPassStyle {
    /// Receiver.
    This,
    /// Parameter can be filled only positionally.
    PosOnly,
    /// Parameter can filled both positionally and by name by default,
    /// or named only if followed by `*args` or named only.
    Default,
    /// Parameter can be filled by name.
    NamedOnly,
    /// `*args`.
    Args,
    /// `**kwargs`.
    Kwargs,
    /// `&Arguments`.
    Arguments,
}

#[derive(Debug)]
pub(crate) struct StarArg {
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub mutable: Option<syn::Token![mut]>,
    pub pass_style: StarArgPassStyle,
    pub name: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub source: StarArgSource,
}

#[derive(Debug, PartialEq)]
pub(crate) enum StarArgSource {
    Unknown,
    This,
    Parameters,
    Argument(usize),
    Required(usize),
    Optional(usize),
}

#[derive(Debug)]
pub(crate) enum StarFunSource {
    /// Function signature is single `Arguments` parameter.
    Arguments,
    /// Function signature is `this` parameter followed by `Arguments` parameter.
    ThisArguments,
    /// Normal function which uses a signature and parameters parser.
    Signature { count: usize },
    /// Fast-path function of some required parameters, followed by some optional parameters.
    /// No named parameters or `*args`/`**kwargs`.
    Positional { required: usize, optional: usize },
}

impl StarArg {
    pub fn is_option(&self) -> bool {
        is_type_name(&self.ty, "Option")
    }

    pub(crate) fn is_required(&self) -> bool {
        !self.is_option() && self.default.is_none()
    }

    /// Remove the `Option` if it exists, otherwise return the real type.
    pub fn without_option(&self) -> &Type {
        unpack_option(&self.ty).unwrap_or(&self.ty)
    }

    pub fn is_value(&self) -> bool {
        is_type_name(&self.ty, "Value")
    }

    pub fn requires_signature(&self) -> bool {
        // We need to use a signature if something has a name
        // There are *args or **kwargs
        // There is a default that needs promoting to a Value (since the signature stores that value)
        self.pass_style != StarArgPassStyle::PosOnly
            || self.pass_style == StarArgPassStyle::Args
            || self.pass_style == StarArgPassStyle::Kwargs
            || (self.is_value() && self.default.is_some())
    }
}

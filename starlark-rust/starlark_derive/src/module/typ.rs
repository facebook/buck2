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

use proc_macro2::{Ident, Span};
use syn::{spanned::Spanned, Attribute, Block, Expr, Type, Visibility};

use crate::module::{parse::ModuleKind, util::is_type_name};

#[derive(Debug)]
pub(crate) struct StarModule {
    pub module_kind: ModuleKind,
    pub visibility: Visibility,
    // We reuse the users globals_builder to make sure `use` statements etc
    // make sense
    pub globals_builder: Type,
    pub name: Ident,
    pub docstring: Option<String>,
    pub stmts: Vec<StarStmt>,
}

impl StarModule {
    pub(crate) fn span(&self) -> Span {
        let mut span = self.name.span();
        for stmt in &self.stmts {
            if let Some(new_span) = span.join(stmt.span()) {
                span = new_span;
            }
        }
        span
    }
}

#[derive(Debug)]
pub(crate) enum StarStmt {
    Const(StarConst),
    Fun(StarFun),
    Attr(StarAttr),
}

impl StarStmt {
    pub(crate) fn span(&self) -> Span {
        match self {
            StarStmt::Const(c) => c.span(),
            StarStmt::Fun(c) => c.span(),
            StarStmt::Attr(c) => c.span(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct StarConst {
    pub name: Ident,
    pub ty: Type,
    pub value: Expr,
}

impl StarConst {
    pub(crate) fn span(&self) -> Span {
        self.name
            .span()
            .join(self.value.span())
            .unwrap_or_else(|| self.name.span())
    }
}

#[derive(Debug)]
pub(crate) struct StarFun {
    pub name: Ident,
    pub type_attribute: Option<Expr>,
    pub attrs: Vec<Attribute>,
    pub args: Vec<StarArg>,
    /// `anyhow::Result<T>`.
    pub return_type: Type,
    /// `T`.
    pub return_type_arg: Type,
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

    pub(crate) fn span(&self) -> Span {
        self.name
            .span()
            .join(self.body.span())
            .unwrap_or_else(|| self.name.span())
    }

    pub(crate) fn args_span(&self) -> Span {
        self.args
            .iter()
            .map(|a| a.span)
            .reduce(|a, b| a.join(b).unwrap_or(a))
            .unwrap_or_else(|| self.name.span())
    }
}

#[derive(Debug)]
pub(crate) struct StarAttr {
    pub name: Ident,
    pub arg: Type,
    pub attrs: Vec<Attribute>,
    /// `anyhow::Result<T>`.
    pub return_type: Type,
    /// `T`.
    pub return_type_arg: Type,
    pub speculative_exec_safe: bool,
    pub body: Block,
    pub docstring: Option<String>,
}

impl StarAttr {
    pub(crate) fn span(&self) -> Span {
        self.name
            .span()
            .join(self.body.span())
            .unwrap_or_else(|| self.name.span())
    }
}

#[derive(Debug)]
pub(crate) struct StarArg {
    pub span: Span,
    pub attrs: Vec<Attribute>,
    pub mutable: bool,
    /// Function parameter is positional-only.
    pub pos_only: bool,
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
    Unknown,
    /// Function signature is single `Arguments` parameter.
    Parameters,
    /// Function signature is `this` parameter followed by `Arguments` parameter.
    ThisParameters,
    Argument(usize),
    Positional(usize, usize),
}

impl StarModule {
    pub fn resolve(&mut self) -> syn::Result<()> {
        for x in &mut self.stmts {
            if let StarStmt::Fun(x) = x {
                x.resolve(self.module_kind)?;
            }
        }
        Ok(())
    }
}

impl StarFun {
    #[allow(clippy::branches_sharing_code)] // False positive
    pub fn resolve(&mut self, module_kind: ModuleKind) -> syn::Result<()> {
        fn requires_signature(x: &StarArg) -> bool {
            // We need to use a signature if something has a name
            // There are *args or **kwargs
            // There is a default that needs promoting to a Value (since the signature stores that value)
            !x.pos_only || x.is_args() || x.is_kwargs() || (x.is_value() && x.default.is_some())
        }

        if self.args.len() == 1 && self.args[0].is_arguments() {
            self.args[0].source = StarArgSource::Parameters;
            self.source = StarFunSource::Parameters;
        } else if self.args.len() == 2 && self.args[0].is_this() && self.args[1].is_arguments() {
            self.args[0].source = StarArgSource::This;
            self.args[1].source = StarArgSource::Parameters;
            self.source = StarFunSource::ThisParameters;
        } else {
            let use_arguments = self
                .args
                .iter()
                .filter(|x| !x.is_this())
                .any(requires_signature);
            if use_arguments {
                let mut argument = 0;
                for x in &mut self.args {
                    if x.is_this() {
                        x.source = StarArgSource::This;
                    } else {
                        x.source = StarArgSource::Argument(argument);
                        argument += 1;
                    }
                }
                self.source = StarFunSource::Argument(argument);
            } else {
                let mut required = 0;
                let mut optional = 0;
                for x in &mut self.args {
                    if x.is_this() {
                        x.source = StarArgSource::This;
                        continue;
                    }
                    if optional == 0 && x.default.is_none() && !x.is_option() {
                        x.source = StarArgSource::Required(required);
                        required += 1;
                    } else {
                        x.source = StarArgSource::Optional(optional);
                        optional += 1;
                    }
                }
                self.source = StarFunSource::Positional(required, optional);
            }
        }

        if self.is_method() && module_kind != ModuleKind::Methods {
            return Err(syn::Error::new(
                self.span(),
                "Methods can only be defined in methods module",
            ));
        }

        Ok(())
    }
}

impl StarArg {
    pub fn is_arguments(&self) -> bool {
        if let Type::Reference(ty) = &self.ty {
            is_type_name(&ty.elem, "Arguments")
        } else {
            false
        }
    }

    pub fn is_option(&self) -> bool {
        is_type_name(&self.ty, "Option")
    }

    pub fn is_value(&self) -> bool {
        is_type_name(&self.ty, "Value")
    }

    pub fn is_this(&self) -> bool {
        self.name == "this" || self.name == "_this"
    }

    pub fn is_args(&self) -> bool {
        self.name == "args"
    }

    pub fn is_kwargs(&self) -> bool {
        self.name == "kwargs"
    }
}

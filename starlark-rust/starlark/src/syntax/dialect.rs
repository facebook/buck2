/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use gazebo::dupe::Dupe;
use thiserror::Error;

use crate::{
    codemap::{CodeMap, Pos, Span, Spanned},
    errors::Diagnostic,
    syntax::ast::Visibility,
};

#[derive(Error, Debug)]
enum DialectError {
    #[error("`def` is not allowed in this dialect")]
    Def,
    #[error("`lambda` is not allowed in this dialect")]
    Lambda,
    #[error("`load` is not allowed in this dialect")]
    Load,
    #[error("* keyword-only-arguments is not allowed in this dialect")]
    KeywordOnlyArguments,
    #[error("type annotations are not allowed in this dialect")]
    Types,
}

/// How to handle type annotations in Starlark.
#[derive(Debug, Clone, Copy, Dupe, Eq, PartialEq, Hash)]
pub enum DialectTypes {
    /// Prohibit types at parse time.
    Disable,
    /// Allow types at parse time, but ignore types at runtime.
    ParseOnly,
    /// Check types at runtime.
    Enable,
}

/// Starlark language features to enable, e.g. [`Standard`](Dialect::Standard) to follow the Starlark standard.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Dialect {
    /// Are `def` statements permitted.
    /// Enabled in both [`Standard`](Dialect::Standard) and [`Extended`](Dialect::Extended).
    pub enable_def: bool,
    /// Are `lambda` expressions permitted.
    /// Enabled in both [`Standard`](Dialect::Standard) and [`Extended`](Dialect::Extended).
    pub enable_lambda: bool,
    /// Are `load` statements permitted.
    /// Enabled in both [`Standard`](Dialect::Standard) and [`Extended`](Dialect::Extended).
    pub enable_load: bool,
    /// Are `*` keyword-only arguments allowed as per [PEP 3102](https://www.python.org/dev/peps/pep-3102/).
    /// Only enabled in [`Extended`](Dialect::Extended).
    pub enable_keyword_only_arguments: bool,
    /// Are expressions allowed in type positions as per [PEP 484](https://www.python.org/dev/peps/pep-0484/).
    /// Only enabled in [`Extended`](Dialect::Extended).
    pub enable_types: DialectTypes,
    /// Are tabs permitted for indentation. If permitted, tabs are equivalent to 8 spaces.
    /// Enabled in both [`Standard`](Dialect::Standard) and [`Extended`](Dialect::Extended).
    pub enable_tabs: bool,
    /// Do `load()` statements reexport their definition.
    /// Enabled in both [`Standard`](Dialect::Standard) and [`Extended`](Dialect::Extended),
    /// but may change in future definitions of the standard.
    pub enable_load_reexport: bool,
    /// Are `for`, `if` and other statements allowed at the top level.
    /// Only enabled in [`Extended`](Dialect::Extended).
    pub enable_top_level_stmt: bool,
}

// These are morally enumerations, so give them enumeration-like names
// even though they are actually global constants
#[allow(non_upper_case_globals)]
impl Dialect {
    /// Follow the [Starlark language standard](https://github.com/bazelbuild/starlark/blob/master/spec.md) as much as possible.
    pub const Standard: Self = Self {
        enable_def: true,
        enable_lambda: true,
        enable_load: true,
        enable_keyword_only_arguments: false,
        enable_types: DialectTypes::Disable,
        enable_tabs: true,
        enable_load_reexport: true, // But they plan to change it
        enable_top_level_stmt: false,
    };

    /// A superset of [`Standard`](Dialect::Standard), including extra features (types, top-level statements etc).
    pub const Extended: Self = Self {
        enable_def: true,
        enable_lambda: true,
        enable_load: true,
        enable_keyword_only_arguments: true,
        enable_types: DialectTypes::Enable,
        enable_tabs: true,
        enable_load_reexport: true,
        enable_top_level_stmt: true,
    };
}

fn err<T>(codemap: &CodeMap, span: Span, err: DialectError) -> anyhow::Result<T> {
    Err(Diagnostic::new(err, span, codemap))
}

impl Dialect {
    pub(crate) fn check_lambda<T>(
        &self,
        codemap: &CodeMap,
        x: Spanned<T>,
    ) -> anyhow::Result<Spanned<T>> {
        if self.enable_lambda {
            Ok(x)
        } else {
            err(codemap, x.span, DialectError::Lambda)
        }
    }

    pub(crate) fn check_def<T>(
        &self,
        codemap: &CodeMap,
        x: Spanned<T>,
    ) -> anyhow::Result<Spanned<T>> {
        if self.enable_def {
            Ok(x)
        } else {
            err(codemap, x.span, DialectError::Def)
        }
    }

    pub(crate) fn check_load<T>(
        &self,
        codemap: &CodeMap,
        x: Spanned<T>,
    ) -> anyhow::Result<Spanned<T>> {
        if self.enable_load {
            Ok(x)
        } else {
            err(codemap, x.span, DialectError::Load)
        }
    }

    pub(crate) fn check_keyword_only_arguments<T>(
        &self,
        codemap: &CodeMap,
        begin: usize,
        end: usize,
        x: T,
    ) -> anyhow::Result<T> {
        let span = Span::new(Pos::new(begin as u32), Pos::new(end as u32));
        if self.enable_keyword_only_arguments {
            Ok(x)
        } else {
            err(codemap, span, DialectError::KeywordOnlyArguments)
        }
    }

    pub(crate) fn check_type<T>(
        &self,
        codemap: &CodeMap,
        x: Spanned<T>,
    ) -> anyhow::Result<Spanned<T>> {
        if self.enable_types != DialectTypes::Disable {
            Ok(x)
        } else {
            err(codemap, x.span, DialectError::Types)
        }
    }

    pub(crate) fn load_visibility(&self) -> Visibility {
        if self.enable_load_reexport {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }
}

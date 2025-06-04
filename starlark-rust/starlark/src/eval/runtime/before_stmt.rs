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

//! Configuration of `BeforeStmt` instrumentation of bytecode.

use crate::codemap::FileSpanRef;
use crate::eval::Evaluator;

/// Configuration of `BeforeStmt` instrumentation of bytecode.
#[derive(Default)]
pub(crate) struct BeforeStmt<'a, 'e: 'a> {
    /// Functions to run before each statement.
    pub(crate) before_stmt: Vec<BeforeStmtFunc<'a, 'e>>,
    /// Explicitly request generation of `BeforeStmt` instructions
    /// even if no `before_stmt` functions are registered.
    /// This is needed when compiling dependencies of a file to be profiled.
    pub(crate) instrument: bool,
}

/// This is used by DAP, and it is not public API.
// TODO(cjhopman): pull DAP into the crate, and hide this function.
#[doc(hidden)]
pub enum BeforeStmtFunc<'a, 'e: 'a> {
    Fn(&'a dyn Fn(FileSpanRef, bool, &mut Evaluator<'_, '_, 'e>)),
    Dyn(Box<dyn BeforeStmtFuncDyn<'e>>),
}

impl<'a, 'e: 'a> BeforeStmtFunc<'a, 'e> {
    pub(crate) fn call<'v>(
        &mut self,
        span: FileSpanRef,
        continued: bool,
        eval: &mut Evaluator<'v, 'a, 'e>,
    ) -> crate::Result<()> {
        match self {
            BeforeStmtFunc::Fn(f) => {
                f(span, continued, eval);
                Ok(())
            }
            BeforeStmtFunc::Dyn(d) => d.call(span, continued, eval),
        }
    }

    pub(crate) fn from_fn(
        value: &'a dyn Fn(FileSpanRef, bool, &mut Evaluator<'_, '_, 'e>),
    ) -> Self {
        Self::Fn(value)
    }

    #[doc(hidden)]
    pub fn from_dyn(value: Box<dyn BeforeStmtFuncDyn<'e>>) -> Self {
        Self::Dyn(value)
    }
}

/// This is used by DAP, and it is not public API.
// TODO(cjhopman): pull DAP into the crate, and hide this function.
#[doc(hidden)]
pub trait BeforeStmtFuncDyn<'e> {
    /// This is used by DAP, and it is not public API.
    // TODO(cjhopman): pull DAP into the crate, and hide this function.
    #[doc(hidden)]
    fn call<'v>(
        &mut self,
        span: FileSpanRef,
        continued: bool,
        eval: &mut Evaluator<'v, '_, 'e>,
    ) -> crate::Result<()>;
}

impl<'a, 'e: 'a> BeforeStmt<'a, 'e> {
    pub(crate) fn enabled(&self) -> bool {
        self.instrument || !self.before_stmt.is_empty()
    }
}

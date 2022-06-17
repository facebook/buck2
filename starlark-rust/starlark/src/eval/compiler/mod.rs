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

pub(crate) mod args;
pub(crate) mod call;
pub(crate) mod compr;
pub(crate) mod constants;
pub(crate) mod def;
pub(crate) mod def_inline;
pub(crate) mod expr;
pub(crate) mod expr_bool;
pub(crate) mod known;
pub(crate) mod module;
pub(crate) mod scope;
pub(crate) mod small_vec_1;
pub(crate) mod span;
pub(crate) mod stmt;

use std::fmt::Debug;

use crate::{
    codemap::CodeMap,
    environment::Globals,
    errors::Diagnostic,
    eval::{
        compiler::scope::{ScopeData, ScopeId, ScopeNames},
        runtime::call_stack::FrozenFileSpan,
        Evaluator,
    },
    values::FrozenRef,
};

/// Error of evaluation of an expression.
#[derive(Debug)]
pub(crate) struct EvalException(pub(crate) anyhow::Error);

#[cold]
#[inline(never)]
fn add_span_to_error(e: anyhow::Error, span: FrozenFileSpan, eval: &Evaluator) -> anyhow::Error {
    Diagnostic::modify(e, |d: &mut Diagnostic| {
        d.set_span(span.span(), &span.file());
        d.set_call_stack(|| eval.call_stack.to_diagnostic_frames(span.inlined_frames));
    })
}

#[cold]
#[inline(never)]
pub(crate) fn add_span_to_expr_error(
    e: anyhow::Error,
    span: FrozenFileSpan,
    eval: &Evaluator,
) -> EvalException {
    EvalException(add_span_to_error(e, span, eval))
}

/// Convert syntax error to spanned evaluation exception
#[inline(always)]
pub(crate) fn expr_throw<'v, T>(
    r: anyhow::Result<T>,
    span: FrozenFileSpan,
    eval: &Evaluator<'v, '_>,
) -> Result<T, EvalException> {
    match r {
        Ok(v) => Ok(v),
        Err(e) => Err(add_span_to_expr_error(e, span, eval)),
    }
}

pub(crate) struct Compiler<'v, 'a, 'e> {
    pub(crate) eval: &'e mut Evaluator<'v, 'a>,
    pub(crate) scope_data: ScopeData,
    pub(crate) locals: Vec<ScopeId>,
    pub(crate) globals: FrozenRef<'static, Globals>,
    pub(crate) codemap: FrozenRef<'static, CodeMap>,
    pub(crate) has_before_stmt: bool,
    pub(crate) bc_profile: bool,
    pub(crate) check_types: bool,
}

impl Compiler<'_, '_, '_> {
    pub(crate) fn enter_scope(&mut self, scope_id: ScopeId) {
        self.locals.push(scope_id);
    }

    pub(crate) fn exit_scope(&mut self) -> &mut ScopeNames {
        let scope_id = self.locals.pop().unwrap();
        self.scope_data.mut_scope(scope_id)
    }
}

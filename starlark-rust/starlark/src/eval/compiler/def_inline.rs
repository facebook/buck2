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

//! Inline functions.

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        compiler::{
            def::ParameterCompiled,
            expr::{ExprBinOp, ExprCompiled, ExprLogicalBinOp, ExprUnOp},
            span::IrSpanned,
            stmt::{StmtCompiled, StmtsCompiled},
        },
        runtime::{call_stack::FrozenFileSpan, slots::LocalSlotId},
    },
    values::{FrozenStringValue, FrozenValue},
};

/// Function body suitable for inlining.
#[derive(Debug)]
pub(crate) enum InlineDefBody {
    /// Function body is `return type(x) == "y"`
    ReturnTypeIs(FrozenStringValue),
    /// Any expression which can be safely inlined.
    ///
    /// See the function where this enum variant is computed for the definition
    /// of safe to inline expression.
    ReturnSafeToInlineExpr(IrSpanned<ExprCompiled>),
}

/// If a statement is `return type(x) == "y"` where `x` is a first slot.
fn is_return_type_is(stmt: &StmtsCompiled) -> Option<FrozenStringValue> {
    match stmt.first().map(|s| &s.node) {
        Some(StmtCompiled::Return(IrSpanned {
            node:
                ExprCompiled::TypeIs(
                    box IrSpanned {
                        // Slot 0 is a slot for the first function parameter.
                        node: ExprCompiled::Local(LocalSlotId(0), ..),
                        ..
                    },
                    t,
                ),
            ..
        })) => Some(*t),
        _ => None,
    }
}

#[derive(Default)]
struct IsSafeToInlineExpr {
    /// How many expressions we visited already.
    counter: u32,
}

impl IsSafeToInlineExpr {
    fn is_safe_to_inline_opt_expr(&mut self, expr: &Option<IrSpanned<ExprCompiled>>) -> bool {
        if let Some(expr) = expr {
            self.is_safe_to_inline_expr(expr)
        } else {
            true
        }
    }

    /// Expression which is has no access to locals or globals.
    fn is_safe_to_inline_expr(&mut self, expr: &ExprCompiled) -> bool {
        // Do not inline too large functions.
        if self.counter > 100 {
            return false;
        }
        self.counter += 1;
        match expr {
            ExprCompiled::Value(..) => true,
            ExprCompiled::Local(..)
            | ExprCompiled::LocalCaptured(..)
            | ExprCompiled::Module(..)
            | ExprCompiled::Def(..) => false,
            ExprCompiled::Call(call) => {
                self.is_safe_to_inline_expr(&call.fun)
                    && call
                        .args
                        .arg_exprs()
                        .all(|e| self.is_safe_to_inline_expr(e))
            }
            ExprCompiled::Compr(..) => {
                // TODO: some comprehensions are safe to inline.
                false
            }
            ExprCompiled::Dot(expr, field) => {
                let _: &Symbol = field;
                self.is_safe_to_inline_expr(expr)
            }
            ExprCompiled::ArrayIndirection(box (array, index)) => {
                self.is_safe_to_inline_expr(array) && self.is_safe_to_inline_expr(index)
            }
            ExprCompiled::Slice(box (a, b, c, d)) => {
                self.is_safe_to_inline_expr(a)
                    && self.is_safe_to_inline_opt_expr(b)
                    && self.is_safe_to_inline_opt_expr(c)
                    && self.is_safe_to_inline_opt_expr(d)
            }
            ExprCompiled::Op(bin_op, box (a, b)) => {
                let _: &ExprBinOp = bin_op;
                self.is_safe_to_inline_expr(a) && self.is_safe_to_inline_expr(b)
            }
            ExprCompiled::UnOp(un_op, arg) => {
                let _: &ExprUnOp = un_op;
                self.is_safe_to_inline_expr(arg)
            }
            ExprCompiled::TypeIs(v, t) => {
                let _: FrozenStringValue = *t;
                self.is_safe_to_inline_expr(v)
            }
            ExprCompiled::Tuple(xs) | ExprCompiled::List(xs) => {
                xs.iter().all(|x| self.is_safe_to_inline_expr(x))
            }
            ExprCompiled::Dict(xs) => xs
                .iter()
                .all(|(x, y)| self.is_safe_to_inline_expr(x) && self.is_safe_to_inline_expr(y)),
            ExprCompiled::If(box (c, t, f)) => {
                self.is_safe_to_inline_expr(c)
                    && self.is_safe_to_inline_expr(t)
                    && self.is_safe_to_inline_expr(f)
            }
            ExprCompiled::Not(x) => self.is_safe_to_inline_expr(x),
            ExprCompiled::LogicalBinOp(op, box (x, y)) => {
                let _: &ExprLogicalBinOp = op;
                self.is_safe_to_inline_expr(x) && self.is_safe_to_inline_expr(y)
            }
            ExprCompiled::Seq(box (x, y)) => {
                self.is_safe_to_inline_expr(x) && self.is_safe_to_inline_expr(y)
            }
            ExprCompiled::PercentSOne(box (before, v, after))
            | ExprCompiled::FormatOne(box (before, v, after)) => {
                let _: (FrozenStringValue, FrozenStringValue) = (*before, *after);
                self.is_safe_to_inline_expr(v)
            }
        }
    }
}

/// Function body is a `return` safe to inline expression (as defined above).
fn is_return_safe_to_inline_expr(stmts: &StmtsCompiled) -> Option<IrSpanned<ExprCompiled>> {
    match stmts.first() {
        None => {
            // Empty function is equivalent to `return None`.
            Some(IrSpanned {
                span: FrozenFileSpan::default(),
                node: ExprCompiled::Value(FrozenValue::new_none()),
            })
        }
        Some(stmt) => match &stmt.node {
            StmtCompiled::Return(expr)
                if IsSafeToInlineExpr::default().is_safe_to_inline_expr(expr) =>
            {
                Some(expr.clone())
            }
            _ => None,
        },
    }
}

pub(crate) fn inline_def_body(
    params: &[IrSpanned<ParameterCompiled<IrSpanned<ExprCompiled>>>],
    body: &StmtsCompiled,
) -> Option<InlineDefBody> {
    if params.len() == 1 && params[0].accepts_positional() {
        if let Some(t) = is_return_type_is(body) {
            return Some(InlineDefBody::ReturnTypeIs(t));
        }
    }
    if params.is_empty() {
        if let Some(expr) = is_return_safe_to_inline_expr(body) {
            return Some(InlineDefBody::ReturnSafeToInlineExpr(expr));
        }
    }
    None
}

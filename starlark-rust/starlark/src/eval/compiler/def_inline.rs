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
    eval::{
        compiler::{
            args::ArgsCompiledValue,
            call::CallCompiled,
            def::ParametersCompiled,
            expr::{ExprBinOp, ExprCompiled, ExprLogicalBinOp, ExprUnOp},
            opt_ctx::OptCtx,
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
    let (x, t) = stmt.first()?.as_return()?.as_type_is()?;
    match &x.node {
        // Slot 0 is a slot for the first function parameter.
        ExprCompiled::Local(LocalSlotId(0)) => Some(t),
        _ => None,
    }
}

struct IsSafeToInlineExpr {
    /// Function parameter count.
    param_count: u32,
    /// How many expressions we visited already.
    counter: u32,
}

impl IsSafeToInlineExpr {
    fn new(param_count: u32) -> IsSafeToInlineExpr {
        Self {
            param_count,
            counter: 0,
        }
    }

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
            ExprCompiled::LocalCaptured(..) | ExprCompiled::Module(..) | ExprCompiled::Def(..) => {
                false
            }
            ExprCompiled::Local(l) => {
                // `l >= param_count` should be unreachable, but it is safer this way.
                l.0 < self.param_count
            }
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
            ExprCompiled::LogicalBinOp(op, box (x, y)) => {
                let _: &ExprLogicalBinOp = op;
                self.is_safe_to_inline_expr(x) && self.is_safe_to_inline_expr(y)
            }
            ExprCompiled::Seq(box (x, y)) => {
                self.is_safe_to_inline_expr(x) && self.is_safe_to_inline_expr(y)
            }
        }
    }
}

/// Function body is a `return` safe to inline expression (as defined above).
fn is_return_safe_to_inline_expr(
    stmts: &StmtsCompiled,
    param_count: u32,
) -> Option<IrSpanned<ExprCompiled>> {
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
                if IsSafeToInlineExpr::new(param_count).is_safe_to_inline_expr(expr) =>
            {
                Some(expr.clone())
            }
            _ => None,
        },
    }
}

pub(crate) fn inline_def_body(
    params: &ParametersCompiled<IrSpanned<ExprCompiled>>,
    body: &StmtsCompiled,
) -> Option<InlineDefBody> {
    if params.params.len() == 1 && params.params[0].accepts_positional() {
        if let Some(t) = is_return_type_is(body) {
            return Some(InlineDefBody::ReturnTypeIs(t));
        }
    }
    if !params.has_args_or_kwargs() {
        // It is possible to sometimes inline functions with `*args` or `**kwargs`,
        // but let's postpone that for now.
        let param_count = params.count_param_variables();
        if let Some(expr) = is_return_safe_to_inline_expr(body, param_count) {
            return Some(InlineDefBody::ReturnSafeToInlineExpr(expr));
        }
    }
    None
}

pub(crate) struct CannotInline;

/// Utility to inline function body at call site.
pub(crate) struct InlineDefCallSite<'s, 'v, 'a, 'e> {
    pub(crate) ctx: &'s mut OptCtx<'v, 'a, 'e>,
    pub(crate) slots: &'s [FrozenValue],
}

impl<'s, 'v, 'a, 'e> InlineDefCallSite<'s, 'v, 'a, 'e> {
    fn inline_opt(
        &mut self,
        expr: Option<&IrSpanned<ExprCompiled>>,
    ) -> Result<Option<IrSpanned<ExprCompiled>>, CannotInline> {
        match expr {
            None => Ok(None),
            Some(expr) => Ok(Some(self.inline(expr)?)),
        }
    }

    fn inline_args(&mut self, args: &ArgsCompiledValue) -> Result<ArgsCompiledValue, CannotInline> {
        args.map_exprs(|expr| self.inline(expr))
    }

    fn inline_call(
        &mut self,
        call: &IrSpanned<CallCompiled>,
    ) -> Result<IrSpanned<ExprCompiled>, CannotInline> {
        let span = call.span;
        let CallCompiled { fun, args } = &call.node;
        let fun = self.inline(fun)?;
        let args = self.inline_args(args)?;
        Ok(IrSpanned {
            span,
            node: CallCompiled::call(span, fun, args, self.ctx),
        })
    }

    pub(crate) fn inline(
        &mut self,
        expr: &IrSpanned<ExprCompiled>,
    ) -> Result<IrSpanned<ExprCompiled>, CannotInline> {
        let span = expr.span;
        Ok(match &expr.node {
            e @ ExprCompiled::Value(..) => IrSpanned {
                span,
                node: e.clone(),
            },
            ExprCompiled::Local(local) => {
                let value = self.slots[local.0 as usize];
                IrSpanned {
                    span,
                    node: ExprCompiled::Value(value),
                }
            }
            ExprCompiled::If(box (c, t, f)) => {
                let c = self.inline(c)?;
                let t = self.inline(t)?;
                let f = self.inline(f)?;
                ExprCompiled::if_expr(c, t, f)
            }
            ExprCompiled::LogicalBinOp(op, box (l, r)) => {
                let l = self.inline(l)?;
                let r = self.inline(r)?;
                ExprCompiled::logical_bin_op(*op, l, r)
            }
            ExprCompiled::List(xs) => {
                let xs = xs
                    .iter()
                    .map(|x| self.inline(x))
                    .collect::<Result<Vec<_>, CannotInline>>()?;
                IrSpanned {
                    span,
                    node: ExprCompiled::List(xs),
                }
            }
            ExprCompiled::Tuple(xs) => {
                let xs = xs
                    .iter()
                    .map(|x| self.inline(x))
                    .collect::<Result<Vec<_>, CannotInline>>()?;
                IrSpanned {
                    span,
                    node: ExprCompiled::tuple(xs, self.ctx.frozen_heap()),
                }
            }
            ExprCompiled::Dict(xs) => {
                let xs = xs
                    .iter()
                    .map(|(x, y)| Ok((self.inline(x)?, self.inline(y)?)))
                    .collect::<Result<Vec<_>, CannotInline>>()?;
                IrSpanned {
                    span,
                    node: ExprCompiled::Dict(xs),
                }
            }
            ExprCompiled::Op(op, box (l, r)) => {
                let l = self.inline(l)?;
                let r = self.inline(r)?;
                IrSpanned {
                    span,
                    node: ExprCompiled::bin_op(*op, l, r, self.ctx),
                }
            }
            ExprCompiled::UnOp(op, box x) => {
                let x = self.inline(x)?;
                IrSpanned {
                    span,
                    node: ExprCompiled::un_op(span, op, x, self.ctx),
                }
            }
            ExprCompiled::ArrayIndirection(box (array, index)) => {
                let array = self.inline(array)?;
                let index = self.inline(index)?;
                IrSpanned {
                    span,
                    node: ExprCompiled::array_indirection(array, index, self.ctx),
                }
            }
            ExprCompiled::Slice(box (l, a, b, c)) => {
                let l = self.inline(l)?;
                let a = self.inline_opt(a.as_ref())?;
                let b = self.inline_opt(b.as_ref())?;
                let c = self.inline_opt(c.as_ref())?;
                IrSpanned {
                    span,
                    node: ExprCompiled::Slice(box (l, a, b, c)),
                }
            }
            ExprCompiled::Seq(box (a, b)) => {
                let a = self.inline(a)?;
                let b = self.inline(b)?;
                ExprCompiled::seq(a, b)
            }
            ExprCompiled::Call(call) => return self.inline_call(call),
            // These should be unreachable, but it is safer
            // to do unnecessary work in compiler than crash.
            ExprCompiled::LocalCaptured(..)
            | ExprCompiled::Module(..)
            | ExprCompiled::Compr(..)
            | ExprCompiled::Def(..) => return Err(CannotInline),
        })
    }
}

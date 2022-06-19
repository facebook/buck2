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

//! Compile function calls.

use std::cell::Cell;

use gazebo::prelude::*;

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        compiler::{
            args::ArgsCompiledValue,
            def_inline::{InlineDefBody, InlineDefCallSite},
            expr::ExprCompiled,
            scope::{CstArgument, CstExpr},
            span::IrSpanned,
            stmt::OptimizeOnFreezeContext,
            Compiler,
        },
        runtime::{
            call_stack::FrozenFileSpan, inlined_frame::InlinedFrameAlloc, visit_span::VisitSpanMut,
        },
    },
    syntax::ast::{AstString, ExprP},
    values::{string::interpolation::parse_format_one, FrozenHeap, FrozenValue, Heap},
};

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) struct CallCompiled {
    pub(crate) fun: IrSpanned<ExprCompiled>,
    pub(crate) args: ArgsCompiledValue,
}

impl CallCompiled {
    pub(crate) fn new_method(
        this: IrSpanned<ExprCompiled>,
        field: Symbol,
        getattr_span: FrozenFileSpan,
        args: ArgsCompiledValue,
    ) -> CallCompiled {
        CallCompiled {
            fun: IrSpanned {
                span: getattr_span,
                node: ExprCompiled::Dot(box this, field),
            },
            args,
        }
    }

    /// If this call expression is `len(x)`, return `x`.
    pub(crate) fn as_len(&self) -> Option<&IrSpanned<ExprCompiled>> {
        if !self.fun.is_fn_len() {
            return None;
        }
        self.args.one_pos()
    }

    /// If this call expression is `type(x)`, return `x`.
    pub(crate) fn as_type(&self) -> Option<&IrSpanned<ExprCompiled>> {
        if !self.fun.is_fn_type() {
            return None;
        }
        self.args.one_pos()
    }

    /// This call is infallible and has no side effects.
    pub(crate) fn is_pure_infallible(&self) -> bool {
        match self.as_type() {
            Some(arg) => arg.is_pure_infallible(),
            None => false,
        }
    }

    /// This call is a method call.
    pub(crate) fn method(&self) -> Option<(&IrSpanned<ExprCompiled>, &Symbol, &ArgsCompiledValue)> {
        match &self.fun.node {
            ExprCompiled::Dot(expr, name) => Some((expr, name, &self.args)),
            _ => None,
        }
    }

    /// Try to inline a function like `lambda x: type(x) == "y"`.
    fn try_type_is(fun: &ExprCompiled, args: &ArgsCompiledValue) -> Option<ExprCompiled> {
        let fun = fun.as_frozen_def()?;
        let pos = args.one_pos()?;
        if let Some(InlineDefBody::ReturnTypeIs(t)) = &fun.def_info.inline_def_body {
            Some(ExprCompiled::type_is(pos.clone(), *t))
        } else {
            None
        }
    }

    /// Inline calls to functions which are safe to inline.
    fn try_inline(
        span: FrozenFileSpan,
        fun: &ExprCompiled,
        args: &ArgsCompiledValue,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> Option<IrSpanned<ExprCompiled>> {
        let fun = fun.as_frozen_def()?;

        if fun.parameters.has_args_or_kwargs() {
            // Functions with `*args` or `**kwargs` are not marked safe to inline,
            // but it is safer to also handle it explicitly here.
            return None;
        }

        let expr = if let Some(InlineDefBody::ReturnSafeToInlineExpr(expr)) =
            &fun.def_info.inline_def_body
        {
            expr
        } else {
            return None;
        };

        args.all_values(|arguments| {
            let slots = vec![Cell::new(None); fun.parameters.len()];
            fun.parameters.collect(arguments, &slots, heap).ok()?;

            let slots = slots
                .into_try_map(|value| {
                    // Value must be set, but better ignore optimization here than panic.
                    let value = value.get().ok_or(())?;
                    // Everything should be frozen here, but if not,
                    // it is safer to abandon optimization.
                    value.unpack_frozen().ok_or(())
                })
                .ok()?;

            let mut expr = IrSpanned {
                span,
                node: expr.node.clone(),
            };
            let mut span_alloc = InlinedFrameAlloc::new(frozen_heap);
            expr.visit_spans(&mut |expr_span: &mut FrozenFileSpan| {
                expr_span
                    .inlined_frames
                    .inline_into(span, fun.to_frozen_value(), &mut span_alloc);
            });
            InlineDefCallSite {
                heap,
                frozen_heap,
                slots: &slots,
            }
            .inline(&expr)
            .ok()
        })?
    }

    pub(crate) fn call(
        span: FrozenFileSpan,
        fun: ExprCompiled,
        args: ArgsCompiledValue,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(type_is) = CallCompiled::try_type_is(&fun, &args) {
            return type_is;
        }

        if let Some(inline) = CallCompiled::try_inline(span, &fun, &args, heap, frozen_heap) {
            return inline.node;
        }

        if fun.is_fn_len() {
            if let Some(arg) = args.one_pos() {
                return ExprCompiled::len(span, arg.clone());
            }
        }

        if fun.is_fn_type() {
            if let Some(arg) = args.one_pos() {
                return ExprCompiled::typ(span, arg.clone());
            }
        }

        ExprCompiled::Call(box IrSpanned {
            span,
            node: CallCompiled {
                fun: IrSpanned { span, node: fun },
                args,
            },
        })
    }
}

impl IrSpanned<CallCompiled> {
    pub(crate) fn optimize_on_freeze(&self, ctx: &mut OptimizeOnFreezeContext) -> ExprCompiled {
        let CallCompiled { fun: expr, args } = &self.node;
        let expr = expr.optimize_on_freeze(ctx);
        let args = args.optimize_on_freeze(ctx);
        CallCompiled::call(self.span, expr.node, args, ctx.heap, ctx.frozen_heap)
    }
}

impl Compiler<'_, '_, '_> {
    fn try_spec_exec(
        &mut self,
        span: FrozenFileSpan,
        fun: FrozenValue,
        args: &ArgsCompiledValue,
    ) -> Option<ExprCompiled> {
        // Only if all call arguments are frozen values.
        args.all_values(|arguments| {
            let v = fun.to_value().invoke(arguments, self.eval).ok()?;
            ExprCompiled::try_value(span, v, self.eval.module_env.frozen_heap())
        })?
    }

    fn expr_call_fun_frozen_no_special(
        &mut self,
        span: FrozenFileSpan,
        fun: FrozenValue,
        args: ArgsCompiledValue,
    ) -> ExprCompiled {
        if fun.speculative_exec_safe() {
            if let Some(expr) = self.try_spec_exec(span, fun, &args) {
                return expr;
            }
        }

        CallCompiled::call(
            span,
            ExprCompiled::Value(fun),
            args,
            self.eval.heap(),
            self.eval.frozen_heap(),
        )
    }

    fn expr_call_fun_compiled(
        &mut self,
        span: FrozenFileSpan,
        left: IrSpanned<ExprCompiled>,
        args: Vec<CstArgument>,
    ) -> ExprCompiled {
        let args = self.args(args);
        if let Some(left) = left.as_value() {
            self.expr_call_fun_frozen_no_special(span, left, args)
        } else {
            ExprCompiled::Call(box IrSpanned {
                span,
                node: CallCompiled { fun: left, args },
            })
        }
    }

    fn expr_call_method(
        &mut self,
        span: FrozenFileSpan,
        e: CstExpr,
        s: AstString,
        args: Vec<CstArgument>,
    ) -> ExprCompiled {
        let e = self.expr(e);
        let args = self.args(args);

        // Optimize `"aaa{}bbb".format(arg)`.
        if let (Some(e), Some(_arg)) = (e.as_string(), args.one_pos()) {
            if &s.node == "format" {
                if let Some((before, after)) = parse_format_one(&e) {
                    let before = self.eval.module_env.frozen_heap().alloc_str(&before);
                    let after = self.eval.module_env.frozen_heap().alloc_str(&after);
                    let arg = args.into_one_pos().unwrap();
                    return ExprCompiled::format_one(
                        before,
                        arg,
                        after,
                        self.eval.module_env.heap(),
                        self.eval.module_env.frozen_heap(),
                    );
                }
            }
        }

        let getattr_span = e.span.merge(&FrozenFileSpan::new(self.codemap, s.span));

        let s = Symbol::new(&s.node);
        if let Some(e) = e.as_value() {
            if let Some(v) = ExprCompiled::compile_time_getattr(
                e,
                &s,
                self.eval.module_env.heap(),
                self.eval.module_env.frozen_heap(),
            ) {
                return self.expr_call_fun_frozen_no_special(span, v, args);
            }
        }

        ExprCompiled::Call(box IrSpanned {
            span,
            node: CallCompiled::new_method(e, s, getattr_span, args),
        })
    }

    pub(crate) fn expr_call(
        &mut self,
        span: FrozenFileSpan,
        left: CstExpr,
        args: Vec<CstArgument>,
    ) -> ExprCompiled {
        match left.node {
            ExprP::Dot(box e, s) => self.expr_call_method(span, e, s, args),
            _ => {
                let expr = self.expr(left);
                self.expr_call_fun_compiled(span, expr, args)
            }
        }
    }
}

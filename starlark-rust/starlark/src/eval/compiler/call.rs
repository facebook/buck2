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

use gazebo::{coerce::coerce, prelude::*};

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        compiler::{
            def::InlineDefBody,
            expr::ExprCompiled,
            scope::{CstArgument, CstExpr},
            span::IrSpanned,
            stmt::OptimizeOnFreezeContext,
            Compiler,
        },
        runtime::{
            arguments::{ArgNames, ArgumentsFull},
            call_stack::FrozenFileSpan,
        },
        Arguments,
    },
    gazebo::prelude::SliceExt,
    syntax::ast::{ArgumentP, AstString, ExprP},
    values::{string::interpolation::parse_format_one, FrozenStringValue, FrozenValue},
};

#[derive(Default, Clone, Debug)]
pub(crate) struct ArgsCompiledValue {
    pub(crate) pos_named: Vec<IrSpanned<ExprCompiled>>,
    /// Named arguments compiled.
    ///
    /// Note names are guaranteed to be unique here because names are validated in AST:
    /// named arguments in [`Expr::Call`] are unique.
    pub(crate) names: Vec<(Symbol, FrozenStringValue)>,
    pub(crate) args: Option<IrSpanned<ExprCompiled>>,
    pub(crate) kwargs: Option<IrSpanned<ExprCompiled>>,
}

#[derive(Clone, Debug)]
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

    /// This call is a method call.
    pub(crate) fn method(&self) -> Option<(&IrSpanned<ExprCompiled>, &Symbol, &ArgsCompiledValue)> {
        match &self.fun.node {
            ExprCompiled::Dot(expr, name) => Some((expr, name, &self.args)),
            _ => None,
        }
    }

    pub(crate) fn call(
        span: FrozenFileSpan,
        fun: ExprCompiled,
        args: ArgsCompiledValue,
    ) -> ExprCompiled {
        if let (Some(fun), Some(_pos)) = (fun.as_frozen_def(), args.one_pos()) {
            // Try to inline a function like `lambda x: type(x) == "y"`.
            if let Some(InlineDefBody::ReturnTypeIs(t)) = &fun.def_info.inline_def_body {
                let pos = args.into_one_pos().unwrap();
                return ExprCompiled::type_is(pos, *t);
            }
        }

        if let (Some(fun), true) = (fun.as_frozen_def(), args.is_no_args()) {
            if let Some(InlineDefBody::ReturnSafeToInlineExpr(expr)) = &fun.def_info.inline_def_body
            {
                return expr.node.clone();
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
    pub(crate) fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> ExprCompiled {
        let CallCompiled { fun: expr, args } = &self.node;
        let expr = expr.optimize_on_freeze(ctx);
        let args = args.optimize_on_freeze(ctx);
        CallCompiled::call(self.span, expr.node, args)
    }
}

impl ArgsCompiledValue {
    /// Check if arguments is one positional argument.
    pub(crate) fn one_pos(&self) -> Option<&IrSpanned<ExprCompiled>> {
        let ArgsCompiledValue {
            pos_named,
            names,
            args,
            kwargs,
        } = self;
        match (pos_named.as_slice(), names.as_slice(), args, kwargs) {
            ([pos], [], None, None) => Some(pos),
            _ => None,
        }
    }

    pub(crate) fn into_one_pos(mut self) -> Option<IrSpanned<ExprCompiled>> {
        self.one_pos()?;
        self.pos_named.pop()
    }

    /// Check if arguments is empty (no positional, no named, no star-args, no kwargs).
    pub(crate) fn is_no_args(&self) -> bool {
        let ArgsCompiledValue {
            pos_named,
            names,
            args,
            kwargs,
        } = self;
        matches!(
            (pos_named.as_slice(), names.as_slice(), args, kwargs),
            ([], [], None, None)
        )
    }

    pub(crate) fn pos_only(&self) -> Option<&[IrSpanned<ExprCompiled>]> {
        if self.names.is_empty() && self.args.is_none() && self.kwargs.is_none() {
            Some(&self.pos_named)
        } else {
            None
        }
    }

    fn split_pos_names(&self) -> (&[IrSpanned<ExprCompiled>], &[IrSpanned<ExprCompiled>]) {
        self.pos_named
            .as_slice()
            .split_at(self.pos_named.len() - self.names.len())
    }

    /// Invoke a callback if all arguments are frozen values.
    fn all_values<'v, R>(&self, handler: impl FnOnce(&Arguments<'v, '_>) -> R) -> Option<R> {
        let (pos, named) = self.split_pos_names();
        let pos = pos
            .try_map(|e| e.as_value().map(FrozenValue::to_value).ok_or(()))
            .ok()?;
        let named = named
            .try_map(|e| e.as_value().map(FrozenValue::to_value).ok_or(()))
            .ok()?;
        let args = self
            .args
            .as_ref()
            .try_map(|args| args.as_value().map(FrozenValue::to_value).ok_or(()))
            .ok()?;
        let kwargs = self
            .kwargs
            .as_ref()
            .try_map(|kwargs| kwargs.as_value().map(FrozenValue::to_value).ok_or(()))
            .ok()?;
        Some(handler(&Arguments(ArgumentsFull {
            pos: &pos,
            named: &named,
            names: ArgNames::new(coerce(&self.names)),
            args,
            kwargs,
        })))
    }

    fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> ArgsCompiledValue {
        let ArgsCompiledValue {
            ref pos_named,
            ref names,
            ref args,
            ref kwargs,
        } = *self;
        ArgsCompiledValue {
            pos_named: pos_named.map(|p| p.optimize_on_freeze(ctx)),
            names: names.clone(),
            args: args.as_ref().map(|a| a.optimize_on_freeze(ctx)),
            kwargs: kwargs.as_ref().map(|a| a.optimize_on_freeze(ctx)),
        }
    }
}

impl Compiler<'_, '_, '_> {
    fn args(&mut self, args: Vec<CstArgument>) -> ArgsCompiledValue {
        let mut res = ArgsCompiledValue::default();
        for x in args {
            match x.node {
                ArgumentP::Positional(x) => res.pos_named.push(self.expr(x)),
                ArgumentP::Named(name, value) => {
                    let fv = self
                        .eval
                        .module_env
                        .frozen_heap()
                        .alloc_str(name.node.as_str());
                    res.names.push((Symbol::new(&name.node), fv));
                    res.pos_named.push(self.expr(value));
                }
                ArgumentP::Args(x) => res.args = Some(self.expr(x)),
                ArgumentP::KwArgs(x) => res.kwargs = Some(self.expr(x)),
            }
        }
        res
    }

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

        CallCompiled::call(span, ExprCompiled::Value(fun), args)
    }

    fn expr_call_fun_frozen(
        &mut self,
        span: FrozenFileSpan,
        left: FrozenValue,
        mut args: Vec<CstArgument>,
    ) -> ExprCompiled {
        let one_positional = args.len() == 1 && args[0].is_positional();
        if left == self.constants.fn_type && one_positional {
            let expr = args.pop().unwrap().node.into_expr();
            let expr = self.expr(expr);
            ExprCompiled::typ(expr)
        } else if left == self.constants.fn_len && one_positional {
            let x = self.expr(args.pop().unwrap().node.into_expr());
            ExprCompiled::len(x)
        } else {
            let args = self.args(args);
            self.expr_call_fun_frozen_no_special(span, left, args)
        }
    }

    fn expr_call_fun_compiled(
        &mut self,
        span: FrozenFileSpan,
        left: IrSpanned<ExprCompiled>,
        args: Vec<CstArgument>,
    ) -> ExprCompiled {
        if let Some(left) = left.as_value() {
            self.expr_call_fun_frozen(span, left, args)
        } else {
            let args = self.args(args);
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

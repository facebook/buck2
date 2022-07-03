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

use gazebo::coerce::coerce;
use gazebo::prelude::*;

use crate::collections::symbol_map::Symbol;
use crate::eval::compiler::expr::ExprCompiled;
use crate::eval::compiler::opt_ctx::OptCtx;
use crate::eval::compiler::scope::CstArgument;
use crate::eval::compiler::span::IrSpanned;
use crate::eval::compiler::Compiler;
use crate::eval::runtime::arguments::ArgNames;
use crate::eval::runtime::arguments::ArgumentsFull;
use crate::eval::Arguments;
use crate::syntax::ast::ArgumentP;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Value;

#[derive(Default, Clone, Debug, VisitSpanMut)]
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
    pub(crate) fn all_values<'v, R>(
        &self,
        handler: impl FnOnce(&Arguments<'v, '_>) -> R,
    ) -> Option<R> {
        self.all_values_generic(|e| e.as_value().map(FrozenValue::to_value), handler)
    }

    /// Invoke a callback if all arguments are frozen values.
    pub(crate) fn all_values_generic<'v, R>(
        &self,
        expr_to_value: impl Fn(&ExprCompiled) -> Option<Value<'v>>,
        handler: impl FnOnce(&Arguments<'v, '_>) -> R,
    ) -> Option<R> {
        let (pos, named) = self.split_pos_names();
        let pos = pos.try_map(|e| expr_to_value(e).ok_or(())).ok()?;
        let named = named.try_map(|e| expr_to_value(e).ok_or(())).ok()?;
        let args = self
            .args
            .as_ref()
            .try_map(|args| expr_to_value(args).ok_or(()))
            .ok()?;
        let kwargs = self
            .kwargs
            .as_ref()
            .try_map(|kwargs| expr_to_value(kwargs).ok_or(()))
            .ok()?;
        Some(handler(&Arguments(ArgumentsFull {
            pos: &pos,
            named: &named,
            names: ArgNames::new(coerce(&self.names)),
            args,
            kwargs,
        })))
    }

    /// Expressions of all arguments: positional, named, star-args, star-star-args.
    pub(crate) fn arg_exprs(&self) -> impl Iterator<Item = &IrSpanned<ExprCompiled>> {
        self.pos_named
            .iter()
            .chain(self.args.iter())
            .chain(self.kwargs.iter())
    }

    pub(crate) fn map_exprs<E>(
        &self,
        mut f: impl FnMut(&IrSpanned<ExprCompiled>) -> Result<IrSpanned<ExprCompiled>, E>,
    ) -> Result<ArgsCompiledValue, E> {
        let ArgsCompiledValue {
            pos_named,
            names,
            args,
            kwargs,
        } = self;
        Ok(ArgsCompiledValue {
            pos_named: pos_named.try_map(&mut f)?,
            names: names.clone(),
            args: args.as_ref().map(&mut f).transpose()?,
            kwargs: kwargs.as_ref().map(&mut f).transpose()?,
        })
    }

    pub(crate) fn optimize(&self, ctx: &mut OptCtx) -> ArgsCompiledValue {
        enum Never {}
        self.map_exprs(|e| Ok(e.optimize(ctx)))
            .unwrap_or_else(|e: Never| match e {})
    }
}

impl Compiler<'_, '_, '_> {
    pub(crate) fn args(&mut self, args: Vec<CstArgument>) -> ArgsCompiledValue {
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
}

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

//! Evaluation of an expression.

use std::cmp::Ordering;

use gazebo::prelude::*;
use thiserror::Error;

use crate::{
    codemap::Spanned,
    collections::symbol_map::Symbol,
    environment::slots::ModuleSlotId,
    errors::did_you_mean::did_you_mean,
    eval::{
        compiler::{
            args::ArgsCompiledValue,
            call::CallCompiled,
            compr::ComprCompiled,
            constants::Constants,
            def::{DefCompiled, FrozenDef},
            expr_bool::ExprCompiledBool,
            known::list_to_tuple,
            scope::{AssignCount, Captured, CstExpr, ResolvedIdent, Slot},
            span::IrSpanned,
            stmt::OptimizeOnFreezeContext,
            Compiler,
        },
        runtime::{call_stack::FrozenFileSpan, slots::LocalSlotId},
    },
    syntax::{
        ast::{AstExprP, AstLiteral, AstPayload, AstString, BinOp, ExprP, StmtP},
        lexer::TokenInt,
    },
    values::{
        function::BoundMethodGen,
        layout::value_not_special::FrozenValueNotSpecial,
        string::interpolation::parse_percent_s_one,
        types::{
            bigint::StarlarkBigInt,
            bool::StarlarkBool,
            dict::Dict,
            float::StarlarkFloat,
            list::{FrozenList, List},
            range::Range,
            string::interpolation::{format_one, percent_s_one},
            tuple::Tuple,
            unbound::MaybeUnboundValue,
        },
        FrozenHeap, FrozenStringValue, FrozenValue, FrozenValueTyped, Heap, StarlarkValue, Value,
        ValueError, ValueLike,
    },
};

/// `bool` operation.
#[derive(Copy, Clone, Dupe, Eq, PartialEq, Debug)]
pub(crate) enum MaybeNot {
    Id,
    Not,
}

impl MaybeNot {
    pub(crate) fn negate(self) -> MaybeNot {
        match self {
            MaybeNot::Id => MaybeNot::Not,
            MaybeNot::Not => MaybeNot::Id,
        }
    }
}

/// Map result of comparison to boolean.
#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) enum CompareOp {
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
}

impl CompareOp {
    fn as_fn(self) -> fn(Ordering) -> bool {
        match self {
            CompareOp::Less => |x| x == Ordering::Less,
            CompareOp::Greater => |x| x == Ordering::Greater,
            CompareOp::LessOrEqual => |x| x != Ordering::Greater,
            CompareOp::GreaterOrEqual => |x| x != Ordering::Less,
        }
    }
}

#[derive(Copy, Clone, Dupe, Debug, VisitSpanMut)]
pub(crate) enum ExprUnOp {
    Minus,
    Plus,
    BitNot,
}

impl ExprUnOp {
    fn eval<'v>(self, v: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            ExprUnOp::Minus => v.minus(heap),
            ExprUnOp::Plus => v.plus(heap),
            ExprUnOp::BitNot => Ok(Value::new_int(!v.to_int()?)),
        }
    }
}

#[derive(Copy, Clone, Dupe, Debug, VisitSpanMut)]
pub(crate) enum ExprBinOp {
    In,
    Sub,
    Add,
    Multiply,
    Percent,
    Divide,
    FloorDivide,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

impl ExprBinOp {
    fn eval<'v>(self, a: Value<'v>, b: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            ExprBinOp::In => b.is_in(a).map(Value::new_bool),
            ExprBinOp::Sub => a.sub(b, heap),
            ExprBinOp::Add => a.add(b, heap),
            ExprBinOp::Multiply => a.mul(b, heap),
            ExprBinOp::Percent => a.percent(b, heap),
            ExprBinOp::Divide => a.div(b, heap),
            ExprBinOp::FloorDivide => a.floor_div(b, heap),
            ExprBinOp::BitAnd => a.bit_and(b, heap),
            ExprBinOp::BitOr => a.bit_or(b, heap),
            ExprBinOp::BitXor => a.bit_xor(b, heap),
            ExprBinOp::LeftShift => a.left_shift(b, heap),
            ExprBinOp::RightShift => a.right_shift(b, heap),
        }
    }
}

/// Logical binary operator.
#[derive(Copy, Clone, Dupe, Debug, VisitSpanMut, Eq, PartialEq)]
pub(crate) enum ExprLogicalBinOp {
    And,
    Or,
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) enum ExprCompiled {
    Value(FrozenValue),
    /// Read local non-captured variable.
    Local(LocalSlotId),
    /// Read local captured variable.
    LocalCaptured(LocalSlotId),
    Module(ModuleSlotId),
    /// `x == y`
    Equals(Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>),
    /// `cmp(x <=> y)`
    Compare(
        Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>,
        CompareOp,
    ),
    /// `type(x) == "y"`
    TypeIs(Box<IrSpanned<ExprCompiled>>, FrozenStringValue),
    Tuple(Vec<IrSpanned<ExprCompiled>>),
    List(Vec<IrSpanned<ExprCompiled>>),
    Dict(Vec<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>),
    /// Comprehension.
    Compr(ComprCompiled),
    Dot(Box<IrSpanned<ExprCompiled>>, Symbol),
    ArrayIndirection(Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>),
    If(
        Box<(
            // Condition.
            IrSpanned<ExprCompiled>,
            // Then branch.
            IrSpanned<ExprCompiled>,
            // Else branch.
            IrSpanned<ExprCompiled>,
        )>,
    ),
    Slice(
        Box<(
            IrSpanned<ExprCompiled>,
            Option<IrSpanned<ExprCompiled>>,
            Option<IrSpanned<ExprCompiled>>,
            Option<IrSpanned<ExprCompiled>>,
        )>,
    ),
    Not(Box<IrSpanned<ExprCompiled>>),
    UnOp(ExprUnOp, Box<IrSpanned<ExprCompiled>>),
    LogicalBinOp(
        ExprLogicalBinOp,
        Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>,
    ),
    /// Expression equivalent to `(x, y)[1]`: evaluate `x`, discard the result,
    /// then evaluate `y` and use its result.
    Seq(Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>),
    Op(
        ExprBinOp,
        Box<(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)>,
    ),
    /// `"aaa%sbbb" % arg`
    PercentSOne(
        Box<(
            FrozenStringValue,
            IrSpanned<ExprCompiled>,
            FrozenStringValue,
        )>,
    ),
    /// `"aaa%sbbb".format(arg)`
    FormatOne(
        Box<(
            FrozenStringValue,
            IrSpanned<ExprCompiled>,
            FrozenStringValue,
        )>,
    ),
    Call(Box<IrSpanned<CallCompiled>>),
    Def(DefCompiled),
}

impl ExprCompiled {
    pub fn as_value(&self) -> Option<FrozenValue> {
        match self {
            Self::Value(x) => Some(*x),
            _ => None,
        }
    }

    /// Expression is known to be a constant which is a `def`.
    pub(crate) fn as_frozen_def(&self) -> Option<FrozenValueTyped<FrozenDef>> {
        FrozenValueTyped::new(self.as_value()?)
    }

    /// Expression is builtin `len` function.
    pub(crate) fn is_fn_len(&self) -> bool {
        match self.as_value() {
            Some(value) => value == Constants::get().fn_len,
            None => false,
        }
    }

    /// Expression is builtin `type` function.
    pub(crate) fn is_fn_type(&self) -> bool {
        match self.as_value() {
            Some(value) => value == Constants::get().fn_type,
            None => false,
        }
    }

    /// If expression is `type(x)`, return `x`.
    pub(crate) fn as_type(&self) -> Option<&IrSpanned<ExprCompiled>> {
        match self {
            Self::Call(c) => c.as_type(),
            _ => None,
        }
    }

    /// Expression is a frozen value which is builtin.
    pub(crate) fn as_builtin_value(&self) -> Option<FrozenValue> {
        match self {
            Self::Value(x) if x.is_builtin() => Some(*x),
            _ => None,
        }
    }

    /// Is expression a constant string?
    pub(crate) fn as_string(&self) -> Option<FrozenStringValue> {
        FrozenStringValue::new(self.as_value()?)
    }

    /// Try to extract `[c0, c1, ..., cn]` from this expression.
    pub(crate) fn as_list_of_consts(&self) -> Option<Vec<FrozenValue>> {
        match self {
            ExprCompiled::List(xs) => xs.try_map(|x| x.as_value().ok_or(())).ok(),
            ExprCompiled::Value(v) => Some(FrozenList::from_frozen_value(v)?.content().to_owned()),
            _ => None,
        }
    }

    /// Iterable produced by this expression results in empty.
    pub(crate) fn is_iterable_empty(&self) -> bool {
        match self {
            ExprCompiled::List(xs) => xs.is_empty(),
            ExprCompiled::Tuple(xs) => xs.is_empty(),
            ExprCompiled::Dict(xs) => xs.is_empty(),
            ExprCompiled::Value(v) if v.is_builtin() => {
                v.to_value().length().map_or(false, |l| l == 0)
            }
            _ => false,
        }
    }

    /// Result of this expression is definitely `bool`
    /// (if `false` it may also be `bool`).
    fn is_definitely_bool(&self) -> bool {
        match self {
            Self::Value(v) => v.unpack_bool().is_some(),
            Self::Equals(..)
            | Self::TypeIs(..)
            | Self::Not(..)
            | Self::Compare(..)
            | Self::Op(ExprBinOp::In, ..) => true,
            _ => false,
        }
    }

    /// This expression is definitely:
    /// * infallible
    /// * has no effects
    pub(crate) fn is_pure_infallible(&self) -> bool {
        match self {
            Self::Value(..) => true,
            Self::List(xs) | Self::Tuple(xs) => xs.iter().all(|x| x.is_pure_infallible()),
            Self::Dict(xs) => xs.is_empty(),
            Self::TypeIs(x, _t) => x.is_pure_infallible(),
            Self::Not(x) => x.is_pure_infallible(),
            Self::Seq(box (x, y)) => x.is_pure_infallible() && y.is_pure_infallible(),
            Self::LogicalBinOp(_op, box (x, y)) => x.is_pure_infallible() && y.is_pure_infallible(),
            Self::If(box (cond, x, y)) => {
                cond.is_pure_infallible() && x.is_pure_infallible() && y.is_pure_infallible()
            }
            Self::Call(call) => call.is_pure_infallible(),
            _ => false,
        }
    }

    /// If this expression is pure, infallible, and known to produce a value,
    /// return truth of that value.
    pub(crate) fn is_pure_infallible_to_bool(&self) -> Option<bool> {
        match self {
            ExprCompiled::Value(v) => Some(v.to_value().to_bool()),
            ExprCompiled::List(xs) | ExprCompiled::Tuple(xs)
                if xs.iter().all(|x| x.is_pure_infallible()) =>
            {
                Some(!xs.is_empty())
            }
            // TODO(nga): if keys are unique hashable constants, we can fold this to constant too.
            ExprCompiled::Dict(xs) if xs.is_empty() => Some(false),
            ExprCompiled::Not(x) => x.is_pure_infallible_to_bool().map(|x| !x),
            ExprCompiled::LogicalBinOp(op, box (x, y)) => {
                match (
                    op,
                    x.is_pure_infallible_to_bool(),
                    y.is_pure_infallible_to_bool(),
                ) {
                    (ExprLogicalBinOp::And, Some(true), y) => y,
                    (ExprLogicalBinOp::Or, Some(false), y) => y,
                    (ExprLogicalBinOp::And, Some(false), _) => Some(false),
                    (ExprLogicalBinOp::Or, Some(true), _) => Some(true),
                    (_, None, _) => None,
                }
            }
            _ => None,
        }
    }

    /// This expression is local slot.
    pub(crate) fn as_local_non_captured(&self) -> Option<LocalSlotId> {
        match self {
            ExprCompiled::Local(slot) => Some(*slot),
            _ => None,
        }
    }
}

impl IrSpanned<ExprCompiled> {
    pub(crate) fn optimize_on_freeze(
        &self,
        ctx: &OptimizeOnFreezeContext,
    ) -> IrSpanned<ExprCompiled> {
        let span = self.span;
        let expr = match self.node {
            ref e @ (ExprCompiled::Value(..)
            | ExprCompiled::Local(..)
            | ExprCompiled::LocalCaptured(..)) => e.clone(),
            ExprCompiled::Module(slot) => {
                match ctx.module.get_module_data().get_slot(slot) {
                    None => {
                        // Let if fail at runtime.
                        ExprCompiled::Module(slot)
                    }
                    Some(v) => ExprCompiled::Value(v),
                }
            }
            ExprCompiled::Equals(box (ref l, ref r)) => {
                let l = l.optimize_on_freeze(ctx);
                let r = r.optimize_on_freeze(ctx);
                eval_equals(l, r)
            }
            ExprCompiled::Compare(box (ref l, ref r), cmp) => {
                let l = l.optimize_on_freeze(ctx);
                let r = r.optimize_on_freeze(ctx);
                ExprCompiled::compare(l, r, cmp)
            }
            ExprCompiled::TypeIs(box ref e, t) => {
                ExprCompiled::type_is(e.optimize_on_freeze(ctx), t)
            }
            ExprCompiled::Tuple(ref xs) => {
                ExprCompiled::tuple(xs.map(|e| e.optimize_on_freeze(ctx)), ctx.frozen_heap)
            }
            ExprCompiled::List(ref xs) => ExprCompiled::List(xs.map(|e| e.optimize_on_freeze(ctx))),
            ExprCompiled::Dict(ref kvs) => ExprCompiled::Dict(
                kvs.map(|(k, v)| (k.optimize_on_freeze(ctx), v.optimize_on_freeze(ctx))),
            ),
            ExprCompiled::Compr(ref compr) => compr.optimize_on_freeze(ctx),
            ExprCompiled::Dot(box ref object, ref field) => ExprCompiled::dot(
                object.optimize_on_freeze(ctx),
                field,
                ctx.heap,
                ctx.frozen_heap,
            ),
            ExprCompiled::ArrayIndirection(box (ref array, ref index)) => {
                let array = array.optimize_on_freeze(ctx);
                let index = index.optimize_on_freeze(ctx);
                ExprCompiled::array_indirection(array, index, ctx.heap, ctx.frozen_heap)
            }
            ExprCompiled::If(box (ref cond, ref t, ref f)) => {
                let cond = cond.optimize_on_freeze(ctx);
                let t = t.optimize_on_freeze(ctx);
                let f = f.optimize_on_freeze(ctx);
                return ExprCompiled::if_expr(cond, t, f);
            }
            ExprCompiled::Slice(box (ref v, ref start, ref stop, ref step)) => {
                let v = v.optimize_on_freeze(ctx);
                let start = start.as_ref().map(|x| x.optimize_on_freeze(ctx));
                let stop = stop.as_ref().map(|x| x.optimize_on_freeze(ctx));
                let step = step.as_ref().map(|x| x.optimize_on_freeze(ctx));
                ExprCompiled::slice(span, v, start, stop, step, ctx.heap, ctx.frozen_heap)
            }
            ExprCompiled::Not(box ref e) => {
                let e = e.optimize_on_freeze(ctx);
                return ExprCompiled::not(span, e);
            }
            ExprCompiled::UnOp(op, ref e) => {
                let e = e.optimize_on_freeze(ctx);
                ExprCompiled::un_op(op, e, ctx.heap, ctx.frozen_heap)
            }
            ExprCompiled::LogicalBinOp(op, box (ref l, ref r)) => {
                let l = l.optimize_on_freeze(ctx);
                let r = r.optimize_on_freeze(ctx);
                return ExprCompiled::logical_bin_op(op, l, r);
            }
            ExprCompiled::Seq(box (ref l, ref r)) => {
                let l = l.optimize_on_freeze(ctx);
                let r = r.optimize_on_freeze(ctx);
                return ExprCompiled::seq(l, r);
            }
            ExprCompiled::Op(op, box (ref l, ref r)) => {
                let l = l.optimize_on_freeze(ctx);
                let r = r.optimize_on_freeze(ctx);
                ExprCompiled::bin_op(op, l, r, ctx.heap, ctx.frozen_heap)
            }
            ExprCompiled::PercentSOne(box (before, ref arg, after)) => {
                let arg = arg.optimize_on_freeze(ctx);
                ExprCompiled::percent_s_one(before, arg, after, ctx.heap, ctx.frozen_heap)
            }
            ExprCompiled::FormatOne(box (before, ref arg, after)) => {
                let arg = arg.optimize_on_freeze(ctx);
                ExprCompiled::format_one(before, arg, after, ctx.heap, ctx.frozen_heap)
            }
            ref d @ ExprCompiled::Def(..) => d.clone(),
            ExprCompiled::Call(ref call) => call.optimize_on_freeze(ctx),
        };
        IrSpanned { node: expr, span }
    }
}

impl ExprCompiled {
    fn not(span: FrozenFileSpan, expr: IrSpanned<ExprCompiled>) -> IrSpanned<ExprCompiled> {
        match expr.node {
            ExprCompiled::Value(x) => IrSpanned {
                node: ExprCompiled::Value(FrozenValue::new_bool(!x.to_value().to_bool())),
                span,
            },
            // Collapse `not not e` to `e` only if `e` is known to produce a boolean.
            ExprCompiled::Not(box ref e) if e.is_definitely_bool() => e.clone(),
            _ => IrSpanned {
                node: ExprCompiled::Not(box expr),
                span,
            },
        }
    }

    fn or(l: IrSpanned<ExprCompiled>, r: IrSpanned<ExprCompiled>) -> IrSpanned<ExprCompiled> {
        Self::logical_bin_op(ExprLogicalBinOp::Or, l, r)
    }

    fn and(l: IrSpanned<ExprCompiled>, r: IrSpanned<ExprCompiled>) -> IrSpanned<ExprCompiled> {
        Self::logical_bin_op(ExprLogicalBinOp::And, l, r)
    }

    fn logical_bin_op(
        op: ExprLogicalBinOp,
        l: IrSpanned<ExprCompiled>,
        r: IrSpanned<ExprCompiled>,
    ) -> IrSpanned<ExprCompiled> {
        if let Some(l_v) = l.is_pure_infallible_to_bool() {
            if l_v == (op == ExprLogicalBinOp::Or) {
                l
            } else {
                r
            }
        } else {
            let span = l.span.merge(&r.span);
            IrSpanned {
                node: ExprCompiled::LogicalBinOp(op, box (l, r)),
                span,
            }
        }
    }

    pub(crate) fn seq(
        l: IrSpanned<ExprCompiled>,
        r: IrSpanned<ExprCompiled>,
    ) -> IrSpanned<ExprCompiled> {
        if l.is_pure_infallible() {
            r
        } else {
            let span = l.span.merge(&r.span);
            IrSpanned {
                node: ExprCompiled::Seq(box (l, r)),
                span,
            }
        }
    }

    fn percent(
        l: IrSpanned<ExprCompiled>,
        r: IrSpanned<ExprCompiled>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(v) = l.as_string() {
            if let Some((before, after)) = parse_percent_s_one(&v) {
                let before = frozen_heap.alloc_str(&before);
                let after = frozen_heap.alloc_str(&after);
                return ExprCompiled::percent_s_one(before, r, after, heap, frozen_heap);
            }
        }
        ExprCompiled::Op(ExprBinOp::Percent, box (l, r))
    }

    pub(crate) fn percent_s_one(
        before: FrozenStringValue,
        arg: IrSpanned<ExprCompiled>,
        after: FrozenStringValue,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(arg) = arg.as_value() {
            if let Ok(value) = percent_s_one(before.as_str(), arg.to_value(), after.as_str(), heap)
            {
                let value = frozen_heap.alloc_str(value.as_str());
                return ExprCompiled::Value(value.to_frozen_value());
            }
        }

        ExprCompiled::PercentSOne(box (before, arg, after))
    }

    pub(crate) fn format_one(
        before: FrozenStringValue,
        arg: IrSpanned<ExprCompiled>,
        after: FrozenStringValue,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(arg) = arg.as_value() {
            let value = format_one(&before, arg.to_value(), &after, heap);
            let value = frozen_heap.alloc_str(value.as_str());
            return ExprCompiled::Value(value.to_frozen_value());
        }
        ExprCompiled::FormatOne(box (before, arg, after))
    }

    fn add(l: IrSpanned<ExprCompiled>, r: IrSpanned<ExprCompiled>) -> ExprCompiled {
        let span = l.span.merge(&r.span);
        if let (Some(l), Some(r)) = (l.as_list_of_consts(), r.as_list_of_consts()) {
            let lr = l
                .iter()
                .chain(r.iter())
                .map(|x| IrSpanned {
                    node: ExprCompiled::Value(*x),
                    span,
                })
                .collect();
            return ExprCompiled::List(lr);
        }
        ExprCompiled::Op(ExprBinOp::Add, box (l, r))
    }

    fn bin_op(
        bin_op: ExprBinOp,
        l: IrSpanned<ExprCompiled>,
        r: IrSpanned<ExprCompiled>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        let span = l.span.merge(&r.span);
        // Binary operators should have no side effects,
        // but to avoid possible problems, we only fold binary operators on builtin types.
        if let (Some(l), Some(r)) = (l.as_builtin_value(), r.as_builtin_value()) {
            if let Ok(v) = bin_op.eval(l.to_value(), r.to_value(), heap) {
                if let Some(v) = ExprCompiled::try_value(span, v, frozen_heap) {
                    return v;
                }
            }
        }

        match bin_op {
            ExprBinOp::Percent => ExprCompiled::percent(l, r, heap, frozen_heap),
            ExprBinOp::Add => ExprCompiled::add(l, r),
            bin_op => ExprCompiled::Op(bin_op, box (l, r)),
        }
    }

    fn if_expr(
        cond: IrSpanned<ExprCompiled>,
        t: IrSpanned<ExprCompiled>,
        f: IrSpanned<ExprCompiled>,
    ) -> IrSpanned<ExprCompiled> {
        let cond_span = cond.span;
        let cond = ExprCompiledBool::new(cond);
        match cond.node {
            ExprCompiledBool::Const(true) => t,
            ExprCompiledBool::Const(false) => f,
            ExprCompiledBool::Expr(cond) => match cond {
                ExprCompiled::Not(box cond) => ExprCompiled::if_expr(cond, f, t),
                ExprCompiled::Seq(box (x, cond)) => {
                    ExprCompiled::seq(x, ExprCompiled::if_expr(cond, t, f))
                }
                cond => {
                    let cond = IrSpanned {
                        node: cond,
                        span: cond_span,
                    };
                    let span = cond.span.merge(&t.span).merge(&f.span);
                    IrSpanned {
                        node: ExprCompiled::If(box (cond, t, f)),
                        span,
                    }
                }
            },
        }
    }

    fn un_op(
        op: ExprUnOp,
        expr: IrSpanned<ExprCompiled>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(v) = expr.as_builtin_value() {
            if let Ok(v) = op.eval(v.to_value(), heap) {
                if let Some(v) = ExprCompiled::try_value(expr.span, v, frozen_heap) {
                    return v;
                }
            }
        }
        ExprCompiled::UnOp(op, box expr)
    }

    fn try_values(
        span: FrozenFileSpan,
        values: &[Value],
        heap: &FrozenHeap,
    ) -> Option<Vec<IrSpanned<ExprCompiled>>> {
        values
            .try_map(|v| {
                Self::try_value(span, *v, heap)
                    .map(|expr| IrSpanned { span, node: expr })
                    .ok_or(())
            })
            .ok()
    }

    /// Try convert a maybe not frozen value to an expression, or discard it.
    pub(crate) fn try_value(
        span: FrozenFileSpan,
        v: Value,
        heap: &FrozenHeap,
    ) -> Option<ExprCompiled> {
        if let Some(v) = v.unpack_frozen() {
            // If frozen, we are lucky.
            Some(ExprCompiled::Value(v))
        } else if let Some(v) = v.unpack_str() {
            // If string, copy it to frozen heap.
            Some(ExprCompiled::Value(heap.alloc_str(v).to_frozen_value()))
        } else if let Some(v) = v.downcast_ref::<StarlarkFloat>() {
            Some(ExprCompiled::Value(heap.alloc_float(*v)))
        } else if let Some(v) = v.downcast_ref::<Range>() {
            Some(ExprCompiled::Value(heap.alloc(*v)))
        } else if let Some(v) = List::from_value(v) {
            // When spec-safe function returned a non-frozen list,
            // we try to convert that list to a list of constants instruction.
            let items = Self::try_values(span, v.content(), heap)?;
            Some(ExprCompiled::List(items))
        } else if let Some(v) = Tuple::from_value(v) {
            let items = Self::try_values(span, v.content(), heap)?;
            Some(Self::tuple(items, heap))
        } else {
            None
        }
    }

    pub(crate) fn compr(compr: ComprCompiled) -> ExprCompiled {
        match compr {
            ComprCompiled::List(box x, clauses) => {
                if clauses.is_nop() {
                    ExprCompiled::List(Vec::new())
                } else {
                    ExprCompiled::Compr(ComprCompiled::List(box x, clauses))
                }
            }
            ComprCompiled::Dict(box (k, v), clauses) => {
                if clauses.is_nop() {
                    ExprCompiled::Dict(Vec::new())
                } else {
                    ExprCompiled::Compr(ComprCompiled::Dict(box (k, v), clauses))
                }
            }
        }
    }

    /// Construct tuple expression from elements optimizing to frozen tuple value when possible.
    pub(crate) fn tuple(elems: Vec<IrSpanned<ExprCompiled>>, heap: &FrozenHeap) -> ExprCompiled {
        if let Ok(elems) = elems.try_map(|e| e.as_value().ok_or(())) {
            ExprCompiled::Value(heap.alloc_tuple(&elems))
        } else {
            ExprCompiled::Tuple(elems)
        }
    }

    pub(crate) fn compile_time_getattr(
        left: FrozenValue,
        attr: &Symbol,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> Option<FrozenValue> {
        // We assume `getattr` has no side effects.
        let v = get_attr_hashed_raw(left.to_value(), attr, heap).ok()?;
        match v {
            MemberOrValue::Member(m) => match MaybeUnboundValue::new(m) {
                MaybeUnboundValue::Method(m) => {
                    Some(frozen_heap.alloc_simple(BoundMethodGen::new(left, m)))
                }
                MaybeUnboundValue::Attr(..) => None,
            },
            MemberOrValue::Value(v) => v.unpack_frozen(),
        }
    }

    fn dot(
        object: IrSpanned<ExprCompiled>,
        field: &Symbol,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let Some(left) = object.as_value() {
            if let Some(v) = Self::compile_time_getattr(left, field, heap, frozen_heap) {
                return ExprCompiled::Value(v);
            }
        }

        ExprCompiled::Dot(box object, field.clone())
    }

    fn slice(
        span: FrozenFileSpan,
        array: IrSpanned<ExprCompiled>,
        start: Option<IrSpanned<ExprCompiled>>,
        stop: Option<IrSpanned<ExprCompiled>>,
        step: Option<IrSpanned<ExprCompiled>>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        if let (Some(array), Some(start), Some(stop), Some(step)) = (
            array.as_builtin_value(),
            start.as_ref().map(|e| e.as_value()),
            stop.as_ref().map(|e| e.as_value()),
            step.as_ref().map(|e| e.as_value()),
        ) {
            if let Ok(v) = array.to_value().slice(
                start.map(|v| v.to_value()),
                stop.map(|v| v.to_value()),
                step.map(|v| v.to_value()),
                heap,
            ) {
                if let Some(v) = ExprCompiled::try_value(span, v, frozen_heap) {
                    return v;
                }
            }
        }
        ExprCompiled::Slice(box (array, start, stop, step))
    }

    fn array_indirection(
        array: IrSpanned<ExprCompiled>,
        index: IrSpanned<ExprCompiled>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) -> ExprCompiled {
        let span = array.span.merge(&index.span);
        if let (Some(array), Some(index)) = (array.as_builtin_value(), index.as_value()) {
            if let Ok(v) = array.to_value().at(index.to_value(), heap) {
                if let Some(expr) = ExprCompiled::try_value(span, v, frozen_heap) {
                    return expr;
                }
            }
        }
        ExprCompiled::ArrayIndirection(box (array, index))
    }

    pub(crate) fn typ(span: FrozenFileSpan, v: IrSpanned<ExprCompiled>) -> ExprCompiled {
        match &v.node {
            ExprCompiled::Value(v) => {
                ExprCompiled::Value(v.to_value().get_type_value().to_frozen_value())
            }
            ExprCompiled::Tuple(xs) if xs.iter().all(|e| e.is_pure_infallible()) => {
                ExprCompiled::Value(Tuple::get_type_value_static().to_frozen_value())
            }
            ExprCompiled::List(xs) if xs.iter().all(|e| e.is_pure_infallible()) => {
                ExprCompiled::Value(List::get_type_value_static().to_frozen_value())
            }
            ExprCompiled::Dict(xs) if xs.is_empty() => {
                ExprCompiled::Value(Dict::get_type_value_static().to_frozen_value())
            }
            ExprCompiled::TypeIs(x, _t) if x.is_pure_infallible() => {
                ExprCompiled::Value(StarlarkBool::get_type_value_static().to_frozen_value())
            }
            ExprCompiled::Not(x) if x.is_pure_infallible() => {
                ExprCompiled::Value(StarlarkBool::get_type_value_static().to_frozen_value())
            }
            _ => ExprCompiled::Call(box IrSpanned {
                span,
                node: CallCompiled {
                    fun: IrSpanned {
                        span,
                        node: ExprCompiled::Value(Constants::get().fn_type.0),
                    },
                    args: ArgsCompiledValue {
                        pos_named: vec![v],
                        ..ArgsCompiledValue::default()
                    },
                },
            }),
        }
    }

    pub(crate) fn type_is(v: IrSpanned<ExprCompiled>, t: FrozenStringValue) -> ExprCompiled {
        if let Some(v) = v.as_value() {
            return ExprCompiled::Value(FrozenValue::new_bool(
                v.to_value().get_type() == t.as_str(),
            ));
        }
        ExprCompiled::TypeIs(box v, t)
    }

    pub(crate) fn len(span: FrozenFileSpan, arg: IrSpanned<ExprCompiled>) -> ExprCompiled {
        if let Some(arg) = arg.as_value() {
            if let Ok(len) = arg.to_value().length() {
                return ExprCompiled::Value(FrozenValue::new_int(len));
            }
        }
        ExprCompiled::Call(box IrSpanned {
            span,
            node: CallCompiled {
                fun: IrSpanned {
                    span,
                    node: ExprCompiled::Value(Constants::get().fn_len.0),
                },
                args: ArgsCompiledValue {
                    pos_named: vec![arg],
                    ..ArgsCompiledValue::default()
                },
            },
        })
    }

    fn compare(
        l: IrSpanned<ExprCompiled>,
        r: IrSpanned<ExprCompiled>,
        cmp: CompareOp,
    ) -> ExprCompiled {
        if let (Some(l), Some(r)) = (l.as_value(), r.as_value()) {
            // If comparison fails, let it fail in runtime.
            if let Ok(r) = l.compare(r.to_value()) {
                return ExprCompiled::Value(FrozenValue::new_bool((cmp.as_fn())(r)));
            }
        }

        ExprCompiled::Compare(box (l, r), cmp)
    }
}

#[derive(Debug, Clone, Error)]
pub(crate) enum EvalError {
    #[error("Dictionary key repeated for `{0}`")]
    DuplicateDictionaryKey(String),
}

/// Try fold expression `cmp(l == r)` into `cmp(type(x) == "y")`.
/// Return original `l` and `r` arguments if fold was unsuccessful.
fn try_eval_type_is(
    l: IrSpanned<ExprCompiled>,
    r: IrSpanned<ExprCompiled>,
) -> Result<IrSpanned<ExprCompiled>, (IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)> {
    let span = l.span.merge(&r.span);
    if let (Some(l), Some(r)) = (l.as_type(), r.as_string()) {
        Ok(IrSpanned {
            span,
            node: ExprCompiled::type_is(l.clone(), r),
        })
    } else {
        Err((l, r))
    }
}

fn eval_equals(l: IrSpanned<ExprCompiled>, r: IrSpanned<ExprCompiled>) -> ExprCompiled {
    if let (Some(l), Some(r)) = (l.as_value(), r.as_value()) {
        // If comparison fails, let it fail in runtime.
        if let Ok(r) = l.equals(r.to_value()) {
            return ExprCompiled::Value(FrozenValue::new_bool(r));
        }
    }

    let (l, r) = match try_eval_type_is(l, r) {
        Ok(e) => return e.node,
        Err((l, r)) => (l, r),
    };

    let (r, l) = match try_eval_type_is(r, l) {
        Ok(e) => return e.node,
        Err((r, l)) => (r, l),
    };

    ExprCompiled::Equals(box (l, r))
}

impl AstLiteral {
    fn compile(&self, heap: &FrozenHeap) -> FrozenValue {
        match self {
            AstLiteral::Int(i) => match &i.node {
                TokenInt::I32(i) => FrozenValue::new_int(*i),
                TokenInt::BigInt(i) => StarlarkBigInt::alloc_bigint_frozen(i.clone(), heap),
            },
            AstLiteral::Float(f) => heap.alloc(f.node),
            AstLiteral::String(x) => heap.alloc(x.node.as_str()),
        }
    }
}

impl<P: AstPayload> ExprP<P> {
    fn unpack_string_literal(&self) -> Option<&str> {
        match self {
            ExprP::Literal(AstLiteral::String(i)) => Some(&i.node),
            _ => None,
        }
    }

    // Does an entire sequence of additions reduce to a string literal
    fn reduces_to_string<'a>(
        mut op: BinOp,
        mut left: &'a AstExprP<P>,
        mut right: &'a AstExprP<P>,
    ) -> Option<String> {
        let mut results = Vec::new();
        loop {
            if op != BinOp::Add {
                return None;
            }
            // a + b + c  associates as  (a + b) + c
            let x = right.unpack_string_literal()?;
            results.push(x.to_owned());
            match &left.node {
                ExprP::Op(left2, op2, right2) => {
                    op = *op2;
                    left = left2;
                    right = right2;
                }
                _ => {
                    let x = left.unpack_string_literal()?;
                    results.push(x.to_owned());
                    break;
                }
            }
        }
        results.reverse();
        Some(results.concat())
    }
}

#[cold]
#[inline(never)]
fn get_attr_no_attr_error<'v>(x: Value<'v>, attribute: &Symbol) -> anyhow::Error {
    match did_you_mean(attribute.as_str(), x.dir_attr().iter().map(|s| s.as_str())) {
        None => ValueError::NoAttr(x.get_type().to_owned(), attribute.as_str().to_owned()).into(),
        Some(better) => ValueError::NoAttrDidYouMean(
            x.get_type().to_owned(),
            attribute.as_str().to_owned(),
            better.to_owned(),
        )
        .into(),
    }
}

pub(crate) enum MemberOrValue<'v> {
    Member(FrozenValueNotSpecial),
    Value(Value<'v>),
}

#[inline(always)]
pub(crate) fn get_attr_hashed_raw<'v>(
    x: Value<'v>,
    attribute: &Symbol,
    heap: &'v Heap,
) -> anyhow::Result<MemberOrValue<'v>> {
    let aref = x.get_ref();
    if let Some(methods) = aref.get_methods() {
        if let Some(v) = methods.get_frozen_symbol(attribute) {
            return Ok(MemberOrValue::Member(v));
        }
    }
    match aref.get_attr_hashed(attribute.as_str_hashed(), heap) {
        None => Err(get_attr_no_attr_error(x, attribute)),
        Some(x) => Ok(MemberOrValue::Value(x)),
    }
}

pub(crate) fn get_attr_hashed_bind<'v>(
    x: Value<'v>,
    attribute: &Symbol,
    heap: &'v Heap,
) -> anyhow::Result<Value<'v>> {
    let aref = x.get_ref();
    if let Some(methods) = aref.get_methods() {
        if let Some(v) = methods.get_frozen_symbol(attribute) {
            return MaybeUnboundValue::new(v).bind(x, heap);
        }
    }
    match aref.get_attr_hashed(attribute.as_str_hashed(), heap) {
        None => Err(get_attr_no_attr_error(x, attribute)),
        Some(x) => {
            // Only `get_methods` is allowed to return unbound methods,
            // so we assume the value is bound here.
            // TODO(nga): if `NativeMethod` or `NativeAttribute` is returned from `get_attr`,
            //   we get an inconsistency between handling of unbound objects
            //   in various call sites in the crate.
            //   However, `NativeMethod` and `NativeAttribute` are not actually useful
            //   as Starlark values, and can be hidden from public API and can be made
            //   to never appear as user-visible values.
            //   Do that.
            //   Alternatively, allow `NativeMethod` and `NativeAttribute` here,
            //   but that would have negative performance implications.
            Ok(x)
        }
    }
}

impl Compiler<'_, '_, '_> {
    pub fn expr_opt(&mut self, expr: Option<Box<CstExpr>>) -> Option<IrSpanned<ExprCompiled>> {
        expr.map(|v| self.expr(*v))
    }

    fn expr_ident(
        &mut self,
        ident: AstString,
        resolved_ident: Option<ResolvedIdent>,
    ) -> ExprCompiled {
        let resolved_ident =
            resolved_ident.unwrap_or_else(|| panic!("variable not resolved: `{}`", ident.node));
        match resolved_ident {
            ResolvedIdent::Slot((Slot::Local(slot), binding_id)) => {
                let binding = self.scope_data.get_binding(binding_id);

                // We can't look up the local variabless in advance, because they are different each time
                // we go through a new function call.
                match binding.captured {
                    Captured::Yes => ExprCompiled::LocalCaptured(slot),
                    Captured::No => ExprCompiled::Local(slot),
                }
            }
            ResolvedIdent::Slot((Slot::Module(slot), binding_id)) => {
                let binding = self.scope_data.get_binding(binding_id);

                // We can only inline variables if they were assigned once
                // otherwise we might inline the wrong value.
                if binding.assign_count == AssignCount::AtMostOnce {
                    if let Some(v) = self.eval.module_env.slots().get_slot(slot) {
                        // We could inline non-frozen values, but these values
                        // can be garbage-collected, so it is somewhat harder to implement.
                        if let Some(v) = v.unpack_frozen() {
                            return ExprCompiled::Value(v);
                        }
                    }
                }

                ExprCompiled::Module(slot)
            }
            ResolvedIdent::Global(v) => ExprCompiled::Value(v),
        }
    }

    pub(crate) fn expr(&mut self, expr: CstExpr) -> IrSpanned<ExprCompiled> {
        // println!("compile {}", expr.node);
        let span = FrozenFileSpan::new(self.codemap, expr.span);
        let expr = match expr.node {
            ExprP::Identifier(ident, resolved_ident) => self.expr_ident(ident, resolved_ident),
            ExprP::Lambda(params, box inner, scope_id) => {
                let suite = Spanned {
                    span: expr.span,
                    node: StmtP::Return(Some(inner)),
                };
                self.function("lambda", scope_id, params, None, suite)
            }
            ExprP::Tuple(exprs) => {
                let xs = exprs.into_map(|x| self.expr(x));
                ExprCompiled::tuple(xs, self.eval.module_env.frozen_heap())
            }
            ExprP::List(exprs) => {
                let xs = exprs.into_map(|x| self.expr(x));
                ExprCompiled::List(xs)
            }
            ExprP::Dict(exprs) => {
                let xs = exprs.into_map(|(k, v)| (self.expr(k), self.expr(v)));
                ExprCompiled::Dict(xs)
            }
            ExprP::If(box (cond, then_expr, else_expr)) => {
                let cond = self.expr(cond);
                let then_expr = self.expr(then_expr);
                let else_expr = self.expr(else_expr);
                return ExprCompiled::if_expr(cond, then_expr, else_expr);
            }
            ExprP::Dot(left, right) => {
                let left = self.expr(*left);
                let s = Symbol::new(&right.node);

                ExprCompiled::dot(
                    left,
                    &s,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::Call(box left, args) => self.expr_call(span, left, args),
            ExprP::ArrayIndirection(box (array, index)) => {
                let array = self.expr(array);
                let index = self.expr(index);
                ExprCompiled::array_indirection(
                    array,
                    index,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::Slice(collection, start, stop, stride) => {
                let collection = self.expr(*collection);
                let start = start.map(|x| self.expr(*x));
                let stop = stop.map(|x| self.expr(*x));
                let stride = stride.map(|x| self.expr(*x));
                ExprCompiled::slice(
                    span,
                    collection,
                    start,
                    stop,
                    stride,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::Not(expr) => {
                let expr = self.expr(*expr);
                return ExprCompiled::not(span, expr);
            }
            ExprP::Minus(expr) => {
                let expr = self.expr(*expr);
                ExprCompiled::un_op(
                    ExprUnOp::Minus,
                    expr,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::Plus(expr) => {
                let expr = self.expr(*expr);
                ExprCompiled::un_op(
                    ExprUnOp::Plus,
                    expr,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::BitNot(expr) => {
                let expr = self.expr(*expr);
                ExprCompiled::un_op(
                    ExprUnOp::BitNot,
                    expr,
                    self.eval.module_env.heap(),
                    self.eval.module_env.frozen_heap(),
                )
            }
            ExprP::Op(left, op, right) => {
                if let Some(x) = ExprP::reduces_to_string(op, &left, &right) {
                    // Note there's const propagation for `+` on compiled expressions,
                    // but special handling of `+` on AST might be slightly more efficient
                    // (no unnecessary allocations on the heap). So keep it.
                    let val = self.eval.module_env.frozen_heap().alloc(x);
                    ExprCompiled::Value(val)
                } else {
                    let right = if op == BinOp::In || op == BinOp::NotIn {
                        list_to_tuple(*right)
                    } else {
                        *right
                    };

                    let l = self.expr(*left);
                    let r = self.expr(right);
                    match op {
                        BinOp::Or => return ExprCompiled::or(l, r),
                        BinOp::And => return ExprCompiled::and(l, r),
                        BinOp::Equal => eval_equals(l, r),
                        BinOp::NotEqual => {
                            ExprCompiled::not(
                                span,
                                IrSpanned {
                                    span,
                                    node: eval_equals(l, r),
                                },
                            )
                            .node
                        }
                        BinOp::Less => ExprCompiled::compare(l, r, CompareOp::Less),
                        BinOp::Greater => ExprCompiled::compare(l, r, CompareOp::Greater),
                        BinOp::LessOrEqual => ExprCompiled::compare(l, r, CompareOp::LessOrEqual),
                        BinOp::GreaterOrEqual => {
                            ExprCompiled::compare(l, r, CompareOp::GreaterOrEqual)
                        }
                        BinOp::In => ExprCompiled::bin_op(
                            ExprBinOp::In,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::NotIn => {
                            ExprCompiled::not(
                                span,
                                IrSpanned {
                                    span,
                                    node: ExprCompiled::bin_op(
                                        ExprBinOp::In,
                                        l,
                                        r,
                                        self.eval.module_env.heap(),
                                        self.eval.module_env.frozen_heap(),
                                    ),
                                },
                            )
                            .node
                        }
                        BinOp::Subtract => ExprCompiled::bin_op(
                            ExprBinOp::Sub,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::Add => ExprCompiled::bin_op(
                            ExprBinOp::Add,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::Multiply => ExprCompiled::bin_op(
                            ExprBinOp::Multiply,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::Percent => ExprCompiled::bin_op(
                            ExprBinOp::Percent,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::Divide => ExprCompiled::bin_op(
                            ExprBinOp::Divide,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::FloorDivide => ExprCompiled::bin_op(
                            ExprBinOp::FloorDivide,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::BitAnd => ExprCompiled::bin_op(
                            ExprBinOp::BitAnd,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::BitOr => ExprCompiled::bin_op(
                            ExprBinOp::BitOr,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::BitXor => ExprCompiled::bin_op(
                            ExprBinOp::BitXor,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::LeftShift => ExprCompiled::bin_op(
                            ExprBinOp::LeftShift,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                        BinOp::RightShift => ExprCompiled::bin_op(
                            ExprBinOp::RightShift,
                            l,
                            r,
                            self.eval.module_env.heap(),
                            self.eval.module_env.frozen_heap(),
                        ),
                    }
                }
            }
            ExprP::ListComprehension(x, box for_, clauses) => {
                self.list_comprehension(*x, for_, clauses)
            }
            ExprP::DictComprehension(box (k, v), box for_, clauses) => {
                self.dict_comprehension(k, v, for_, clauses)
            }
            ExprP::Literal(x) => {
                let val = x.compile(self.eval.module_env.frozen_heap());
                ExprCompiled::Value(val)
            }
        };
        IrSpanned { node: expr, span }
    }

    /// Like `expr` but returns an expression optimized assuming
    /// only the truth of the result is needed.
    pub(crate) fn expr_truth(&mut self, expr: CstExpr) -> IrSpanned<ExprCompiledBool> {
        let expr = self.expr(expr);
        ExprCompiledBool::new(expr)
    }
}

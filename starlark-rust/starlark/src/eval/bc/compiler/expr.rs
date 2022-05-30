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

//! Compile expressions.

use std::collections::HashSet;

use gazebo::prelude::*;

use crate::{
    collections::{Hashed, SmallMap},
    eval::{
        bc::{
            compiler::if_compiler::write_if_else,
            instr_arg::{ArgPopsStack, ArgPopsStack1, ArgPopsStackMaybe1},
            instr_impl::*,
            slow_arg::BcInstrSlowArg,
            writer::BcWriter,
        },
        compiler::{
            expr::{CompareOp, ExprBinOp, ExprCompiled, ExprUnOp},
            span::IrSpanned,
        },
        runtime::call_stack::FrozenFileSpan,
    },
    values::{
        layout::value_not_special::FrozenValueNotSpecial, FrozenStringValue, FrozenValue, ValueLike,
    },
};

pub(crate) fn write_exprs<'a>(
    exprs: impl IntoIterator<Item = &'a IrSpanned<ExprCompiled>>,
    bc: &mut BcWriter,
) {
    for expr in exprs {
        expr.write_bc(bc);
    }
}

impl IrSpanned<ExprCompiled> {
    fn try_dict_of_consts(
        xs: &[(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)],
    ) -> Option<SmallMap<FrozenValue, FrozenValue>> {
        let mut res = SmallMap::new();
        for (k, v) in xs {
            let k = k.as_value()?.get_hashed().ok()?;
            let v = v.as_value()?;
            let prev = res.insert_hashed(k, v);
            if prev.is_some() {
                // If there are duplicates, so don't take the fast-literal
                // path and go down the slow runtime path (which will raise the error).
                // We have a lint that will likely fire on this issue (and others).
                return None;
            }
        }
        Some(res)
    }

    fn try_dict_const_keys(
        xs: &[(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)],
    ) -> Option<Box<[Hashed<FrozenValue>]>> {
        let mut keys = Vec::new();
        let mut keys_unique = HashSet::new();
        for (k, _) in xs {
            let k = k.as_value()?.get_hashed().ok()?;
            keys.push(k);
            let inserted = keys_unique.insert(k);
            if !inserted {
                // Otherwise fail at runtime
                return None;
            }
        }
        Some(keys.into_boxed_slice())
    }

    fn write_dict(
        span: FrozenFileSpan,
        xs: &[(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>)],
        bc: &mut BcWriter,
    ) {
        if xs.is_empty() {
            bc.write_instr::<InstrDictNew>(span, ());
        } else if let Some(d) = Self::try_dict_of_consts(xs) {
            bc.write_instr::<InstrDictOfConsts>(span, d);
        } else if let Some(keys) = Self::try_dict_const_keys(xs) {
            assert_eq!(keys.len(), xs.len());
            write_exprs(xs.iter().map(|(_, v)| v), bc);
            bc.write_instr::<InstrDictConstKeys>(span, (ArgPopsStack(xs.len() as u32), keys));
        } else {
            let key_spans = xs.map(|(k, _v)| k.span);
            write_exprs(xs.iter().flat_map(|(k, v)| [k, v]), bc);
            bc.write_instr_explicit::<InstrDictNPop>(
                BcInstrSlowArg {
                    span,
                    spans: key_spans,
                },
                ArgPopsStack(xs.len() as u32 * 2),
            );
        }
    }

    fn write_not(expr: &IrSpanned<ExprCompiled>, bc: &mut BcWriter) {
        expr.write_bc(bc);
        bc.write_instr::<InstrNot>(expr.span, ());
    }

    fn write_equals_const(
        span: FrozenFileSpan,
        a: &IrSpanned<ExprCompiled>,
        b: FrozenValue,
        bc: &mut BcWriter,
    ) {
        a.write_bc(bc);
        if let Some(b) = b.to_value().unpack_int_value() {
            bc.write_instr::<InstrEqInt>(span, b);
        } else if b.eq_is_ptr_eq() {
            bc.write_instr::<InstrEqPtr>(span, b);
        } else if let Some(b) = FrozenStringValue::new(b) {
            bc.write_instr::<InstrEqStr>(span, b);
        } else if let Some(b) = FrozenValueNotSpecial::new(b) {
            bc.write_instr::<InstrEqConst>(span, b);
        } else {
            unreachable!("FrozenValue must be either i32, str or not-special");
        }
    }

    fn write_equals(
        span: FrozenFileSpan,
        a: &IrSpanned<ExprCompiled>,
        b: &IrSpanned<ExprCompiled>,
        bc: &mut BcWriter,
    ) {
        if let Some(a) = a.as_value() {
            Self::write_equals_const(span, b, a, bc);
        } else if let Some(b) = b.as_value() {
            Self::write_equals_const(span, a, b, bc);
        } else {
            a.write_bc(bc);
            b.write_bc(bc);
            bc.write_instr::<InstrEq>(span, ());
        }
    }

    pub(crate) fn write_bc(&self, bc: &mut BcWriter) {
        let span = self.span;
        match self.node {
            ExprCompiled::Value(v) => {
                bc.write_const(span, v);
            }
            ExprCompiled::Local(slot) => {
                bc.write_load_local(span, slot);
            }
            ExprCompiled::LocalCaptured(slot) => {
                bc.write_load_local_captured(span, slot);
            }
            ExprCompiled::Module(slot) => {
                bc.write_instr::<InstrLoadModule>(span, slot);
            }
            ExprCompiled::Equals(box (ref a, ref b)) => {
                Self::write_equals(span, a, b, bc);
            }
            ExprCompiled::Compare(box (ref l, ref r), cmp) => {
                l.write_bc(bc);
                r.write_bc(bc);
                match cmp {
                    CompareOp::Less => bc.write_instr::<InstrLess>(span, ()),
                    CompareOp::Greater => bc.write_instr::<InstrGreater>(span, ()),
                    CompareOp::LessOrEqual => bc.write_instr::<InstrLessOrEqual>(span, ()),
                    CompareOp::GreaterOrEqual => bc.write_instr::<InstrGreaterOrEqual>(span, ()),
                }
            }
            ExprCompiled::TypeIs(box ref v, t) => {
                v.write_bc(bc);
                bc.write_instr::<InstrTypeIs>(span, t);
            }
            ExprCompiled::Tuple(ref xs) => {
                write_exprs(xs, bc);
                bc.write_instr::<InstrTupleNPop>(span, ArgPopsStack(xs.len() as u32));
            }
            ExprCompiled::List(ref xs) => {
                if xs.is_empty() {
                    bc.write_instr::<InstrListNew>(span, ());
                } else if xs.iter().all(|x| x.as_value().is_some()) {
                    let content = xs.map(|v| v.as_value().unwrap()).into_boxed_slice();
                    bc.write_instr::<InstrListOfConsts>(span, content);
                } else {
                    write_exprs(xs, bc);
                    bc.write_instr::<InstrListNPop>(span, ArgPopsStack(xs.len() as u32));
                }
            }
            ExprCompiled::Dict(ref xs) => Self::write_dict(span, xs, bc),
            ExprCompiled::Compr(ref compr) => {
                compr.write_bc(span, bc);
            }
            ExprCompiled::Dot(box ref object, ref field) => {
                object.write_bc(bc);
                bc.write_instr::<InstrObjectField>(span, field.clone());
            }
            ExprCompiled::ArrayIndirection(box (ref array, ref index)) => {
                array.write_bc(bc);
                index.write_bc(bc);
                bc.write_instr::<InstrArrayIndex>(span, ());
            }
            ExprCompiled::Slice(box (ref l, ref start, ref stop, ref step)) => {
                l.write_bc(bc);
                write_exprs([start, stop, step].iter().copied().flatten(), bc);
                bc.write_instr::<InstrSlice>(
                    span,
                    (
                        ArgPopsStack1,
                        ArgPopsStackMaybe1(start.is_some()),
                        ArgPopsStackMaybe1(stop.is_some()),
                        ArgPopsStackMaybe1(step.is_some()),
                    ),
                );
            }
            ExprCompiled::Not(box ref expr) => {
                Self::write_not(expr, bc);
            }
            ExprCompiled::UnOp(op, ref expr) => {
                expr.write_bc(bc);
                match op {
                    ExprUnOp::Minus => bc.write_instr::<InstrMinus>(span, ()),
                    ExprUnOp::Plus => bc.write_instr::<InstrPlus>(span, ()),
                    ExprUnOp::BitNot => bc.write_instr::<InstrBitNot>(span, ()),
                }
            }
            ExprCompiled::If(box (ref cond, ref t, ref f)) => {
                write_if_else(
                    cond,
                    |bc| {
                        t.write_bc(bc);
                        // Both then and else branches leave a value on the stack.
                        // But we execute either of them.
                        // So explicitly decrement stack size.
                        bc.stack_sub(1);
                    },
                    |bc| {
                        f.write_bc(bc);
                        bc.stack_sub(1);
                    },
                    bc,
                );
                bc.stack_add(1);
            }
            ExprCompiled::And(box (ref l, ref r)) => {
                l.write_bc(bc);
                bc.write_instr::<InstrDup>(span, ());
                bc.write_if(l.span, |bc| {
                    bc.write_instr::<InstrPop>(span, ());
                    r.write_bc(bc);
                });
            }
            ExprCompiled::Or(box (ref l, ref r)) => {
                l.write_bc(bc);
                bc.write_instr::<InstrDup>(span, ());
                bc.write_if_not(l.span, |bc| {
                    bc.write_instr::<InstrPop>(l.span, ());
                    r.write_bc(bc);
                });
            }
            ExprCompiled::Seq(box (ref l, ref r)) => {
                l.write_bc_for_effect(bc);
                r.write_bc(bc);
            }
            ExprCompiled::Op(op, box (ref l, ref r)) => {
                l.write_bc(bc);
                r.write_bc(bc);
                match op {
                    ExprBinOp::In => bc.write_instr::<InstrIn>(span, ()),
                    ExprBinOp::Sub => bc.write_instr::<InstrSub>(span, ()),
                    ExprBinOp::Add => bc.write_instr::<InstrAdd>(span, ()),
                    ExprBinOp::Multiply => bc.write_instr::<InstrMultiply>(span, ()),
                    ExprBinOp::Divide => bc.write_instr::<InstrDivide>(span, ()),
                    ExprBinOp::FloorDivide => bc.write_instr::<InstrFloorDivide>(span, ()),
                    ExprBinOp::Percent => bc.write_instr::<InstrPercent>(span, ()),
                    ExprBinOp::BitAnd => bc.write_instr::<InstrBitAnd>(span, ()),
                    ExprBinOp::BitOr => bc.write_instr::<InstrBitOr>(span, ()),
                    ExprBinOp::BitXor => bc.write_instr::<InstrBitXor>(span, ()),
                    ExprBinOp::LeftShift => bc.write_instr::<InstrLeftShift>(span, ()),
                    ExprBinOp::RightShift => bc.write_instr::<InstrRightShift>(span, ()),
                }
            }
            ExprCompiled::PercentSOne(box (before, ref arg, after)) => {
                arg.write_bc(bc);
                bc.write_instr::<InstrPercentSOne>(span, (before, after));
            }
            ExprCompiled::FormatOne(box (before, ref arg, after)) => {
                arg.write_bc(bc);
                bc.write_instr::<InstrFormatOne>(span, (before, after));
            }
            ExprCompiled::Call(ref call) => call.write_bc(bc),
            ExprCompiled::Def(ref def) => def.write_bc(span, bc),
        }
    }

    pub(crate) fn write_bc_for_effect(&self, bc: &mut BcWriter) {
        self.write_bc(bc);
        bc.write_instr::<InstrPop>(self.span, ());
    }
}

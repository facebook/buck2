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
            instr_impl::*,
            slow_arg::BcInstrSlowArg,
            stack_ptr::{BcSlotIn, BcSlotInRange, BcSlotOut, BcSlotsInN},
            writer::BcWriter,
        },
        compiler::{
            expr::{CompareOp, ExprBinOp, ExprCompiled, ExprLogicalBinOp, ExprUnOp, MaybeNot},
            span::IrSpanned,
        },
        runtime::call_stack::FrozenFileSpan,
    },
    values::{
        layout::value_not_special::FrozenValueNotSpecial, FrozenStringValue, FrozenValue, ValueLike,
    },
};

/// Try extract consecutive definitely initialized locals from expressions.
fn try_slot_range<'a>(
    exprs: impl IntoIterator<Item = &'a IrSpanned<ExprCompiled>>,
    bc: &BcWriter,
) -> Option<BcSlotInRange> {
    let mut range = BcSlotInRange::default();
    for expr in exprs {
        let local = expr.as_local_non_captured()?;
        let slot = bc.try_definitely_assigned(local)?;
        if !range.try_push(slot) {
            return None;
        }
    }
    Some(range)
}

/// Compile several expressions into consecutive registers.
pub(crate) fn write_exprs<'a>(
    exprs: impl IntoIterator<Item = &'a IrSpanned<ExprCompiled>>,
    bc: &mut BcWriter,
    k: impl FnOnce(BcSlotInRange, &mut BcWriter),
) {
    let exprs: Vec<_> = exprs.into_iter().collect();

    if let Some(slots) = try_slot_range(exprs.iter().copied(), bc) {
        k(slots, bc);
    } else {
        bc.alloc_slots_for_exprs(exprs, |slot, expr, bc| expr.write_bc(slot.to_out(), bc), k)
    }
}

pub(crate) fn write_expr_opt(
    expr: &Option<IrSpanned<ExprCompiled>>,
    bc: &mut BcWriter,
    k: impl FnOnce(Option<BcSlotIn>, &mut BcWriter),
) {
    if let Some(expr) = expr {
        expr.write_bc_cb(bc, |slot, bc| k(Some(slot), bc))
    } else {
        k(None, bc)
    }
}

pub(crate) fn write_n_exprs<const N: usize>(
    exprs: [&IrSpanned<ExprCompiled>; N],
    bc: &mut BcWriter,
    k: impl FnOnce(BcSlotsInN<N>, &mut BcWriter),
) {
    write_exprs(exprs, bc, |slots, bc| k(BcSlotsInN::from_range(slots), bc))
}

impl ExprCompiled {
    /// Mark variables which are definitely assigned after execution of this expression.
    ///
    /// For example, when this expression if executed:
    ///
    /// ```ignore
    /// t if c else f
    /// ```
    ///
    /// `c` is definitely assigned (because if it is not, then execution fails),
    /// but we don't know about `t` or `f` because one of them was not executed.
    pub(crate) fn mark_definitely_assigned_after(&self, bc: &mut BcWriter) {
        match self {
            ExprCompiled::Value(_) => {}
            ExprCompiled::Local(local) => bc.mark_definitely_assigned(*local),
            ExprCompiled::LocalCaptured(_) => {}
            ExprCompiled::Module(_) => {}
            ExprCompiled::Compare(box (a, b), _op) => {
                a.mark_definitely_assigned_after(bc);
                b.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::TypeIs(v, _t) => {
                v.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::Tuple(xs) | ExprCompiled::List(xs) => {
                for x in xs {
                    x.mark_definitely_assigned_after(bc);
                }
            }
            ExprCompiled::Dict(xs) => {
                for (k, v) in xs {
                    k.mark_definitely_assigned_after(bc);
                    v.mark_definitely_assigned_after(bc);
                }
            }
            ExprCompiled::Compr(compr) => compr.mark_definitely_assigned_after(bc),
            ExprCompiled::Dot(object, _field) => {
                object.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::ArrayIndirection(box (array, index)) => {
                array.mark_definitely_assigned_after(bc);
                index.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::If(box (c, _t, _f)) => {
                // Condition is executed unconditionally, so we use it to mark definitely assigned.
                // But we don't know which of the branches will be executed.
                c.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::Slice(box (l, a, b, c)) => {
                l.mark_definitely_assigned_after(bc);
                if let Some(a) = a {
                    a.mark_definitely_assigned_after(bc);
                }
                if let Some(b) = b {
                    b.mark_definitely_assigned_after(bc);
                }
                if let Some(c) = c {
                    c.mark_definitely_assigned_after(bc);
                }
            }
            ExprCompiled::Not(expr) => {
                expr.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::UnOp(_op, expr) => {
                expr.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::LogicalBinOp(_op, box (a, b)) => {
                // `a` is executed unconditionally, but `b` is not,
                // so we mark only `a` as definitely assigned.
                a.mark_definitely_assigned_after(bc);
                let _ = b;
            }
            ExprCompiled::Seq(box (a, b)) => {
                a.mark_definitely_assigned_after(bc);
                b.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::Op(_op, box (a, b)) => {
                a.mark_definitely_assigned_after(bc);
                b.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::PercentSOne(box (_before, arg, _after)) => {
                arg.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::FormatOne(box (_before, arg, _after)) => {
                arg.mark_definitely_assigned_after(bc);
            }
            ExprCompiled::Call(c) => c.mark_definitely_assigned_after(bc),
            ExprCompiled::Def(d) => d.mark_definitely_assigned_after(bc),
        }
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
        target: BcSlotOut,
        bc: &mut BcWriter,
    ) {
        if xs.is_empty() {
            bc.write_instr::<InstrDictNew>(span, target);
        } else if let Some(d) = Self::try_dict_of_consts(xs) {
            bc.write_instr::<InstrDictOfConsts>(span, (d, target));
        } else if let Some(keys) = Self::try_dict_const_keys(xs) {
            assert_eq!(keys.len(), xs.len());
            write_exprs(xs.iter().map(|(_, v)| v), bc, |values, bc| {
                assert_eq!(values.len() as usize, keys.len());
                bc.write_instr::<InstrDictConstKeys>(span, (keys, values.to_range_from(), target));
            });
        } else {
            let key_spans = xs.map(|(k, _v)| k.span);
            write_exprs(xs.iter().flat_map(|(k, v)| [k, v]), bc, |kvs, bc| {
                bc.write_instr_explicit::<InstrDictNPop>(
                    BcInstrSlowArg {
                        span,
                        spans: key_spans,
                    },
                    (kvs, target),
                );
            });
        }
    }

    fn write_not(expr: &IrSpanned<ExprCompiled>, target: BcSlotOut, bc: &mut BcWriter) {
        expr.write_bc_cb(bc, |slot, bc| {
            bc.write_instr::<InstrNot>(expr.span, (slot, target));
        });
    }

    fn write_equals_const(
        span: FrozenFileSpan,
        a: &IrSpanned<ExprCompiled>,
        b: FrozenValue,
        target: BcSlotOut,
        bc: &mut BcWriter,
    ) {
        a.write_bc_cb(bc, |a, bc| {
            if let Some(b) = b.to_value().unpack_int_value() {
                bc.write_instr::<InstrEqInt>(span, (a, b, target));
            } else if b.eq_is_ptr_eq() {
                bc.write_instr::<InstrEqPtr>(span, (a, b, target));
            } else if let Some(b) = FrozenStringValue::new(b) {
                bc.write_instr::<InstrEqStr>(span, (a, b, target));
            } else if let Some(b) = FrozenValueNotSpecial::new(b) {
                bc.write_instr::<InstrEqConst>(span, (a, b, target));
            } else {
                unreachable!("FrozenValue must be either i32, str or not-special");
            }
        });
    }

    fn write_equals(
        span: FrozenFileSpan,
        a: &IrSpanned<ExprCompiled>,
        b: &IrSpanned<ExprCompiled>,
        target: BcSlotOut,
        bc: &mut BcWriter,
    ) {
        if let Some(a) = a.as_value() {
            Self::write_equals_const(span, b, a, target, bc);
        } else if let Some(b) = b.as_value() {
            Self::write_equals_const(span, a, b, target, bc);
        } else {
            write_n_exprs([a, b], bc, |a_b, bc| {
                bc.write_instr::<InstrEq>(span, (a_b, target));
            });
        }
    }

    pub(crate) fn write_bc(&self, target: BcSlotOut, bc: &mut BcWriter) {
        let span = self.span;
        match self.node {
            ExprCompiled::Value(v) => {
                bc.write_const(span, v, target);
            }
            ExprCompiled::Local(slot) => {
                bc.write_load_local(span, slot, target);
            }
            ExprCompiled::LocalCaptured(slot) => {
                bc.write_load_local_captured(span, slot, target);
            }
            ExprCompiled::Module(slot) => {
                bc.write_instr::<InstrLoadModule>(span, (slot, target));
            }
            ExprCompiled::Compare(box (ref l, ref r), cmp) => {
                write_n_exprs([l, r], bc, |l_r, bc| {
                    let arg = (l_r, target);
                    match cmp {
                        CompareOp::Less => bc.write_instr::<InstrLess>(span, arg),
                        CompareOp::Greater => bc.write_instr::<InstrGreater>(span, arg),
                        CompareOp::LessOrEqual => bc.write_instr::<InstrLessOrEqual>(span, arg),
                        CompareOp::GreaterOrEqual => {
                            bc.write_instr::<InstrGreaterOrEqual>(span, arg)
                        }
                    }
                });
            }
            ExprCompiled::TypeIs(box ref v, t) => {
                v.write_bc_cb(bc, |v, bc| {
                    bc.write_instr::<InstrTypeIs>(span, (v, t, target));
                });
            }
            ExprCompiled::Tuple(ref xs) => {
                write_exprs(xs, bc, |xs, bc| {
                    bc.write_instr::<InstrTupleNPop>(span, (xs, target));
                });
            }
            ExprCompiled::List(ref xs) => {
                if xs.is_empty() {
                    bc.write_instr::<InstrListNew>(span, target);
                } else if xs.iter().all(|x| x.as_value().is_some()) {
                    let content = xs.map(|v| v.as_value().unwrap()).into_boxed_slice();
                    bc.write_instr::<InstrListOfConsts>(span, (content, target));
                } else {
                    write_exprs(xs, bc, |xs, bc| {
                        bc.write_instr::<InstrListNPop>(span, (xs, target));
                    });
                }
            }
            ExprCompiled::Dict(ref xs) => Self::write_dict(span, xs, target, bc),
            ExprCompiled::Compr(ref compr) => compr.write_bc(span, target, bc),
            ExprCompiled::Dot(box ref object, ref field) => {
                object.write_bc_cb(bc, |object, bc| {
                    bc.write_instr::<InstrObjectField>(span, (object, field.clone(), target));
                });
            }
            ExprCompiled::ArrayIndirection(box (ref array, ref index)) => {
                write_n_exprs([array, index], bc, |array_index, bc| {
                    bc.write_instr::<InstrArrayIndex>(span, (array_index, target))
                })
            }
            ExprCompiled::Slice(box (ref l, ref start, ref stop, ref step)) => {
                l.write_bc_cb(bc, |l, bc| {
                    write_expr_opt(start, bc, |start, bc| {
                        write_expr_opt(stop, bc, |stop, bc| {
                            write_expr_opt(step, bc, |step, bc| {
                                bc.write_instr::<InstrSlice>(span, (l, start, stop, step, target))
                            })
                        })
                    })
                });
            }
            ExprCompiled::Not(box ref expr) => {
                Self::write_not(expr, target, bc);
            }
            ExprCompiled::UnOp(op, ref expr) => {
                expr.write_bc_cb(bc, |expr, bc| {
                    let arg = (expr, target);
                    match op {
                        ExprUnOp::Minus => bc.write_instr::<InstrMinus>(span, arg),
                        ExprUnOp::Plus => bc.write_instr::<InstrPlus>(span, arg),
                        ExprUnOp::BitNot => bc.write_instr::<InstrBitNot>(span, arg),
                    }
                });
            }
            ExprCompiled::If(box (ref cond, ref t, ref f)) => {
                write_if_else(
                    cond,
                    |bc| t.write_bc(target, bc),
                    |bc| f.write_bc(target, bc),
                    bc,
                );
            }
            ExprCompiled::LogicalBinOp(op, box (ref l, ref r)) => {
                l.write_bc_cb(bc, |l_slot, bc| {
                    let maybe_not = match op {
                        ExprLogicalBinOp::And => MaybeNot::Id,
                        ExprLogicalBinOp::Or => MaybeNot::Not,
                    };
                    bc.write_if_else(
                        l_slot,
                        maybe_not,
                        l.span,
                        |bc| r.write_bc(target, bc),
                        |bc| bc.write_mov(span, l_slot, target),
                    );
                });
            }
            ExprCompiled::Seq(box (ref l, ref r)) => {
                l.write_bc_for_effect(bc);
                r.write_bc(target, bc);
            }
            ExprCompiled::Op(ExprBinOp::Equals, box (ref l, ref r)) => {
                Self::write_equals(span, l, r, target, bc)
            }
            ExprCompiled::Op(op, box (ref l, ref r)) => {
                write_n_exprs([l, r], bc, |l_r, bc| {
                    let arg = (l_r, target);
                    match op {
                        ExprBinOp::Equals => unreachable!("handled above"),
                        ExprBinOp::In => bc.write_instr::<InstrIn>(span, arg),
                        ExprBinOp::Sub => bc.write_instr::<InstrSub>(span, arg),
                        ExprBinOp::Add => bc.write_instr::<InstrAdd>(span, arg),
                        ExprBinOp::Multiply => bc.write_instr::<InstrMultiply>(span, arg),
                        ExprBinOp::Divide => bc.write_instr::<InstrDivide>(span, arg),
                        ExprBinOp::FloorDivide => bc.write_instr::<InstrFloorDivide>(span, arg),
                        ExprBinOp::Percent => bc.write_instr::<InstrPercent>(span, arg),
                        ExprBinOp::BitAnd => bc.write_instr::<InstrBitAnd>(span, arg),
                        ExprBinOp::BitOr => bc.write_instr::<InstrBitOr>(span, arg),
                        ExprBinOp::BitXor => bc.write_instr::<InstrBitXor>(span, arg),
                        ExprBinOp::LeftShift => bc.write_instr::<InstrLeftShift>(span, arg),
                        ExprBinOp::RightShift => bc.write_instr::<InstrRightShift>(span, arg),
                    }
                });
            }
            ExprCompiled::PercentSOne(box (before, ref arg, after)) => {
                arg.write_bc_cb(bc, |arg, bc| {
                    bc.write_instr::<InstrPercentSOne>(span, (before, arg, after, target));
                });
            }
            ExprCompiled::FormatOne(box (before, ref arg, after)) => {
                arg.write_bc_cb(bc, |arg, bc| {
                    bc.write_instr::<InstrFormatOne>(span, (before, arg, after, target));
                });
            }
            ExprCompiled::Call(ref call) => call.write_bc(target, bc),
            ExprCompiled::Def(ref def) => def.write_bc(span, target, bc),
        }
    }

    /// Allocate temporary slot, write expression into it,
    /// and then consume the slot with the callback.
    pub(crate) fn write_bc_cb<R>(
        &self,
        bc: &mut BcWriter,
        k: impl FnOnce(BcSlotIn, &mut BcWriter) -> R,
    ) -> R {
        if let Some(local) = self.as_local_non_captured() {
            // Local is known to be definitely assigned, so there's no need
            // to "load" it just to trigger check that it is assigned.
            if let Some(slot) = bc.try_definitely_assigned(local) {
                return k(slot, bc);
            }
        }

        bc.alloc_slot(|slot, bc| {
            self.write_bc(slot.to_out(), bc);
            k(slot.to_in(), bc)
        })
    }

    pub(crate) fn write_bc_for_effect(&self, bc: &mut BcWriter) {
        self.write_bc_cb(bc, |slot, _bc| {
            let _ = slot;
        });
    }
}

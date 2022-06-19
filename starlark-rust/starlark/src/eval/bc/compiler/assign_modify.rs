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

//! Write operators like `+=`.

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        bc::{
            compiler::expr::write_n_exprs,
            instr_impl::{
                InstrAddAssign, InstrArrayIndex, InstrArrayIndexSet, InstrBitAnd, InstrBitOrAssign,
                InstrBitXor, InstrDivide, InstrFloorDivide, InstrLeftShift, InstrLoadModule,
                InstrMultiply, InstrObjectField, InstrPercent, InstrRightShift,
                InstrSetObjectField, InstrStoreModule, InstrSub,
            },
            stack_ptr::{BcSlotIn, BcSlotOut, BcSlotsN},
            writer::BcWriter,
        },
        compiler::{expr::ExprCompiled, scope::Captured, span::IrSpanned, stmt::AssignModifyLhs},
        runtime::call_stack::FrozenFileSpan,
    },
    syntax::ast::AssignOp,
};

impl AssignOp {
    fn write_bc(
        self,
        v0: BcSlotIn,
        v1: BcSlotIn,
        target: BcSlotOut,
        span: FrozenFileSpan,
        bc: &mut BcWriter,
    ) {
        let arg = (v0, v1, target);
        match self {
            AssignOp::Add => bc.write_instr::<InstrAddAssign>(span, arg),
            AssignOp::Subtract => bc.write_instr::<InstrSub>(span, arg),
            AssignOp::Multiply => bc.write_instr::<InstrMultiply>(span, arg),
            AssignOp::Divide => bc.write_instr::<InstrDivide>(span, arg),
            AssignOp::FloorDivide => bc.write_instr::<InstrFloorDivide>(span, arg),
            AssignOp::Percent => bc.write_instr::<InstrPercent>(span, arg),
            AssignOp::BitAnd => bc.write_instr::<InstrBitAnd>(span, arg),
            AssignOp::BitOr => bc.write_instr::<InstrBitOrAssign>(span, arg),
            AssignOp::BitXor => bc.write_instr::<InstrBitXor>(span, arg),
            AssignOp::LeftShift => bc.write_instr::<InstrLeftShift>(span, arg),
            AssignOp::RightShift => bc.write_instr::<InstrRightShift>(span, arg),
        }
    }
}

impl AssignModifyLhs {
    /// After evaluation of `x[y] += ...`, variables `x` and `y` are definitely assigned.
    pub(crate) fn mark_definitely_assigned_after(&self, bc: &mut BcWriter) {
        match self {
            AssignModifyLhs::Dot(object, _field) => object.mark_definitely_assigned_after(bc),
            AssignModifyLhs::Array(array, index) => {
                array.mark_definitely_assigned_after(bc);
                index.mark_definitely_assigned_after(bc);
            }
            AssignModifyLhs::Local(local) => match local.node {
                (_, Captured::Yes) => {}
                (local, Captured::No) => bc.mark_definitely_assigned(local),
            },
            AssignModifyLhs::Module(_) => {}
        }
    }

    pub(crate) fn write_bc(
        &self,
        span: FrozenFileSpan,
        op: AssignOp,
        rhs: &IrSpanned<ExprCompiled>,
        bc: &mut BcWriter,
    ) {
        match *self {
            AssignModifyLhs::Dot(ref object, ref field) => {
                object.write_bc_cb(bc, |object, bc| {
                    bc.alloc_slots_c(|lhs_rhs: BcSlotsN<2>, bc| {
                        let field = Symbol::new(field.as_str());
                        bc.write_instr::<InstrObjectField>(
                            span,
                            (object, field.clone(), lhs_rhs.get::<0>().to_out()),
                        );
                        rhs.write_bc(lhs_rhs.get::<1>().to_out(), bc);
                        op.write_bc(
                            lhs_rhs.get::<0>().to_in(),
                            lhs_rhs.get::<1>().to_in(),
                            lhs_rhs.get::<1>().to_out(),
                            span,
                            bc,
                        );
                        bc.write_instr::<InstrSetObjectField>(
                            span,
                            (lhs_rhs.get::<1>().to_in(), object, field),
                        );
                    })
                });
            }
            AssignModifyLhs::Array(ref array, ref index) => {
                write_n_exprs([array, index], bc, |[array, index], bc| {
                    bc.alloc_slots_c(|slots: BcSlotsN<2>, bc| {
                        let temp_slot = slots.get::<0>();
                        let rhs_slot = slots.get::<1>();

                        bc.write_instr::<InstrArrayIndex>(span, (array, index, temp_slot.to_out()));
                        rhs.write_bc(rhs_slot.to_out(), bc);
                        op.write_bc(
                            temp_slot.to_in(),
                            rhs_slot.to_in(),
                            temp_slot.to_out(),
                            span,
                            bc,
                        );
                        bc.write_instr::<InstrArrayIndexSet>(
                            span,
                            (array, index, temp_slot.to_in()),
                        );
                    })
                });
            }
            AssignModifyLhs::Local(s) => bc.alloc_slots_c(|lhs_rhs: BcSlotsN<2>, bc| {
                let (slot, captured) = s.node;
                match captured {
                    Captured::Yes => {
                        bc.write_load_local_captured(span, slot, lhs_rhs.get::<0>().to_out())
                    }
                    Captured::No => bc.write_load_local(span, slot, lhs_rhs.get::<0>().to_out()),
                }
                rhs.write_bc(lhs_rhs.get::<1>().to_out(), bc);

                op.write_bc(
                    lhs_rhs.get::<0>().to_in(),
                    lhs_rhs.get::<1>().to_in(),
                    lhs_rhs.get::<1>().to_out(),
                    span,
                    bc,
                );
                match captured {
                    Captured::Yes => {
                        bc.write_store_local_captured(span, lhs_rhs.get::<1>().to_in(), slot)
                    }
                    Captured::No => {
                        bc.write_mov(span, lhs_rhs.get::<1>().to_in(), slot.to_bc_slot().to_out())
                    }
                }
            }),
            AssignModifyLhs::Module(m) => bc.alloc_slots_c(|lhs_rhs: BcSlotsN<2>, bc| {
                let slot = m.node;
                bc.write_instr::<InstrLoadModule>(span, (slot, lhs_rhs.get::<0>().to_out()));
                rhs.write_bc(lhs_rhs.get::<1>().to_out(), bc);
                op.write_bc(
                    lhs_rhs.get::<0>().to_in(),
                    lhs_rhs.get::<1>().to_in(),
                    lhs_rhs.get::<1>().to_out(),
                    span,
                    bc,
                );
                bc.write_instr::<InstrStoreModule>(span, (lhs_rhs.get::<1>().to_in(), slot));
            }),
        }
    }
}

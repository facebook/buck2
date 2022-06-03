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
            instr_impl::{
                InstrAddAssign, InstrArrayIndex, InstrArrayIndexSet, InstrBitAnd, InstrBitOrAssign,
                InstrBitXor, InstrDivide, InstrFloorDivide, InstrLeftShift, InstrLoadModule,
                InstrMultiply, InstrObjectField, InstrPercent, InstrRightShift,
                InstrSetObjectField, InstrStoreModule, InstrSub,
            },
            stack_ptr::{BcSlotOut, BcSlotsInN, BcSlotsN},
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
        source: BcSlotsInN<2>,
        target: BcSlotOut,
        span: FrozenFileSpan,
        bc: &mut BcWriter,
    ) {
        let arg = (source, target);
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
                    bc.alloc_slots_c(|lhs_rhs, bc| {
                        let field = Symbol::new(field.as_str());
                        bc.write_instr::<InstrObjectField>(
                            span,
                            (object, field.clone(), lhs_rhs.get::<0>().to_out()),
                        );
                        rhs.write_bc(lhs_rhs.get::<1>().to_out(), bc);
                        op.write_bc(lhs_rhs.to_in(), lhs_rhs.get::<1>().to_out(), span, bc);
                        bc.write_instr::<InstrSetObjectField>(
                            span,
                            (lhs_rhs.get::<1>().to_in(), object, field),
                        );
                    })
                });
            }
            AssignModifyLhs::Array(ref array, ref index) => {
                bc.alloc_slots_c(|slots: BcSlotsN<4>, bc| {
                    let array_slot = slots.get::<0>();
                    let index_slot = slots.get::<1>();
                    let temp_slot = slots.get::<2>();
                    let rhs_slot = slots.get::<3>();

                    let array_index_slots: BcSlotsN<2> = BcSlotsN {
                        start: slots.get::<0>(),
                    };
                    let temp_rhs_slots: BcSlotsN<2> = BcSlotsN {
                        start: slots.get::<2>(),
                    };

                    array.write_bc(array_slot.to_out(), bc);
                    index.write_bc(index_slot.to_out(), bc);

                    bc.write_instr::<InstrArrayIndex>(
                        span,
                        (array_index_slots.to_in(), temp_slot.to_out()),
                    );
                    rhs.write_bc(rhs_slot.to_out(), bc);
                    op.write_bc(temp_rhs_slots.to_in(), temp_slot.to_out(), span, bc);
                    bc.write_instr::<InstrArrayIndexSet>(
                        span,
                        (array_index_slots.to_in(), temp_slot.to_in()),
                    );
                })
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

                op.write_bc(lhs_rhs.to_in(), lhs_rhs.get::<1>().to_out(), span, bc);
                match captured {
                    Captured::Yes => {
                        bc.write_store_local_captured(span, lhs_rhs.get::<1>().to_in(), slot)
                    }
                    Captured::No => bc.write_store_local(
                        span,
                        lhs_rhs.get::<1>().to_in(),
                        slot.to_bc_slot().to_out(),
                    ),
                }
            }),
            AssignModifyLhs::Module(m) => bc.alloc_slots_c(|lhs_rhs: BcSlotsN<2>, bc| {
                let slot = m.node;
                bc.write_instr::<InstrLoadModule>(span, (slot, lhs_rhs.get::<0>().to_out()));
                rhs.write_bc(lhs_rhs.get::<1>().to_out(), bc);
                op.write_bc(lhs_rhs.to_in(), lhs_rhs.get::<1>().to_out(), span, bc);
                let lhs_rhs = lhs_rhs.to_in();
                bc.write_instr::<InstrStoreModule>(span, (lhs_rhs.get::<1>(), slot));
            }),
        }
    }
}

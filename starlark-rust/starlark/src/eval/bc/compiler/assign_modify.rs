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
                InstrAddAssign, InstrArrayIndexNoPop, InstrArrayIndexSet, InstrBitAnd,
                InstrBitOrAssign, InstrBitXor, InstrDivide, InstrDup, InstrFloorDivide,
                InstrLeftShift, InstrLoadModule, InstrMultiply, InstrObjectField,
                InstrObjectSetField, InstrPercent, InstrRightShift, InstrStoreModule, InstrSub,
            },
            writer::BcWriter,
        },
        compiler::{expr::ExprCompiled, scope::Captured, span::IrSpanned, stmt::AssignModifyLhs},
        runtime::call_stack::FrozenFileSpan,
    },
    syntax::ast::AssignOp,
};

impl AssignOp {
    fn write_bc(self, span: FrozenFileSpan, bc: &mut BcWriter) {
        match self {
            AssignOp::Add => bc.write_instr::<InstrAddAssign>(span, ()),
            AssignOp::Subtract => bc.write_instr::<InstrSub>(span, ()),
            AssignOp::Multiply => bc.write_instr::<InstrMultiply>(span, ()),
            AssignOp::Divide => bc.write_instr::<InstrDivide>(span, ()),
            AssignOp::FloorDivide => bc.write_instr::<InstrFloorDivide>(span, ()),
            AssignOp::Percent => bc.write_instr::<InstrPercent>(span, ()),
            AssignOp::BitAnd => bc.write_instr::<InstrBitAnd>(span, ()),
            AssignOp::BitOr => bc.write_instr::<InstrBitOrAssign>(span, ()),
            AssignOp::BitXor => bc.write_instr::<InstrBitXor>(span, ()),
            AssignOp::LeftShift => bc.write_instr::<InstrLeftShift>(span, ()),
            AssignOp::RightShift => bc.write_instr::<InstrRightShift>(span, ()),
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
                let field = Symbol::new(field.as_str());
                object.write_bc(bc);
                bc.write_instr::<InstrDup>(span, ());
                bc.write_instr::<InstrObjectField>(span, field.clone());
                rhs.write_bc(bc);
                op.write_bc(span, bc);
                bc.write_instr::<InstrObjectSetField>(span, field);
            }
            AssignModifyLhs::Array(ref array, ref index) => {
                array.write_bc(bc);
                index.write_bc(bc);
                bc.write_instr::<InstrArrayIndexNoPop>(span, ());
                rhs.write_bc(bc);
                op.write_bc(span, bc);
                bc.write_instr::<InstrArrayIndexSet>(span, ());
            }
            AssignModifyLhs::Local(s) => {
                let (slot, captured) = s.node;
                match captured {
                    Captured::Yes => bc.write_load_local_captured(span, slot),
                    Captured::No => bc.write_load_local(span, slot),
                }
                rhs.write_bc(bc);
                op.write_bc(span, bc);
                match captured {
                    Captured::Yes => bc.write_store_local_captured(span, slot),
                    Captured::No => bc.write_store_local(span, slot),
                }
            }
            AssignModifyLhs::Module(m) => {
                let slot = m.node;
                bc.write_instr::<InstrLoadModule>(span, slot);
                rhs.write_bc(bc);
                op.write_bc(span, bc);
                bc.write_instr::<InstrStoreModule>(span, slot);
            }
        }
    }
}

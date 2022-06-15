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

//! Compile comprehensions.

use crate::eval::{
    bc::{
        compiler::{expr::write_n_exprs, if_compiler::write_if_then, stmt::write_for},
        instr_impl::{
            InstrComprDictInsert, InstrComprListAppend, InstrContinue, InstrDictNew, InstrListNew,
        },
        stack_ptr::BcSlotOut,
        writer::BcWriter,
    },
    compiler::{
        compr::{ClauseCompiled, ComprCompiled},
        expr::MaybeNot,
    },
    runtime::call_stack::FrozenFileSpan,
};

impl ClauseCompiled {
    fn write_bc(
        &self,
        bc: &mut BcWriter,
        rem: &[ClauseCompiled],
        term: impl FnOnce(&mut BcWriter),
    ) {
        write_for(&self.over, &self.var, self.over.span, bc, |bc| {
            for c in &self.ifs {
                write_if_then(
                    c,
                    MaybeNot::Not,
                    |bc| {
                        bc.write_instr::<InstrContinue>(c.span, ());
                    },
                    bc,
                );
            }

            match rem.split_last() {
                Some((first, rem)) => {
                    first.write_bc(bc, rem, term);
                }
                None => {
                    term(bc);
                }
            }
        })
    }
}

impl ComprCompiled {
    /// After evaluation of comprehension like `[(x, z) for x in y for z in w]`,
    /// we can mark `y` as definitely assigned.
    pub(crate) fn mark_definitely_assigned_after(&self, bc: &mut BcWriter) {
        let clauses = self.clauses();
        // We know that first loop argument is executed, and we don't know anything else.
        clauses
            .split_last()
            .0
            .over
            .mark_definitely_assigned_after(bc);
    }

    pub(crate) fn write_bc(&self, span: FrozenFileSpan, target: BcSlotOut, bc: &mut BcWriter) {
        bc.alloc_slot(|temp, bc| {
            match *self {
                ComprCompiled::List(box ref expr, ref clauses) => {
                    bc.write_instr::<InstrListNew>(span, temp.to_out());
                    let (first, rem) = clauses.split_last();
                    first.write_bc(bc, rem, |bc| {
                        expr.write_bc_cb(bc, |expr_slot, bc| {
                            bc.write_instr::<InstrComprListAppend>(
                                expr.span,
                                (temp.to_in(), expr_slot),
                            )
                        });
                    });
                }
                ComprCompiled::Dict(box (ref k, ref v), ref clauses) => {
                    bc.write_instr::<InstrDictNew>(span, temp.to_out());
                    let (first, rem) = clauses.split_last();
                    first.write_bc(bc, rem, |bc| {
                        write_n_exprs([k, v], bc, |k_v, bc| {
                            bc.write_instr::<InstrComprDictInsert>(k.span, (temp.to_in(), k_v));
                        });
                    });
                }
            };
            bc.write_mov(span, temp.to_in(), target);
        });
    }
}

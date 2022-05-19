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

//! Compile assignment lhs.

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        bc::{
            instr_arg::ArgPushesStack,
            instr_impl::{
                InstrSetArrayIndex, InstrSetObjectField, InstrStoreModuleAndExport, InstrUnpack,
            },
            writer::BcWriter,
        },
        compiler::{scope::Captured, span::IrSpanned, stmt::AssignCompiledValue},
    },
};

impl IrSpanned<AssignCompiledValue> {
    pub(crate) fn write_bc(&self, bc: &mut BcWriter) {
        let span = self.span;
        match self.node {
            AssignCompiledValue::Dot(ref object, ref field) => {
                object.write_bc(bc);
                let symbol = Symbol::new(field.as_str());
                bc.write_instr::<InstrSetObjectField>(span, symbol);
            }
            AssignCompiledValue::ArrayIndirection(ref array, ref index) => {
                array.write_bc(bc);
                index.write_bc(bc);
                bc.write_instr::<InstrSetArrayIndex>(span, ());
            }
            AssignCompiledValue::Tuple(ref xs) => {
                bc.write_instr::<InstrUnpack>(span, ArgPushesStack(xs.len() as u32));
                for x in xs {
                    x.write_bc(bc);
                }
            }
            AssignCompiledValue::Local(slot, Captured::No) => {
                bc.write_store_local(span, slot);
            }
            AssignCompiledValue::Local(slot, Captured::Yes) => {
                bc.write_store_local_captured(span, slot);
            }
            AssignCompiledValue::Module(slot, ref name) => {
                bc.write_instr::<InstrStoreModuleAndExport>(span, (slot, name.clone()));
            }
        }
    }
}

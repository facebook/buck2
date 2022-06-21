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

//! Instruction representation in memory.

use std::mem;

use crate::eval::bc::instr::BcInstr;
use crate::eval::bc::opcode::BcOpcode;
use crate::eval::bc::opcode::BcOpcodeHandler;
use crate::values::FrozenValue;

/// All instructions must be word aligned.
pub(crate) const BC_INSTR_ALIGN: usize = mem::align_of::<FrozenValue>();

/// Instruction header.
#[repr(C)]
pub(crate) struct BcInstrHeader {
    pub(crate) opcode: BcOpcode,
}

/// How instructions are stored in memory.
#[repr(C)]
pub(crate) struct BcInstrRepr<I: BcInstr> {
    pub(crate) header: BcInstrHeader,
    pub(crate) arg: I::Arg,
    // Align all instructions to make IP increment simple.
    pub(crate) _align: [FrozenValue; 0],
}

impl<I: BcInstr> BcInstrRepr<I> {
    pub(crate) fn new(arg: I::Arg) -> BcInstrRepr<I> {
        BcInstrRepr::<I>::assert_align();
        BcInstrRepr {
            header: BcInstrHeader {
                opcode: BcOpcode::for_instr::<I>(),
            },
            arg,
            _align: [],
        }
    }

    pub(crate) const fn assert_align() {
        // If alignment is different, we do not compute addresses properly.
        // Practically everything has `usize` alignment.
        // This would break if we had some types like `__m128` in instruction arguments,
        // but we don't.
        assert!(mem::align_of::<BcInstrRepr<I>>() == BC_INSTR_ALIGN);
        assert!(mem::size_of::<BcInstrRepr<I>>() % BC_INSTR_ALIGN == 0);
    }
}

impl BcOpcode {
    /// Size of instruction.
    pub(crate) fn size_of_repr(self) -> usize {
        struct HandlerImpl;

        impl BcOpcodeHandler<usize> for HandlerImpl {
            fn handle<I: BcInstr>(self) -> usize {
                <BcInstrRepr<I>>::assert_align();

                mem::size_of::<BcInstrRepr<I>>()
            }
        }

        self.dispatch(HandlerImpl)
    }
}

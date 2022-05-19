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

//! Define the bytecode instruction.

use crate::{
    eval::{
        bc::{
            addr::BcPtrAddr, instr_arg::BcInstrArg, stack_ptr::BcStackPtr,
            stack_values::BcStackValues,
        },
        Evaluator,
    },
    values::Value,
};

/// Result of instruction evaluation.
// This is more efficient than `Result<R, EvalException>`,
// see in compiler explorer: https://rust.godbolt.org/z/81rj4jb4M
#[must_use]
pub(crate) enum InstrControl<'v, 'b> {
    /// Go to address.
    Next(BcPtrAddr<'b>),
    /// Return from the function.
    Return(Value<'v>),
    /// Go to the next loop iteration.
    ///
    /// Note there's no range check for blocks, so each loop body ends with continue instruction.
    LoopContinue,
    /// Break from the loop.
    LoopBreak,
    /// Error.
    Err(anyhow::Error),
}

pub(crate) trait BcInstr: Sized + 'static {
    /// Values this instruction pops off the stack.
    type Pop<'v>: BcStackValues<'v>;
    /// Values this instruction pushes on the stack.
    type Push<'v>: BcStackValues<'v>;
    /// Fixed instruction argument (which may encode additional arguments
    /// pushed or popped from the stack by the instruction implementation).
    type Arg: BcInstrArg;

    /// How many values popped off the stack? This is used to compute the stack size.
    fn npops(arg: &Self::Arg) -> u32 {
        Self::Pop::<'static>::N + BcInstrArg::pops_stack(arg)
    }

    /// How many values pushed on the stack? This is used to compute the stack size.
    fn npushs(arg: &Self::Arg) -> u32 {
        Self::Push::<'static>::N + BcInstrArg::pushes_stack(arg)
    }

    /// Execute the instruction.
    fn run<'v, 'b>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        arg: &Self::Arg,
    ) -> InstrControl<'v, 'b>;
}

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

//! Bytecode writer.

use std::cmp;

use crate::{
    eval::{
        bc::{
            addr::{BcAddr, BcAddrOffset},
            bytecode::Bc,
            instr::BcInstr,
            instr_impl::{
                InstrBr, InstrConst, InstrContinue, InstrForLoop, InstrIfBr, InstrIfNotBr,
                InstrLoadLocal, InstrLoadLocalCaptured, InstrMov, InstrProfileBc,
                InstrStoreLocalCaptured,
            },
            instrs::{BcInstrsWriter, PatchAddr},
            opcode::BcOpcode,
            slow_arg::BcInstrSlowArg,
            stack_ptr::{BcSlot, BcSlotRange, BcSlotsN},
        },
        runtime::{call_stack::FrozenFileSpan, slots::LocalSlotId},
    },
    values::{FrozenHeap, FrozenRef, FrozenValue},
};

/// Write bytecode here.
pub(crate) struct BcWriter<'f> {
    /// Insert bytecode profiling instructions.
    profile: bool,

    /// Serialized instructions.
    instrs: BcInstrsWriter,
    /// Instruction spans, used for errors.
    slow_args: Vec<(BcAddr, BcInstrSlowArg)>,
    /// Current stack size.
    stack_size: u32,
    /// Local slot count.
    local_count: u32,
    /// Function parameter count.
    param_count: u32,
    /// Max observed stack size.
    max_stack_size: u32,

    /// Allocate various objects here.
    pub(crate) heap: &'f FrozenHeap,
}

impl<'f> BcWriter<'f> {
    /// Empty.
    pub(crate) fn new(
        profile: bool,
        local_count: u32,
        param_count: u32,
        heap: &'f FrozenHeap,
    ) -> BcWriter<'f> {
        assert!(param_count <= local_count);
        BcWriter {
            profile,
            instrs: BcInstrsWriter::new(),
            slow_args: Vec::new(),
            stack_size: 0,
            local_count,
            param_count,
            max_stack_size: 0,
            heap,
        }
    }

    /// Finish writing the bytecode.
    pub(crate) fn finish(self) -> Bc {
        let BcWriter {
            profile: has_before_instr,
            instrs,
            slow_args: spans,
            stack_size,
            local_count,
            param_count,
            max_stack_size,
            heap,
        } = self;
        let _ = has_before_instr;
        let _ = heap;
        let _ = param_count;
        assert_eq!(stack_size, 0);
        Bc {
            instrs: instrs.finish(spans),
            local_count,
            max_stack_size,
        }
    }

    /// Current offset.
    fn ip(&self) -> BcAddr {
        self.instrs.ip()
    }

    /// Version of instruction write with explicit slow arg arg.
    fn do_write_generic_explicit<I: BcInstr>(
        &mut self,
        slow_arg: BcInstrSlowArg,
        arg: I::Arg,
    ) -> (BcAddr, *const I::Arg) {
        if self.profile {
            // This instruction does not fail, so do not write span for it.
            self.instrs
                .write::<InstrProfileBc>(BcOpcode::for_instr::<I>());
        }
        self.slow_args.push((self.ip(), slow_arg));
        self.instrs.write::<I>(arg)
    }

    /// Write an instruction, return address and argument.
    fn write_instr_ret_arg_explicit<I: BcInstr>(
        &mut self,
        slow_arg: BcInstrSlowArg,
        arg: I::Arg,
    ) -> (BcAddr, *const I::Arg) {
        self.do_write_generic_explicit::<I>(slow_arg, arg)
    }

    fn write_instr_ret_arg<I: BcInstr>(
        &mut self,
        span: FrozenFileSpan,
        arg: I::Arg,
    ) -> (BcAddr, *const I::Arg) {
        self.write_instr_ret_arg_explicit::<I>(
            BcInstrSlowArg {
                span,
                ..Default::default()
            },
            arg,
        )
    }

    pub(crate) fn write_instr_explicit<I: BcInstr>(
        &mut self,
        slow_arg: BcInstrSlowArg,
        arg: I::Arg,
    ) {
        self.write_instr_ret_arg_explicit::<I>(slow_arg, arg);
    }

    /// Write an instruction.
    pub(crate) fn write_instr<I: BcInstr>(&mut self, span: FrozenFileSpan, arg: I::Arg) {
        self.write_instr_explicit::<I>(
            BcInstrSlowArg {
                span,
                ..Default::default()
            },
            arg,
        );
    }

    /// Write load constant instruction.
    pub(crate) fn write_const(&mut self, span: FrozenFileSpan, value: FrozenValue, slot: BcSlot) {
        assert!(slot.0 < self.local_count + self.stack_size);

        self.write_instr::<InstrConst>(span, (value, slot));
    }

    /// Write load local instruction.
    pub(crate) fn write_load_local(
        &mut self,
        span: FrozenFileSpan,
        slot: LocalSlotId,
        target: BcSlot,
    ) {
        assert!(slot.0 < self.local_count);

        self.write_instr::<InstrLoadLocal>(span, (slot, target));
    }

    pub(crate) fn write_load_local_captured(
        &mut self,
        span: FrozenFileSpan,
        source: LocalSlotId,
        target: BcSlot,
    ) {
        assert!(source.0 < self.local_count);
        assert!(target.0 < self.local_count + self.stack_size);
        self.write_instr_ret_arg::<InstrLoadLocalCaptured>(span, (source, target));
    }

    pub(crate) fn write_store_local(
        &mut self,
        span: FrozenFileSpan,
        source: BcSlot,
        target: BcSlot,
    ) {
        assert!(source.0 < self.local_count + self.stack_size);
        assert!(target.0 < self.local_count + self.stack_size);
        self.write_instr_ret_arg::<InstrMov>(span, (source, target));
    }

    pub(crate) fn write_store_local_captured(
        &mut self,
        span: FrozenFileSpan,
        source: BcSlot,
        target: LocalSlotId,
    ) {
        assert!(source.0 < self.local_count + self.stack_size);
        assert!(target.0 < self.local_count);
        self.write_instr_ret_arg::<InstrStoreLocalCaptured>(span, (source, target));
    }

    /// Patch previously writted address with current IP.
    pub(crate) fn patch_addr(&mut self, addr: PatchAddr) {
        self.instrs.patch_addr(addr);
    }

    pub(crate) fn patch_addrs(&mut self, addrs: Vec<PatchAddr>) {
        for adds in addrs {
            self.patch_addr(adds);
        }
    }

    /// Write branch.
    pub(crate) fn write_br(&mut self, span: FrozenFileSpan) -> PatchAddr {
        let (addr, arg) = self.write_instr_ret_arg::<InstrBr>(span, BcAddrOffset::FORWARD);
        self.instrs.addr_to_patch(addr, arg)
    }

    /// Write conditional branch.
    pub(crate) fn write_if_not_br(&mut self, cond: BcSlot, span: FrozenFileSpan) -> PatchAddr {
        let (addr, arg) =
            self.write_instr_ret_arg::<InstrIfNotBr>(span, (cond, BcAddrOffset::FORWARD));
        self.instrs.addr_to_patch(addr, unsafe { &(*arg).1 })
    }

    /// Write conditional branch.
    pub(crate) fn write_if_br(&mut self, cond: BcSlot, span: FrozenFileSpan) -> PatchAddr {
        let (addr, arg) =
            self.write_instr_ret_arg::<InstrIfBr>(span, (cond, BcAddrOffset::FORWARD));
        self.instrs.addr_to_patch(addr, unsafe { &(*arg).1 })
    }

    /// Write if-else block.
    pub(crate) fn write_if_else(
        &mut self,
        cond: BcSlot,
        span: FrozenFileSpan,
        then_block: impl FnOnce(&mut Self),
        else_block: impl FnOnce(&mut Self),
    ) {
        let else_target = self.write_if_not_br(cond, span);
        then_block(self);
        let end_target = self.write_br(span);
        self.patch_addr(else_target);
        else_block(self);
        self.patch_addr(end_target);
    }

    /// Write for loop.
    pub(crate) fn write_for(
        &mut self,
        over: BcSlot,
        var: BcSlot,
        span: FrozenFileSpan,
        body: impl FnOnce(&mut Self),
    ) {
        let (addr, arg) =
            self.write_instr_ret_arg::<InstrForLoop>(span, (over, var, BcAddrOffset::FORWARD));
        let end_patch = self.instrs.addr_to_patch(addr, unsafe { &(*arg).2 });
        body(self);
        self.write_instr::<InstrContinue>(span, ());
        self.patch_addr(end_patch);
    }

    fn stack_add(&mut self, add: u32) {
        self.stack_size += add;
        self.max_stack_size = cmp::max(self.max_stack_size, self.stack_size);
    }

    fn stack_sub(&mut self, sub: u32) {
        assert!(self.stack_size >= sub);
        self.stack_size -= sub;
    }

    pub(crate) fn is_definitely_assigned(&self, local: LocalSlotId) -> bool {
        assert!(local.0 < self.local_count);
        local.0 < self.param_count
    }

    /// Allocate a temporary slot, and call a callback.
    ///
    /// The slot is valid during the callback run, and can be reused later.
    pub(crate) fn alloc_slot<R>(&mut self, k: impl FnOnce(BcSlot, &mut BcWriter) -> R) -> R {
        let slot = BcSlot(self.local_count + self.stack_size);
        self.stack_add(1);
        let r = k(slot, self);
        self.stack_sub(1);
        r
    }

    /// Allocate several slots for the duration of callback run.
    pub(crate) fn alloc_slots<R>(
        &mut self,
        count: u32,
        k: impl FnOnce(BcSlotRange, &mut BcWriter) -> R,
    ) -> R {
        let slots = BcSlotRange {
            start: BcSlot(self.local_count + self.stack_size),
            end: BcSlot(self.local_count + self.stack_size + count),
        };
        self.stack_add(count);
        let r = k(slots, self);
        self.stack_sub(count);
        r
    }

    /// Allocate several slots.
    pub(crate) fn alloc_slots_c<const N: usize, R>(
        &mut self,
        k: impl FnOnce(BcSlotsN<N>, &mut BcWriter) -> R,
    ) -> R {
        self.alloc_slots(N as u32, |slots, bc| k(BcSlotsN::from_range(slots), bc))
    }

    /// Allocate several slots for typical compilation of several expressions.
    pub(crate) fn alloc_slots_for_exprs<K, R>(
        &mut self,
        // Iterate over the elements.
        exprs: impl IntoIterator<Item = K>,
        // Invoke a callback which fills the slots.
        mut expr: impl FnMut(BcSlot, K, &mut BcWriter),
        // And then invoke a callback which consumes all the slots again together.
        k: impl FnOnce(BcSlotRange, &mut BcWriter) -> R,
    ) -> R {
        let start = BcSlot(self.local_count + self.stack_size);
        let mut end = start;
        for item in exprs {
            self.stack_add(1);
            // `expr` callback may allocate more temporary slots,
            // but they are released after the callback returns.
            // So resulting slots are sequential.
            expr(end, item, self);
            end = BcSlot(end.0 + 1);
        }
        let r = k(BcSlotRange { start, end }, self);
        self.stack_sub(end.0 - start.0);
        r
    }

    pub(crate) fn alloc_file_span(
        &self,
        span: FrozenFileSpan,
    ) -> FrozenRef<'static, FrozenFileSpan> {
        self.heap.alloc_any(span)
    }
}

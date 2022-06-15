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
            definitely_assigned::BcDefinitelyAssigned,
            instr::BcInstr,
            instr_impl::{
                InstrBr, InstrConst, InstrContinue, InstrForLoop, InstrIfBr, InstrIfNotBr,
                InstrLoadLocal, InstrLoadLocalCaptured, InstrMov, InstrProfileBc,
                InstrStoreLocalCaptured,
            },
            instrs::{BcInstrsWriter, PatchAddr},
            opcode::BcOpcode,
            slow_arg::BcInstrSlowArg,
            stack_ptr::{BcSlot, BcSlotIn, BcSlotInRange, BcSlotOut, BcSlotRange, BcSlotsN},
        },
        compiler::expr::MaybeNot,
        runtime::{call_stack::FrozenFileSpan, slots::LocalSlotId},
    },
    values::{FrozenHeap, FrozenRef, FrozenValue},
};

/// Write bytecode here.
pub(crate) struct BcWriter<'f> {
    /// Insert bytecode profiling instructions.
    profile: bool,
    /// Insert `RecordCallEnter`/`RecordCallExit` instructions.
    record_call_enter_exit: bool,

    /// Serialized instructions.
    instrs: BcInstrsWriter,
    /// Instruction spans, used for errors.
    slow_args: Vec<(BcAddr, BcInstrSlowArg)>,
    /// Current stack size.
    stack_size: u32,
    /// Local slot count.
    local_count: u32,
    /// Local variables which are known to be definitely assigned at current program point.
    definitely_assigned: BcDefinitelyAssigned,
    /// Max observed stack size.
    max_stack_size: u32,

    /// Allocate various objects here.
    pub(crate) heap: &'f FrozenHeap,
}

impl<'f> BcWriter<'f> {
    /// Empty.
    pub(crate) fn new(
        profile: bool,
        call_enter_exit: bool,
        local_count: u32,
        param_count: u32,
        heap: &'f FrozenHeap,
    ) -> BcWriter<'f> {
        assert!(param_count <= local_count);
        let mut definitely_assigned = BcDefinitelyAssigned::new(local_count);
        for i in 0..param_count {
            definitely_assigned.mark_definitely_assigned(LocalSlotId(i));
        }
        BcWriter {
            profile,
            record_call_enter_exit: call_enter_exit,
            instrs: BcInstrsWriter::new(),
            slow_args: Vec::new(),
            stack_size: 0,
            local_count,
            definitely_assigned,
            max_stack_size: 0,
            heap,
        }
    }

    /// Finish writing the bytecode.
    #[allow(clippy::let_underscore_drop)]
    pub(crate) fn finish(self) -> Bc {
        let BcWriter {
            profile: has_before_instr,
            record_call_enter_exit: call_enter_exit,
            instrs,
            slow_args: spans,
            stack_size,
            local_count,
            definitely_assigned,
            max_stack_size,
            heap,
        } = self;
        let _ = has_before_instr;
        let _ = call_enter_exit;
        let _ = heap;
        let _ = definitely_assigned;
        assert_eq!(stack_size, 0);
        Bc {
            instrs: instrs.finish(spans),
            local_count,
            max_stack_size,
        }
    }

    pub(crate) fn record_call_enter_exit(&self) -> bool {
        self.record_call_enter_exit
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
    pub(crate) fn write_const(
        &mut self,
        span: FrozenFileSpan,
        value: FrozenValue,
        slot: BcSlotOut,
    ) {
        assert!(slot.get().0 < self.local_count + self.stack_size);

        self.write_instr::<InstrConst>(span, (value, slot));
    }

    /// Write load local instruction.
    pub(crate) fn write_load_local(
        &mut self,
        span: FrozenFileSpan,
        slot: LocalSlotId,
        target: BcSlotOut,
    ) {
        assert!(slot.0 < self.local_count);

        if let Some(slot) = self.try_definitely_assigned(slot) {
            self.write_instr::<InstrMov>(span, (slot, target));
        } else {
            self.write_instr::<InstrLoadLocal>(span, (slot, target));
        }
    }

    pub(crate) fn write_load_local_captured(
        &mut self,
        span: FrozenFileSpan,
        source: LocalSlotId,
        target: BcSlotOut,
    ) {
        assert!(source.0 < self.local_count);
        assert!(target.get().0 < self.local_count + self.stack_size);
        self.write_instr_ret_arg::<InstrLoadLocalCaptured>(span, (source, target));
    }

    pub(crate) fn write_mov(&mut self, span: FrozenFileSpan, source: BcSlotIn, target: BcSlotOut) {
        assert!(source.get().0 < self.local_count + self.stack_size);
        assert!(target.get().0 < self.local_count + self.stack_size);
        self.write_instr_ret_arg::<InstrMov>(span, (source, target));
    }

    pub(crate) fn write_store_local_captured(
        &mut self,
        span: FrozenFileSpan,
        source: BcSlotIn,
        target: LocalSlotId,
    ) {
        assert!(source.get().0 < self.local_count + self.stack_size);
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
    pub(crate) fn write_if_not_br(&mut self, cond: BcSlotIn, span: FrozenFileSpan) -> PatchAddr {
        let (addr, arg) =
            self.write_instr_ret_arg::<InstrIfNotBr>(span, (cond, BcAddrOffset::FORWARD));
        self.instrs.addr_to_patch(addr, unsafe { &(*arg).1 })
    }

    /// Write conditional branch.
    pub(crate) fn write_if_br(&mut self, cond: BcSlotIn, span: FrozenFileSpan) -> PatchAddr {
        let (addr, arg) =
            self.write_instr_ret_arg::<InstrIfBr>(span, (cond, BcAddrOffset::FORWARD));
        self.instrs.addr_to_patch(addr, unsafe { &(*arg).1 })
    }

    fn write_if_else_impl(
        &mut self,
        cond: BcSlotIn,
        span: FrozenFileSpan,
        then_block: impl FnOnce(&mut Self),
        else_block: impl FnOnce(&mut Self),
    ) {
        let definitely_assigned = self.save_definitely_assigned();

        let else_target = self.write_if_not_br(cond, span);
        then_block(self);
        let end_target = self.write_br(span);

        self.restore_definitely_assigned(definitely_assigned.clone());

        self.patch_addr(else_target);
        else_block(self);
        self.patch_addr(end_target);

        self.restore_definitely_assigned(definitely_assigned);
    }

    /// Write if-else block.
    pub(crate) fn write_if_else(
        &mut self,
        cond: BcSlotIn,
        maybe_not: MaybeNot,
        span: FrozenFileSpan,
        then_block: impl FnOnce(&mut Self),
        else_block: impl FnOnce(&mut Self),
    ) {
        match maybe_not {
            MaybeNot::Id => self.write_if_else_impl(cond, span, then_block, else_block),
            MaybeNot::Not => self.write_if_else_impl(cond, span, else_block, then_block),
        }
    }

    /// Write for loop.
    pub(crate) fn write_for(
        &mut self,
        over: BcSlotIn,
        var: BcSlotOut,
        span: FrozenFileSpan,
        body: impl FnOnce(&mut Self),
    ) {
        // Definitely assigned save/restore is redundant here, it is performed more precisely
        // by the caller. But it is safer to do it here anyway.
        let definitely_assigned = self.save_definitely_assigned();

        let (addr, arg) =
            self.write_instr_ret_arg::<InstrForLoop>(span, (over, var, BcAddrOffset::FORWARD));
        let end_patch = self.instrs.addr_to_patch(addr, unsafe { &(*arg).2 });
        body(self);
        self.write_instr::<InstrContinue>(span, ());
        self.patch_addr(end_patch);

        self.restore_definitely_assigned(definitely_assigned);
    }

    fn stack_add(&mut self, add: u32) {
        self.stack_size += add;
        self.max_stack_size = cmp::max(self.max_stack_size, self.stack_size);
    }

    fn stack_sub(&mut self, sub: u32) {
        assert!(self.stack_size >= sub);
        self.stack_size -= sub;
    }

    /// Convert local variable to BC slot if it is known to be definitely assigned
    /// at this execution point.
    pub(crate) fn try_definitely_assigned(&self, local: LocalSlotId) -> Option<BcSlotIn> {
        assert!(local.0 < self.local_count);
        if self.definitely_assigned.is_definitely_assigned(local) {
            Some(local.to_bc_slot().to_in())
        } else {
            None
        }
    }

    pub(crate) fn mark_definitely_assigned(&mut self, local: LocalSlotId) {
        self.definitely_assigned.mark_definitely_assigned(local);
    }

    pub(crate) fn save_definitely_assigned(&self) -> BcDefinitelyAssigned {
        self.definitely_assigned.clone()
    }

    pub(crate) fn restore_definitely_assigned(&mut self, saved: BcDefinitelyAssigned) {
        saved.assert_smaller_then(&self.definitely_assigned);
        self.definitely_assigned = saved;
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
        k: impl FnOnce(BcSlotInRange, &mut BcWriter) -> R,
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
        let range = if end == start {
            // This is not really necessary, empty range is equally valid
            // with any starting point, but this makes bytecode output
            // (in particular, in golden tests) more readable.
            BcSlotInRange::default()
        } else {
            BcSlotRange { start, end }.to_in()
        };
        let r = k(range, self);
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

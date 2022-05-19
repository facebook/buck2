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

use std::{cmp, mem};

use crate::{
    eval::{
        bc::{
            addr::{BcAddr, BcAddrOffset},
            bytecode::Bc,
            instr::BcInstr,
            instr_impl::{
                InstrBr, InstrConst, InstrConst2, InstrConst3, InstrConst4, InstrContinue,
                InstrForLoop, InstrIfBr, InstrIfNotBr, InstrLoadLocal, InstrLoadLocal2,
                InstrLoadLocal3, InstrLoadLocal4, InstrLoadLocalAndConst, InstrLoadLocalCaptured,
                InstrProfileBc, InstrStoreLocal, InstrStoreLocalCaptured,
            },
            instrs::{BcInstrsWriter, PatchAddr},
            opcode::BcOpcode,
            slow_arg::BcInstrSlowArg,
        },
        compiler::span::IrSpanned,
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
    /// Max observed stack size.
    max_stack_size: u32,

    // Consts are queued after locals
    queued_locals: Vec<IrSpanned<LocalSlotId>>,
    queued_consts: Vec<IrSpanned<FrozenValue>>,

    /// Allocate various objects here.
    heap: &'f FrozenHeap,
}

impl<'f> BcWriter<'f> {
    /// Empty.
    pub(crate) fn new(profile: bool, local_count: u32, heap: &'f FrozenHeap) -> BcWriter<'f> {
        BcWriter {
            profile,
            instrs: BcInstrsWriter::new(),
            slow_args: Vec::new(),
            stack_size: 0,
            local_count,
            max_stack_size: 0,
            queued_consts: Vec::new(),
            queued_locals: Vec::new(),
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
            max_stack_size,
            queued_consts,
            queued_locals,
            heap,
        } = self;
        let _ = has_before_instr;
        let _ = heap;
        assert!(queued_locals.is_empty());
        assert!(queued_consts.is_empty());
        assert_eq!(stack_size, 0);
        Bc {
            instrs: instrs.finish(spans),
            local_count,
            max_stack_size,
        }
    }

    /// Current offset.
    fn ip(&self) -> BcAddr {
        assert!(self.queued_consts.is_empty());
        self.instrs.ip()
    }

    /// Flush queued consts and locals instructions.
    fn flush_instrs(&mut self) {
        let mut queued_locals = mem::take(&mut self.queued_locals);
        let mut queued_consts = mem::take(&mut self.queued_consts);
        let mut locals_slice = queued_locals.as_slice();
        let mut consts_slice = queued_consts.as_slice();

        while let [l0, l1, l2, l3, rem @ ..] = locals_slice {
            self.do_write_generic_explicit::<InstrLoadLocal4>(
                BcInstrSlowArg {
                    span: l0.span.merge(&l1.span).merge(&l2.span).merge(&l3.span),
                    spans: vec![l0.span, l1.span, l2.span, l3.span],
                },
                [l0.node, l1.node, l2.node, l3.node],
            );
            locals_slice = rem;
        }
        match (locals_slice, consts_slice) {
            ([], _) => {}
            ([l0], [c0, rem @ ..]) => {
                self.do_write_generic::<InstrLoadLocalAndConst>(l0.span, (l0.node, c0.node));
                consts_slice = rem;
            }
            ([l0], _) => {
                self.do_write_generic::<InstrLoadLocal>(l0.span, l0.node);
            }
            ([l0, l1], _) => {
                self.do_write_generic_explicit::<InstrLoadLocal2>(
                    BcInstrSlowArg {
                        span: l0.span.merge(&l1.span),
                        spans: vec![l0.span, l1.span],
                    },
                    [l0.node, l1.node],
                );
            }
            ([l0, l1, l2], _) => {
                self.do_write_generic_explicit::<InstrLoadLocal3>(
                    BcInstrSlowArg {
                        span: l0.span.merge(&l1.span).merge(&l2.span),
                        spans: vec![l0.span, l1.span, l2.span],
                    },
                    [l0.node, l1.node, l2.node],
                );
            }
            _ => unreachable!(),
        }

        while let [v0, v1, v2, v3, rem @ ..] = consts_slice {
            self.do_write_generic::<InstrConst4>(
                v0.span.merge(&v1.span).merge(&v2.span).merge(&v3.span),
                [v0.node, v1.node, v2.node, v3.node],
            );
            consts_slice = rem;
        }
        match consts_slice {
            [] => {}
            [v0] => {
                self.do_write_generic::<InstrConst>(v0.span, v0.node);
            }
            [v0, v1] => {
                self.do_write_generic::<InstrConst2>(v0.span.merge(&v1.span), [v0.node, v1.node]);
            }
            [v0, v1, v2] => {
                self.do_write_generic::<InstrConst3>(
                    v0.span.merge(&v1.span).merge(&v2.span),
                    [v0.node, v1.node, v2.node],
                );
            }
            _ => unreachable!(),
        }

        queued_locals.clear();
        queued_consts.clear();
        self.queued_locals = queued_locals;
        self.queued_consts = queued_consts;
    }

    /// Update stack size after the instruction.
    fn update_stack_size<I: BcInstr>(&mut self, arg: &I::Arg) {
        self.stack_sub(I::npops(arg));
        self.stack_add(I::npushs(arg));
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

    /// Actually write the instruction.
    fn do_write_generic<I: BcInstr>(
        &mut self,
        span: FrozenFileSpan,
        arg: I::Arg,
    ) -> (BcAddr, *const I::Arg) {
        self.do_write_generic_explicit::<I>(
            BcInstrSlowArg {
                span,
                ..Default::default()
            },
            arg,
        )
    }

    /// Write an instruction, return address and argument.
    fn write_instr_ret_arg_explicit<I: BcInstr>(
        &mut self,
        slow_arg: BcInstrSlowArg,
        arg: I::Arg,
    ) -> (BcAddr, *const I::Arg) {
        // Local and const instructions must be queued.
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::Const);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::Const2);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::Const3);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::Const4);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocal);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocal2);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocal3);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocal4);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocalAndConst);
        self.flush_instrs();
        self.update_stack_size::<I>(&arg);
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
        // Load and store local instructions need be written with custom functions.
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::LoadLocalCaptured);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::StoreLocal);
        assert_ne!(BcOpcode::for_instr::<I>(), BcOpcode::StoreLocalCaptured);

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
    pub(crate) fn write_const(&mut self, span: FrozenFileSpan, value: FrozenValue) {
        // Do not write it yet, queue it, so we could batch it.
        self.queued_consts.push(IrSpanned { node: value, span });
        self.stack_add(1);
    }

    /// Write load local instruction.
    pub(crate) fn write_load_local(&mut self, span: FrozenFileSpan, slot: LocalSlotId) {
        assert!(slot.0 < self.local_count);

        // Consts must be queued after locals, so if any consts are queued, flush them.
        if !self.queued_consts.is_empty() {
            self.flush_instrs();
        }
        // Do not write it yet, queue it, so we could batch it.
        self.queued_locals.push(IrSpanned { node: slot, span });
        self.stack_add(1);
    }

    pub(crate) fn write_load_local_captured(&mut self, span: FrozenFileSpan, slot: LocalSlotId) {
        assert!(slot.0 < self.local_count);
        self.write_instr_ret_arg::<InstrLoadLocalCaptured>(span, slot);
    }

    pub(crate) fn write_store_local(&mut self, span: FrozenFileSpan, slot: LocalSlotId) {
        assert!(slot.0 < self.local_count);
        self.write_instr_ret_arg::<InstrStoreLocal>(span, slot);
    }

    pub(crate) fn write_store_local_captured(&mut self, span: FrozenFileSpan, slot: LocalSlotId) {
        assert!(slot.0 < self.local_count);
        self.write_instr_ret_arg::<InstrStoreLocalCaptured>(span, slot);
    }

    /// Patch previously writted address with current IP.
    pub(crate) fn patch_addr(&mut self, addr: PatchAddr) {
        self.flush_instrs();
        self.instrs.patch_addr(addr);
    }

    pub(crate) fn patch_addrs(&mut self, addrs: Vec<PatchAddr>) {
        for adds in addrs {
            self.patch_addr(adds);
        }
    }

    /// Write branch.
    pub(crate) fn write_br(&mut self, span: FrozenFileSpan) -> PatchAddr {
        let arg = self.write_instr_ret_arg::<InstrBr>(span, BcAddrOffset::FORWARD);
        self.instrs.addr_to_patch(arg)
    }

    /// Write conditional branch.
    pub(crate) fn write_if_not_br(&mut self, span: FrozenFileSpan) -> PatchAddr {
        let arg = self.write_instr_ret_arg::<InstrIfNotBr>(span, BcAddrOffset::FORWARD);
        self.instrs.addr_to_patch(arg)
    }

    /// Write conditional branch.
    pub(crate) fn write_if_br(&mut self, span: FrozenFileSpan) -> PatchAddr {
        let arg = self.write_instr_ret_arg::<InstrIfBr>(span, BcAddrOffset::FORWARD);
        self.instrs.addr_to_patch(arg)
    }

    /// Write if block.
    pub(crate) fn write_if(&mut self, span: FrozenFileSpan, then_block: impl FnOnce(&mut Self)) {
        let patch_addr = self.write_if_not_br(span);
        then_block(self);
        self.patch_addr(patch_addr);
    }

    /// Write if block.
    pub(crate) fn write_if_not(
        &mut self,
        span: FrozenFileSpan,
        then_block: impl FnOnce(&mut Self),
    ) {
        let patch_addr = self.write_if_br(span);
        then_block(self);
        self.patch_addr(patch_addr);
    }

    /// Write for loop.
    pub(crate) fn write_for(&mut self, span: FrozenFileSpan, body: impl FnOnce(&mut Self)) {
        let arg = self.write_instr_ret_arg::<InstrForLoop>(span, BcAddrOffset::FORWARD);
        let end_patch = self.instrs.addr_to_patch(arg);
        let ss = self.stack_size();
        body(self);
        assert!(
            self.stack_size() + 1 == ss,
            "Loop body must consume stack variable"
        );
        self.write_instr::<InstrContinue>(span, ());
        self.patch_addr(end_patch);
    }

    pub(crate) fn stack_add(&mut self, add: u32) {
        self.stack_size += add;
        self.max_stack_size = cmp::max(self.max_stack_size, self.stack_size);
    }

    pub(crate) fn stack_sub(&mut self, sub: u32) {
        assert!(self.stack_size >= sub);
        self.stack_size -= sub;
    }

    /// Current stack size.
    pub(crate) fn stack_size(&self) -> u32 {
        self.stack_size
    }

    pub(crate) fn alloc_file_span(
        &self,
        span: FrozenFileSpan,
    ) -> FrozenRef<'static, FrozenFileSpan> {
        self.heap.alloc_any(span)
    }
}

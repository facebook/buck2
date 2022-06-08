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

//! Compile function calls.

use either::Either;

use crate::{
    collections::symbol_map::Symbol,
    eval::{
        bc::{
            call::{BcCallArgsFull, BcCallArgsPos},
            compiler::expr::{write_expr_opt, write_exprs},
            instr_impl::{
                InstrCall, InstrCallFrozen, InstrCallFrozenDef, InstrCallFrozenDefPos,
                InstrCallFrozenNative, InstrCallFrozenNativePos, InstrCallFrozenPos,
                InstrCallMaybeKnownMethod, InstrCallMaybeKnownMethodPos, InstrCallMethod,
                InstrCallMethodPos, InstrCallPos, InstrLen, InstrObjectFieldRaw,
                InstrRecordCallEnter, InstrRecordCallExit, InstrType,
            },
            native_function::BcNativeFunction,
            stack_ptr::{BcSlotIn, BcSlotOut},
            writer::BcWriter,
        },
        compiler::{
            args::ArgsCompiledValue, call::CallCompiled, def::FrozenDef, expr::ExprCompiled,
            span::IrSpanned,
        },
        runtime::call_stack::FrozenFileSpan,
    },
    values::{
        function::NativeFunction, types::known_methods::get_known_method, FrozenValue,
        FrozenValueTyped,
    },
};

impl ArgsCompiledValue {
    /// After evaluation of function arguments like `foo(a, b=c[d], **e)`,
    /// variables `a`, `b`, `c`, `d`, and `e` are definitely assigned.
    fn mark_definitely_assigned_after(&self, bc: &mut BcWriter) {
        let ArgsCompiledValue {
            pos_named,
            names,
            args,
            kwargs,
        } = self;
        for n in pos_named {
            n.mark_definitely_assigned_after(bc);
        }
        let _ = names;
        if let Some(args) = args {
            args.mark_definitely_assigned_after(bc);
        }
        if let Some(kwargs) = kwargs {
            kwargs.mark_definitely_assigned_after(bc);
        }
    }

    fn write_bc(&self, bc: &mut BcWriter, k: impl FnOnce(BcCallArgsFull<Symbol>, &mut BcWriter)) {
        write_exprs(&self.pos_named, bc, |pos_named, bc| {
            write_expr_opt(&self.args, bc, |args, bc| {
                write_expr_opt(&self.kwargs, bc, |kwargs, bc| {
                    let args_full = BcCallArgsFull {
                        pos_named,
                        names: self.names.clone().into_boxed_slice(),
                        args,
                        kwargs,
                    };
                    k(args_full, bc)
                })
            })
        });
    }
}

impl CallCompiled {
    /// After evaluation of call like `a[b](c.d)`,
    /// variables `a`, `b`, and `c` are definitely assigned.
    pub(crate) fn mark_definitely_assigned_after(&self, bc: &mut BcWriter) {
        let CallCompiled { fun, args } = self;
        fun.mark_definitely_assigned_after(bc);
        args.mark_definitely_assigned_after(bc);
    }
}

impl IrSpanned<CallCompiled> {
    fn write_record_call_enter_exit_slot(
        span: FrozenFileSpan,
        fun: BcSlotIn,
        bc: &mut BcWriter,
        k: impl FnOnce(&mut BcWriter),
    ) {
        assert!(bc.record_call_enter_exit());

        bc.write_instr::<InstrRecordCallEnter>(span, fun);
        k(bc);
        bc.write_instr::<InstrRecordCallExit>(span, ());
    }

    fn write_maybe_record_call_enter_exit_slot(
        span: FrozenFileSpan,
        fun: BcSlotIn,
        bc: &mut BcWriter,
        k: impl FnOnce(&mut BcWriter),
    ) {
        if bc.record_call_enter_exit() {
            Self::write_record_call_enter_exit_slot(span, fun, bc, k);
        } else {
            k(bc);
        }
    }

    fn write_maybe_record_call_enter_exit_const(
        span: FrozenFileSpan,
        fun: FrozenValue,
        bc: &mut BcWriter,
        k: impl FnOnce(&mut BcWriter),
    ) {
        if bc.record_call_enter_exit() {
            bc.alloc_slot(|slot, bc| {
                bc.write_const(span, fun, slot.to_out());
                Self::write_record_call_enter_exit_slot(span, slot.to_in(), bc, k);
            });
        } else {
            k(bc);
        }
    }

    fn write_maybe_record_call_enter_exit_method(
        span: FrozenFileSpan,
        this: BcSlotIn,
        field: &Symbol,
        bc: &mut BcWriter,
        k: impl FnOnce(&mut BcWriter),
    ) {
        if bc.record_call_enter_exit() {
            bc.alloc_slot(|method, bc| {
                bc.write_instr::<InstrObjectFieldRaw>(span, (this, field.clone(), method.to_out()));
                Self::write_record_call_enter_exit_slot(span, method.to_in(), bc, k);
            })
        } else {
            k(bc);
        }
    }

    fn write_args(
        args: &ArgsCompiledValue,
        bc: &mut BcWriter,
        k: impl FnOnce(Either<BcCallArgsPos, BcCallArgsFull<Symbol>>, &mut BcWriter),
    ) {
        if let Some(pos) = args.pos_only() {
            write_exprs(pos, bc, |pos, bc| {
                let args = Either::Left(BcCallArgsPos { pos });
                k(args, bc)
            })
        } else {
            args.write_bc(bc, |args_full, bc| {
                let args = Either::Right(args_full);
                k(args, bc)
            })
        }
    }

    fn write_args_then_maybe_record_call_enter_exit(
        args: &ArgsCompiledValue,
        fun: FrozenValue,
        fun_span: FrozenFileSpan,
        bc: &mut BcWriter,
        k: impl FnOnce(Either<BcCallArgsPos, BcCallArgsFull<Symbol>>, &mut BcWriter),
    ) {
        // Write arguments.
        Self::write_args(args, bc, |args, bc| {
            // Write `RecordCallEnter` if needed.
            Self::write_maybe_record_call_enter_exit_const(fun_span, fun, bc, |bc| {
                // Write the call.
                k(args, bc)
            })
            // Write `RecordCallExit` if needed.
        })
    }

    fn write_call_frozen(
        span: FrozenFileSpan,
        fun: FrozenValue,
        args: &ArgsCompiledValue,
        target: BcSlotOut,
        bc: &mut BcWriter,
    ) {
        let file_span = bc.alloc_file_span(span);
        if let Some(fun) = FrozenValueTyped::<FrozenDef>::new(fun) {
            Self::write_args_then_maybe_record_call_enter_exit(
                args,
                fun.to_frozen_value(),
                span,
                bc,
                |args, bc| match args {
                    Either::Left(npops) => bc.write_instr::<InstrCallFrozenDefPos>(
                        span,
                        (fun, npops, file_span, target),
                    ),
                    Either::Right(args) => bc.write_instr::<InstrCallFrozenDef>(
                        span,
                        (fun, args.resolve(fun.as_ref()), file_span, target),
                    ),
                },
            )
        } else if let Some(fun) = FrozenValueTyped::<NativeFunction>::new(fun) {
            let fun = BcNativeFunction::new(fun);
            Self::write_args_then_maybe_record_call_enter_exit(
                args,
                fun.to_frozen_value(),
                span,
                bc,
                |args, bc| match args {
                    Either::Left(npops) => {
                        bc.write_instr::<InstrCallFrozenNativePos>(
                            span,
                            (fun, npops, file_span, target),
                        );
                    }
                    Either::Right(args) => {
                        bc.write_instr::<InstrCallFrozenNative>(
                            span,
                            (fun, args, file_span, target),
                        );
                    }
                },
            )
        } else {
            Self::write_args_then_maybe_record_call_enter_exit(args, fun, span, bc, |args, bc| {
                match args {
                    Either::Left(npops) => {
                        bc.write_instr::<InstrCallFrozenPos>(span, (fun, npops, file_span, target));
                    }
                    Either::Right(args) => {
                        bc.write_instr::<InstrCallFrozen>(span, (fun, args, file_span, target));
                    }
                }
            })
        }
    }

    fn write_call_method(
        target: BcSlotOut,
        span: FrozenFileSpan,
        this: &IrSpanned<ExprCompiled>,
        symbol: &Symbol,
        args: &ArgsCompiledValue,
        bc: &mut BcWriter,
    ) {
        this.write_bc_cb(bc, |this, bc| {
            let file_span = bc.alloc_file_span(span);
            let known_method = get_known_method(symbol.as_str());
            if let Some(pos) = args.pos_only() {
                write_exprs(pos, bc, |pos, bc| {
                    Self::write_maybe_record_call_enter_exit_method(span, this, symbol, bc, |bc| {
                        if let Some(known_method) = known_method {
                            bc.write_instr::<InstrCallMaybeKnownMethodPos>(
                                span,
                                (
                                    this,
                                    symbol.clone(),
                                    known_method,
                                    BcCallArgsPos { pos },
                                    file_span,
                                    target,
                                ),
                            );
                        } else {
                            bc.write_instr::<InstrCallMethodPos>(
                                span,
                                (
                                    this,
                                    symbol.clone(),
                                    BcCallArgsPos { pos },
                                    file_span,
                                    target,
                                ),
                            );
                        }
                    })
                });
            } else {
                args.write_bc(bc, |args, bc| {
                    Self::write_maybe_record_call_enter_exit_method(span, this, symbol, bc, |bc| {
                        if let Some(known_method) = known_method {
                            bc.write_instr::<InstrCallMaybeKnownMethod>(
                                span,
                                (this, symbol.clone(), known_method, args, file_span, target),
                            );
                        } else {
                            bc.write_instr::<InstrCallMethod>(
                                span,
                                (this, symbol.clone(), args, file_span, target),
                            );
                        }
                    })
                })
            }
        })
    }

    pub(crate) fn write_bc(&self, target: BcSlotOut, bc: &mut BcWriter) {
        if let Some(arg) = self.as_len() {
            return arg.write_bc_cb(bc, |arg, bc| {
                bc.write_instr::<InstrLen>(self.span, (arg, target));
            });
        } else if let Some(arg) = self.as_type() {
            return arg.write_bc_cb(bc, |arg, bc| {
                bc.write_instr::<InstrType>(self.span, (arg, target));
            });
        }

        let span = self.span;
        let file_span = bc.alloc_file_span(span);
        match self.method() {
            None => match self.fun.as_value() {
                Some(f) => Self::write_call_frozen(span, f, &self.args, target, bc),
                None => {
                    self.fun.write_bc_cb(bc, |fun, bc| {
                        Self::write_args(&self.args, bc, |args, bc| {
                            Self::write_maybe_record_call_enter_exit_slot(span, fun, bc, |bc| {
                                match args {
                                    Either::Left(npops) => bc.write_instr::<InstrCallPos>(
                                        span,
                                        (fun, npops, file_span, target),
                                    ),
                                    Either::Right(args) => {
                                        bc.write_instr::<InstrCall>(
                                            span,
                                            (fun, args, file_span, target),
                                        );
                                    }
                                }
                            })
                        })
                    });
                }
            },
            Some((this, symbol, args)) => {
                Self::write_call_method(target, span, this, symbol, args, bc)
            }
        }
    }
}

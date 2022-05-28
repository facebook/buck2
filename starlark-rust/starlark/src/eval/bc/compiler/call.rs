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
            compiler::expr::write_exprs,
            instr_arg::ArgPopsStack1,
            instr_impl::{
                InstrCall, InstrCallFrozen, InstrCallFrozenDef, InstrCallFrozenDefPos,
                InstrCallFrozenNative, InstrCallFrozenNativePos, InstrCallFrozenPos,
                InstrCallMaybeKnownMethod, InstrCallMaybeKnownMethodPos, InstrCallMethod,
                InstrCallMethodPos, InstrCallPos,
            },
            writer::BcWriter,
        },
        compiler::{args::ArgsCompiledValue, call::CallCompiled, def::FrozenDef, span::IrSpanned},
        runtime::call_stack::FrozenFileSpan,
    },
    values::{
        function::NativeFunction, types::known_methods::get_known_method, FrozenValue,
        FrozenValueTyped,
    },
};

impl ArgsCompiledValue {
    fn write_bc(&self, bc: &mut BcWriter) -> BcCallArgsFull<Symbol> {
        write_exprs(&self.pos_named, bc);
        write_exprs(&self.args, bc);
        write_exprs(&self.kwargs, bc);
        BcCallArgsFull {
            pos_named: self.pos_named.len().try_into().unwrap(),
            names: self.names.clone().into_boxed_slice(),
            args: self.args.is_some(),
            kwargs: self.kwargs.is_some(),
        }
    }
}

impl IrSpanned<CallCompiled> {
    fn write_args(
        args: &ArgsCompiledValue,
        bc: &mut BcWriter,
    ) -> Either<BcCallArgsPos, BcCallArgsFull<Symbol>> {
        if let Some(pos) = args.pos_only() {
            write_exprs(pos, bc);
            Either::Left(BcCallArgsPos {
                pos: pos.len() as u32,
            })
        } else {
            let args = args.write_bc(bc);
            Either::Right(args)
        }
    }

    fn write_call_frozen(
        span: FrozenFileSpan,
        fun: FrozenValue,
        args: &ArgsCompiledValue,
        bc: &mut BcWriter,
    ) {
        let file_span = bc.alloc_file_span(span);
        if let Some(fun) = FrozenValueTyped::<FrozenDef>::new(fun) {
            match Self::write_args(args, bc) {
                Either::Left(npops) => {
                    bc.write_instr::<InstrCallFrozenDefPos>(span, (fun, npops, file_span));
                }
                Either::Right(args) => {
                    bc.write_instr::<InstrCallFrozenDef>(
                        span,
                        (fun, args.resolve(fun.as_ref()), file_span),
                    );
                }
            }
        } else if let Some(fun) = FrozenValueTyped::<NativeFunction>::new(fun) {
            match Self::write_args(args, bc) {
                Either::Left(npops) => {
                    bc.write_instr::<InstrCallFrozenNativePos>(span, (fun, npops, file_span));
                }
                Either::Right(args) => {
                    bc.write_instr::<InstrCallFrozenNative>(span, (fun, args, file_span));
                }
            }
        } else {
            match Self::write_args(args, bc) {
                Either::Left(npops) => {
                    bc.write_instr::<InstrCallFrozenPos>(span, (fun, npops, file_span));
                }
                Either::Right(args) => {
                    bc.write_instr::<InstrCallFrozen>(span, (fun, args, file_span));
                }
            }
        }
    }

    pub(crate) fn write_bc(&self, bc: &mut BcWriter) {
        let span = self.span;
        let file_span = bc.alloc_file_span(span);
        match self.method() {
            None => match self.fun.as_value() {
                Some(f) => Self::write_call_frozen(span, f, &self.args, bc),
                None => {
                    self.fun.write_bc(bc);
                    match Self::write_args(&self.args, bc) {
                        Either::Left(npops) => {
                            bc.write_instr::<InstrCallPos>(span, (ArgPopsStack1, npops, file_span))
                        }
                        Either::Right(args) => {
                            bc.write_instr::<InstrCall>(span, (ArgPopsStack1, args, file_span));
                        }
                    }
                }
            },
            Some((this, symbol, args)) => {
                this.write_bc(bc);
                let file_span = bc.alloc_file_span(span);
                let symbol = symbol.clone();
                let known_method = get_known_method(symbol.as_str());
                if let Some(pos) = args.pos_only() {
                    write_exprs(pos, bc);
                    if let Some(known_method) = known_method {
                        bc.write_instr::<InstrCallMaybeKnownMethodPos>(
                            span,
                            (
                                ArgPopsStack1,
                                symbol,
                                known_method,
                                BcCallArgsPos {
                                    pos: pos.len() as u32,
                                },
                                file_span,
                            ),
                        );
                    } else {
                        bc.write_instr::<InstrCallMethodPos>(
                            span,
                            (
                                ArgPopsStack1,
                                symbol,
                                BcCallArgsPos {
                                    pos: pos.len() as u32,
                                },
                                file_span,
                            ),
                        );
                    }
                } else {
                    let args = args.write_bc(bc);
                    if let Some(known_method) = known_method {
                        bc.write_instr::<InstrCallMaybeKnownMethod>(
                            span,
                            (ArgPopsStack1, symbol, known_method, args, file_span),
                        );
                    } else {
                        bc.write_instr::<InstrCallMethod>(
                            span,
                            (ArgPopsStack1, symbol, args, file_span),
                        );
                    }
                }
            }
        }
    }
}

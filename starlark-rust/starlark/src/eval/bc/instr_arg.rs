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

//! Instruction arguments.

use std::{
    fmt,
    fmt::{Display, Formatter, Write},
};

use itertools::Itertools;

use crate::{
    collections::{symbol_map::Symbol, Hashed, SmallMap},
    environment::slots::ModuleSlotId,
    eval::{
        bc::{
            addr::{BcAddr, BcAddrOffset, BcPtrAddr},
            call::{BcCallArgsFull, BcCallArgsPos},
            instr::BcInstr,
            instr_impl::InstrDefData,
            native_function::BcNativeFunction,
            opcode::{BcOpcode, BcOpcodeHandler},
            slow_arg::BcInstrSlowArg,
            stack_ptr::{BcSlotIn, BcSlotInRange, BcSlotInRangeFrom, BcSlotOut, BcSlotsInN},
        },
        runtime::{arguments::ArgSymbol, call_stack::FrozenFileSpan, slots::LocalSlotId},
    },
    values::{
        layout::value_not_special::FrozenValueNotSpecial, types::known_methods::KnownMethod,
        FrozenRef, FrozenValue, FrozenValueTyped, StarlarkValue,
    },
};

/// Truncate value if it is too long.
struct TruncateValueRepr(FrozenValue);

impl Display for TruncateValueRepr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let repr = self.0.to_value().to_repr();
        // Truncate too long constants (like dicts with hundreds of elements).
        if repr.len() > 100 {
            write!(f, "<{}>", self.0.to_value().get_type())
        } else {
            write!(f, "{}", repr)
        }
    }
}

/// Instruction fixed argument.
pub(crate) trait BcInstrArg: 'static {
    /// Append space then append the argument, or append nothing if the argument is empty.
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result;
    /// Collect instruction jump addresses.
    fn visit_jump_addr(param: &Self, consumer: &mut dyn FnMut(BcAddrOffset));
}

impl BcInstrArg for () {
    fn fmt_append(_param: &Self, _ip: BcAddr, _f: &mut dyn Write) -> fmt::Result {
        Ok(())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for u32 {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for i32 {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<A: BcInstrArg, B: BcInstrArg> BcInstrArg for (A, B) {
    fn fmt_append((a, b): &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        A::fmt_append(a, ip, f)?;
        B::fmt_append(b, ip, f)?;
        Ok(())
    }

    fn visit_jump_addr((a, b): &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        BcInstrArg::visit_jump_addr(a, consumer);
        BcInstrArg::visit_jump_addr(b, consumer);
    }
}

impl<A: BcInstrArg, B: BcInstrArg, C: BcInstrArg> BcInstrArg for (A, B, C) {
    fn fmt_append((a, b, c): &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        A::fmt_append(a, ip, f)?;
        B::fmt_append(b, ip, f)?;
        C::fmt_append(c, ip, f)?;
        Ok(())
    }

    fn visit_jump_addr((a, b, c): &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        BcInstrArg::visit_jump_addr(a, consumer);
        BcInstrArg::visit_jump_addr(b, consumer);
        BcInstrArg::visit_jump_addr(c, consumer);
    }
}

#[allow(clippy::many_single_char_names)]
impl<A: BcInstrArg, B: BcInstrArg, C: BcInstrArg, D: BcInstrArg> BcInstrArg for (A, B, C, D) {
    fn fmt_append((a, b, c, d): &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        A::fmt_append(a, ip, f)?;
        B::fmt_append(b, ip, f)?;
        C::fmt_append(c, ip, f)?;
        D::fmt_append(d, ip, f)?;
        Ok(())
    }

    fn visit_jump_addr((a, b, c, d): &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        BcInstrArg::visit_jump_addr(a, consumer);
        BcInstrArg::visit_jump_addr(b, consumer);
        BcInstrArg::visit_jump_addr(c, consumer);
        BcInstrArg::visit_jump_addr(d, consumer);
    }
}

#[allow(clippy::many_single_char_names)]
impl<A: BcInstrArg, B: BcInstrArg, C: BcInstrArg, D: BcInstrArg, E: BcInstrArg> BcInstrArg
    for (A, B, C, D, E)
{
    fn fmt_append((a, b, c, d, e): &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        A::fmt_append(a, ip, f)?;
        B::fmt_append(b, ip, f)?;
        C::fmt_append(c, ip, f)?;
        D::fmt_append(d, ip, f)?;
        E::fmt_append(e, ip, f)?;
        Ok(())
    }

    fn visit_jump_addr((a, b, c, d, e): &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        BcInstrArg::visit_jump_addr(a, consumer);
        BcInstrArg::visit_jump_addr(b, consumer);
        BcInstrArg::visit_jump_addr(c, consumer);
        BcInstrArg::visit_jump_addr(d, consumer);
        BcInstrArg::visit_jump_addr(e, consumer);
    }
}

#[allow(clippy::many_single_char_names)]
impl<A: BcInstrArg, B: BcInstrArg, C: BcInstrArg, D: BcInstrArg, E: BcInstrArg, F: BcInstrArg>
    BcInstrArg for (A, B, C, D, E, F)
{
    fn fmt_append((a, b, c, d, e, f): &Self, ip: BcAddr, w: &mut dyn Write) -> fmt::Result {
        A::fmt_append(a, ip, w)?;
        B::fmt_append(b, ip, w)?;
        C::fmt_append(c, ip, w)?;
        D::fmt_append(d, ip, w)?;
        E::fmt_append(e, ip, w)?;
        F::fmt_append(f, ip, w)?;
        Ok(())
    }

    fn visit_jump_addr((a, b, c, d, e, f): &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        BcInstrArg::visit_jump_addr(a, consumer);
        BcInstrArg::visit_jump_addr(b, consumer);
        BcInstrArg::visit_jump_addr(c, consumer);
        BcInstrArg::visit_jump_addr(d, consumer);
        BcInstrArg::visit_jump_addr(e, consumer);
        BcInstrArg::visit_jump_addr(f, consumer);
    }
}

impl<A: BcInstrArg, const N: usize> BcInstrArg for [A; N] {
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        for a in param {
            A::fmt_append(a, ip, f)?;
        }
        Ok(())
    }

    fn visit_jump_addr(param: &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        for a in param {
            BcInstrArg::visit_jump_addr(a, consumer);
        }
    }
}

impl BcInstrArg for BcAddrOffset {
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", ip.offset(*param).0)
    }

    fn visit_jump_addr(param: &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        consumer(*param);
    }
}

impl BcInstrArg for BcAddr {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.0)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for FrozenValue {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", TruncateValueRepr(*param))
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for FrozenValueNotSpecial {
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        FrozenValue::fmt_append(&param.to_frozen_value(), ip, f)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<T: BcInstrArg> BcInstrArg for Option<T> {
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        match param {
            None => write!(f, " ()"),
            Some(v) => T::fmt_append(v, ip, f),
        }
    }

    fn visit_jump_addr(param: &Self, consumer: &mut dyn FnMut(BcAddrOffset)) {
        if let Some(param) = param {
            T::visit_jump_addr(param, consumer);
        }
    }
}

impl BcInstrArg for String {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, "{:?}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<T: Display> BcInstrArg for FrozenRef<'static, T>
where
    FrozenRef<'static, T>: Copy,
{
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.as_ref())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<T: Display> BcInstrArg for FrozenRef<'static, [T]>
where
    FrozenRef<'static, T>: Copy,
{
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(
            f,
            " [{}]",
            param.iter().map(|v| format!("{}", v)).join(", ")
        )
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<T: StarlarkValue<'static>> BcInstrArg for FrozenValueTyped<'static, T> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", TruncateValueRepr(param.to_frozen_value()))
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcNativeFunction {
    fn fmt_append(param: &Self, ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        BcInstrArg::fmt_append(&param.fun(), ip, f)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for LocalSlotId {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " &{}", param.0)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcSlotIn {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.get())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcSlotOut {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.get())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcSlotInRange {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}..{}", param.start, param.end)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<const N: usize> BcInstrArg for BcSlotsInN<N> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}..{}", param.start(), param.end())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcSlotInRangeFrom {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}..", param.0)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for ModuleSlotId {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " m{}", param.0)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for FrozenFileSpan {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

/// Opcode as instruction argument.
impl BcInstrArg for BcOpcode {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {:?}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for KnownMethod {
    fn fmt_append(_param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " <m>")
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for Vec<(BcAddr, BcInstrSlowArg)> {
    fn fmt_append(_param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " args")
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for Symbol {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.as_str())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for Box<[FrozenValue]> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " [")?;
        for (i, v) in param.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", TruncateValueRepr(*v))?;
        }
        write!(f, "]")?;
        Ok(())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for Box<[Hashed<FrozenValue>]> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " [")?;
        for (i, v) in param.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", TruncateValueRepr(*v.key()))?;
        }
        write!(f, "]")?;
        Ok(())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for SmallMap<FrozenValue, FrozenValue> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {{")?;
        for (i, (k, v)) in param.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", TruncateValueRepr(*k), TruncateValueRepr(*v))?;
        }
        write!(f, "}}")?;
        Ok(())
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for InstrDefData {
    fn fmt_append(_param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " InstrDefData")
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl<S: ArgSymbol> BcInstrArg for BcCallArgsFull<S> {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {{{}}}", param)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcInstrArg for BcCallArgsPos {
    fn fmt_append(param: &Self, _ip: BcAddr, f: &mut dyn Write) -> fmt::Result {
        write!(f, " {}", param.pos)
    }

    fn visit_jump_addr(_param: &Self, _consumer: &mut dyn FnMut(BcAddrOffset)) {}
}

impl BcOpcode {
    /// Format instruction argument.
    pub(crate) fn fmt_append_arg(
        self,
        ptr: BcPtrAddr,
        ip: BcAddr,
        f: &mut dyn Write,
    ) -> fmt::Result {
        struct HandlerImpl<'b, 'g> {
            ptr: BcPtrAddr<'b>,
            ip: BcAddr,
            f: &'g mut dyn Write,
        }

        impl BcOpcodeHandler<fmt::Result> for HandlerImpl<'_, '_> {
            fn handle<I: BcInstr>(self) -> fmt::Result {
                let HandlerImpl { ptr, ip, f } = self;
                let instr = ptr.get_instr::<I>();
                I::Arg::fmt_append(&instr.arg, ip, f)
            }
        }

        self.dispatch(HandlerImpl { ptr, ip, f })
    }

    pub(crate) fn visit_jump_addr(self, ptr: BcPtrAddr, consumer: &mut dyn FnMut(BcAddrOffset)) {
        struct HandlerImpl<'b, 'c> {
            ptr: BcPtrAddr<'b>,
            consumer: &'c mut dyn FnMut(BcAddrOffset),
        }

        impl BcOpcodeHandler<()> for HandlerImpl<'_, '_> {
            fn handle<I: BcInstr>(self) {
                let HandlerImpl { ptr, consumer } = self;
                let instr = ptr.get_instr::<I>();
                I::Arg::visit_jump_addr(&instr.arg, consumer);
            }
        }

        self.dispatch(HandlerImpl { ptr, consumer });
    }
}

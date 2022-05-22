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

//! Instruction implementations.

use std::{cmp::Ordering, marker, mem::MaybeUninit, ptr};

use gazebo::coerce::coerce;

use crate::{
    collections::{symbol_map::Symbol, Hashed, SmallMap},
    environment::slots::ModuleSlotId,
    eval::{
        bc::{
            addr::{BcAddr, BcAddrOffset, BcPtrAddr},
            bytecode::{run_block, Bc, RunBlockResult},
            call::{BcCallArgs, BcCallArgsForDef, BcCallArgsFull, BcCallArgsPos},
            instr::{BcInstr, InstrControl},
            instr_arg::{
                ArgPopsStack, ArgPopsStack1, ArgPopsStackMaybe1, ArgPushesStack, BcInstrArg,
            },
            opcode::BcOpcode,
            slow_arg::BcInstrSlowArg,
            stack_ptr::BcStackPtr,
            stack_values::BcStackValues,
        },
        compiler::{
            add_span_to_expr_error,
            def::{Def, FrozenDef, ParameterCompiled},
            expr::{get_attr_hashed_bind, get_attr_hashed_raw, EvalError, MemberOrValue},
            expr_throw,
            scope::Captured,
            span::IrSpanned,
            stmt::{add_assign, before_stmt, bit_or_assign, possible_gc, AssignError},
            EvalException,
        },
        runtime::{arguments::ResolvedArgName, call_stack::FrozenFileSpan, slots::LocalSlotId},
        Arguments, DefInfo, Evaluator, ParametersSpec,
    },
    private::Private,
    values::{
        dict::Dict,
        function::NativeFunction,
        list::List,
        string::interpolation::{format_one, percent_s_one},
        types::known_methods::KnownMethod,
        typing::TypeCompiled,
        FrozenRef, FrozenStringValue, FrozenValue, FrozenValueTyped, Heap, StarlarkValue,
        StringValue, StringValueLike, Value,
    },
};

/// Instructions which either fail or proceed to the following instruction,
/// and it returns error with span.
pub(crate) trait InstrNoFlowImpl: 'static {
    type Pop<'v>: BcStackValues<'v>;
    type Push<'v>: BcStackValues<'v>
    where
        Self: 'v;
    type Arg: BcInstrArg;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr,
        arg: &Self::Arg,
        pops: Self::Pop<'v>,
    ) -> anyhow::Result<Self::Push<'v>>;
}

pub(crate) struct InstrNoFlow<I: InstrNoFlowImpl>(marker::PhantomData<I>);

impl<I: InstrNoFlowImpl> BcInstr for InstrNoFlow<I> {
    type Pop<'v> = I::Pop<'v>;
    type Push<'v> = I::Push<'v>;
    type Arg = I::Arg;

    #[inline(always)]
    fn run<'v, 'b>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        arg: &Self::Arg,
    ) -> InstrControl<'v, 'b> {
        let pops = BcStackValues::pop(stack);
        match I::run_with_args(eval, stack, ip, arg, pops) {
            Ok(pushes) => {
                BcStackValues::push(stack, pushes);
                InstrControl::Next(ip.add_instr::<Self>())
            }
            Err(e) => InstrControl::Err(e),
        }
    }
}

pub(crate) struct InstrDupImpl;
pub(crate) struct InstrPopImpl;

pub(crate) type InstrDup = InstrNoFlow<InstrDupImpl>;
pub(crate) type InstrPop = InstrNoFlow<InstrPopImpl>;

impl InstrNoFlowImpl for InstrDupImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = [Value<'v>; 2];
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        v: Value<'v>,
    ) -> anyhow::Result<[Value<'v>; 2]> {
        Ok([v, v])
    }
}

impl InstrNoFlowImpl for InstrPopImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        _v: Value<'v>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

pub(crate) struct InstrConstImpl;
pub(crate) type InstrConst = InstrNoFlow<InstrConstImpl>;

impl InstrNoFlowImpl for InstrConstImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = FrozenValue;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &FrozenValue,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        Ok(arg.to_value())
    }
}

pub(crate) struct InstrConstNImpl<const N: usize>;
pub(crate) type InstrConst2 = InstrNoFlow<InstrConstNImpl<2>>;
pub(crate) type InstrConst3 = InstrNoFlow<InstrConstNImpl<3>>;
pub(crate) type InstrConst4 = InstrNoFlow<InstrConstNImpl<4>>;

impl<const N: usize> InstrNoFlowImpl for InstrConstNImpl<N> {
    type Pop<'v> = ();
    type Push<'v> = [Value<'v>; N];
    type Arg = [FrozenValue; N];

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        vs: &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<[Value<'v>; N]> {
        Ok(coerce(*vs))
    }
}

pub(crate) struct InstrLoadLocalImpl;
pub(crate) struct InstrLoadLocalAndConstImpl;
pub(crate) struct InstrLoadLocalCapturedImpl;
pub(crate) struct InstrLoadModuleImpl;
pub(crate) struct InstrStoreLocalImpl;
pub(crate) struct InstrStoreLocalCapturedImpl;
pub(crate) struct InstrStoreModuleImpl;
pub(crate) struct InstrStoreModuleAndExportImpl;
pub(crate) struct InstrUnpackImpl;
pub(crate) struct InstrArrayIndexImpl;
pub(crate) struct InstrArrayIndexNoPopImpl;
pub(crate) struct InstrSetArrayIndexImpl;
pub(crate) struct InstrArrayIndexSetImpl;
pub(crate) struct InstrObjectFieldImpl;
pub(crate) struct InstrSetObjectFieldImpl;
pub(crate) struct InstrObjectSetFieldImpl;
pub(crate) struct InstrSliceImpl;

pub(crate) type InstrLoadLocal = InstrNoFlow<InstrLoadLocalImpl>;
pub(crate) type InstrLoadLocalAndConst = InstrNoFlow<InstrLoadLocalAndConstImpl>;
pub(crate) type InstrLoadLocalCaptured = InstrNoFlow<InstrLoadLocalCapturedImpl>;
pub(crate) type InstrLoadModule = InstrNoFlow<InstrLoadModuleImpl>;
pub(crate) type InstrStoreLocal = InstrNoFlow<InstrStoreLocalImpl>;
pub(crate) type InstrStoreLocalCaptured = InstrNoFlow<InstrStoreLocalCapturedImpl>;
pub(crate) type InstrStoreModule = InstrNoFlow<InstrStoreModuleImpl>;
pub(crate) type InstrStoreModuleAndExport = InstrNoFlow<InstrStoreModuleAndExportImpl>;
pub(crate) type InstrUnpack = InstrNoFlow<InstrUnpackImpl>;
pub(crate) type InstrArrayIndex = InstrNoFlow<InstrArrayIndexImpl>;
pub(crate) type InstrArrayIndexNoPop = InstrNoFlow<InstrArrayIndexNoPopImpl>;
pub(crate) type InstrSetArrayIndex = InstrNoFlow<InstrSetArrayIndexImpl>;
pub(crate) type InstrArrayIndexSet = InstrNoFlow<InstrArrayIndexSetImpl>;
pub(crate) type InstrObjectField = InstrNoFlow<InstrObjectFieldImpl>;
pub(crate) type InstrSetObjectField = InstrNoFlow<InstrSetObjectFieldImpl>;
pub(crate) type InstrObjectSetField = InstrNoFlow<InstrObjectSetFieldImpl>;
pub(crate) type InstrSlice = InstrNoFlow<InstrSliceImpl>;

impl InstrNoFlowImpl for InstrLoadLocalImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = LocalSlotId;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &LocalSlotId,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        eval.get_slot_local(*arg)
    }
}

#[inline(always)]
fn load_local<'v, const N: usize>(
    eval: &mut Evaluator<'v, '_>,
    ip: BcPtrAddr,
    slots: &[LocalSlotId; N],
) -> anyhow::Result<[Value<'v>; N]> {
    #[cold]
    #[inline(never)]
    fn fail<'v>(
        eval: &mut Evaluator<'v, '_>,
        ip: BcPtrAddr,
        index: usize,
        slot: LocalSlotId,
    ) -> anyhow::Error {
        let err = eval.local_var_referenced_before_assignment(slot);
        let spans = &Bc::slow_arg_at_ptr(ip).spans;
        let span = spans[index];
        add_span_to_expr_error(err, span, eval).0
    }

    let mut values = MaybeUninit::uninit();
    #[allow(clippy::needless_range_loop)]
    for i in 0..N {
        let values: *mut [Value; N] = values.as_mut_ptr();
        let slot = slots[i];
        match eval.current_frame.get_slot(slot) {
            Some(v) => unsafe {
                *(*values).get_unchecked_mut(i) = v;
            },
            None => return Err(fail(eval, ip, i, slot)),
        }
    }
    Ok(unsafe { values.assume_init() })
}

pub(crate) struct InstrLocalLocalNImpl<const N: usize>;
pub(crate) type InstrLoadLocal2 = InstrNoFlow<InstrLocalLocalNImpl<2>>;
pub(crate) type InstrLoadLocal3 = InstrNoFlow<InstrLocalLocalNImpl<3>>;
pub(crate) type InstrLoadLocal4 = InstrNoFlow<InstrLocalLocalNImpl<4>>;

impl<const N: usize> InstrNoFlowImpl for InstrLocalLocalNImpl<N> {
    type Pop<'v> = ();
    type Push<'v> = [Value<'v>; N];
    type Arg = [LocalSlotId; N];

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr,
        slots: &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<[Value<'v>; N]> {
        load_local(eval, ip, slots)
    }
}

impl InstrNoFlowImpl for InstrLoadLocalAndConstImpl {
    type Pop<'v> = ();
    type Push<'v> = [Value<'v>; 2];
    type Arg = (LocalSlotId, FrozenValue);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (slot, value): &Self::Arg,
        (): (),
    ) -> anyhow::Result<[Value<'v>; 2]> {
        Ok([eval.get_slot_local(*slot)?, value.to_value()])
    }
}

impl InstrNoFlowImpl for InstrLoadLocalCapturedImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = LocalSlotId;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &LocalSlotId,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        eval.get_slot_local_captured(*arg)
    }
}

impl InstrNoFlowImpl for InstrLoadModuleImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ModuleSlotId;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &ModuleSlotId,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        eval.get_slot_module(*arg)
    }
}

impl InstrNoFlowImpl for InstrStoreLocalImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = LocalSlotId;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &LocalSlotId,
        v: Value<'v>,
    ) -> anyhow::Result<()> {
        eval.set_slot_local(*arg, v);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrStoreLocalCapturedImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = LocalSlotId;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &LocalSlotId,
        v: Value<'v>,
    ) -> anyhow::Result<()> {
        eval.set_slot_local_captured(*arg, v);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrStoreModuleAndExportImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = (ModuleSlotId, String);

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (slot, name): &(ModuleSlotId, String),
        v: Value<'v>,
    ) -> anyhow::Result<()> {
        v.export_as(name.as_str(), eval);
        eval.set_slot_module(*slot, v);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrStoreModuleImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = ModuleSlotId;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        slot: &ModuleSlotId,
        v: Value<'v>,
    ) -> anyhow::Result<()> {
        eval.set_slot_module(*slot, v);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrUnpackImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = ArgPushesStack;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &Self::Arg,
        v: Value<'v>,
    ) -> anyhow::Result<()> {
        let nvl = v.length()?;
        if nvl != arg.0 as i32 {
            return Err(AssignError::IncorrectNumberOfValueToUnpack(arg.0 as i32, nvl).into());
        }
        let places = stack.push_slice_placeholder(*arg);
        v.with_iterator(eval.heap(), |items| {
            let mut i = 0;
            for item in items {
                // Use unconditional assertion here because we cannot trust
                // user defined `length` and `with_iterator` consistently.
                assert!(i != arg.0 as usize);
                unsafe {
                    (*places.get_unchecked_mut(places.len() - i - 1)).write(item);
                }
                i += 1;
            }
            assert!(i == arg.0 as usize);
        })?;
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrArrayIndexImpl {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        [array, index]: [Value<'v>; 2],
    ) -> anyhow::Result<Value<'v>> {
        array.at(index, eval.heap())
    }
}

impl InstrNoFlowImpl for InstrArrayIndexNoPopImpl {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = [Value<'v>; 3];
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        [array, index]: [Value<'v>; 2],
    ) -> anyhow::Result<[Value<'v>; 3]> {
        let value = array.at(index, eval.heap())?;
        Ok([array, index, value])
    }
}

impl InstrNoFlowImpl for InstrSetArrayIndexImpl {
    type Pop<'v> = [Value<'v>; 3];
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        [value, array, index]: [Value<'v>; 3],
    ) -> anyhow::Result<()> {
        array.set_at(index, value)
    }
}

impl InstrNoFlowImpl for InstrArrayIndexSetImpl {
    type Pop<'v> = [Value<'v>; 3];
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        [array, index, value]: [Value<'v>; 3],
    ) -> anyhow::Result<()> {
        array.set_at(index, value)
    }
}

impl InstrNoFlowImpl for InstrObjectFieldImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = Symbol;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        symbol: &Symbol,
        object: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        get_attr_hashed_bind(object, symbol, eval.heap())
    }
}

impl InstrNoFlowImpl for InstrSetObjectFieldImpl {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = ();
    type Arg = Symbol;

    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        symbol: &Symbol,
        [v, o]: [Value<'v>; 2],
    ) -> anyhow::Result<()> {
        o.set_attr(symbol.as_str(), v)
    }
}

impl InstrNoFlowImpl for InstrObjectSetFieldImpl {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = ();
    type Arg = Symbol;

    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        symbol: &Symbol,
        [o, v]: [Value<'v>; 2],
    ) -> anyhow::Result<()> {
        o.set_attr(symbol.as_str(), v)
    }
}

impl InstrNoFlowImpl for InstrSliceImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (
        ArgPopsStack1,
        ArgPopsStackMaybe1,
        ArgPopsStackMaybe1,
        ArgPopsStackMaybe1,
    );

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (_list, start, stop, step): &Self::Arg,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        let step = stack.pop_maybe(*step);
        let stop = stack.pop_maybe(*stop);
        let start = stack.pop_maybe(*start);
        let list = stack.pop();

        list.slice(start, stop, step, eval.heap())
    }
}

pub(crate) struct InstrEqImpl;
pub(crate) struct InstrEqConstImpl;
pub(crate) struct InstrEqPtrImpl;
pub(crate) struct InstrEqStrImpl;
pub(crate) struct InstrEqIntImpl;

pub(crate) type InstrEq = InstrBinOp<InstrEqImpl>;
pub(crate) type InstrEqConst = InstrNoFlow<InstrEqConstImpl>;
pub(crate) type InstrEqPtr = InstrNoFlow<InstrEqPtrImpl>;
pub(crate) type InstrEqStr = InstrNoFlow<InstrEqStrImpl>;
pub(crate) type InstrEqInt = InstrNoFlow<InstrEqIntImpl>;

impl InstrBinOpImpl for InstrEqImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.equals(v1).map(Value::new_bool)
    }
}

impl InstrNoFlowImpl for InstrEqConstImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = FrozenValue;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &FrozenValue,
        pops: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        // TODO(nga): we know that `arg` is neither int nor string,
        //   so we could do faster unpacking.
        pops.equals(arg.to_value()).map(Value::new_bool)
    }
}

impl InstrNoFlowImpl for InstrEqPtrImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = FrozenValue;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &FrozenValue,
        pops: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_bool(pops.ptr_eq(arg.to_value())))
    }
}

impl InstrNoFlowImpl for InstrEqIntImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = i32;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &i32,
        pops: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        if let Some(value) = pops.unpack_int() {
            Ok(Value::new_bool(value == *arg))
        } else {
            Ok(Value::new_bool(pops.equals(Value::new_int(*arg))?))
        }
    }
}

impl InstrNoFlowImpl for InstrEqStrImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = FrozenStringValue;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        arg: &FrozenStringValue,
        pops: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        if let Some(value) = StringValue::new(pops) {
            Ok(Value::new_bool(value == arg.to_string_value()))
        } else {
            Ok(Value::new_bool(false))
        }
    }
}

pub(crate) struct InstrNotImpl;
pub(crate) struct InstrMinusImpl;
pub(crate) struct InstrPlusImpl;
pub(crate) struct InstrBitNotImpl;

pub(crate) type InstrNot = InstrUnOp<InstrNotImpl>;
pub(crate) type InstrMinus = InstrUnOp<InstrMinusImpl>;
pub(crate) type InstrPlus = InstrUnOp<InstrPlusImpl>;
pub(crate) type InstrBitNot = InstrUnOp<InstrBitNotImpl>;

impl InstrUnOpImpl for InstrNotImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_bool(!v.to_bool()))
    }
}

impl InstrUnOpImpl for InstrPlusImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v.plus(heap)
    }
}

impl InstrUnOpImpl for InstrMinusImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v.minus(heap)
    }
}

impl InstrUnOpImpl for InstrBitNotImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v.bit_not(heap)
    }
}

pub(crate) trait InstrBinOpImpl: 'static {
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

pub(crate) trait InstrUnOpImpl: 'static {
    fn eval<'v>(v: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>>;
}

pub(crate) struct InstrBinOpWrapper<I: InstrBinOpImpl>(marker::PhantomData<I>);
pub(crate) struct InstrUnOpWrapper<I: InstrUnOpImpl>(marker::PhantomData<I>);
pub(crate) type InstrBinOp<I> = InstrNoFlow<InstrBinOpWrapper<I>>;
pub(crate) type InstrUnOp<I> = InstrNoFlow<InstrUnOpWrapper<I>>;

impl<I: InstrBinOpImpl> InstrNoFlowImpl for InstrBinOpWrapper<I> {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        [v0, v1]: [Value<'v>; 2],
    ) -> anyhow::Result<Value<'v>> {
        I::eval(v0, v1, eval.heap())
    }
}

impl<I: InstrUnOpImpl> InstrNoFlowImpl for InstrUnOpWrapper<I> {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        v: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        I::eval(v, eval.heap())
    }
}

pub(crate) struct InstrAddImpl;
pub(crate) struct InstrAddAssignImpl;
pub(crate) struct InstrSubImpl;
pub(crate) struct InstrMultiplyImpl;
pub(crate) struct InstrPercentImpl;
pub(crate) struct InstrDivideImpl;
pub(crate) struct InstrFloorDivideImpl;
pub(crate) struct InstrBitAndImpl;
pub(crate) struct InstrBitOrImpl;
pub(crate) struct InstrBitOrAssignImpl;
pub(crate) struct InstrBitXorImpl;
pub(crate) struct InstrLeftShiftImpl;
pub(crate) struct InstrRightShiftImpl;
pub(crate) struct InstrInImpl;

pub(crate) type InstrAdd = InstrBinOp<InstrAddImpl>;
pub(crate) type InstrAddAssign = InstrBinOp<InstrAddAssignImpl>;
pub(crate) type InstrSub = InstrBinOp<InstrSubImpl>;
pub(crate) type InstrMultiply = InstrBinOp<InstrMultiplyImpl>;
pub(crate) type InstrPercent = InstrBinOp<InstrPercentImpl>;
pub(crate) type InstrDivide = InstrBinOp<InstrDivideImpl>;
pub(crate) type InstrFloorDivide = InstrBinOp<InstrFloorDivideImpl>;
pub(crate) type InstrBitAnd = InstrBinOp<InstrBitAndImpl>;
pub(crate) type InstrBitOr = InstrBinOp<InstrBitOrImpl>;
pub(crate) type InstrBitOrAssign = InstrBinOp<InstrBitOrAssignImpl>;
pub(crate) type InstrBitXor = InstrBinOp<InstrBitXorImpl>;
pub(crate) type InstrLeftShift = InstrBinOp<InstrLeftShiftImpl>;
pub(crate) type InstrRightShift = InstrBinOp<InstrRightShiftImpl>;
pub(crate) type InstrIn = InstrBinOp<InstrInImpl>;

impl InstrBinOpImpl for InstrAddImpl {
    #[inline(always)]
    fn eval<'v>(l: Value<'v>, r: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        l.add(r, heap)
    }
}

impl InstrBinOpImpl for InstrAddAssignImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        add_assign(v0, v1, heap)
    }
}

impl InstrBinOpImpl for InstrSubImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.sub(v1, heap)
    }
}

impl InstrBinOpImpl for InstrMultiplyImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.mul(v1, heap)
    }
}

impl InstrBinOpImpl for InstrPercentImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.percent(v1, heap)
    }
}

impl InstrBinOpImpl for InstrFloorDivideImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.floor_div(v1, heap)
    }
}

impl InstrBinOpImpl for InstrDivideImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.div(v1, heap)
    }
}

impl InstrBinOpImpl for InstrBitAndImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.bit_and(v1, heap)
    }
}

impl InstrBinOpImpl for InstrBitOrImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.bit_or(v1, heap)
    }
}

impl InstrBinOpImpl for InstrBitOrAssignImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        bit_or_assign(v0, v1, heap)
    }
}

impl InstrBinOpImpl for InstrBitXorImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.bit_xor(v1, heap)
    }
}

impl InstrBinOpImpl for InstrLeftShiftImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.left_shift(v1, heap)
    }
}

impl InstrBinOpImpl for InstrRightShiftImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        v0.right_shift(v1, heap)
    }
}

impl InstrBinOpImpl for InstrInImpl {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_bool(v1.is_in(v0)?))
    }
}

pub(crate) struct InstrPercentSOneImpl;
pub(crate) type InstrPercentSOne = InstrNoFlow<InstrPercentSOneImpl>;
pub(crate) struct InstrFormatOneImpl;
pub(crate) type InstrFormatOne = InstrNoFlow<InstrFormatOneImpl>;

impl InstrNoFlowImpl for InstrPercentSOneImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = (FrozenStringValue, FrozenStringValue);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (before, after): &Self::Arg,
        arg: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        percent_s_one(before.as_str(), arg, after.as_str(), eval.heap()).map(|v| v.to_value())
    }
}

impl InstrNoFlowImpl for InstrFormatOneImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = (FrozenStringValue, FrozenStringValue);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (before, after): &Self::Arg,
        arg: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(format_one(before.as_str(), arg, after.as_str(), eval.heap()).to_value())
    }
}

pub(crate) trait InstrCompareImpl: 'static {
    fn eval_compare(ordering: Ordering) -> bool;
}

pub(crate) struct InstrCompare<I: InstrCompareImpl>(marker::PhantomData<I>);

impl<I: InstrCompareImpl> InstrBinOpImpl for InstrCompare<I> {
    #[inline(always)]
    fn eval<'v>(v0: Value<'v>, v1: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_bool(I::eval_compare(v0.compare(v1)?)))
    }
}

pub(crate) struct InstrLessImpl;
pub(crate) struct InstrGreaterImpl;
pub(crate) struct InstrLessOrEqualImpl;
pub(crate) struct InstrGreaterOrEqualImpl;

pub(crate) type InstrLess = InstrBinOp<InstrCompare<InstrLessImpl>>;
pub(crate) type InstrGreater = InstrBinOp<InstrCompare<InstrGreaterImpl>>;
pub(crate) type InstrLessOrEqual = InstrBinOp<InstrCompare<InstrLessOrEqualImpl>>;
pub(crate) type InstrGreaterOrEqual = InstrBinOp<InstrCompare<InstrGreaterOrEqualImpl>>;

impl InstrCompareImpl for InstrLessImpl {
    #[inline(always)]
    fn eval_compare(ordering: Ordering) -> bool {
        ordering == Ordering::Less
    }
}

impl InstrCompareImpl for InstrGreaterImpl {
    #[inline(always)]
    fn eval_compare(ordering: Ordering) -> bool {
        ordering == Ordering::Greater
    }
}

impl InstrCompareImpl for InstrLessOrEqualImpl {
    #[inline(always)]
    fn eval_compare(ordering: Ordering) -> bool {
        ordering != Ordering::Greater
    }
}

impl InstrCompareImpl for InstrGreaterOrEqualImpl {
    #[inline(always)]
    fn eval_compare(ordering: Ordering) -> bool {
        ordering != Ordering::Less
    }
}

pub(crate) struct InstrTypeImpl;
pub(crate) type InstrType = InstrUnOp<InstrTypeImpl>;

impl InstrUnOpImpl for InstrTypeImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(v.get_type_value().to_frozen_value().to_value())
    }
}

pub(crate) struct InstrTypeIsImpl;
pub(crate) type InstrTypeIs = InstrNoFlow<InstrTypeIsImpl>;

impl InstrNoFlowImpl for InstrTypeIsImpl {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = FrozenStringValue;

    #[inline(always)]
    fn run_with_args<'v>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        t: &FrozenStringValue,
        v: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_bool(v.get_type_value() == *t))
    }
}

pub(crate) struct InstrLenImpl;
pub(crate) type InstrLen = InstrUnOp<InstrLenImpl>;

impl InstrUnOpImpl for InstrLenImpl {
    #[inline(always)]
    fn eval<'v>(v: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(Value::new_int(v.length()?))
    }
}

pub(crate) struct InstrTupleNPopImpl;
pub(crate) struct InstrListNPopImpl;
pub(crate) struct InstrListOfConstsImpl;
pub(crate) struct InstrDictOfConstsImpl;
pub(crate) struct InstrDictConstKeysImpl;
pub(crate) struct InstrDictNPopImpl;
pub(crate) struct InstrListNewImpl;
pub(crate) struct InstrDictNewImpl;

pub(crate) type InstrTupleNPop = InstrNoFlow<InstrTupleNPopImpl>;
pub(crate) type InstrListNew = InstrNoFlow<InstrListNewImpl>;
pub(crate) type InstrListNPop = InstrNoFlow<InstrListNPopImpl>;
pub(crate) type InstrListOfConsts = InstrNoFlow<InstrListOfConstsImpl>;
pub(crate) type InstrDictNew = InstrNoFlow<InstrDictNewImpl>;
pub(crate) type InstrDictOfConsts = InstrNoFlow<InstrDictOfConstsImpl>;
pub(crate) type InstrDictConstKeys = InstrNoFlow<InstrDictConstKeysImpl>;
pub(crate) type InstrDictNPop = InstrNoFlow<InstrDictNPopImpl>;

impl InstrNoFlowImpl for InstrTupleNPopImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ArgPopsStack;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        npops: &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let items = stack.pop_slice(*npops);
        Ok(eval.heap().alloc_tuple(items))
    }
}

impl InstrNoFlowImpl for InstrListNPopImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ArgPopsStack;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        npops: &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let items = stack.pop_slice(*npops);
        Ok(eval.heap().alloc_list(items))
    }
}

impl InstrNoFlowImpl for InstrListOfConstsImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = Box<[FrozenValue]>;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        values: &Self::Arg,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval.heap().alloc_list(coerce(&values)))
    }
}

impl InstrNoFlowImpl for InstrDictOfConstsImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = SmallMap<FrozenValue, FrozenValue>;

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        values: &Self::Arg,
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval.heap().alloc(Dict::new((*coerce(values)).clone())))
    }
}

impl InstrNoFlowImpl for InstrDictNPopImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ArgPopsStack;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr,
        npops: &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let items = stack.pop_slice(*npops);
        debug_assert!(items.len() % 2 == 0);
        let mut dict = SmallMap::with_capacity(items.len() / 2);
        for i in 0..items.len() / 2 {
            let k = items[i * 2];
            let v = items[i * 2 + 1];
            let k = match k.get_hashed() {
                Ok(k) => k,
                Err(e) => {
                    let spans = &Bc::slow_arg_at_ptr(ip).spans;
                    return Err(add_span_to_expr_error(e, spans[i], eval).0);
                }
            };
            let prev = dict.insert_hashed(k, v);
            if prev.is_some() {
                let e = EvalError::DuplicateDictionaryKey(k.key().to_string()).into();
                let spans = &Bc::slow_arg_at_ptr(ip).spans;
                return Err(add_span_to_expr_error(e, spans[i], eval).0);
            }
        }
        Ok(eval.heap().alloc(Dict::new(dict)))
    }
}

impl InstrNoFlowImpl for InstrDictConstKeysImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (ArgPopsStack, Box<[Hashed<FrozenValue>]>);

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        (npops, keys): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let values = stack.pop_slice(*npops);
        assert!(keys.len() == values.len());
        let mut dict = SmallMap::with_capacity(keys.len());
        for (k, v) in keys.iter().zip(values) {
            let prev = dict.insert_hashed(*k, *v);
            debug_assert!(prev.is_none());
        }
        Ok(eval.heap().alloc(Dict::new(coerce(dict))))
    }
}

impl InstrNoFlowImpl for InstrListNewImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        (): &(),
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval.heap().alloc_list(&[]))
    }
}

impl InstrNoFlowImpl for InstrDictNewImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        (): &(),
        (): (),
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval.heap().alloc(Dict::default()))
    }
}

pub(crate) struct InstrComprListAppend;
pub(crate) struct InstrComprDictInsert;

impl BcInstr for InstrComprListAppend {
    type Pop<'v> = [Value<'v>; 2];
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run<'v, 'b>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr<'b>,
        (): &(),
    ) -> InstrControl<'v, 'b> {
        let item = stack.pop();
        let list = stack.top();
        // SAFETY: in generated bytecode this slot can be only occupied by a mutable list.
        let list = unsafe { List::from_value_unchecked_mut(list) };
        list.push(item, eval.heap());
        InstrControl::LoopContinue
    }
}

impl BcInstr for InstrComprDictInsert {
    type Pop<'v> = [Value<'v>; 3];
    type Push<'v> = Value<'v>;
    type Arg = ();

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        (): &(),
    ) -> InstrControl<'v, 'b> {
        let [key, value] = stack.pop_array();
        let dict = stack.top();
        let key = match key.get_hashed() {
            Ok(key) => key,
            Err(e) => return InstrControl::Err(e),
        };
        // SAFETY: in generated bytecode this slot can be only occupied by a mutable dict.
        let mut dict = unsafe { Dict::from_value_unchecked_mut(dict) };
        dict.insert_hashed(key, value);
        InstrControl::LoopContinue
    }
}

pub(crate) struct InstrBr;
pub(crate) struct InstrIfBr;
pub(crate) struct InstrIfNotBr;

impl BcInstr for InstrBr {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = BcAddrOffset;

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        target: &BcAddrOffset,
    ) -> InstrControl<'v, 'b> {
        InstrControl::Next(ip.add_rel(*target))
    }
}

impl BcInstr for InstrIfBr {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = BcAddrOffset;

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        target: &BcAddrOffset,
    ) -> InstrControl<'v, 'b> {
        let cond = stack.pop();
        if cond.to_bool() {
            InstrControl::Next(ip.add_rel(*target))
        } else {
            InstrControl::Next(ip.add_instr::<Self>())
        }
    }
}

impl BcInstr for InstrIfNotBr {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = BcAddrOffset;

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        target: &BcAddrOffset,
    ) -> InstrControl<'v, 'b> {
        let cond = stack.pop();
        if !cond.to_bool() {
            InstrControl::Next(ip.add_rel(*target))
        } else {
            InstrControl::Next(ip.add_instr::<Self>())
        }
    }
}

pub(crate) struct InstrForLoop;
pub(crate) struct InstrBreak;
pub(crate) struct InstrContinue;

impl BcInstr for InstrForLoop {
    type Pop<'v> = Value<'v>;
    type Push<'v> = Value<'v>;
    type Arg = BcAddrOffset;

    fn run<'v, 'b>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        ip: BcPtrAddr<'b>,
        loop_end: &BcAddrOffset,
    ) -> InstrControl<'v, 'b> {
        let ss = stack.stack_offset();

        let collection = stack.pop();

        enum LoopResult<'v> {
            Ok,
            Return(Value<'v>),
            Err(EvalException),
        }

        let iter_ret = collection.with_iterator(eval.heap(), |iter| {
            let loop_start = ip.add_instr::<Self>();
            for item in iter {
                stack.push(item);
                debug_assert!(stack.stack_offset() == ss);
                match run_block(eval, stack, loop_start) {
                    RunBlockResult::Continue => {}
                    RunBlockResult::Break => return LoopResult::Ok,
                    RunBlockResult::Return(v) => return LoopResult::Return(v),
                    RunBlockResult::Err(e) => return LoopResult::Err(e),
                }
            }
            LoopResult::Ok
        });
        match iter_ret {
            Ok(LoopResult::Ok) => {
                debug_assert!(stack.stack_offset() + 1 == ss);
                InstrControl::Next(ip.add_rel(*loop_end))
            }
            Ok(LoopResult::Return(v)) => {
                debug_assert!(stack.stack_offset() + 1 == ss);
                InstrControl::Return(v)
            }
            Ok(LoopResult::Err(e)) => InstrControl::Err(e.0),
            Err(e) => InstrControl::Err(e),
        }
    }
}

impl BcInstr for InstrBreak {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        (): &(),
    ) -> InstrControl<'v, 'b> {
        InstrControl::LoopBreak
    }
}

impl BcInstr for InstrContinue {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        (): &(),
    ) -> InstrControl<'v, 'b> {
        InstrControl::LoopContinue
    }
}

pub(crate) struct InstrReturnConst;
pub(crate) struct InstrReturn;

impl BcInstr for InstrReturnConst {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = FrozenValue;

    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        value: &FrozenValue,
    ) -> InstrControl<'v, 'b> {
        InstrControl::Return(value.to_value())
    }
}

impl BcInstr for InstrReturn {
    type Pop<'v> = Value<'v>;
    type Push<'v> = ();
    type Arg = ();

    #[inline(always)]
    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        (): &(),
    ) -> InstrControl<'v, 'b> {
        let v = stack.pop();
        InstrControl::Return(v)
    }
}

pub(crate) struct InstrDefImpl;
pub(crate) type InstrDef = InstrNoFlow<InstrDefImpl>;

#[derive(Debug)]
pub(crate) struct InstrDefData {
    pub(crate) function_name: String,
    pub(crate) params: Vec<IrSpanned<ParameterCompiled<u32>>>,
    pub(crate) return_type: Option<IrSpanned<u32>>,
    pub(crate) info: FrozenRef<'static, DefInfo>,
    pub(crate) check_types: bool,
}

impl InstrNoFlowImpl for InstrDefImpl {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (ArgPopsStack, InstrDefData);

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (pops, def_data): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let pop = stack.pop_slice(*pops);

        let mut parameters =
            ParametersSpec::with_capacity(def_data.function_name.clone(), def_data.params.len());
        parameters.no_more_positional_only_args();
        let mut parameter_types = Vec::new();
        let mut parameter_captures = Vec::new();

        let mut pop_index = 0;

        // count here rather than enumerate because '*' doesn't get a real
        // index in the parameter mapping, and it messes up the indexes
        let mut i = 0;
        for x in &def_data.params {
            if let Some((name, Some(t))) = x.name_ty() {
                assert!(*t == pop_index);
                let v = pop[pop_index as usize];
                pop_index += 1;
                if def_data.check_types {
                    parameter_types.push((
                        i,
                        name.name.clone(),
                        v,
                        expr_throw(TypeCompiled::new(v, eval.heap()), x.span, eval)
                            .map_err(|e| e.0)?,
                    ));
                }
            }
            match &x.node {
                ParameterCompiled::Normal(n, _) => parameters.required(&n.name),
                ParameterCompiled::WithDefaultValue(n, ty, v) => {
                    assert!(*v == pop_index);
                    let value = pop[pop_index as usize];
                    pop_index += 1;

                    if ty.is_some() && def_data.check_types {
                        // Check the type of the default
                        let (_, _, ty_value, ty_compiled) = parameter_types.last().unwrap();
                        expr_throw(
                            value.check_type_compiled(*ty_value, ty_compiled, Some(&n.name)),
                            x.span,
                            eval,
                        )
                        .map_err(|e| e.0)?;
                    }
                    parameters.defaulted(&n.name, value);
                }
                ParameterCompiled::NoArgs => parameters.no_more_positional_args(),
                ParameterCompiled::Args(_, _) => parameters.args(),
                ParameterCompiled::KwArgs(_, _) => parameters.kwargs(),
            };
            if let Captured::Yes = x.captured() {
                parameter_captures.push(i);
            }
            if !matches!(x.node, ParameterCompiled::NoArgs) {
                i += 1;
            }
        }
        let return_type = match &def_data.return_type {
            None => None,
            Some(v) => {
                assert!(v.node == pop_index);
                let value = pop[pop_index as usize];
                pop_index += 1;
                if def_data.check_types {
                    Some((
                        value,
                        expr_throw(TypeCompiled::new(value, eval.heap()), v.span, eval)
                            .map_err(|e| e.0)?,
                    ))
                } else {
                    None
                }
            }
        };
        assert!(pop_index as usize == pop.len());
        Ok(eval.heap().alloc(Def::new(
            parameters.finish(),
            parameter_captures,
            parameter_types,
            return_type,
            def_data.info,
            eval,
        )))
    }
}

/// A frozen function argument to a call instruction.
pub(crate) trait BcFrozenCallable: BcInstrArg + Copy {
    fn bc_invoke<'v>(
        self,
        location: FrozenRef<'static, FrozenFileSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>>;
}

impl BcFrozenCallable for FrozenValue {
    #[inline(always)]
    fn bc_invoke<'v>(
        self,
        location: FrozenRef<'static, FrozenFileSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.to_value().invoke_with_loc(Some(location), args, eval)
    }
}

impl BcFrozenCallable for FrozenValueTyped<'static, FrozenDef> {
    #[inline(always)]
    fn bc_invoke<'v>(
        self,
        location: FrozenRef<'static, FrozenFileSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        eval.with_call_stack(self.to_value(), Some(location), |eval| {
            self.as_ref().invoke(self.to_value(), args, eval)
        })
    }
}

impl BcFrozenCallable for FrozenValueTyped<'static, NativeFunction> {
    #[inline(always)]
    fn bc_invoke<'v>(
        self,
        location: FrozenRef<'static, FrozenFileSpan>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        eval.with_call_stack(self.to_value(), Some(location), |eval| {
            self.as_ref().invoke(self.to_value(), args, eval)
        })
    }
}

pub(crate) struct InstrCallImpl<A: BcCallArgs<Symbol>>(marker::PhantomData<fn(A)>);
pub(crate) struct InstrCallFrozenGenericImpl<F: BcFrozenCallable, A: BcCallArgs<Symbol>>(
    marker::PhantomData<(F, A)>,
);
pub(crate) struct InstrCallFrozenDefImpl<A: BcCallArgsForDef>(marker::PhantomData<A>);
pub(crate) struct InstrCallMethodImpl<A: BcCallArgs<Symbol>>(marker::PhantomData<A>);
pub(crate) struct InstrCallMaybeKnownMethodImpl<A: BcCallArgs<Symbol>>(marker::PhantomData<A>);

pub(crate) type InstrCall = InstrNoFlow<InstrCallImpl<BcCallArgsFull<Symbol>>>;
pub(crate) type InstrCallPos = InstrNoFlow<InstrCallImpl<BcCallArgsPos>>;
pub(crate) type InstrCallFrozenDef =
    InstrNoFlow<InstrCallFrozenDefImpl<BcCallArgsFull<ResolvedArgName>>>;
pub(crate) type InstrCallFrozenDefPos = InstrNoFlow<InstrCallFrozenDefImpl<BcCallArgsPos>>;
pub(crate) type InstrCallFrozenNative = InstrNoFlow<
    InstrCallFrozenGenericImpl<FrozenValueTyped<'static, NativeFunction>, BcCallArgsFull<Symbol>>,
>;
pub(crate) type InstrCallFrozenNativePos = InstrNoFlow<
    InstrCallFrozenGenericImpl<FrozenValueTyped<'static, NativeFunction>, BcCallArgsPos>,
>;
pub(crate) type InstrCallFrozen =
    InstrNoFlow<InstrCallFrozenGenericImpl<FrozenValue, BcCallArgsFull<Symbol>>>;
pub(crate) type InstrCallFrozenPos =
    InstrNoFlow<InstrCallFrozenGenericImpl<FrozenValue, BcCallArgsPos>>;
pub(crate) type InstrCallMethod = InstrNoFlow<InstrCallMethodImpl<BcCallArgsFull<Symbol>>>;
pub(crate) type InstrCallMethodPos = InstrNoFlow<InstrCallMethodImpl<BcCallArgsPos>>;
pub(crate) type InstrCallMaybeKnownMethod =
    InstrNoFlow<InstrCallMaybeKnownMethodImpl<BcCallArgsFull<Symbol>>>;
pub(crate) type InstrCallMaybeKnownMethodPos =
    InstrNoFlow<InstrCallMaybeKnownMethodImpl<BcCallArgsPos>>;

impl<A: BcCallArgs<Symbol>> InstrNoFlowImpl for InstrCallImpl<A> {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (ArgPopsStack1, A, FrozenRef<'static, FrozenFileSpan>);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (_pop1, args, span): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let arguments = Arguments(args.pop_from_stack(stack));
        let f = stack.pop();
        f.invoke_with_loc(Some(*span), &arguments, eval)
    }
}

impl<F: BcFrozenCallable, A: BcCallArgs<Symbol>> InstrNoFlowImpl
    for InstrCallFrozenGenericImpl<F, A>
{
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (F, A, FrozenRef<'static, FrozenFileSpan>);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (fun, args, span): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let arguments = Arguments(args.pop_from_stack(stack));
        fun.bc_invoke(*span, &arguments, eval)
    }
}

impl<A: BcCallArgsForDef> InstrNoFlowImpl for InstrCallFrozenDefImpl<A> {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (
        FrozenValueTyped<'static, FrozenDef>,
        A,
        FrozenRef<'static, FrozenFileSpan>,
    );

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (fun, args, span): &Self::Arg,
        _pops: (),
    ) -> Result<Value<'v>, anyhow::Error> {
        let arguments = args.pop_from_stack(stack);
        eval.with_call_stack(fun.to_value(), Some(*span), |eval| {
            fun.as_ref().invoke_with_args(&arguments, eval)
        })
    }
}

/// Common of method invocation instructions.
#[inline(always)]
fn call_method_common<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: Value<'v>,
    symbol: &Symbol,
    arguments: &Arguments<'v, '_>,
    span: FrozenRef<'static, FrozenFileSpan>,
) -> anyhow::Result<Value<'v>> {
    // TODO: wrong span: should be span of `object.method`, not of the whole expression
    let method = get_attr_hashed_raw(this, symbol, eval.heap())?;
    match method {
        MemberOrValue::Member(member) => {
            member.to_value().invoke_method(this, span, arguments, eval)
        }
        MemberOrValue::Value(value) => value.invoke_with_loc(Some(span), arguments, eval),
    }
}

/// Common of method invocation instructions where a method is likely stdlib method.
#[inline(always)]
fn call_maybe_known_method_common<'v>(
    eval: &mut Evaluator<'v, '_>,
    this: Value<'v>,
    symbol: &Symbol,
    known_method: &KnownMethod,
    arguments: &Arguments<'v, '_>,
    span: FrozenRef<'static, FrozenFileSpan>,
) -> anyhow::Result<Value<'v>> {
    if let Some(methods) = this.get_ref().get_methods() {
        // Instead of method lookup by name, we compare `Methods` pointers.
        // If pointers are equal, getattr would return the same method
        // we already have.
        if ptr::eq(methods, known_method.type_methods) {
            return eval.with_call_stack(known_method.method.to_value(), Some(span), |eval| {
                known_method.method.invoke_method(
                    known_method.method.to_value(),
                    this,
                    arguments,
                    eval,
                    Private,
                )
            });
        }
    }

    call_method_common(eval, this, symbol, arguments, span)
}

impl<A: BcCallArgs<Symbol>> InstrNoFlowImpl for InstrCallMethodImpl<A> {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (ArgPopsStack1, Symbol, A, FrozenRef<'static, FrozenFileSpan>);

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (_pop1, symbol, args, span): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let arguments = Arguments(args.pop_from_stack(stack));
        let this = stack.pop();
        call_method_common(eval, this, symbol, &arguments, *span)
    }
}

impl<A: BcCallArgs<Symbol>> InstrNoFlowImpl for InstrCallMaybeKnownMethodImpl<A> {
    type Pop<'v> = ();
    type Push<'v> = Value<'v>;
    type Arg = (
        ArgPopsStack1,
        Symbol,
        KnownMethod,
        A,
        FrozenRef<'static, FrozenFileSpan>,
    );

    #[inline(always)]
    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (_pop1, symbol, known_method, args, span): &Self::Arg,
        _pops: (),
    ) -> anyhow::Result<Value<'v>> {
        let arguments = Arguments(args.pop_from_stack(stack));
        let this = stack.pop();
        call_maybe_known_method_common(eval, this, symbol, known_method, &arguments, *span)
    }
}

pub(crate) struct InstrPossibleGcImpl;
pub(crate) struct InstrBeforeStmtImpl;
pub(crate) struct InstrProfileBcImpl;

pub(crate) type InstrPossibleGc = InstrNoFlow<InstrPossibleGcImpl>;
pub(crate) type InstrBeforeStmt = InstrNoFlow<InstrBeforeStmtImpl>;
pub(crate) type InstrProfileBc = InstrNoFlow<InstrProfileBcImpl>;

impl InstrNoFlowImpl for InstrPossibleGcImpl {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = ();

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        (): &(),
        (): (),
    ) -> anyhow::Result<()> {
        possible_gc(eval);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrBeforeStmtImpl {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = FrozenFileSpan;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _: BcPtrAddr,
        span: &Self::Arg,
        (): (),
    ) -> anyhow::Result<()> {
        before_stmt(*span, eval);
        Ok(())
    }
}

impl InstrNoFlowImpl for InstrProfileBcImpl {
    type Pop<'v> = ();
    type Push<'v> = ();
    type Arg = BcOpcode;

    fn run_with_args<'v>(
        eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr,
        opcode: &BcOpcode,
        (): (),
    ) -> anyhow::Result<()> {
        eval.bc_profile.before_instr(*opcode);
        Ok(())
    }
}

/// Pseudo-instruction:
/// * to store bytecode metadata (i.e. spans): when bytecode is evaluated, we only have IP,
///   we don't have a pointer to bytecode object. To obtain spans by IP, we scroll
///   through the instruction until we encounter this pseudo-instruction.
/// * as a safety against memory overruns. Function block must terminate with return instruction,
///  but if return was missed, this instruction is executed and it panics.
pub(crate) struct InstrEnd;

impl BcInstr for InstrEnd {
    type Pop<'v> = ();
    type Push<'v> = ();
    /// Offset of current instruction and spans of all instructions.
    type Arg = (BcAddr, Vec<(BcAddr, BcInstrSlowArg)>);

    fn run<'v, 'b>(
        _eval: &mut Evaluator<'v, '_>,
        _stack: &mut BcStackPtr<'v, '_>,
        _ip: BcPtrAddr<'b>,
        (_this_instr_offset, _spans): &Self::Arg,
    ) -> InstrControl<'v, 'b> {
        unreachable!("this instruction is not meant to be executed");
    }
}

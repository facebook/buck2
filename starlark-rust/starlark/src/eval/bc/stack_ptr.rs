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

//! Stack pointer.

use std::{cell::Cell, marker, mem, mem::MaybeUninit, ptr, slice};

use gazebo::coerce::coerce_ref;

use crate::{
    eval::{
        bc::{
            call::{BcCallArgsFull, BcCallArgsPos},
            if_debug::IfDebug,
            instr_arg::{ArgPopsStack, ArgPopsStackMaybe1, ArgPushesStack},
        },
        runtime::arguments::{ArgNames, ArgSymbol, ArgumentsFull},
    },
    values::Value,
};

/// Valid pointer range for the stack.
///
/// Only used when debugging assertions enabled.
struct StackRange<'v, 's> {
    base: *mut Value<'v>,
    max_stack_size: usize,
    _marker: marker::PhantomData<&'s [Value<'v>]>,
}

impl<'v, 's> StackRange<'v, 's> {
    fn new(slice: &'s mut [MaybeUninit<Value<'v>>]) -> StackRange<'v, 's> {
        StackRange {
            base: slice.as_mut_ptr() as *mut Value<'v>,
            max_stack_size: slice.len(),
            _marker: marker::PhantomData,
        }
    }
}

/// A pointer to the current offset in the stack.
///
/// For performance, this is a real pointer rather than stack+offset.
///
/// This type does not do any range checks, so it is very unsafe.
pub(crate) struct BcStackPtr<'v, 's> {
    ptr: Cell<*mut Value<'v>>,
    // But we do range checks when debug assertions enabled.
    range: IfDebug<StackRange<'v, 's>>,
}

impl<'v, 's> BcStackPtr<'v, 's> {
    pub(crate) fn ptr(&self) -> *mut Value<'v> {
        self.ptr.get()
    }

    #[inline(always)]
    pub(crate) fn new(slice: &'s mut [MaybeUninit<Value<'v>>]) -> BcStackPtr<'v, 's> {
        BcStackPtr {
            ptr: Cell::new(slice.as_mut_ptr() as *mut Value<'v>),
            range: IfDebug::new(StackRange::new(slice)),
        }
    }

    /// Currently filled stack.
    fn stack_size_if_debug(&self) -> usize {
        unsafe {
            self.ptr
                .get()
                .offset_from(self.range.get_ref_if_debug().base) as usize
        }
    }

    /// Stack offset relative to some unspecified base.
    pub(crate) fn stack_offset(&self) -> usize {
        self.ptr.get() as usize / mem::size_of::<Value>()
    }

    /// Substack size from the current pointer to the top.
    fn remaining_if_debug(&self) -> usize {
        self.range.get_ref_if_debug().max_stack_size - self.stack_size_if_debug()
    }

    /// Assert stack size is zero when assertions enabled.
    pub(crate) fn debug_assert_zero(&self) {
        debug_assert!(self.stack_size_if_debug() == 0);
    }

    fn sub_assign(&self, delta: usize) {
        debug_assert!(self.stack_size_if_debug() >= delta);
        unsafe {
            self.ptr.set(self.ptr.get().sub(delta));
        }
    }

    fn add_assign(&self, delta: usize) {
        debug_assert!(self.remaining_if_debug() >= delta);
        unsafe {
            self.ptr.set(self.ptr.get().add(delta));
        }
    }

    pub(crate) fn top(&self) -> Value<'v> {
        debug_assert!(self.remaining_if_debug() >= 1);
        unsafe { ptr::read(self.ptr.get().sub(1)) }
    }

    pub(crate) fn pop(&self) -> Value<'v> {
        unsafe {
            self.sub_assign(1);
            ptr::read(self.ptr.get())
        }
    }

    pub(crate) fn pop_slice(&self, n: ArgPopsStack) -> &[Value<'v>] {
        self.sub_assign(n.0 as usize);
        unsafe { slice::from_raw_parts(self.ptr.get(), n.0 as usize) }
    }

    pub(crate) fn pop_maybe(&self, n: ArgPopsStackMaybe1) -> Option<Value<'v>> {
        if n.0 { Some(self.pop()) } else { None }
    }

    pub(crate) fn pop_array<const N: usize>(&self) -> [Value<'v>; N] {
        self.sub_assign(N);
        unsafe { ptr::read(self.ptr.get() as *const [Value; N]) }
    }

    pub(crate) fn pop_args<'a, S: ArgSymbol>(
        &'a self,
        a: &'a BcCallArgsFull<S>,
    ) -> ArgumentsFull<'v, 'a, S> {
        let kwargs = if a.kwargs { Some(self.pop()) } else { None };
        let args = if a.args { Some(self.pop()) } else { None };
        let pos_named = self.pop_slice(ArgPopsStack(a.pos_named));
        let (pos, named) = pos_named.split_at(pos_named.len() - a.names.len());
        ArgumentsFull {
            pos,
            named,
            names: ArgNames::new(coerce_ref(&a.names)),
            args,
            kwargs,
        }
    }

    pub(crate) fn pop_args_pos<'a, S: ArgSymbol>(
        &'a self,
        npos: &BcCallArgsPos,
    ) -> ArgumentsFull<'v, 'a, S> {
        let pos = self.pop_slice(ArgPopsStack(npos.pos));
        ArgumentsFull {
            pos,
            named: &[],
            names: ArgNames::new(&[]),
            args: None,
            kwargs: None,
        }
    }

    pub(crate) fn push(&mut self, value: Value<'v>) {
        unsafe {
            ptr::write(self.ptr.get(), value);
        }
        self.add_assign(1);
    }

    pub(crate) fn push_array<const N: usize>(&mut self, values: [Value<'v>; N]) {
        unsafe {
            ptr::write(self.ptr.get() as *mut [Value<'v>; N], values);
        }
        self.add_assign(N);
    }

    /// Increment the stack size, but do not write the values, but return the pointer to
    /// the values to be written instead.
    ///
    /// Caller must fill the stack or error the execution.
    pub(crate) fn push_slice_placeholder(
        &mut self,
        n: ArgPushesStack,
    ) -> &mut [MaybeUninit<Value<'v>>] {
        let r = unsafe {
            slice::from_raw_parts_mut(self.ptr.get() as *mut MaybeUninit<Value>, n.0 as usize)
        };
        self.add_assign(n.0 as usize);
        r
    }
}

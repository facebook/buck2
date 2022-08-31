/*
 * Copyright 2018 The Starlark in Rust Authors.
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

//! Detect recursion when doing `repr` or `to_json`.

use std::cell::Cell;
use std::intrinsics::unlikely;

use crate::collections::SmallSet;
use crate::values::layout::pointer::RawPointer;
use crate::values::Value;

/// Pop the stack on drop.
pub(crate) struct ReprStackGuard;

/// Pop the stack on drop.
pub(crate) struct JsonStackGuard;

impl Drop for ReprStackGuard {
    fn drop(&mut self) {
        let mut stack = REPR_STACK.take();
        let popped = stack.pop();
        debug_assert!(popped.is_some());
        REPR_STACK.set(stack);
    }
}

impl Drop for JsonStackGuard {
    fn drop(&mut self) {
        let mut stack = JSON_STACK.take();
        let popped = stack.pop();
        debug_assert!(popped.is_some());
        JSON_STACK.set(stack);
    }
}

/// Release per-thread memory.
///
/// `#[thread_local]` is fast, but it [does not call destructor on thread exit][1].
/// `thread_local!` is somewhat slower.
///
/// So we use `#[thread_local]` for fast access and `thread_local!` for releasing memory.
///
/// Note this is similar to how `thread_local!` actually implemented,
/// but this is somewhat more efficient.
///
/// [1]: https://play.rust-lang.org/?version=nightly&mode=debug&edition=2021&gist=874a250ed9642d3e1559a8ba7d26abfb
struct ReleaseMemoryOnThreadExit;

impl Drop for ReleaseMemoryOnThreadExit {
    fn drop(&mut self) {
        // We replace per-thread data with empty sets.
        // So destructors for these fields won't be called,
        // but that's fine because we don't need to release memory of empty sets.
        REPR_STACK.take();
        JSON_STACK.take();
    }
}

thread_local! {
    static RELEASE_MEMORY_ON_THREAD_EXIT: ReleaseMemoryOnThreadExit = ReleaseMemoryOnThreadExit;
}

/// Register a callback to release memory on thread exit.
#[cold]
#[inline(never)]
fn init_release_memory_on_thread_exit() {
    RELEASE_MEMORY_ON_THREAD_EXIT.with(|_| {});
}

/// Returned when `repr` is called recursively and a cycle is detected.
pub(crate) struct ReprCycle;

/// Returned when `to_json` is called recursively and a cycle is detected.
pub(crate) struct JsonCycle;

#[thread_local]
static REPR_STACK: Cell<SmallSet<RawPointer>> = Cell::new(SmallSet::new());

#[thread_local]
static JSON_STACK: Cell<SmallSet<RawPointer>> = Cell::new(SmallSet::new());

/// Push a value to the stack, return error if it is already on the stack.
pub(crate) fn repr_stack_push(value: Value) -> Result<ReprStackGuard, ReprCycle> {
    let mut stack = REPR_STACK.take();
    if unlikely(stack.capacity() == 0) {
        init_release_memory_on_thread_exit();
    }
    if unlikely(!stack.insert(value.ptr_value())) {
        REPR_STACK.set(stack);
        Err(ReprCycle)
    } else {
        REPR_STACK.set(stack);
        Ok(ReprStackGuard)
    }
}

/// Push a value to the stack, return error if it is already on the stack.
pub(crate) fn json_stack_push(value: Value) -> Result<JsonStackGuard, JsonCycle> {
    let mut stack = JSON_STACK.take();
    if unlikely(stack.capacity() == 0) {
        init_release_memory_on_thread_exit();
    }
    if unlikely(!stack.insert(value.ptr_value())) {
        JSON_STACK.set(stack);
        Err(JsonCycle)
    } else {
        JSON_STACK.set(stack);
        Ok(JsonStackGuard)
    }
}

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

//! Starlark call stack.

// FIXME: I think we should rewrite the CallStack stuff entirely:
// * Don't keep a call stack, just a call stack depth.
// * When people did StackGuard.inc just do CallStack.inc, we need less info
// once it's an int so can reuse.
// * When an exception happens, decorate it with the call stack on the way back
//   up, in eval_call.

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::intrinsics::unlikely;

use gazebo::prelude::*;
use once_cell::sync::Lazy;

use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Span;
use crate::errors::Frame;
use crate::eval::runtime::inlined_frame::InlinedFrames;
use crate::values::FrozenRef;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub(crate) struct FrozenFileSpan {
    pub(crate) file: FrozenRef<'static, CodeMap>,
    pub(crate) span: Span,
    /// Parent frames.
    pub(crate) inlined_frames: InlinedFrames,
}

impl FrozenFileSpan {
    pub(crate) const fn new_unchecked(file: FrozenRef<'static, CodeMap>, span: Span) -> Self {
        FrozenFileSpan {
            file,
            span,
            inlined_frames: InlinedFrames { frames: None },
        }
    }

    pub(crate) fn new(file: FrozenRef<'static, CodeMap>, span: Span) -> Self {
        // Check the span is valid: this will panic if the span is not valid.
        file.source_span(span);

        Self::new_unchecked(file, span)
    }

    pub(crate) fn file(&self) -> FrozenRef<'static, CodeMap> {
        self.file
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    pub(crate) fn end_span(&self) -> FrozenFileSpan {
        FrozenFileSpan {
            file: self.file,
            span: self.span.end_span(),
            inlined_frames: self.inlined_frames,
        }
    }
}

impl Default for FrozenFileSpan {
    fn default() -> Self {
        static EMPTY_FILE: Lazy<CodeMap> = Lazy::new(CodeMap::default);
        FrozenFileSpan::new(FrozenRef::new(&EMPTY_FILE), Span::default())
    }
}

impl Display for FrozenFileSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.to_file_span(), f)
    }
}

impl FrozenFileSpan {
    pub(crate) fn to_file_span(&self) -> FileSpan {
        FileSpan {
            file: (*self.file).dupe(),
            span: self.span,
        }
    }

    pub(crate) fn merge(&self, other: &FrozenFileSpan) -> FrozenFileSpan {
        if self.file == other.file {
            FrozenFileSpan {
                file: self.file,
                span: self.span.merge(other.span),
                inlined_frames: self.inlined_frames,
            }
        } else {
            // We need to pick something if we merge two spans from different files.
            *self
        }
    }
}

// A value akin to Frame, but can be created cheaply, since it doesn't resolve
// anything in advance.
// The downside is it has a lifetime on 'v and keeps alive the whole CodeMap.
#[derive(Clone, Copy, Dupe)]
struct CheapFrame<'v> {
    function: Value<'v>,
    span: Option<FrozenRef<'static, FrozenFileSpan>>,
}

impl CheapFrame<'_> {
    fn location(&self) -> Option<FileSpan> {
        self.span.map(|span| span.to_file_span())
    }

    fn extend_frames(&self, frames: &mut Vec<Frame>) {
        if let Some(span) = self.span {
            span.inlined_frames.extend_frames(frames);
        }
        frames.push(Frame {
            name: self.function.name_for_call_stack(),
            location: self.location(),
        });
    }
}

impl Debug for CheapFrame<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut x = f.debug_struct("Frame");
        x.field("function", &self.function);
        x.field("span", &self.span);
        x.finish()
    }
}

#[derive(Debug, thiserror::Error)]
enum CallStackError {
    #[error("Requested {0}-th top frame, but stack size is {1} (internal error)")]
    StackIsTooShallowForNthTopFrame(usize, usize),
    #[error("Starlark call stack overflow")]
    Overflow,
}

/// Starlark call stack.
#[derive(Debug)]
pub(crate) struct CheapCallStack<'v> {
    count: usize,
    stack: [CheapFrame<'v>; MAX_CALLSTACK_RECURSION],
}

impl<'v> Default for CheapCallStack<'v> {
    fn default() -> Self {
        Self {
            count: 0,
            stack: [CheapFrame {
                function: Value::new_none(),
                span: None,
            }; MAX_CALLSTACK_RECURSION],
        }
    }
}

// Currently, each frame typically allocates about 1K of native stack size (see `test_frame_size`),
// but it is a bit more complicated:
// * each for loop in a frame allocates more native stack
// * inlined functions do not allocate native stack
// Practically max call stack depends on native stack size,
// and depending on environment, it may be configured differently, for example:
// * macOS default stack size is 512KB
// * Linux default stack size is 8MB
// * [tokio default stack size is 2MB][1]
// [1] https://docs.rs/tokio/0.2.1/tokio/runtime/struct.Builder.html#method.thread_stack_size
// TODO(nga): count loops in call stack size.
// TODO(nga): make it configurable.
const MAX_CALLSTACK_RECURSION: usize = 40;

unsafe impl<'v> Trace<'v> for CheapCallStack<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let (used, unused) = self.stack.split_at_mut(self.count);
        for x in used {
            x.function.trace(tracer);
        }
        // Not required, but since we are chosing not to walk those above
        // the current stack depth, it's good practice to blank those values out
        for x in unused {
            x.function = Value::new_none();
            x.span = None;
        }
    }
}

impl<'v> CheapCallStack<'v> {
    /// Push an element to the stack. It is important the each `push` is paired
    /// with a `pop`.
    pub(crate) fn push(
        &mut self,
        function: Value<'v>,
        span: Option<FrozenRef<'static, FrozenFileSpan>>,
    ) -> anyhow::Result<()> {
        if unlikely(self.count >= MAX_CALLSTACK_RECURSION) {
            return Err(CallStackError::Overflow.into());
        }
        self.stack[self.count] = CheapFrame { function, span };
        self.count += 1;
        Ok(())
    }

    /// Remove the top element from the stack. Called after `push`.
    pub(crate) fn pop(&mut self) {
        debug_assert!(self.count >= 1);
        // We could clear the elements, but don't need to bother
        self.count -= 1;
    }

    /// The location at the top of the stack. May be `None` if
    /// either there the stack is empty, or the top of the stack lacks location
    /// information (e.g. called from Rust).
    pub(crate) fn top_location(&self) -> Option<FileSpan> {
        if self.count == 0 {
            None
        } else {
            self.stack[self.count - 1].location()
        }
    }

    /// `n`-th element from the top of the stack.
    pub(crate) fn top_nth_function(&self, n: usize) -> anyhow::Result<Value<'v>> {
        let index = self
            .count
            .checked_sub(1)
            .and_then(|x| x.checked_sub(n))
            .ok_or(CallStackError::StackIsTooShallowForNthTopFrame(
                n, self.count,
            ))?;
        Ok(self.stack[index].function)
    }

    pub(crate) fn to_diagnostic_frames(&self, inlined_frames: InlinedFrames) -> CallStack {
        // The first entry is just the entire module, so skip it
        let mut frames = Vec::new();
        for frame in &self.stack[1..self.count] {
            frame.extend_frames(&mut frames);
        }
        inlined_frames.extend_frames(&mut frames);
        CallStack { frames }
    }

    /// List the entries on the stack as values
    pub(crate) fn to_function_values(&self) -> Vec<Value<'v>> {
        self.stack[1..self.count].map(|x| x.function)
    }
}

/// Owned call stack.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct CallStack {
    frames: Vec<Frame>,
}

impl CallStack {
    /// Is the call stack empty?
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Take the contained frames.
    pub fn into_frames(self) -> Vec<Frame> {
        self.frames
    }
}

impl Display for CallStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.frames.is_empty() {
            // Match Python output.
            writeln!(f, "Traceback (most recent call last):")?;
            // TODO(nga): use real module name.
            let mut prev = "<module>";
            for x in &self.frames {
                x.write_two_lines("  ", prev, f)?;
                prev = &x.name;
            }
        }
        Ok(())
    }
}

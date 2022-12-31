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

use std::fmt;
use std::fmt::Display;

use gazebo::dupe::Dupe;
use once_cell::sync::Lazy;

use crate::codemap::CodeMap;
use crate::codemap::FileSpan;
use crate::codemap::Span;
use crate::eval::runtime::inlined_frame::InlinedFrames;
use crate::values::FrozenRef;

/// Span of the call frame (including inlined call frames).
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub(crate) struct FrameSpan {
    pub(crate) file: FrozenRef<'static, CodeMap>,
    pub(crate) span: Span,
    /// Parent frames.
    pub(crate) inlined_frames: InlinedFrames,
}

impl FrameSpan {
    pub(crate) const fn new_unchecked(file: FrozenRef<'static, CodeMap>, span: Span) -> Self {
        FrameSpan {
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

    pub(crate) fn end_span(&self) -> FrameSpan {
        FrameSpan {
            file: self.file,
            span: self.span.end_span(),
            inlined_frames: self.inlined_frames,
        }
    }
}

impl Default for FrameSpan {
    fn default() -> Self {
        static EMPTY_FILE: Lazy<CodeMap> = Lazy::new(CodeMap::default);
        FrameSpan::new(FrozenRef::new(&EMPTY_FILE), Span::default())
    }
}

impl Display for FrameSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.to_file_span(), f)
    }
}

impl FrameSpan {
    pub(crate) fn to_file_span(&self) -> FileSpan {
        FileSpan {
            file: (*self.file).dupe(),
            span: self.span,
        }
    }

    pub(crate) fn merge(&self, other: &FrameSpan) -> FrameSpan {
        if self.file == other.file {
            FrameSpan {
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

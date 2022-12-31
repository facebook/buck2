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
use crate::codemap::Span;
use crate::eval::runtime::frozen_file_span::FrozenFileSpan;
use crate::eval::runtime::inlined_frame::InlinedFrames;
use crate::values::FrozenRef;

/// Span of the call frame (including inlined call frames).
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub(crate) struct FrameSpan {
    pub(crate) span: FrozenFileSpan,
    /// Parent frames.
    pub(crate) inlined_frames: InlinedFrames,
}

impl FrameSpan {
    pub(crate) const fn new(span: FrozenFileSpan) -> FrameSpan {
        FrameSpan {
            span,
            inlined_frames: InlinedFrames { frames: None },
        }
    }

    pub(crate) fn end_span(&self) -> FrameSpan {
        FrameSpan {
            span: self.span.end_span(),
            inlined_frames: self.inlined_frames,
        }
    }
}

impl Default for FrameSpan {
    fn default() -> Self {
        static EMPTY_FILE: Lazy<CodeMap> = Lazy::new(CodeMap::default);
        FrameSpan::new(FrozenFileSpan::new(
            FrozenRef::new(&EMPTY_FILE),
            Span::default(),
        ))
    }
}

impl Display for FrameSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO(nga): either display inlined frames or remove this.
        Display::fmt(&self.span.to_file_span(), f)
    }
}

impl FrameSpan {
    pub(crate) fn merge(&self, other: &FrameSpan) -> FrameSpan {
        // TODO(nga): merge inlined frames.
        FrameSpan {
            span: self.span.merge(&other.span),
            inlined_frames: self.inlined_frames,
        }
    }
}

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

use std::cmp;

use allocative::Allocative;

use crate::values::layout::aligned_size::AlignedSize;
use crate::values::layout::heap::arena::MIN_ALLOC;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::value_alloc_size::ValueAllocSize;

/// Size of `AValue` without header.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative)]
pub(crate) struct ValueSize {
    size: AlignedSize,
}

impl ValueSize {
    #[inline]
    pub(crate) const fn new(size: AlignedSize) -> ValueSize {
        ValueSize { size }
    }

    #[inline]
    pub(crate) fn add_header(self) -> ValueAllocSize {
        ValueAllocSize::new(cmp::max(
            AlignedSize::of::<AValueHeader>() + self.size,
            // We require at least usize space available for overwrite/blackhole.
            MIN_ALLOC,
        ))
    }

    #[inline]
    pub(crate) const fn align_up(bytes: usize) -> ValueSize {
        ValueSize::new(AlignedSize::align_up(bytes))
    }

    #[inline]
    pub(crate) const fn of_align_up<T>() -> ValueSize {
        ValueSize::new(AlignedSize::of_align_up::<T>())
    }
}

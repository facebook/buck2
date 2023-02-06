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

use std::alloc::Layout;
use std::mem;
use std::ops::Add;

use allocative::Allocative;
use dupe::Dupe;

use crate::values::layout::heap::repr::AValueHeader;

/// Allocations in Starlark are word-aligned, and this type represents the size of an allocation.
#[derive(
    Copy, Clone, Dupe, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Allocative
)]
#[repr(transparent)]
pub(crate) struct AlignedSize {
    /// Starlark only supports objects smaller than 1<<32.
    bytes: u32,
}

impl AlignedSize {
    const MAX_SIZE: AlignedSize =
        AlignedSize::new_bytes(u32::MAX as usize - AValueHeader::ALIGN + 1);

    #[inline]
    pub(crate) const fn new_bytes(bytes: usize) -> AlignedSize {
        assert!(bytes % AValueHeader::ALIGN == 0);
        assert!(bytes as u32 as usize == bytes);
        let bytes = bytes as u32;
        AlignedSize { bytes }
    }

    #[inline]
    pub(crate) const fn align_up(bytes: usize) -> AlignedSize {
        assert!(bytes <= AlignedSize::MAX_SIZE.bytes() as usize);
        let bytes = (bytes + AValueHeader::ALIGN - 1) & !(AValueHeader::ALIGN - 1);
        let bytes = bytes as u32;
        AlignedSize { bytes }
    }

    #[inline]
    pub(crate) const fn bytes(self) -> u32 {
        self.bytes
    }

    #[inline]
    pub(crate) const fn of<T>() -> AlignedSize {
        assert!(mem::align_of::<T>() == AValueHeader::ALIGN);
        AlignedSize::new_bytes(mem::size_of::<T>())
    }

    #[inline]
    pub(crate) const fn of_align_up<T>() -> AlignedSize {
        AlignedSize::align_up(mem::size_of::<T>())
    }

    #[inline]
    pub(crate) const fn layout(self) -> Layout {
        match Layout::from_size_align(self.bytes as usize, AValueHeader::ALIGN) {
            Ok(layout) => layout,
            Err(_) => panic!("Layout::from_size_align failed"),
        }
    }
}

impl Add for AlignedSize {
    type Output = AlignedSize;

    #[inline]
    fn add(self, rhs: AlignedSize) -> AlignedSize {
        AlignedSize::new_bytes(self.bytes.checked_add(rhs.bytes).unwrap() as usize)
    }
}

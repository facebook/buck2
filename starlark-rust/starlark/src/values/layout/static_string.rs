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

//! Statically allocated strings.

use std::{ptr, sync::atomic::AtomicU32};

use crate::values::{
    layout::{arena::AValueRepr, avalue::VALUE_STR_A_VALUE_PTR},
    string::StarlarkStrN,
    FrozenStringValue, FrozenValue,
};

/// A constant string that can be converted to a [`FrozenValue`].
#[repr(C)] // Must match this layout on the heap
pub struct StarlarkStrNRepr<const N: usize> {
    repr: AValueRepr<StarlarkStrN<N>>,
}

impl<const N: usize> StarlarkStrNRepr<N> {
    /// Create a new [`StarlarkStrNRepr`] given a string of size `N`.
    /// If the string has a different size it will fail.
    pub const fn new(s: &str) -> Self {
        assert!(N == s.len());
        assert!(N as u32 as usize == N);
        let mut payload = [0u8; N];
        unsafe { ptr::copy_nonoverlapping(s.as_ptr(), payload.as_mut_ptr(), N) };
        Self {
            repr: AValueRepr {
                header: VALUE_STR_A_VALUE_PTR,
                payload: StarlarkStrN {
                    len: N as u32,
                    hash: AtomicU32::new(0),
                    body: payload,
                },
            },
        }
    }

    /// Obtain the [`FrozenValue`] for a [`StarlarkStrNRepr`].
    pub fn unpack(&'static self) -> FrozenValue {
        FrozenValue::new_ptr(&self.repr.header, true)
    }

    /// Erase the type parameter, giving a slightly nicer user experience.
    pub fn erase(&'static self) -> FrozenStringValue {
        unsafe { FrozenStringValue::new_unchecked(self.unpack()) }
    }
}

pub(crate) static VALUE_EMPTY_STRING: StarlarkStrNRepr<0> = StarlarkStrNRepr::new("");

#[inline(always)]
pub(crate) fn constant_string(x: &str) -> Option<FrozenStringValue> {
    if x.len() > 1 {
        None
    } else if x.is_empty() {
        Some(VALUE_EMPTY_STRING.erase())
    } else {
        // If the string is 1 byte long there can only be up to the first 128 characters present
        // therefore this index will be total
        Some(VALUE_BYTE_STRINGS[x.as_bytes()[0] as usize].erase())
    }
}

pub(crate) static VALUE_BYTE_STRINGS: [StarlarkStrNRepr<1>; 128] = [
    StarlarkStrNRepr::new("\x00"),
    StarlarkStrNRepr::new("\x01"),
    StarlarkStrNRepr::new("\x02"),
    StarlarkStrNRepr::new("\x03"),
    StarlarkStrNRepr::new("\x04"),
    StarlarkStrNRepr::new("\x05"),
    StarlarkStrNRepr::new("\x06"),
    StarlarkStrNRepr::new("\x07"),
    StarlarkStrNRepr::new("\x08"),
    StarlarkStrNRepr::new("\x09"),
    StarlarkStrNRepr::new("\x0A"),
    StarlarkStrNRepr::new("\x0B"),
    StarlarkStrNRepr::new("\x0C"),
    StarlarkStrNRepr::new("\x0D"),
    StarlarkStrNRepr::new("\x0E"),
    StarlarkStrNRepr::new("\x0F"),
    StarlarkStrNRepr::new("\x10"),
    StarlarkStrNRepr::new("\x11"),
    StarlarkStrNRepr::new("\x12"),
    StarlarkStrNRepr::new("\x13"),
    StarlarkStrNRepr::new("\x14"),
    StarlarkStrNRepr::new("\x15"),
    StarlarkStrNRepr::new("\x16"),
    StarlarkStrNRepr::new("\x17"),
    StarlarkStrNRepr::new("\x18"),
    StarlarkStrNRepr::new("\x19"),
    StarlarkStrNRepr::new("\x1A"),
    StarlarkStrNRepr::new("\x1B"),
    StarlarkStrNRepr::new("\x1C"),
    StarlarkStrNRepr::new("\x1D"),
    StarlarkStrNRepr::new("\x1E"),
    StarlarkStrNRepr::new("\x1F"),
    StarlarkStrNRepr::new("\x20"),
    StarlarkStrNRepr::new("\x21"),
    StarlarkStrNRepr::new("\x22"),
    StarlarkStrNRepr::new("\x23"),
    StarlarkStrNRepr::new("\x24"),
    StarlarkStrNRepr::new("\x25"),
    StarlarkStrNRepr::new("\x26"),
    StarlarkStrNRepr::new("\x27"),
    StarlarkStrNRepr::new("\x28"),
    StarlarkStrNRepr::new("\x29"),
    StarlarkStrNRepr::new("\x2A"),
    StarlarkStrNRepr::new("\x2B"),
    StarlarkStrNRepr::new("\x2C"),
    StarlarkStrNRepr::new("\x2D"),
    StarlarkStrNRepr::new("\x2E"),
    StarlarkStrNRepr::new("\x2F"),
    StarlarkStrNRepr::new("\x30"),
    StarlarkStrNRepr::new("\x31"),
    StarlarkStrNRepr::new("\x32"),
    StarlarkStrNRepr::new("\x33"),
    StarlarkStrNRepr::new("\x34"),
    StarlarkStrNRepr::new("\x35"),
    StarlarkStrNRepr::new("\x36"),
    StarlarkStrNRepr::new("\x37"),
    StarlarkStrNRepr::new("\x38"),
    StarlarkStrNRepr::new("\x39"),
    StarlarkStrNRepr::new("\x3A"),
    StarlarkStrNRepr::new("\x3B"),
    StarlarkStrNRepr::new("\x3C"),
    StarlarkStrNRepr::new("\x3D"),
    StarlarkStrNRepr::new("\x3E"),
    StarlarkStrNRepr::new("\x3F"),
    StarlarkStrNRepr::new("\x40"),
    StarlarkStrNRepr::new("\x41"),
    StarlarkStrNRepr::new("\x42"),
    StarlarkStrNRepr::new("\x43"),
    StarlarkStrNRepr::new("\x44"),
    StarlarkStrNRepr::new("\x45"),
    StarlarkStrNRepr::new("\x46"),
    StarlarkStrNRepr::new("\x47"),
    StarlarkStrNRepr::new("\x48"),
    StarlarkStrNRepr::new("\x49"),
    StarlarkStrNRepr::new("\x4A"),
    StarlarkStrNRepr::new("\x4B"),
    StarlarkStrNRepr::new("\x4C"),
    StarlarkStrNRepr::new("\x4D"),
    StarlarkStrNRepr::new("\x4E"),
    StarlarkStrNRepr::new("\x4F"),
    StarlarkStrNRepr::new("\x50"),
    StarlarkStrNRepr::new("\x51"),
    StarlarkStrNRepr::new("\x52"),
    StarlarkStrNRepr::new("\x53"),
    StarlarkStrNRepr::new("\x54"),
    StarlarkStrNRepr::new("\x55"),
    StarlarkStrNRepr::new("\x56"),
    StarlarkStrNRepr::new("\x57"),
    StarlarkStrNRepr::new("\x58"),
    StarlarkStrNRepr::new("\x59"),
    StarlarkStrNRepr::new("\x5A"),
    StarlarkStrNRepr::new("\x5B"),
    StarlarkStrNRepr::new("\x5C"),
    StarlarkStrNRepr::new("\x5D"),
    StarlarkStrNRepr::new("\x5E"),
    StarlarkStrNRepr::new("\x5F"),
    StarlarkStrNRepr::new("\x60"),
    StarlarkStrNRepr::new("\x61"),
    StarlarkStrNRepr::new("\x62"),
    StarlarkStrNRepr::new("\x63"),
    StarlarkStrNRepr::new("\x64"),
    StarlarkStrNRepr::new("\x65"),
    StarlarkStrNRepr::new("\x66"),
    StarlarkStrNRepr::new("\x67"),
    StarlarkStrNRepr::new("\x68"),
    StarlarkStrNRepr::new("\x69"),
    StarlarkStrNRepr::new("\x6A"),
    StarlarkStrNRepr::new("\x6B"),
    StarlarkStrNRepr::new("\x6C"),
    StarlarkStrNRepr::new("\x6D"),
    StarlarkStrNRepr::new("\x6E"),
    StarlarkStrNRepr::new("\x6F"),
    StarlarkStrNRepr::new("\x70"),
    StarlarkStrNRepr::new("\x71"),
    StarlarkStrNRepr::new("\x72"),
    StarlarkStrNRepr::new("\x73"),
    StarlarkStrNRepr::new("\x74"),
    StarlarkStrNRepr::new("\x75"),
    StarlarkStrNRepr::new("\x76"),
    StarlarkStrNRepr::new("\x77"),
    StarlarkStrNRepr::new("\x78"),
    StarlarkStrNRepr::new("\x79"),
    StarlarkStrNRepr::new("\x7A"),
    StarlarkStrNRepr::new("\x7B"),
    StarlarkStrNRepr::new("\x7C"),
    StarlarkStrNRepr::new("\x7D"),
    StarlarkStrNRepr::new("\x7E"),
    StarlarkStrNRepr::new("\x7F"),
];

/// Create a [`FrozenStringValue`].
#[macro_export]
macro_rules! const_frozen_string {
    ($s:expr) => {{
        const N: usize = $s.len();
        static X: starlark::values::StarlarkStrNRepr<N> =
            starlark::values::StarlarkStrNRepr::new($s);
        X.erase()
    }};
}

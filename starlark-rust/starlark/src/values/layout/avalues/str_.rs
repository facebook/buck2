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

use std::ptr::copy_nonoverlapping;

use starlark_map::Hashed;

use crate::collections::StarlarkHashValue;
use crate::pagable::vtable_register::register_special_avalue_frozen;
use crate::values::FreezeResult;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::StringValueLike as _;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::constant_string;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::heap::repr::AValueHeader;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::layout::heap::repr::ForwardPtr;
use crate::values::string::str_type::StarlarkStr;

pub(crate) const VALUE_STR_A_VALUE_PTR: AValueHeader =
    AValueHeader::new_const::<StarlarkStrAValue>();

#[inline]
pub(crate) fn starlark_str<'v>(
    len: usize,
    hash: StarlarkHashValue,
) -> AValueImpl<'v, impl AValue<'v, ExtraElem = usize> + Send + Sync> {
    AValueImpl::<StarlarkStrAValue>::new(unsafe { StarlarkStr::new(len, hash) })
}

pub(crate) struct StarlarkStrAValue;

impl<'v> AValue<'v> for StarlarkStrAValue {
    type StarlarkValue = StarlarkStr;

    type ExtraElem = usize;

    fn extra_len(value: &StarlarkStr) -> usize {
        StarlarkStr::payload_len_for_len(value.len())
    }

    fn offset_of_extra() -> usize {
        StarlarkStr::offset_of_content()
    }

    const IS_STR: bool = true;

    unsafe fn heap_freeze(
        me: *mut AValueRepr<Self::StarlarkValue>,
        freezer: &Freezer,
    ) -> FreezeResult<FrozenValue> {
        unsafe {
            debug_assert!(
                (*me).payload.len() > 1,
                "short strings are allocated statically"
            );

            let s = (*me).payload.as_str();
            let fv = freezer.alloc(s);
            debug_assert!(fv.is_str());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_frozen(fv),
            );
            Ok(fv)
        }
    }

    unsafe fn heap_copy(
        me: *mut AValueRepr<Self::StarlarkValue>,
        tracer: &Tracer<'v>,
    ) -> Value<'v> {
        unsafe {
            debug_assert!(
                (*me).payload.len() > 1,
                "short strings are allocated statically"
            );

            let s = (*me).payload.as_str();
            let v = tracer.alloc_str(s);
            debug_assert!(v.is_str());
            AValueHeader::overwrite_with_forward::<Self::StarlarkValue>(
                me,
                ForwardPtr::new_unfrozen(v),
            );
            v
        }
    }
}

impl FrozenHeap {
    /// Allocate a string on this heap. Be careful about the warnings around
    /// [`FrozenValue`].
    ///
    /// Since the heap is frozen, we always prefer to intern the string in order
    /// to deduplicate it and save some memory.
    pub fn alloc_str(&self, x: &str) -> FrozenStringValue {
        self.alloc_str_intern(x)
    }

    /// Intern string.
    pub(crate) fn alloc_str_intern(&self, s: &str) -> FrozenStringValue {
        self.alloc_str_hashed(Hashed::new(s))
    }

    /// Allocate prehashed string.
    pub fn alloc_str_hashed(&self, s: Hashed<&str>) -> FrozenStringValue {
        if let Some(s) = constant_string(*s) {
            s
        } else {
            self.string_interner().intern(s, || {
                self.alloc_str_init(s.len(), s.hash(), |dest| unsafe {
                    copy_nonoverlapping(s.as_ptr(), dest, s.len())
                })
            })
        }
    }
}

impl<'v> Heap<'v> {
    /// Allocate a string on the heap.
    pub fn alloc_str(self, x: &str) -> StringValue<'v> {
        if let Some(x) = constant_string(x) {
            x.to_string_value()
        } else {
            self.alloc_str_init(x.len(), StarlarkStr::UNINIT_HASH, |dest| unsafe {
                copy_nonoverlapping(x.as_ptr(), dest, x.len())
            })
        }
    }

    /// Intern string.
    pub fn alloc_str_intern(self, x: &str) -> StringValue<'v> {
        if let Some(x) = constant_string(x) {
            x.to_string_value()
        } else {
            let x = Hashed::new(x);
            self.string_interner().intern(x, || {
                self.alloc_str_init(x.len(), x.hash(), |dest| unsafe {
                    copy_nonoverlapping(x.as_ptr(), dest, x.len())
                })
            })
        }
    }

    /// Allocate a string on the heap, based on two concatenated strings.
    pub fn alloc_str_concat(self, x: &str, y: &str) -> StringValue<'v> {
        if x.is_empty() {
            self.alloc_str(y)
        } else if y.is_empty() {
            self.alloc_str(x)
        } else {
            self.alloc_str_init(x.len() + y.len(), StarlarkStr::UNINIT_HASH, |dest| unsafe {
                copy_nonoverlapping(x.as_ptr(), dest, x.len());
                copy_nonoverlapping(y.as_ptr(), dest.add(x.len()), y.len())
            })
        }
    }

    /// Allocate a string on the heap, based on three concatenated strings.
    pub fn alloc_str_concat3(self, x: &str, y: &str, z: &str) -> StringValue<'v> {
        if x.is_empty() {
            self.alloc_str_concat(y, z)
        } else if y.is_empty() {
            self.alloc_str_concat(x, z)
        } else if z.is_empty() {
            self.alloc_str_concat(x, y)
        } else {
            self.alloc_str_init(
                x.len() + y.len() + z.len(),
                StarlarkStr::UNINIT_HASH,
                |dest| unsafe {
                    copy_nonoverlapping(x.as_ptr(), dest, x.len());
                    let dest = dest.add(x.len());
                    copy_nonoverlapping(y.as_ptr(), dest, y.len());
                    let dest = dest.add(y.len());
                    copy_nonoverlapping(z.as_ptr(), dest, z.len());
                },
            )
        }
    }

    pub(crate) fn alloc_char(self, x: char) -> StringValue<'v> {
        let mut dst = [0; 4];
        let res = x.encode_utf8(&mut dst);
        self.alloc_str(res)
    }
}

// Register vtable for StarlarkStr (special type not handled by #[starlark_value] macro).
register_special_avalue_frozen!(StarlarkStr, StarlarkStrAValue);

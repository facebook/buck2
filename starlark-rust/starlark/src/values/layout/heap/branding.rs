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

//! Documentation-only module to provide an overview of branding in starlark.
//!
//! The lifetimes of starlark values are tied to the lifetimes of a heap that they are allocated in.
//! Starlark uses branded accesses to the heap to ensure that starlark values cannot escape the
//! lifetime of the heap they're tied to, without needing to reference count access to the heap on
//! each starlark value. This module explains how that works.
//!
//! The most important factor in understanding branding is understanding the `'v` lifetime that
//! appears on `Value<'v>`, `Heap<'v>`, and similar types. Unusually, this lifetime should not be
//! understood as representing the "object lifetime" of the heap in a traditional sense. Instead,
//! the `'v` lifetime functions as a unique identifier of a particular heap. In other words, the
//! contract is that if there are two `Value<'v>`s for the same `'v`, the values are allocated in
//! the same heap.
//!
//! To see how this works in practice, it's best to consider an example of how this might fail.
//! Consider code like this:
//!
//! ```rust,ignore
//! # use crate::values::Heap;
//! # use crate::values::Value;
//! # use crate::values::tuple::AllocTuple;
//! Heap::temp(|heap1| {
//!     Heap::temp(|heap2| {
//!         let s1: Value<'_> = heap1.alloc_str("abc").to_value();
//!         let v: Value<'_> = heap2.alloc(AllocTuple([s1]));
//!     })
//! })
//! ```
//!
//! This code attempts to first allocate a string value in `heap1` and then allocate a value in
//! `heap2` that references that value. This is an example of the exact kind of "cross-heap
//! confusion" that we want to disallow, and indeed this fails to compile (though the error message
//! is unfortunately not super helpful):
//!
//! ```rust,ignore
//! error[E0521]: borrowed data escapes outside of closure
//!   --> fbcode/buck2/starlark-rust/starlark/src/values/layout/heap/branding.rs:55:32
//!    |
//! 52 |     Heap::temp(|heap1| {
//!    |                 -----
//!    |                 |
//!    |                 `heap1` is a reference that is only valid in the closure body
//!    |                 has type `Heap<'1>`
//! ...
//! 55 |             let v: Value<'_> = heap2.alloc(AllocTuple([s1]));
//!    |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!    |                                |
//!    |                                `heap1` escapes the closure body here
//!    |                                argument requires that `'1` must outlive `'static`
//! ```
//!
//! ### Implementation
//!
//! "Branding" is a searchable term that will yield lots of online discussion about how to make use
//! of this pattern, but in general implementing this behavior consists of only two parts:
//!
//!  1. `Value<'v>`, `Heap<'v>`, and all similar types must be invariant in `'v`. This ensures that
//!     it's never possible to convert a `Value<'v1>` to a `Value<'v2>`.
//!  2. The "root" of all accesses to heaps must be via "branded closures" like `F: for<'v>
//!     FnOnce(Heap<'v>)`, essentially functions with signatures similar to `Heap::temp`.
//!
//! That's it; combined, this means that user code can never prove that the `'v1` coming from one
//! branded closure is the same as the `'v2` coming from another.
//!
//! ### References to frozen heaps
//!
//! To support references to values in frozen heaps, we expand the above contract somewhat. Instead
//! of requiring that a `Value<'v>` must be allocated in a `Heap<'v>`, we require that it must be
//! allocated in a `Heap<'v>` *or in any frozen heap that the `Heap<'v>` depends on.*
//!
//! From the perspective of object lifetimes this is obviously sane (the `Heap<'v>` will keep the
//! dependent frozen heaps alive). We make use of the expanded contract by then providing an API
//! like this:
//!
//! ```rust,ignore
//! impl<'v> Heap<'v> {
//!     pub fn access_owned_frozen_value(self, v: OwnedFrozenValue) -> Value<'v>;
//! }
//! ```
//!
//! `access_owned_frozen_value` adds the heap the frozen value is associated with as a dependency of
//! the current heap, and then returns a `Value<'v>`; the `'v` lifetime in the return value
//! essentially acts as a proof/endorsement that the given value is sound to use "within the context
//! of the current heap."

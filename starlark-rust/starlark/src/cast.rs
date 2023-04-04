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

#[inline(always)]
pub(crate) fn ptr_to_usize<T: ?Sized>(x: &T) -> usize {
    x as *const T as *const () as usize
}

/// Undefined behaviour if the argument is zero, or does not satisfy the alignment
/// of type `T`.
#[inline(always)]
pub(crate) unsafe fn usize_to_ptr<'a, T>(x: usize) -> &'a T {
    &*(x as *const T)
}

/// Undefined behaviour if the argument does not satisfy the alignment of type `To`.
#[inline(always)]
pub(crate) unsafe fn ptr<From, To>(x: &From) -> &To {
    &*(x as *const From as *const To)
}

#[inline(always)]
pub(crate) unsafe fn ptr_lifetime<'a, 'b, T: ?Sized>(x: &'a T) -> &'b T {
    &*(x as *const T)
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(try_blocks)]
#![feature(type_alias_impl_trait)]

mod actions;
mod context;

/// This crate has no public API, everything it implements is linked with `LateBinding`.
/// So declare this symbol to be referenced from main crate to make sure
/// this crate is linked.
pub fn ensure_linked() {}

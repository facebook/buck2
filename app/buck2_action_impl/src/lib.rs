/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod context;

/// This crate has `#[ctor]` and no public members.
///
/// Reference this symbol from `cli` crate so linter would not complain about unused dependency
/// and linker won't erase this dependency on macOS.
pub fn this_crate_is_used() {}

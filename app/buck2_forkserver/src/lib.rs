/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(error_generic_member_access)]
#![cfg_attr(windows, feature(windows_process_extensions_main_thread_handle))]
pub mod client;
pub(crate) mod convert;
pub mod run;

#[cfg(unix)]
pub mod unix;

#[cfg(windows)]
mod win;

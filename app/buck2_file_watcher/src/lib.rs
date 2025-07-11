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
#![feature(used_with_arg)]

pub mod dep_files;
#[cfg(fbcode_build)]
mod edenfs;
pub mod file_watcher;
mod fs_hash_crawler;
pub mod mergebase;
mod notify;
mod stats;
mod watchman;

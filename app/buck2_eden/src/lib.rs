/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This can't be built in our OSS implementation.
#![cfg(fbcode_build)]
#![feature(error_generic_member_access)]
#![feature(used_with_arg)]

pub mod connection;
pub mod io_provider;

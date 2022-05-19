/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod aquery;
pub mod build;
pub mod clean;
pub mod client;
pub mod client_utils;
pub mod common;
pub mod cquery;
#[cfg_attr(unix, path = "daemon_unix.rs")]
#[cfg_attr(windows, path = "daemon_windows.rs")]
pub mod daemon_utils;
pub mod dice_dump;
pub mod docs;
pub mod install;
pub(crate) mod json;
pub mod materialize;
pub mod panic;
pub mod profile;
pub mod server;
pub mod targets;
pub mod targets_show_outputs;
pub mod test;
pub mod uquery;

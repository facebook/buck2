/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(box_syntax)]
#![feature(try_blocks)]

pub mod active_commands;
pub mod configs;
pub mod ctx;
pub mod daemon;
pub mod dice_tracker;
pub mod docs;
pub mod file_watcher;
pub mod heartbeat_guard;
pub mod host_info;
pub mod jemalloc_stats;
pub mod lsp;
pub mod materialize;
pub(crate) mod process_stats;
pub mod profile;
pub mod snapshot;
pub mod streaming_request_handler;
pub mod watchman;

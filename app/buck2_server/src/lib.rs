/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(try_blocks)]

pub mod active_commands;
pub mod builtin_docs;
mod clean_stale;
mod configs;
mod ctx;
pub mod daemon;
mod dice_tracker;
mod file_status;
mod file_watcher;
mod heartbeat_guard;
mod host_info;
mod jemalloc_stats;
pub mod lsp;
mod materialize;
mod net_io;
pub mod profile;
mod snapshot;
mod starlark_debug;
mod streaming_request_handler;
mod subscription;
mod trace_io;

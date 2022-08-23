/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(box_syntax)]
#![feature(async_closure)]

pub mod active_commands;
pub mod build;
pub mod clean;
pub mod client_utils;
pub mod configs;
pub mod ctx;
pub mod daemon;
pub mod dice_tracker;
pub mod docs;
pub mod dot;
pub mod file_watcher;
pub mod heartbeat_guard;
pub mod host_info;
pub mod install;
pub mod jemalloc_stats;
pub mod json;
pub mod lsp;
pub mod materialize;
pub mod profile;
pub mod query;
pub mod snapshot;
pub mod streaming_request_handler;
pub mod target_hash;
pub mod targets;
pub mod targets_show_outputs;
pub mod watchman;
pub mod with_current_directory;

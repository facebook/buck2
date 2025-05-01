/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(try_blocks)]
#![feature(once_cell_try)]
#![feature(used_with_arg)]

pub mod active_commands;
mod clean_stale;
mod cpu_usage_collector;
mod ctx;
pub mod daemon;
mod dice_tracker;
mod file_status;
mod heartbeat_guard;
mod host_info;
mod jemalloc_stats;
pub mod lsp;
mod materialize;
mod net_io;
pub(crate) mod new_generic;
pub mod profile;
mod snapshot;
mod subscription;
mod trace_io;
mod version_control_revision;

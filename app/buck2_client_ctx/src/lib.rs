/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(once_cell_try)]
#![feature(error_generic_member_access)]
#![feature(if_let_guard)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(used_with_arg)]
#![feature(round_char_boundary)]

pub mod argfiles;
pub mod client_cpu_tracker;
pub mod client_ctx;
pub mod client_metadata;
pub mod command_outcome;
pub mod common;
pub mod console_interaction_stream;
pub mod daemon;
pub mod daemon_constraints;
pub mod event_log_options;
pub mod events_ctx;
pub mod exit_result;
pub mod file_tailers;
pub mod final_console;
pub mod ide_support;
pub mod immediate_config;
pub mod output_destination_arg;
pub mod path_arg;
pub mod query_args;
pub mod restarter;
pub mod signal_handler;
pub mod startup_deadline;
pub mod stdin;
pub mod stdio;
pub mod stream_util;
pub mod streaming;
pub mod subscribers;
pub mod thread_dump;
pub mod ticker;
pub mod tokio_runtime_setup;
pub mod upload_re_logs;
pub mod version;

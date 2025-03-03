/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

//! Implementation of several server commands.

#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(try_blocks)]

pub mod commands;
pub mod dot;
pub mod html;
pub(crate) mod json;
pub(crate) mod query_output_format;
pub mod target_hash;

pub fn init_late_bindings() {
    commands::init_commands::init_other_server_commands();
    commands::query::printer::init_print_action_node();
}

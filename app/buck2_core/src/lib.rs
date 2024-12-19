/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(decl_macro)]
#![feature(never_type)]
#![feature(pattern)]
#![feature(box_patterns)]
#![feature(impl_trait_in_assoc_type)]
#![feature(io_error_more)]
#![feature(once_cell_try)]
#![feature(try_blocks)]
#![feature(used_with_arg)]
#![feature(let_chains)]

#[macro_use]
pub mod error;

mod ascii_char_set;
pub mod async_once_cell;
pub mod build_file_path;
pub mod bxl;
pub mod bzl;
pub mod category;
pub mod cells;
pub mod ci;
pub mod client_only;
pub mod configuration;
pub mod deferred;
pub mod directory_digest;
pub mod env;
pub mod event;
pub mod execution_types;
pub mod fs;
pub mod global_cfg_options;
pub mod io_counters;
pub mod logging;
pub mod package;
pub mod pattern;
pub mod plugins;
pub mod provider;
pub mod rollout_percentage;
pub mod target;
pub mod target_aliases;
pub mod unsafe_send_future;

pub use env::__macro_refs::buck2_env;
pub use env::__macro_refs::buck2_env_name;

/// Marker for things that are only sensible to use inside Facebook,
/// not intended to be complete, but intended to be useful to audit
/// en-mass at some point in the future.
pub fn facebook_only() {}

/// Emit one expression or another depending on whether this is an open source or internal build.
#[macro_export]
macro_rules! if_else_opensource {
    ($opensource:expr, $internal:expr $(,)?
    ) => {
        // @oss-disable: $internal
        $opensource // @oss-enable
    };
}

#[inline]
pub fn is_open_source() -> bool {
    if_else_opensource!(true, false)
}

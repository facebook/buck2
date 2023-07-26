/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Definitions of core functionality just for bxl functions to access

pub mod alloc_node;
pub mod analysis_result;
pub mod artifacts;
pub mod audit;
pub mod build_result;
pub(crate) mod bxl_function;
pub mod cli_args;
pub mod context;
pub mod cquery;
pub mod event;
pub mod file_expr;
pub mod file_set;
pub mod functions;
pub(crate) mod globals;
pub mod nodes;
pub mod providers_expr;
mod query_util;
pub mod target_expr;
pub mod targetset;
pub mod time;
pub mod uquery;

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Definitions of core functionality just for bxl functions to access

pub(crate) mod alloc_node;
pub(crate) mod analysis_result;
pub(crate) mod aquery;
pub(crate) mod artifacts;
pub(crate) mod audit;
pub(crate) mod build_result;
pub(crate) mod bxl_function;
pub(crate) mod cli_args;
pub(crate) mod context;
pub(crate) mod cquery;
pub(crate) mod event;
pub(crate) mod file_expr;
pub(crate) mod file_set;
pub(crate) mod functions;
pub(crate) mod globals;
pub(crate) mod nodes;
pub(crate) mod providers_expr;
mod query_util;
pub(crate) mod tag;
pub(crate) mod target_expr;
pub(crate) mod target_list_expr;
pub(crate) mod target_universe;
pub(crate) mod targetset;
pub(crate) mod time;
pub(crate) mod type_names;
pub(crate) mod uquery;

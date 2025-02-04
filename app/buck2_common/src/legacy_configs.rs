/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Contains utilities for dealing with buckv1 concepts (ex. buckv1's
//! .buckconfig files as configuration)

mod access;
mod aggregator;
pub mod args;
pub mod cells;
pub mod configs;
pub mod dice;
pub mod file_ops;
pub mod key;
mod parser;
pub(crate) mod path;
pub mod view;

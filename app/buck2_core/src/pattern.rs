/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implements target pattern resolution.
#![doc = include_str!("pattern/target_pattern.md")]

mod ascii_pattern;
pub mod package;
pub mod parse_package;
pub mod pattern;
pub mod pattern_type;
pub mod query_file_literal;
pub mod unparsed;

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(never_type)]
#![feature(trait_alias)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(used_with_arg)]
#![feature(trait_upcasting)]
#![feature(let_chains)]

pub mod artifact;
pub mod artifact_utils;
pub mod artifact_value;
pub mod bxl;
pub mod digest;
pub mod digest_config;
pub mod directory;
pub mod entry;
pub mod execute;
pub mod knobs;
pub mod materialize;
pub mod output_size;
pub mod path;
pub mod re;

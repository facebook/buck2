/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![feature(error_generic_member_access)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(exit_status_error)]
#![feature(used_with_arg)]

pub mod build_info;
pub mod dice;
pub mod manifold;
pub mod materializer;
pub mod rage;
pub mod source_control;
pub mod system_info;

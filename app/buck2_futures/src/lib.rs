/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(assert_matches)]

pub mod cancellation;
mod details;
pub mod drop;
pub mod drop_on_ready;
mod maybe_future;
pub mod owning_future;
pub mod spawn;
pub mod spawner;

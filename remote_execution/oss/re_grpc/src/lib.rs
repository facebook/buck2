/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

mod client;
mod digest;
mod error;
mod grpc;
mod metadata;
mod request;
mod response;
pub use client::*;
pub use digest::*;
pub use error::*;
pub use grpc::*;
pub use metadata::*;
pub use request::*;
pub use response::*;

pub fn get_network_stats() -> anyhow::Result<NetworkStatisticsResponse> {
    // TODO: Support this in this client.
    Ok(NetworkStatisticsResponse {
        downloaded: 0,
        uploaded: 0,
        _dot_dot_default: (),
    })
}

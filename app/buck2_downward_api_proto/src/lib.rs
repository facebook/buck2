/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Protobufs for ineteracting with Buck's DownwardApi over GPRC. This isn't the protocol Buck v1
//! speaks, where the DownwardApi is accessed over named pipes with serialized JSON payloads. This
//! is a different way to make the same calls.

// We put this in a module for easier naming in convert.
mod proto {
    tonic::include_proto!("buck.downward_api");
}

pub use proto::*;

mod convert;

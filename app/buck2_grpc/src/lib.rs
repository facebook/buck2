/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]

mod channel;
mod server;
mod util;

pub use channel::DuplexChannel;
pub use channel::make_channel;
pub use server::ServerHandle;
pub use server::spawn_oneshot;
pub use util::to_tonic;

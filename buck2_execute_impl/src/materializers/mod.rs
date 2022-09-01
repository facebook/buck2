/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(all(unix, feature = "eden_materializer"))]
pub mod eden;

#[cfg(all(not(feature = "eden_materializer"), fbcode_build, unix))]
compile_error!("eden_materializer must be enabled when compiling in fbcode");

pub mod deferred;
pub mod filetree;
pub mod immediate;
pub mod io;

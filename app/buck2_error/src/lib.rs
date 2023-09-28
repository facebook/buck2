/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod any;
mod context;
mod error;
mod format;

pub use any::AnyError;
pub use context::Context;
pub use error::Error;

pub type Result<T> = std::result::Result<T, crate::Error>;

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cancellable results of DICE

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use thiserror::Error;

#[allow(unused)] // TODO temporary
pub(crate) type CancellableResult<T> = Result<T, Cancelled>;

#[allow(unused)] // TODO temporary
#[derive(Clone, Dupe, Display, Debug, Error, Allocative)]
#[display(fmt = "{:?}", self)]
pub(crate) struct Cancelled;

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Defines a future with explicit cancellation

use std::sync::Arc;

/// Context available to the function running inside the future to control and manage it's own
/// cancellation
pub struct CancellationContext(Arc<CancellationContextShared>);

#[allow(unused)]
struct CancellationContextShared {}

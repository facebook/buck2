/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;

use allocative::Allocative;

/// `StarlarkProfileDataAndStats`, but without dependency on starlark.
pub trait StarlarkProfileDataAndStatsDyn: Debug + Allocative + Any + Send + Sync + 'static {
    fn as_any(&self) -> &dyn Any;
}

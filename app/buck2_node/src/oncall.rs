/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

/// The `oncall` annotation for a `BUCK` file.
#[derive(Debug, Hash, Allocative, Eq, PartialEq, Dupe, Clone)]
pub struct Oncall(Arc<String>);

impl Oncall {
    pub fn new(oncall: &str) -> Self {
        Self(Arc::new(oncall.to_owned()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

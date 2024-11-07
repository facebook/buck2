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

/// The unique identifier for this action within dynamic actions.
#[derive(
    Clone,
    Dupe,
    Debug,
    Eq,
    Hash,
    PartialEq,
    derive_more::Display,
    Allocative
)]
#[display("{}", key)]
pub struct DynamicActionsActionKey {
    key: Arc<str>,
}

impl DynamicActionsActionKey {
    pub fn new(key: &str) -> DynamicActionsActionKey {
        DynamicActionsActionKey {
            // TODO(nga): validate something.
            key: Arc::from(key),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.key
    }
}

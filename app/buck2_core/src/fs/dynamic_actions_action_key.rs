/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use buck2_util::arc_str::ArcS;
use dupe::Dupe;

use crate::fs::paths::file_name::FileName;

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
    key: ArcS<FileName>,
}

impl DynamicActionsActionKey {
    pub(crate) fn new(key: &str) -> buck2_error::Result<DynamicActionsActionKey> {
        Ok(DynamicActionsActionKey {
            key: ArcS::try_from(key).internal_error("Action key must be a valid file name")?,
        })
    }

    pub fn as_str(&self) -> &str {
        self.key.as_str()
    }

    pub fn as_file_name(&self) -> &FileName {
        &self.key
    }
}

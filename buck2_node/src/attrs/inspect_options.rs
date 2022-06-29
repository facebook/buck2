/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::dupe::Dupe;

#[derive(Clone, Dupe, Copy)]
pub enum AttrInspectOptions {
    DefaultOnly,
    DefinedOnly,
    All,
}

impl AttrInspectOptions {
    pub fn include_defined(&self) -> bool {
        match self {
            AttrInspectOptions::DefaultOnly => false,
            _ => true,
        }
    }

    pub fn include_default(&self) -> bool {
        match self {
            AttrInspectOptions::DefinedOnly => false,
            _ => true,
        }
    }
}

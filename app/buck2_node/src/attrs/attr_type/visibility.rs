/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use once_cell::sync::Lazy;

use crate::attrs::attr_type::AttrType;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct VisibilityAttrType;

impl VisibilityAttrType {
    /// Visibility attribute is essentially a list of strings.
    /// This is useful in certain operations.
    pub fn pretend_attr_type() -> &'static AttrType {
        static LAZY: Lazy<AttrType> = Lazy::new(|| AttrType::list(AttrType::string()));
        &LAZY
    }
}

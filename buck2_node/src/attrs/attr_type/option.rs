/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::attrs::attr_type::AttrType;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct OptionAttrType {
    pub inner: AttrType,
}

impl OptionAttrType {
    pub fn new(inner: AttrType) -> Self {
        Self { inner }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.option({}{})", self.inner, arg)
    }
}

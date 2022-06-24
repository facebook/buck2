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

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct DictAttrType {
    pub key: AttrType,
    pub value: AttrType,
    pub sorted: bool,
}

impl DictAttrType {
    pub fn new(key: AttrType, value: AttrType, sorted: bool) -> Self {
        Self { key, value, sorted }
    }

    pub(crate) fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(
            f,
            "attr.dict({}, {}, sorted={}{})",
            self.key, self.value, self.sorted, arg
        )
    }
}

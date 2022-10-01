/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Pointer;

use crate::attrs::attr_type::AttrType;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct OneOfAttrType {
    pub xs: Vec<AttrType>,
}

impl OneOfAttrType {
    pub fn new(xs: Vec<AttrType>) -> OneOfAttrType {
        Self { xs }
    }

    pub fn fmt_with_arg(&self, f: &mut fmt::Formatter<'_>, arg: &str) -> fmt::Result {
        write!(f, "attrs.one_of(")?;
        for (i, x) in self.xs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            x.fmt(f)?;
        }
        write!(f, "{})", arg)
    }

    pub(crate) fn any_supports_concat(&self) -> bool {
        self.xs.iter().any(AttrType::supports_concat)
    }
}

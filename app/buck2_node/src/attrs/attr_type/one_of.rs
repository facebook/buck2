/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use allocative::Allocative;
use buck2_error::internal_error;
use pagable::Pagable;

use crate::attrs::attr_type::AttrType;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
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
            write!(f, "{x}")?;
        }
        write!(f, "{arg})")
    }

    pub(crate) fn any_supports_concat(&self) -> bool {
        self.xs.iter().any(AttrType::supports_concat)
    }

    pub(crate) fn get(&self, i: u32) -> buck2_error::Result<&AttrType> {
        self.xs
            .get(i as usize)
            .ok_or_else(|| internal_error!("Oneof index ({i}) out of bounds (internal error)"))
    }
}

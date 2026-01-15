/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_util::arc_str::ArcSlice;
use pagable::Pagable;

use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::coerced_attr::CoercedAttr;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative)]
pub struct AnyAttrType;

impl AnyAttrType {
    pub fn empty_string() -> CoercedAttr {
        CoercedAttr::String(StringLiteral::default())
    }

    pub fn empty_list() -> CoercedAttr {
        CoercedAttr::List(ListLiteral(ArcSlice::new([])))
    }
}

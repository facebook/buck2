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
use buck2_util::arc_str::ArcStr;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::coerced_attr::CoercedAttr;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct AnyAttrType;

impl AnyAttrType {
    pub fn empty_string() -> CoercedAttr {
        CoercedAttr::new_literal(AttrLiteral::String(ArcStr::default()))
    }

    pub fn empty_list() -> CoercedAttr {
        CoercedAttr::new_literal(AttrLiteral::List(Arc::new([])))
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use starlark::values::dict::Dict;
use starlark::values::list::List;
use starlark::values::tuple::Tuple;
use starlark::values::Value;

use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrLiteral;

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct AnyAttrType;

impl AnyAttrType {
    pub(crate) fn empty_string() -> CoercedAttr {
        CoercedAttr::new_literal(AttrLiteral::String("".to_owned()))
    }

    pub(crate) fn empty_list(element_type: AttrType) -> CoercedAttr {
        CoercedAttr::new_literal(AttrLiteral::List(Default::default(), element_type))
    }
}

fn to_coerced_literal(value: Value) -> CoercedAttr {
    CoercedAttr::Literal(to_literal(value))
}

fn to_literal(value: Value) -> AttrLiteral<CoercedAttr> {
    if value.is_none() {
        AttrLiteral::None
    } else if let Some(x) = value.unpack_bool() {
        AttrLiteral::Bool(x)
    } else if let Some(x) = value.unpack_int() {
        AttrLiteral::Int(x)
    } else if let Some(x) = Dict::from_value(value) {
        AttrLiteral::Dict(
            x.iter()
                .map(|(k, v)| (to_coerced_literal(k), to_coerced_literal(v)))
                .collect(),
        )
    } else if let Some(x) = Tuple::from_value(value) {
        AttrLiteral::Tuple(x.iter().map(to_coerced_literal).collect())
    } else if let Some(x) = List::from_value(value) {
        AttrLiteral::List(x.iter().map(to_coerced_literal).collect(), AttrType::any())
    } else {
        AttrLiteral::String(value.to_str())
    }
}

impl AnyAttrType {
    pub(crate) fn coerce_item(
        &self,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        Ok(to_literal(value))
    }

    pub(crate) fn starlark_type(&self) -> String {
        "\"\"".to_owned()
    }
}

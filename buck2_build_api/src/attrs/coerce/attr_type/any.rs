/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::dict::Dict;
use starlark::values::list::List;
use starlark::values::tuple::Tuple;
use starlark::values::Value;

use crate::attrs::coerce::AttrTypeCoerce;

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

impl AttrTypeCoerce for AnyAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        Ok(to_literal(value))
    }

    fn starlark_type(&self) -> String {
        "\"\"".to_owned()
    }
}

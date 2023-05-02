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
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::tuple::TupleRef;
use starlark::values::Value;

use crate::attrs::coerce::AttrTypeCoerce;

fn to_coerced_literal(value: Value, ctx: &dyn AttrCoercionContext) -> CoercedAttr {
    CoercedAttr::Literal(to_literal(value, ctx))
}

fn to_literal(value: Value, ctx: &dyn AttrCoercionContext) -> AttrLiteral<CoercedAttr> {
    if value.is_none() {
        AttrLiteral::None
    } else if let Some(x) = value.unpack_bool() {
        AttrLiteral::Bool(BoolLiteral(x))
    } else if let Some(x) = value.unpack_int() {
        AttrLiteral::Int(x)
    } else if let Some(x) = DictRef::from_value(value) {
        AttrLiteral::Dict(
            x.iter()
                .map(|(k, v)| (to_coerced_literal(k, ctx), to_coerced_literal(v, ctx)))
                .collect(),
        )
    } else if let Some(x) = TupleRef::from_value(value) {
        AttrLiteral::Tuple(ctx.intern_list(x.iter().map(|v| to_coerced_literal(v, ctx)).collect()))
    } else if let Some(x) = ListRef::from_value(value) {
        AttrLiteral::List(ListLiteral(
            ctx.intern_list(
                x.iter()
                    .map(|v| to_coerced_literal(v, ctx))
                    .collect::<Vec<_>>(),
            ),
        ))
    } else {
        AttrLiteral::String(StringLiteral(match value.unpack_str() {
            Some(s) => ctx.intern_str(s),
            None => ctx.intern_str(&value.to_str()),
        }))
    }
}

impl AttrTypeCoerce for AnyAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        Ok(to_literal(value, ctx))
    }

    fn starlark_type(&self) -> String {
        "\"\"".to_owned()
    }
}

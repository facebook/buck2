/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::attr_type::tuple::TupleLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::typing::Ty;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::tuple::TupleRef;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum AnyError {
    #[error("Cannot coerce value of type `{0}` to any: `{1}`")]
    CannotCoerce(&'static str, String),
}

fn to_literal(value: Value, ctx: &dyn AttrCoercionContext) -> buck2_error::Result<CoercedAttr> {
    if value.is_none() {
        Ok(CoercedAttr::None)
    } else if let Some(x) = value.unpack_bool() {
        Ok(CoercedAttr::Bool(BoolLiteral(x)))
    } else if let Some(x) = i64::unpack_value(value)? {
        Ok(CoercedAttr::Int(x))
    } else if let Some(x) = DictRef::from_value(value) {
        Ok(CoercedAttr::Dict(
            x.iter()
                .map(|(k, v)| Ok((to_literal(k, ctx)?, to_literal(v, ctx)?)))
                .collect::<buck2_error::Result<_>>()?,
        ))
    } else if let Some(x) = TupleRef::from_value(value) {
        Ok(CoercedAttr::Tuple(TupleLiteral(
            ctx.intern_list(
                x.iter()
                    .map(|v| to_literal(v, ctx))
                    .collect::<buck2_error::Result<Vec<_>>>()?,
            ),
        )))
    } else if let Some(x) = ListRef::from_value(value) {
        Ok(CoercedAttr::List(ListLiteral(
            ctx.intern_list(
                x.iter()
                    .map(|v| to_literal(v, ctx))
                    .collect::<buck2_error::Result<Vec<_>>>()?,
            ),
        )))
    } else if let Some(s) = value.unpack_str() {
        Ok(CoercedAttr::String(StringLiteral(ctx.intern_str(s))))
    } else {
        soft_error!(
            "coerce_to_any",
            AnyError::CannotCoerce(value.get_type(), value.to_repr()).into()
        )?;
        Ok(CoercedAttr::String(StringLiteral(
            ctx.intern_str(&value.to_str()),
        )))
    }
}

impl AttrTypeCoerce for AnyAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        to_literal(value, ctx)
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(Ty::any())
    }
}

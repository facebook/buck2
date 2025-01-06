/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use buck2_node::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use dupe::Dupe;
use starlark::typing::Ty;
use starlark::values::string::STRING_TYPE;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::error::CoercionError;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for DepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let label = value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;

        let label = ctx.coerce_providers_label(label)?;

        Ok(CoercedAttr::Dep(label))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(Ty::string())
    }
}

impl AttrTypeCoerce for ExplicitConfiguredDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let (label_value, platform_value): (Value, Value) = UnpackValue::unpack_value(value)?
            .ok_or_else(|| {
                CoercionError::type_error("Tuple must be a pair of two strings", value)
            })?;

        let label_string = label_value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;
        let label = ctx.coerce_providers_label(label_string)?;

        let platform_string = platform_value
            .unpack_str()
            .ok_or_else(|| CoercionError::type_error(STRING_TYPE, value))?;
        let platform = ctx.coerce_target_label(platform_string)?;

        Ok(CoercedAttr::ExplicitConfiguredDep(Box::new(
            UnconfiguredExplicitConfiguredDep {
                attr_type: self.dupe(),
                label,
                platform,
            },
        )))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Tuple(vec![
            TyMaybeSelect::Basic(Ty::string()),
            TyMaybeSelect::Basic(Ty::string()),
        ])
    }
}

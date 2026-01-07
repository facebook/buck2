/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_error::BuckErrorContext;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use buck2_interpreter::types::opaque_metadata::OpaqueMetadata;
use buck2_node::attrs::attr_type::target_modifiers::TargetModifiersAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::values::TargetModifiersValue;
use starlark::values::Value;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::attrs::coerce::AttrTypeCoerce;
use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;

impl AttrTypeCoerce for TargetModifiersAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(internal_error!("modifiers attribute is not configurable"));
        }
        let value = value
            .to_json_value()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
            .with_buck_error_context(|| {
                format!(
                    "Target modifiers attribute is not convertible to JSON: {}",
                    value.to_repr(),
                )
            })?;

        Ok(CoercedAttr::TargetModifiers(TargetModifiersValue::new(
            value,
        )))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(OpaqueMetadata::starlark_type_repr())
    }
}

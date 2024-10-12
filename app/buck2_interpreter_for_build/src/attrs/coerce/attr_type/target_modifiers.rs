/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use buck2_error::internal_error_anyhow;
use buck2_interpreter::types::opaque_metadata::OpaqueMetadata;
use buck2_node::attrs::attr_type::target_modifiers::TargetModifiersAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::values::TargetModifiersValue;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, buck2_error::Error)]
enum TargetModifiersAttrTypeCoerceError {
    #[error(
         "Target modifiers attribute is not convertible to JSON: {}",
         .value
     )]
    ValueIsNotJson { value: String },
}

impl AttrTypeCoerce for TargetModifiersAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(internal_error_anyhow!(
                "modifiers attribute is not configurable"
            ));
        }
        let value = value.to_json_value().with_context(|| {
            TargetModifiersAttrTypeCoerceError::ValueIsNotJson {
                value: value.to_repr(),
            }
        })?;

        Ok(CoercedAttr::TargetModifiers(TargetModifiersValue::new(
            value,
        )))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(OpaqueMetadata::starlark_type_repr())
    }
}

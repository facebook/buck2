/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::visibility::VisibilityAttrType;
use buck2_node::attrs::attr_type::within_view::WithinViewAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::attr_type::visibility::parse_visibility_with_view;
use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, thiserror::Error)]
enum WithinViewAttrTypeCoerceError {
    #[error("Within view attribute is not configurable (internal error)")]
    AttrTypeNotConfigurable,
}

impl AttrTypeCoerce for WithinViewAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(WithinViewAttrTypeCoerceError::AttrTypeNotConfigurable.into());
        }
        Ok(CoercedAttr::WithinView(
            parse_visibility_with_view(ctx, value)?.build_within_view(),
        ))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        // Starlark type of the attribute is the same.
        VisibilityAttrType.starlark_type()
    }
}

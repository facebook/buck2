/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::visibility::VisibilityAttrType;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilityWithinViewBuilder;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::list::coerce_list;
use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::AttrTypeCoerce;
use crate::interpreter::selector::StarlarkSelector;

#[derive(Debug, buck2_error::Error)]
enum VisibilityAttrTypeCoerceError {
    #[error("Visibility attribute is not configurable (internal error)")]
    #[buck2(tag = Tier0)]
    AttrTypeNotConfigurable,
    #[error("Visibility must be a list of string, got `{0}`")]
    #[buck2(tag = Input)]
    WrongType(String),
    #[error("Visibility attribute is not configurable (i.e. cannot use `select()`): `{0}`")]
    #[buck2(tag = Input)]
    NotConfigurable(String),
}

impl AttrTypeCoerce for VisibilityAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(VisibilityAttrTypeCoerceError::AttrTypeNotConfigurable.into());
        }
        Ok(CoercedAttr::Visibility(
            parse_visibility_with_view(ctx, value)?.build_visibility(),
        ))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        AttrType::list(AttrType::string()).starlark_type()
    }
}

pub(crate) fn parse_visibility_with_view(
    ctx: &dyn AttrCoercionContext,
    attr: Value,
) -> buck2_error::Result<VisibilityWithinViewBuilder> {
    let list = match coerce_list(attr) {
        Ok(list) => list,
        Err(e) => {
            if StarlarkSelector::from_value(attr).is_some() {
                return Err(VisibilityAttrTypeCoerceError::NotConfigurable(attr.to_repr()).into());
            }
            return Err(e);
        }
    };

    let mut builder = VisibilityWithinViewBuilder::with_capacity(list.len());
    for item in list {
        let Some(item) = item.unpack_str() else {
            if StarlarkSelector::from_value(*item).is_some() {
                return Err(VisibilityAttrTypeCoerceError::NotConfigurable(attr.to_repr()).into());
            }
            return Err(VisibilityAttrTypeCoerceError::WrongType(attr.to_repr()).into());
        };

        if item == VisibilityPattern::PUBLIC {
            // TODO(cjhopman): We should probably enforce that this is the only entry.
            builder.add_public();
        } else {
            builder.add(VisibilityPattern(ctx.coerce_target_pattern(item)?));
        }
    }
    Ok(builder)
}

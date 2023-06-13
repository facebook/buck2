/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::visibility::VisibilityAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::visibility::VisibilityPattern;
use buck2_node::visibility::VisibilitySpecification;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::list::coerce_list;
use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::AttrTypeCoerce;
use crate::interpreter::selector::StarlarkSelector;

#[derive(Debug, thiserror::Error)]
enum VisibilityAttrTypeCoerceError {
    #[error("Visibility attribute is not configurable (internal error)")]
    AttrTypeNotConfigurable,
    #[error("Visibility must be a list of string, got `{0}`")]
    WrongType(String),
    #[error("Visibility attribute is not configurable (i.e. cannot use `select()`): `{0}`")]
    NotConfigurable(String),
}

impl AttrTypeCoerce for VisibilityAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(VisibilityAttrTypeCoerceError::AttrTypeNotConfigurable.into());
        }
        let visibility = parse_visibility(ctx, value)?;
        Ok(CoercedAttr::Visibility(visibility))
    }

    fn starlark_type(&self) -> String {
        VisibilityAttrType::pretend_attr_type().starlark_type()
    }
}

fn parse_visibility(
    ctx: &dyn AttrCoercionContext,
    attr: Value,
) -> anyhow::Result<VisibilitySpecification> {
    let list = match coerce_list(attr) {
        Ok(list) => list,
        Err(e) => {
            if StarlarkSelector::from_value(attr).is_some() {
                return Err(VisibilityAttrTypeCoerceError::NotConfigurable(attr.to_repr()).into());
            }
            return Err(e);
        }
    };

    let mut specs: Option<Vec<_>> = None;
    for item in list {
        let Some(item) = item.unpack_str() else {
            if StarlarkSelector::from_value(*item).is_some() {
                return Err(VisibilityAttrTypeCoerceError::NotConfigurable(
                    attr.to_repr(),
                )
                .into());
            }
            return Err(VisibilityAttrTypeCoerceError::WrongType(
                attr.to_repr(),
            ).into());
        };

        if item == VisibilityPattern::PUBLIC {
            // TODO(cjhopman): We should probably enforce that this is the only entry.
            // TODO(nga): validate the remaining patterns are correct.
            return Ok(VisibilitySpecification::Public);
        }

        specs
            .get_or_insert_with(|| Vec::with_capacity(list.len()))
            .push(VisibilityPattern(ctx.coerce_target_pattern(item)?));
    }
    match specs {
        None => Ok(VisibilitySpecification::DEFAULT),
        Some(specs) => Ok(VisibilitySpecification::VisibleTo(
            specs.into_iter().collect(),
        )),
    }
}

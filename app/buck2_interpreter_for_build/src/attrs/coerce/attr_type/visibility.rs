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

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for VisibilityAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        // TODO(nga): unnecessary coercion step.
        let coerced_list_of_strings =
            VisibilityAttrType::pretend_attr_type().coerce_item(configurable, ctx, value)?;
        let visibility = parse_visibility(ctx, &coerced_list_of_strings)?;
        Ok(CoercedAttr::Visibility(visibility))
    }

    fn starlark_type(&self) -> String {
        VisibilityAttrType::pretend_attr_type().starlark_type()
    }
}

fn parse_visibility(
    ctx: &dyn AttrCoercionContext,
    attr: &CoercedAttr,
) -> anyhow::Result<VisibilitySpecification> {
    let visibility = match attr {
        CoercedAttr::List(list) => &**list,
        CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
            unreachable!("coercion of visibility verified it's not configurable")
        }
        _ => {
            unreachable!("coercion of visibility verified the type")
        }
    };

    let mut specs: Option<Vec<_>> = None;
    for item in visibility.iter() {
        let value = match item {
            CoercedAttr::String(value) => value,
            CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
                unreachable!("coercion of visibility verified it's not configurable")
            }
            _ => {
                unreachable!("coercion of visibility verified the type")
            }
        };

        if value.as_str() == VisibilityPattern::PUBLIC {
            // TODO(cjhopman): We should probably enforce that this is the only entry.
            return Ok(VisibilitySpecification::Public);
        }

        specs
            .get_or_insert_with(|| Vec::with_capacity(visibility.len()))
            .push(VisibilityPattern(ctx.coerce_target_pattern(value)?));
    }
    match specs {
        None => Ok(VisibilitySpecification::DEFAULT),
        Some(specs) => Ok(VisibilitySpecification::VisibleTo(
            specs.into_iter().collect(),
        )),
    }
}

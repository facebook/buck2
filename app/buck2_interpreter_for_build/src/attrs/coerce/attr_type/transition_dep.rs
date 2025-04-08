/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::configuration::transition::id::TransitionId;
use buck2_node::attrs::attr_type::transition_dep::CoercedTransitionDep;
use buck2_node::attrs::attr_type::transition_dep::TransitionDepAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::typing::Ty;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::ty_maybe_select::TyMaybeSelect;
use crate::attrs::coerce::AttrTypeCoerce;

impl AttrTypeCoerce for TransitionDepAttrType {
    fn coerce_item(
        &self,
        _configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> buck2_error::Result<CoercedAttr> {
        let (dep, transition) = if self.transition.is_some() {
            (ctx.coerce_providers_label(value.unpack_str_err()?)?, None)
        } else {
            let (dep, transition) = UnpackValue::unpack_value_err(value)?;
            (
                ctx.coerce_providers_label(dep)?,
                Some(Arc::new(TransitionId::Target(
                    ctx.coerce_providers_label(transition)?,
                ))),
            )
        };

        Ok(CoercedAttr::TransitionDep(Box::new(CoercedTransitionDep {
            dep,
            transition,
        })))
    }

    fn starlark_type(&self) -> TyMaybeSelect {
        TyMaybeSelect::Basic(Ty::string())
    }
}

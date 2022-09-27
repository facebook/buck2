/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::target::TargetLabel;
use buck2_node::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use starlark::values::Value;

use crate::attrs::resolve::ctx::AttrResolutionContext;

pub(crate) trait ConfigurationDepAttrTypeExt {
    fn resolve_single<'v>(
        ctx: &dyn AttrResolutionContext<'v>,
        label: &TargetLabel,
    ) -> anyhow::Result<Value<'v>> {
        Ok(ctx.heap().alloc(label.to_string()))
    }
}

impl ConfigurationDepAttrTypeExt for ConfigurationDepAttrType {}

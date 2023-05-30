/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use starlark::values::Value;

use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacros;

pub mod query;

pub(crate) trait ConfiguredStringWithMacrosExt {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredStringWithMacrosExt for ConfiguredStringWithMacros {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>> {
        ResolvedStringWithMacros::resolved(self, ctx)
    }
}

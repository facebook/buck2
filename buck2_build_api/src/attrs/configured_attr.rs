/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::configured_attr::ConfiguredAttr;
use starlark::values::Value;

use crate::attrs::analysis::AttrResolutionContext;
use crate::attrs::attr_type::attr_literal::ConfiguredAttrLiteralExt;

pub trait ConfiguredAttrExt {
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Vec<Value<'v>>>;

    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredAttrExt for ConfiguredAttr {
    /// "Resolves" the configured value to the resolved value provided to the rule implementation.
    ///
    /// `resolve` may return multiple values. It is up to the caller to fail if
    /// an inappropriate number of elements is returned. e.g. `attr.list()` might
    /// accept and merge multiple returned values from `attr.source()`, but
    /// `attr.optional()` might only accept a single value, and fail otherwise.
    fn resolve<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Vec<Value<'v>>> {
        self.0.resolve(ctx)
    }

    /// Resolving a single value is common, so `resolve_single` will validate
    /// this function's output, and return a single value or an error.
    fn resolve_single<'v>(&self, ctx: &'v dyn AttrResolutionContext) -> anyhow::Result<Value<'v>> {
        self.0.resolve_single(ctx)
    }
}

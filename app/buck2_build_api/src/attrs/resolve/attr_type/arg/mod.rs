/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacrosPart;
use dupe::Dupe;
use starlark::values::Value;

use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::resolved_macro::ResolvedMacro;
use crate::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacros;
use crate::interpreter::rule_defs::resolved_macro::ResolvedStringWithMacrosPart;

pub mod query;

pub(crate) trait ConfiguredStringWithMacrosExt {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>>;
}

impl ConfiguredStringWithMacrosExt for ConfiguredStringWithMacros {
    fn resolve<'v>(&self, ctx: &dyn AttrResolutionContext<'v>) -> anyhow::Result<Value<'v>> {
        let resolved_parts = match self {
            ConfiguredStringWithMacros::StringPart(s) => {
                vec![ResolvedStringWithMacrosPart::String(s.dupe())]
            }
            ConfiguredStringWithMacros::ManyParts(ref parts) => {
                let mut resolved_parts = Vec::with_capacity(parts.len());
                for part in parts.iter() {
                    match part {
                        ConfiguredStringWithMacrosPart::String(s) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::String(s.dupe()));
                        }
                        ConfiguredStringWithMacrosPart::Macro(write_to_file, m) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::Macro(
                                *write_to_file,
                                ResolvedMacro::resolved(m, ctx)
                                    .with_context(|| format!("Error resolving `{}`.", part))?,
                            ));
                        }
                    }
                }
                resolved_parts
            }
        };

        Ok(ctx
            .heap()
            .alloc(ResolvedStringWithMacros::new(resolved_parts)))
    }
}

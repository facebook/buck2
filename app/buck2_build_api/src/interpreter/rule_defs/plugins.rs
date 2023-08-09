/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;

use allocative::Allocative;
use buck2_core::plugins::PluginKind;
use buck2_interpreter::plugins::PLUGIN_KIND_FROM_VALUE;
use derive_more::Display;
use dupe::Dupe;
use starlark::coerce::coerce;
use starlark::coerce::CoerceKey;
use starlark::starlark_complex_value;
use starlark::values::starlark_value;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark_map::small_map::SmallMap;

/// Wrapper around `PluginKind` to impl `Trace` and `Freeze`
#[derive(
    Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative, Freeze, Trace
)]
#[repr(transparent)]
struct PluginKindWrapper(
    #[freeze(identity)]
    // SAFETY: `PluginKind` does not contain any starlark values
    #[trace(unsafe_ignore)]
    PluginKind,
);

// SAFETY: Trivial coercion is always correct
unsafe impl Coerce<PluginKindWrapper> for PluginKindWrapper {}
unsafe impl CoerceKey<PluginKindWrapper> for PluginKindWrapper {}
// SAFETY: `#[repr(transparent)]` and impls are derived
unsafe impl Coerce<PluginKindWrapper> for PluginKind {}
unsafe impl CoerceKey<PluginKindWrapper> for PluginKind {}

impl Borrow<PluginKind> for PluginKindWrapper {
    fn borrow(&self) -> &PluginKind {
        &self.0
    }
}

/// The value found in `ctx.plugins`
#[derive(
    Debug,
    Display,
    Trace,
    Coerce,
    Freeze,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[display(fmt = "<ctx.plugins>")]
#[repr(transparent)]
pub struct AnalysisPluginsGen<V> {
    plugins: SmallMap<PluginKindWrapper, V>,
}

starlark_complex_value!(pub AnalysisPlugins);

#[derive(Debug, thiserror::Error)]
enum AnalysisPluginsError {
    #[error("The rule did not declare that it uses plugins of kind {0}")]
    PluginKindNotUsed(PluginKind),
}

#[starlark_value(type = "AnalysisPlugins")]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for AnalysisPluginsGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn at(&self, index: Value<'v>, _heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let kind = (PLUGIN_KIND_FROM_VALUE.get()?)(index)?;
        match self.plugins.get(&kind) {
            Some(v) => Ok(v.to_value()),
            None => Err(AnalysisPluginsError::PluginKindNotUsed(kind).into()),
        }
    }

    fn is_in(&self, other: Value<'v>) -> anyhow::Result<bool> {
        let kind = (PLUGIN_KIND_FROM_VALUE.get()?)(other)?;
        Ok(self.plugins.contains_key(&kind))
    }
}

impl<'v> AnalysisPlugins<'v> {
    pub fn new(plugins: SmallMap<PluginKind, Value<'v>>) -> Self {
        Self {
            plugins: coerce(plugins),
        }
    }
}

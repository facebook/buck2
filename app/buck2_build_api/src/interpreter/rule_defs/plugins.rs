/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;

use allocative::Allocative;
use buck2_core::plugins::PluginKind;
use buck2_interpreter::plugins::PLUGIN_KIND_FROM_VALUE;
use derive_more::Display;
use dupe::Dupe;
use starlark::coerce::CoerceKey;
use starlark::coerce::coerce;
use starlark::starlark_complex_value;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark_map::small_map::SmallMap;

/// Wrapper around `PluginKind` to impl `Trace` and `Freeze`
#[derive(
    Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative, Freeze, Trace
)]
#[repr(transparent)]
struct PluginKindWrapper(#[freeze(identity)] PluginKind);

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
#[display("<ctx.plugins>")]
#[repr(transparent)]
pub struct AnalysisPluginsGen<V: ValueLifetimeless> {
    plugins: SmallMap<PluginKindWrapper, V>,
}

starlark_complex_value!(pub AnalysisPlugins);

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum AnalysisPluginsError {
    #[error("The rule did not declare that it uses plugins of kind {0}")]
    PluginKindNotUsed(PluginKind),
}

#[starlark_value(type = "AnalysisPlugins")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for AnalysisPluginsGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn at(&self, index: Value<'v>, _heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        let kind = (PLUGIN_KIND_FROM_VALUE.get()?)(index)?;
        match self.plugins.get(&kind) {
            Some(v) => Ok(v.to_value()),
            None => {
                Err(buck2_error::Error::from(AnalysisPluginsError::PluginKindNotUsed(kind)).into())
            }
        }
    }

    fn is_in(&self, other: Value<'v>) -> starlark::Result<bool> {
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::result::SharedResult;
use dice::DiceComputations;
use dice::DiceTransaction;
use dice::InjectedKey;
use dice::Key;
use gazebo::dupe::Dupe;
use starlark::eval::ProfileMode;

use crate::starlark_profiler::StarlarkProfilerInstrumentation;

#[derive(Debug, derive_more::Display, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkProfilerInstrumentationKey;

#[async_trait]
impl Key for StarlarkProfilerInstrumentationKey {
    type Value = SharedResult<Option<StarlarkProfilerInstrumentation>>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        if let Some(instr) = ctx.get_starlark_profiler_instrumentation_override().await? {
            return Ok(Some(instr));
        }

        let cell_resolver = ctx.get_cell_resolver().await?;
        let instr = ctx
            .parse_legacy_config_property::<ProfileMode>(
                cell_resolver.root_cell(),
                "buck2",
                "starlark_instrumentation_mode",
            )
            .await?;

        Ok(instr.map(StarlarkProfilerInstrumentation::new))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

/// Global Starlark compiler instrumentation level.
///
/// We profile only leaf computations (`BUCK` files or analysis),
/// and this key defines instrumentation of all the Starlark files,
/// regardless of whether profiled entity depends on them or not.
/// It's easier to implement with single global key,
/// the downside is we invalidate parse results when we switch
/// between normal operation/profiling.
#[derive(Debug, derive_more::Display, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkProfilerInstrumentationOverrideKey;

impl InjectedKey for StarlarkProfilerInstrumentationOverrideKey {
    type Value = Option<StarlarkProfilerInstrumentation>;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
pub trait SetStarlarkProfilerInstrumentation {
    fn set_starlark_profiler_instrumentation_override(
        &self,
        instrumentation: Option<StarlarkProfilerInstrumentation>,
    ) -> anyhow::Result<()>;
}

#[async_trait]
pub trait GetStarlarkProfilerInstrumentation {
    async fn get_starlark_profiler_instrumentation_override(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>>;

    async fn get_starlark_profiler_instrumentation(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>>;
}

#[async_trait]
impl SetStarlarkProfilerInstrumentation for DiceTransaction {
    fn set_starlark_profiler_instrumentation_override(
        &self,
        instrumentation: Option<StarlarkProfilerInstrumentation>,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to([(StarlarkProfilerInstrumentationOverrideKey, instrumentation)])?)
    }
}

#[async_trait]
impl GetStarlarkProfilerInstrumentation for DiceComputations {
    async fn get_starlark_profiler_instrumentation_override(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>> {
        Ok(self
            .compute(&StarlarkProfilerInstrumentationOverrideKey)
            .await?)
    }

    async fn get_starlark_profiler_instrumentation(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>> {
        Ok(self.compute(&StarlarkProfilerInstrumentationKey).await??)
    }
}

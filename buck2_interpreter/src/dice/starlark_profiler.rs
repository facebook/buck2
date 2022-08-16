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

use crate::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use crate::starlark_profiler::StarlarkProfilerInstrumentation;

#[derive(Debug, thiserror::Error)]
enum StarlarkProfilerError {
    #[error("profiler is not configured to profile last element (internal error)")]
    ProfilerConfigurationNotLast,
}

/// Global profiling configuration.
#[derive(PartialEq, Eq, Clone, Dupe, Debug)]
pub enum StarlarkProfilerConfiguration {
    /// No profiling.
    None,
    /// Instrument everything, but no profiling.
    Instrument(StarlarkProfilerInstrumentation),
    /// Profile loading of one `BUCK`, everything else is instrumented.
    ProfileLastLoading(ProfileMode),
    /// Profile analysis of the last target, everything else is instrumented.
    ProfileLastAnalysis(ProfileMode),
    /// Profile analysis targets recursively.
    ProfileAnalysisRecursively(ProfileMode),
}

impl Default for StarlarkProfilerConfiguration {
    fn default() -> StarlarkProfilerConfiguration {
        StarlarkProfilerConfiguration::None
    }
}

impl StarlarkProfilerConfiguration {
    /// Instrumentation for `.bzl` files.
    pub fn bzl_instrumentation(&self) -> Option<StarlarkProfilerInstrumentation> {
        match self {
            StarlarkProfilerConfiguration::None => None,
            StarlarkProfilerConfiguration::Instrument(instrumentation) => {
                Some(instrumentation.dupe())
            }
            StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode)
            | StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode)
            | StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode) => {
                Some(StarlarkProfilerInstrumentation::new(profile_mode.dupe()))
            }
        }
    }

    pub fn profile_last_loading(&self) -> anyhow::Result<&ProfileMode> {
        match self {
            StarlarkProfilerConfiguration::None
            | StarlarkProfilerConfiguration::Instrument(_)
            | StarlarkProfilerConfiguration::ProfileLastAnalysis(_)
            | StarlarkProfilerConfiguration::ProfileAnalysisRecursively(_) => {
                Err(StarlarkProfilerError::ProfilerConfigurationNotLast.into())
            }
            StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode) => Ok(profile_mode),
        }
    }

    pub fn profile_last_analysis(&self) -> anyhow::Result<&ProfileMode> {
        match self {
            StarlarkProfilerConfiguration::None
            | StarlarkProfilerConfiguration::Instrument(_)
            | StarlarkProfilerConfiguration::ProfileLastLoading(_) => {
                Err(StarlarkProfilerError::ProfilerConfigurationNotLast.into())
            }
            StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode)
            | StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode) => {
                Ok(profile_mode)
            }
        }
    }

    /// Profile mode for intermediate target analysis.
    pub fn profile_mode_for_intermediate_analysis(&self) -> StarlarkProfileModeOrInstrumentation {
        match self {
            StarlarkProfilerConfiguration::None => StarlarkProfileModeOrInstrumentation::None,
            StarlarkProfilerConfiguration::Instrument(instrumentation) => {
                StarlarkProfileModeOrInstrumentation::Instrument(instrumentation.dupe())
            }
            StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode)
            | StarlarkProfilerConfiguration::ProfileLastAnalysis(profile_mode) => {
                StarlarkProfileModeOrInstrumentation::Instrument(
                    StarlarkProfilerInstrumentation::new(profile_mode.dupe()),
                )
            }
            StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode) => {
                StarlarkProfileModeOrInstrumentation::Profile(profile_mode.dupe())
            }
        }
    }
}

#[derive(Debug, derive_more::Display, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
struct StarlarkProfilerConfigurationKey;

#[derive(Debug, derive_more::Display, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkProfilerInstrumentationKey;

#[derive(Debug, derive_more::Display, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkProfileModeForIntermediateAnalysisKey;

#[async_trait]
impl Key for StarlarkProfilerConfigurationKey {
    type Value = SharedResult<StarlarkProfilerConfiguration>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let mut configuration = get_starlark_profiler_instrumentation_override(ctx).await?;

        if let StarlarkProfilerConfiguration::None = configuration {
            let cell_resolver = ctx.get_cell_resolver().await?;
            let instr = ctx
                .parse_legacy_config_property::<ProfileMode>(
                    cell_resolver.root_cell(),
                    "buck2",
                    "starlark_instrumentation_mode",
                )
                .await?;

            if let Some(instr) = instr {
                configuration = StarlarkProfilerConfiguration::Instrument(
                    StarlarkProfilerInstrumentation::new(instr),
                );
            }
        }

        Ok(configuration)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[async_trait]
impl Key for StarlarkProfilerInstrumentationKey {
    type Value = SharedResult<Option<StarlarkProfilerInstrumentation>>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
    ) -> SharedResult<Option<StarlarkProfilerInstrumentation>> {
        let configuration = get_starlark_profiler_configuration(ctx).await?;
        Ok(configuration.bzl_instrumentation())
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[async_trait]
impl Key for StarlarkProfileModeForIntermediateAnalysisKey {
    type Value = SharedResult<StarlarkProfileModeOrInstrumentation>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
    ) -> SharedResult<StarlarkProfileModeOrInstrumentation> {
        let configuration = get_starlark_profiler_configuration(ctx).await?;
        Ok(configuration.profile_mode_for_intermediate_analysis())
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
    type Value = StarlarkProfilerConfiguration;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
pub trait SetStarlarkProfilerInstrumentation {
    fn set_starlark_profiler_instrumentation_override(
        &self,
        instrumentation: StarlarkProfilerConfiguration,
    ) -> anyhow::Result<()>;
}

#[async_trait]
pub trait GetStarlarkProfilerInstrumentation {
    async fn get_starlark_profiler_instrumentation(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>>;

    /// Profile mode for non-final targe analysis.
    async fn get_profile_mode_for_intermediate_analysis(
        &self,
    ) -> anyhow::Result<StarlarkProfileModeOrInstrumentation>;
}

#[async_trait]
impl SetStarlarkProfilerInstrumentation for DiceTransaction {
    fn set_starlark_profiler_instrumentation_override(
        &self,
        instrumentation: StarlarkProfilerConfiguration,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to([(StarlarkProfilerInstrumentationOverrideKey, instrumentation)])?)
    }
}

async fn get_starlark_profiler_instrumentation_override(
    ctx: &DiceComputations,
) -> anyhow::Result<StarlarkProfilerConfiguration> {
    Ok(ctx
        .compute(&StarlarkProfilerInstrumentationOverrideKey)
        .await?)
}

/// Global profiler configuration.
///
/// This funtion is not exposed outside,
/// because accessing full configuration may invalidate too much.
async fn get_starlark_profiler_configuration(
    ctx: &DiceComputations,
) -> anyhow::Result<StarlarkProfilerConfiguration> {
    Ok(ctx.compute(&StarlarkProfilerConfigurationKey).await??)
}

#[async_trait]
impl GetStarlarkProfilerInstrumentation for DiceComputations {
    async fn get_starlark_profiler_instrumentation(
        &self,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>> {
        Ok(self.compute(&StarlarkProfilerInstrumentationKey).await??)
    }

    async fn get_profile_mode_for_intermediate_analysis(
        &self,
    ) -> anyhow::Result<StarlarkProfileModeOrInstrumentation> {
        Ok(self
            .compute(&StarlarkProfileModeForIntermediateAnalysisKey)
            .await??)
    }
}

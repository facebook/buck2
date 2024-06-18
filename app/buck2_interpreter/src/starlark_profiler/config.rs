/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dice::Key;
use dupe::Dupe;
use starlark::eval::ProfileMode;

use crate::starlark_profiler::mode::StarlarkProfileMode;

#[derive(Debug, buck2_error::Error)]
enum StarlarkProfilerError {
    #[error("profiler is not configured to profile last element (internal error)")]
    ProfilerConfigurationNotLast,
}

/// Global profiling configuration.
#[derive(PartialEq, Eq, Clone, Dupe, Debug, Allocative)]
#[derive(Default)]
pub enum StarlarkProfilerConfiguration {
    /// No profiling.
    #[default]
    None,
    /// Profile loading of one `BUCK`, everything else is instrumented.
    ProfileLastLoading(ProfileMode),
    /// Profile analysis of the last target, everything else is instrumented.
    ProfileLastAnalysis(ProfileMode),
    /// Profile analysis targets recursively.
    ProfileAnalysisRecursively(ProfileMode),
    /// Profile BXL
    ProfileBxl(ProfileMode),
}

impl StarlarkProfilerConfiguration {
    pub fn profile_last_loading(&self) -> anyhow::Result<&ProfileMode> {
        match self {
            StarlarkProfilerConfiguration::None
            | StarlarkProfilerConfiguration::ProfileLastAnalysis(_)
            | StarlarkProfilerConfiguration::ProfileAnalysisRecursively(_)
            | StarlarkProfilerConfiguration::ProfileBxl(_) => {
                Err(StarlarkProfilerError::ProfilerConfigurationNotLast.into())
            }
            StarlarkProfilerConfiguration::ProfileLastLoading(profile_mode) => Ok(profile_mode),
        }
    }

    /// Profile mode for intermediate target analysis.
    fn profile_mode_for_intermediate_analysis(&self) -> StarlarkProfileMode {
        match self {
            StarlarkProfilerConfiguration::None
            | StarlarkProfilerConfiguration::ProfileLastLoading(_)
            | StarlarkProfilerConfiguration::ProfileLastAnalysis(_)
            | StarlarkProfilerConfiguration::ProfileBxl(_) => StarlarkProfileMode::None,
            StarlarkProfilerConfiguration::ProfileAnalysisRecursively(profile_mode) => {
                StarlarkProfileMode::Profile(profile_mode.dupe())
            }
        }
    }
}

#[derive(
    Debug,
    derive_more::Display,
    Copy,
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Allocative
)]
#[display(fmt = "{:?}", self)]
struct StarlarkProfileModeForIntermediateAnalysisKey;

#[async_trait]
impl Key for StarlarkProfileModeForIntermediateAnalysisKey {
    type Value = buck2_error::Result<StarlarkProfileMode>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> buck2_error::Result<StarlarkProfileMode> {
        let configuration = ctx.compute(&StarlarkProfilerConfigurationKey).await?;
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
#[derive(
    Debug,
    derive_more::Display,
    Copy,
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Allocative
)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkProfilerConfigurationKey;

impl InjectedKey for StarlarkProfilerConfigurationKey {
    type Value = StarlarkProfilerConfiguration;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
pub trait SetStarlarkProfilerInstrumentation {
    fn set_starlark_profiler_configuration(
        &mut self,
        instrumentation: StarlarkProfilerConfiguration,
    ) -> anyhow::Result<()>;
}

#[async_trait]
pub trait GetStarlarkProfilerInstrumentation {
    /// Profile mode for non-final targe analysis.
    async fn get_profile_mode_for_intermediate_analysis(
        &mut self,
    ) -> anyhow::Result<StarlarkProfileMode>;
}

#[async_trait]
impl SetStarlarkProfilerInstrumentation for DiceTransactionUpdater {
    fn set_starlark_profiler_configuration(
        &mut self,
        configuration: StarlarkProfilerConfiguration,
    ) -> anyhow::Result<()> {
        Ok(self.changed_to([(StarlarkProfilerConfigurationKey, configuration)])?)
    }
}

#[async_trait]
impl GetStarlarkProfilerInstrumentation for DiceComputations<'_> {
    async fn get_profile_mode_for_intermediate_analysis(
        &mut self,
    ) -> anyhow::Result<StarlarkProfileMode> {
        Ok(self
            .compute(&StarlarkProfileModeForIntermediateAnalysisKey)
            .await??)
    }
}

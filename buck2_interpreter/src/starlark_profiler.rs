/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::Path;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use gazebo::dupe::Dupe;
use starlark::environment::FrozenModule;
use starlark::eval::Evaluator;
use starlark::eval::ProfileData;
use starlark::eval::ProfileMode;
use starlark::values::AggregateHeapProfileInfo;

#[derive(Debug, thiserror::Error)]
enum StarlarkProfilerError {
    #[error("will_freeze field was initialized incorrectly (internal error)")]
    IncorrectWillFreeze,
    #[error(
        "Retained memory profiling is available only for analysis profile \
        (which freezes the module)"
    )]
    RetainedMemoryNotFrozen,
    #[error("profile mode and profile data are inconsistent (internal error)")]
    InconsistentProfileModeAndProfileData,
}

/// When profiling Starlark file, all dependencies of that file must be
/// "instrumented" otherwise the profiler won't work.
///
/// This struct defines instrumentation level for the module.
#[derive(Debug, PartialEq, Clone, Dupe)]
pub struct StarlarkProfilerInstrumentation {
    profile_mode: ProfileMode,
}

impl StarlarkProfilerInstrumentation {
    pub fn new(profile_mode: ProfileMode) -> Self {
        Self { profile_mode }
    }

    pub fn enable(&self, eval: &mut Evaluator) -> anyhow::Result<()> {
        eval.enable_profile_instrumentation(&self.profile_mode)
    }

    pub fn into_profile_mode(self) -> ProfileMode {
        self.profile_mode
    }
}

/// Collected profile data.
#[derive(Debug)]
enum StarlarkProfileData {
    /// Collected from `Evaluator`.
    Evaluator(ProfileData),
    /// Collected from `FrozenModule`.
    Frozen(AggregateHeapProfileInfo),
}

impl StarlarkProfileData {
    fn gen(&self, profile_mode: &ProfileMode) -> anyhow::Result<String> {
        match (self, profile_mode) {
            (StarlarkProfileData::Evaluator(data), _) => Ok(data.gen()),
            (StarlarkProfileData::Frozen(data), ProfileMode::HeapSummaryRetained) => {
                Ok(data.gen_summary_csv())
            }
            (StarlarkProfileData::Frozen(data), ProfileMode::HeapFlameRetained) => {
                Ok(data.gen_flame_graph())
            }
            (StarlarkProfileData::Frozen(_), _) => {
                Err(StarlarkProfilerError::InconsistentProfileModeAndProfileData.into())
            }
        }
    }

    fn write(&self, path: &Path, profile_mode: &ProfileMode) -> anyhow::Result<()> {
        let data = self.gen(profile_mode)?;
        fs::write(path, data).with_context(|| format!("write profile data to `{}`", path.display()))
    }
}

#[derive(Debug)]
pub struct StarlarkProfileDataAndStats {
    profile_mode: ProfileMode,
    profile_data: StarlarkProfileData,
    initialized_at: Instant,
    finalized_at: Instant,
    total_allocated_bytes: usize,
}

impl StarlarkProfileDataAndStats {
    pub fn elapsed(&self) -> Duration {
        self.finalized_at.duration_since(self.initialized_at)
    }

    pub fn total_allocated_bytes(&self) -> usize {
        self.total_allocated_bytes
    }

    pub fn write(&self, path: &Path) -> anyhow::Result<()> {
        self.profile_data.write(path, &self.profile_mode)
    }
}

pub struct StarlarkProfiler {
    profile_mode: ProfileMode,
    /// Evaluation will freeze the module.
    /// (And frozen module will be passed to `visit_frozen_module`).
    will_freeze: bool,

    initialized_at: Option<Instant>,
    finalized_at: Option<Instant>,
    profile_data: Option<StarlarkProfileData>,
    total_allocated_bytes: Option<usize>,
}

impl StarlarkProfiler {
    pub fn new(profile_mode: ProfileMode, will_freeze: bool) -> StarlarkProfiler {
        Self {
            profile_mode,
            will_freeze,
            initialized_at: None,
            finalized_at: None,
            profile_data: None,
            total_allocated_bytes: None,
        }
    }

    /// Collect all profiling data.
    pub fn finish(self) -> anyhow::Result<StarlarkProfileDataAndStats> {
        Ok(StarlarkProfileDataAndStats {
            profile_mode: self.profile_mode,
            initialized_at: self
                .initialized_at
                .context("did not initialize (internal error)")?,
            finalized_at: self
                .finalized_at
                .context("did not finalize (internal error)")?,
            total_allocated_bytes: self
                .total_allocated_bytes
                .context("did not visit heap (internal error)")?,
            profile_data: self
                .profile_data
                .context("profile_data not initialized (internal error)")?,
        })
    }

    /// Instrumentation level required by `bzl` files loaded by the profiled module.
    fn instrumentation(&self) -> Option<StarlarkProfilerInstrumentation> {
        Some(StarlarkProfilerInstrumentation {
            profile_mode: self.profile_mode.dupe(),
        })
    }

    /// Prepare an Evaluator to capture output relevant to this profiler.
    fn initialize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        eval.enable_profile(&self.profile_mode)?;
        self.initialized_at = Some(Instant::now());
        Ok(())
    }

    /// Post-analysis, produce the output of this profiler.
    fn evaluation_complete(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        self.finalized_at = Some(Instant::now());
        if !matches!(
            self.profile_mode,
            ProfileMode::HeapSummaryRetained | ProfileMode::HeapFlameRetained
        ) {
            self.profile_data = Some(StarlarkProfileData::Evaluator(eval.gen_profile()?));
        }
        Ok(())
    }

    fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()> {
        if self.will_freeze != module.is_some() {
            return Err(StarlarkProfilerError::IncorrectWillFreeze.into());
        }

        match self.profile_mode {
            ProfileMode::HeapSummaryRetained | ProfileMode::HeapFlameRetained => {
                let module = module.ok_or(StarlarkProfilerError::RetainedMemoryNotFrozen)?;
                let profile = module.aggregated_heap_profile_info()?.clone();
                self.profile_data = Some(StarlarkProfileData::Frozen(profile));
            }
            _ => {}
        }

        let total_allocated_bytes = module.map_or(0, |module| {
            module
                .frozen_heap()
                .allocated_summary()
                .total_allocated_bytes()
        });

        self.total_allocated_bytes = Some(total_allocated_bytes);

        Ok(())
    }
}

enum StarlarkProfilerOrInstrumentationImpl<'p> {
    None,
    Profiler(&'p mut StarlarkProfiler),
    Instrumentation(StarlarkProfilerInstrumentation),
}

/// Modules can be evaluated with profiling or with instrumentation for profiling.
/// This type enapsulates this logic.
pub struct StarlarkProfilerOrInstrumentation<'p>(StarlarkProfilerOrInstrumentationImpl<'p>);

impl<'p> StarlarkProfilerOrInstrumentation<'p> {
    pub fn new(
        profiler: &'p mut StarlarkProfiler,
        instrumentation: Option<StarlarkProfilerInstrumentation>,
    ) -> StarlarkProfilerOrInstrumentation<'p> {
        match (profiler.instrumentation(), instrumentation) {
            (None, None) => StarlarkProfilerOrInstrumentation::disabled(),
            (Some(pi), Some(i)) => {
                // Sanity check.
                assert_eq!(
                    pi.profile_mode, i.profile_mode,
                    "profiler is incompatible with instrumentation"
                );
                StarlarkProfilerOrInstrumentation::for_profiler(profiler)
            }
            (None, Some(i)) => StarlarkProfilerOrInstrumentation::instrumentation(i),
            (Some(_), None) => panic!("profiler, but no instrumentation"),
        }
    }

    pub fn for_profiler(profiler: &'p mut StarlarkProfiler) -> Self {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::Profiler(profiler))
    }

    /// Instrumentation only.
    pub fn instrumentation(instrumentation: StarlarkProfilerInstrumentation) -> Self {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::Instrumentation(
            instrumentation,
        ))
    }

    /// Instrumentation only.
    pub fn maybe_instrumentation(instrumentation: Option<StarlarkProfilerInstrumentation>) -> Self {
        match instrumentation {
            None => StarlarkProfilerOrInstrumentation::disabled(),
            Some(i) => StarlarkProfilerOrInstrumentation::instrumentation(i),
        }
    }

    /// No profiling.
    pub fn disabled() -> StarlarkProfilerOrInstrumentation<'p> {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::None)
    }

    pub fn initialize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        match &mut self.0 {
            StarlarkProfilerOrInstrumentationImpl::None => Ok(()),
            StarlarkProfilerOrInstrumentationImpl::Profiler(profiler) => profiler.initialize(eval),
            StarlarkProfilerOrInstrumentationImpl::Instrumentation(instrumentation) => {
                instrumentation.enable(eval)
            }
        }
    }

    pub fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()> {
        if let StarlarkProfilerOrInstrumentationImpl::Profiler(profiler) = &mut self.0 {
            profiler.visit_frozen_module(module)
        } else {
            Ok(())
        }
    }

    pub fn evaluation_complete(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        if let StarlarkProfilerOrInstrumentationImpl::Profiler(profiler) = &mut self.0 {
            profiler.evaluation_complete(eval)
        } else {
            Ok(())
        }
    }
}

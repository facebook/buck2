/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use gazebo::dupe::Dupe;
use starlark::environment::FrozenModule;
use starlark::eval::Evaluator;
use starlark::eval::ProfileMode;

#[derive(Debug, thiserror::Error)]
enum StarlarkProfilerError {
    #[error("will_freeze field was initialized incorrectly (internal error)")]
    IncorrectWillFreeze,
    #[error(
        "Retained memory profiling is available only for analysis profile \
        (which freezes the module)"
    )]
    RetainedMemoryNotFrozen,
}

/// When profiling Starlark file, all dependencies of that file must be
/// "instrumented" otherwise the profiler won't work.
///
/// This struct defines instrumentation level for the module.
#[derive(Debug, Default, PartialEq, Clone, Dupe)]
pub struct StarlarkProfilerInstrumentation {
    profile_mode: Option<ProfileMode>,
}

impl StarlarkProfilerInstrumentation {
    pub fn new(profile_mode: Option<ProfileMode>) -> Self {
        Self { profile_mode }
    }

    pub fn enable(&self, eval: &mut Evaluator) {
        if let Some(profile_mode) = &self.profile_mode {
            eval.enable_profile_instrumentation(profile_mode);
        }
    }

    /// True iff the set of instrumentations in this is a subset
    /// of the set of instrumentations in other.
    pub fn is_subset(&self, other: &StarlarkProfilerInstrumentation) -> bool {
        match (&self.profile_mode, &other.profile_mode) {
            (None, _) => true,
            (Some(_), None) => false,
            (Some(profile_mode), Some(other_profile_mode)) => profile_mode == other_profile_mode,
        }
    }

    pub fn into_profile_mode(self) -> Option<ProfileMode> {
        self.profile_mode
    }
}

pub trait StarlarkProfiler: Send + Sync {
    /// Instrumentation level required by `bzl` files loaded by the profiled module.
    fn instrumentation(&self) -> StarlarkProfilerInstrumentation;

    /// Prepare an Evaluator to capture output relevant to this profiler.
    fn initialize(&mut self, eval: &mut Evaluator);

    /// Post-analysis, produce the output of this profiler.
    fn finalize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()>;

    fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()>;
}

/// A profiler that does nothing.
pub struct Disabled;

impl StarlarkProfiler for Disabled {
    fn instrumentation(&self) -> StarlarkProfilerInstrumentation {
        StarlarkProfilerInstrumentation { profile_mode: None }
    }

    fn initialize(&mut self, _: &mut Evaluator) {}

    fn finalize(&mut self, _: &mut Evaluator) -> anyhow::Result<()> {
        Ok(())
    }

    fn visit_frozen_module(&mut self, _: Option<&FrozenModule>) -> anyhow::Result<()> {
        Ok(())
    }
}

pub struct StarlarkProfilerImpl {
    profile_mode: ProfileMode,
    path: PathBuf,
    /// Evaluation will freeze the module.
    /// (And frozen module will be passed to `visit_frozen_module`).
    will_freeze: bool,

    initialized_at: Option<Instant>,
    finalized_at: Option<Instant>,
    total_allocated_bytes: Option<usize>,
}

impl StarlarkProfilerImpl {
    pub fn new(
        profile_mode: ProfileMode,
        path: PathBuf,
        will_freeze: bool,
    ) -> StarlarkProfilerImpl {
        Self {
            profile_mode,
            path,
            will_freeze,
            initialized_at: None,
            finalized_at: None,
            total_allocated_bytes: None,
        }
    }

    pub fn elapsed(&self) -> anyhow::Result<Duration> {
        Ok(self.finalized_at.context("Did not finalize")?
            - self.initialized_at.context("Did not initialize")?)
    }

    pub fn total_allocated_bytes(&self) -> anyhow::Result<usize> {
        self.total_allocated_bytes.context("Did not visit heap")
    }
}

impl StarlarkProfiler for StarlarkProfilerImpl {
    fn instrumentation(&self) -> StarlarkProfilerInstrumentation {
        StarlarkProfilerInstrumentation {
            profile_mode: Some(self.profile_mode.dupe()),
        }
    }

    fn initialize(&mut self, eval: &mut Evaluator) {
        eval.enable_profile(&self.profile_mode);
        self.initialized_at = Some(Instant::now());
    }

    fn finalize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        self.finalized_at = Some(Instant::now());
        if !matches!(
            self.profile_mode,
            ProfileMode::HeapSummaryRetained | ProfileMode::HeapFlameRetained
        ) {
            eval.write_profile(&self.profile_mode, &self.path)
                .context("Failed to write profile")?;
        }
        Ok(())
    }

    fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()> {
        if self.will_freeze != module.is_some() {
            return Err(StarlarkProfilerError::IncorrectWillFreeze.into());
        }

        match self.profile_mode {
            ProfileMode::HeapSummaryRetained => {
                let module = module.ok_or(StarlarkProfilerError::RetainedMemoryNotFrozen)?;
                let profile = module.gen_heap_summary_profile()?;
                fs::write(&self.path, profile).context("Failed to write heap summary profile")?;
            }
            ProfileMode::HeapFlameRetained => {
                let module = module.ok_or(StarlarkProfilerError::RetainedMemoryNotFrozen)?;
                let profile = module.gen_heap_flame_profile()?;
                fs::write(&self.path, profile).context("Failed to write heap flame profile")?;
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
    Profiler(&'p mut dyn StarlarkProfiler),
    Instrumentation(StarlarkProfilerInstrumentation),
}

/// Modules can be evaluated with profiling or with instrumentation for profiling.
/// This type enapsulates this logic.
pub struct StarlarkProfilerOrInstrumentation<'p>(StarlarkProfilerOrInstrumentationImpl<'p>);

impl<'p> StarlarkProfilerOrInstrumentation<'p> {
    pub fn new(
        profiler: &'p mut dyn StarlarkProfiler,
        instrumentation: StarlarkProfilerInstrumentation,
    ) -> StarlarkProfilerOrInstrumentation<'p> {
        // Sanity check.
        assert!(profiler.instrumentation().is_subset(&instrumentation));
        // TODO(nga): profiler.instrumentation() is actually profile mode. Rename something.
        if profiler.instrumentation().profile_mode.is_some() {
            StarlarkProfilerOrInstrumentation::for_profiler(profiler)
        } else {
            StarlarkProfilerOrInstrumentation::instrumentation(instrumentation)
        }
    }

    pub fn for_profiler(profiler: &'p mut dyn StarlarkProfiler) -> Self {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::Profiler(profiler))
    }

    /// Instrumentation only.
    pub fn instrumentation(instrumentation: StarlarkProfilerInstrumentation) -> Self {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::Instrumentation(
            instrumentation,
        ))
    }

    /// No profiling.
    pub fn disabled() -> StarlarkProfilerOrInstrumentation<'p> {
        StarlarkProfilerOrInstrumentation(StarlarkProfilerOrInstrumentationImpl::None)
    }

    pub fn initialize(&mut self, eval: &mut Evaluator) {
        match &mut self.0 {
            StarlarkProfilerOrInstrumentationImpl::None => {}
            StarlarkProfilerOrInstrumentationImpl::Profiler(profiler) => {
                profiler.initialize(eval);
            }
            StarlarkProfilerOrInstrumentationImpl::Instrumentation(instrumentation) => {
                instrumentation.enable(eval);
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

    pub fn finalize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        if let StarlarkProfilerOrInstrumentationImpl::Profiler(profiler) = &mut self.0 {
            profiler.finalize(eval)
        } else {
            Ok(())
        }
    }
}

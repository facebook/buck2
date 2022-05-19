/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use anyhow::Context;
use gazebo::dupe::Dupe;
use starlark::{
    eval::{Evaluator, ProfileMode},
    values::FrozenHeapRef,
};

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

    fn visit_heap(&mut self, heap: Option<&FrozenHeapRef>) -> anyhow::Result<()>;
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

    fn visit_heap(&mut self, _: Option<&FrozenHeapRef>) -> anyhow::Result<()> {
        Ok(())
    }
}

pub struct StarlarkProfilerImpl {
    profile_mode: ProfileMode,
    path: PathBuf,

    initialized_at: Option<Instant>,
    finalized_at: Option<Instant>,
    total_allocated_bytes: Option<usize>,
}

impl StarlarkProfilerImpl {
    pub fn new(profile_mode: ProfileMode, path: PathBuf) -> StarlarkProfilerImpl {
        Self {
            profile_mode,
            path,
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
        eval.write_profile(&self.profile_mode, &self.path)
            .context("Failed to write profile")?;
        Ok(())
    }

    fn visit_heap(&mut self, heap: Option<&FrozenHeapRef>) -> anyhow::Result<()> {
        let total_allocated_bytes = heap.map_or(0, |heap| {
            heap.allocated_summary()
                .summary
                .values()
                .map(|(_, bytes)| bytes)
                .sum()
        });

        self.total_allocated_bytes = Some(total_allocated_bytes);

        Ok(())
    }
}

/// Modules can be evaluated with profiling or with instrumentation for profiling.
/// This type enapsulates this logic.
pub struct StarlarkProfilerOrInstrumentation<'p> {
    profiler: Option<&'p mut dyn StarlarkProfiler>,
    instrumentation: StarlarkProfilerInstrumentation,
}

impl<'p> StarlarkProfilerOrInstrumentation<'p> {
    pub fn new(
        profiler: &'p mut dyn StarlarkProfiler,
        instrumentation: StarlarkProfilerInstrumentation,
    ) -> StarlarkProfilerOrInstrumentation<'p> {
        // Sanity check.
        assert!(profiler.instrumentation().is_subset(&instrumentation));
        let profiler = Some(profiler);
        StarlarkProfilerOrInstrumentation {
            profiler,
            instrumentation,
        }
    }

    pub fn for_profiler(profiler: &'p mut dyn StarlarkProfiler) -> Self {
        let instrumentation = profiler.instrumentation();
        Self::new(profiler, instrumentation)
    }

    /// No profiling.
    pub fn disabled() -> StarlarkProfilerOrInstrumentation<'p> {
        StarlarkProfilerOrInstrumentation {
            profiler: None,
            instrumentation: StarlarkProfilerInstrumentation::default(),
        }
    }

    pub fn initialize(&mut self, eval: &mut Evaluator) {
        self.instrumentation.enable(eval);
        if let Some(profiler) = &mut self.profiler {
            profiler.initialize(eval);
        }
    }

    pub fn visit_heap(&mut self, heap: Option<&FrozenHeapRef>) -> anyhow::Result<()> {
        if let Some(profiler) = &mut self.profiler {
            profiler.visit_heap(heap)?;
        }
        Ok(())
    }

    pub fn finalize(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
        if let Some(profiler) = &mut self.profiler {
            profiler.finalize(eval)?;
        }
        Ok(())
    }
}

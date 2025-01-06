/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use buck2_error::conversion::from_any;
use buck2_error::internal_error;
use buck2_error::starlark_error::from_starlark;
use buck2_error::BuckErrorContext;
use starlark::environment::FrozenModule;
use starlark::eval::Evaluator;
use starlark::eval::ProfileData;
use starlark::eval::ProfileMode;

use crate::starlark_profiler::data::ProfileTarget;
use crate::starlark_profiler::data::StarlarkProfileDataAndStats;

#[derive(Debug, buck2_error::Error)]
enum StarlarkProfilerError {
    #[error(
        "Retained memory profiling is available only for analysis profile \
        or bxl profile (which freezes the module)"
    )]
    RetainedMemoryNotFrozen,
}

pub struct StarlarkProfiler {
    profile_mode: ProfileMode,
    /// Evaluation will freeze the module.
    /// (And frozen module will be passed to `visit_frozen_module`).
    will_freeze: bool,

    initialized_at: Option<Instant>,
    finalized_at: Option<Instant>,
    profile_data: Option<ProfileData>,
    total_retained_bytes: Option<usize>,

    target: ProfileTarget,
}

impl StarlarkProfiler {
    pub fn new(
        profile_mode: ProfileMode,
        will_freeze: bool,
        target: ProfileTarget,
    ) -> StarlarkProfiler {
        Self {
            profile_mode,
            will_freeze,
            initialized_at: None,
            finalized_at: None,
            profile_data: None,
            total_retained_bytes: None,
            target,
        }
    }

    /// Collect all profiling data.
    pub fn finish(self) -> buck2_error::Result<StarlarkProfileDataAndStats> {
        Ok(StarlarkProfileDataAndStats {
            initialized_at: self.initialized_at.internal_error("did not initialize")?,
            finalized_at: self.finalized_at.internal_error("did not finalize")?,
            total_retained_bytes: self
                .total_retained_bytes
                .internal_error("did not visit heap")?,
            profile_data: self
                .profile_data
                .internal_error("profile_data not initialized")?,
            targets: vec![self.target],
        })
    }

    /// Prepare an Evaluator to capture output relevant to this profiler.
    fn initialize(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()> {
        eval.enable_profile(&self.profile_mode).map_err(from_any)?;
        self.initialized_at = Some(Instant::now());
        Ok(())
    }

    /// Post-analysis, produce the output of this profiler.
    fn evaluation_complete(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()> {
        self.finalized_at = Some(Instant::now());
        if !self.profile_mode.requires_frozen_module() {
            self.profile_data = Some(eval.gen_profile().map_err(from_starlark)?);
        }
        Ok(())
    }

    fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> buck2_error::Result<()> {
        if self.will_freeze != module.is_some() {
            return Err(internal_error!(
                "will_freeze field was initialized incorrectly"
            ));
        }

        if self.profile_mode.requires_frozen_module() {
            let module = module.ok_or(StarlarkProfilerError::RetainedMemoryNotFrozen)?;
            let profile = module.heap_profile().map_err(from_any)?;
            self.profile_data = Some(profile);
        }

        let total_retained_bytes = module.map_or(0, |module| {
            module
                .frozen_heap()
                .allocated_summary()
                .total_allocated_bytes()
        });

        self.total_retained_bytes = Some(total_retained_bytes);

        Ok(())
    }
}

enum StarlarkProfilerOptImpl<'p> {
    None,
    Profiler(&'p mut StarlarkProfiler),
}

/// Modules can be evaluated with profiling or with instrumentation for profiling.
/// This type enapsulates this logic.
pub struct StarlarkProfilerOpt<'p>(StarlarkProfilerOptImpl<'p>);

impl<'p> StarlarkProfilerOpt<'p> {
    pub fn for_profiler(profiler: &'p mut StarlarkProfiler) -> Self {
        StarlarkProfilerOpt(StarlarkProfilerOptImpl::Profiler(profiler))
    }

    /// No profiling.
    pub fn disabled() -> StarlarkProfilerOpt<'p> {
        StarlarkProfilerOpt(StarlarkProfilerOptImpl::None)
    }

    pub fn initialize(&mut self, eval: &mut Evaluator) -> buck2_error::Result<bool> {
        match &mut self.0 {
            StarlarkProfilerOptImpl::None => Ok(false),
            StarlarkProfilerOptImpl::Profiler(profiler) => profiler.initialize(eval).map(|_| true),
        }
    }

    pub fn visit_frozen_module(
        &mut self,
        module: Option<&FrozenModule>,
    ) -> buck2_error::Result<()> {
        if let StarlarkProfilerOptImpl::Profiler(profiler) = &mut self.0 {
            profiler.visit_frozen_module(module)
        } else {
            Ok(())
        }
    }

    pub fn evaluation_complete(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()> {
        if let StarlarkProfilerOptImpl::Profiler(profiler) = &mut self.0 {
            profiler.evaluation_complete(eval)
        } else {
            Ok(())
        }
    }
}

pub enum StarlarkProfilerOptVal {
    Disabled,
    Profiler(StarlarkProfiler),
}

impl StarlarkProfilerOptVal {
    pub fn as_mut(&mut self) -> StarlarkProfilerOpt {
        match self {
            StarlarkProfilerOptVal::Disabled => StarlarkProfilerOpt::disabled(),
            StarlarkProfilerOptVal::Profiler(profiler) => {
                StarlarkProfilerOpt::for_profiler(profiler)
            }
        }
    }

    pub fn finish(self) -> buck2_error::Result<Option<StarlarkProfileDataAndStats>> {
        match self {
            StarlarkProfilerOptVal::Disabled => Ok(None),
            StarlarkProfilerOptVal::Profiler(profiler) => profiler.finish().map(Some),
        }
    }
}

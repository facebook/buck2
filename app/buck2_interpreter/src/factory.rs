/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::conversion::from_any_with_tag;
use dupe::Dupe;
use starlark::environment::Module;
use starlark::eval::Evaluator;

use crate::dice::starlark_provider::CancellationPoller;
use crate::starlark_debug::StarlarkDebugController;
use crate::starlark_profiler::profiler::StarlarkProfiler;

pub struct StarlarkEvaluatorProvider<'x, 'a> {
    profiler: &'x mut StarlarkProfiler,
    debugger: Option<Box<dyn StarlarkDebugController>>,
    starlark_max_callstack_size: Option<usize>,
    cancellation: CancellationPoller<'a>,
}

impl<'x, 'a> StarlarkEvaluatorProvider<'x, 'a> {
    /// Trivial provider that just constructs an Evaluator. Useful for tests (but not necessarily limited to them).
    pub fn passthrough(profiler: &'x mut StarlarkProfiler) -> Self {
        Self {
            profiler,
            debugger: None,
            starlark_max_callstack_size: None,
            cancellation: CancellationPoller::None,
        }
    }

    pub(crate) fn new(
        profiler: &'x mut StarlarkProfiler,
        debugger: Option<Box<dyn StarlarkDebugController>>,
        starlark_max_callstack_size: Option<usize>,
        cancellation: CancellationPoller<'a>,
    ) -> Self {
        Self {
            profiler,
            debugger,
            starlark_max_callstack_size,
            cancellation,
        }
    }

    /// Creates an Evaluator for a module. The evaluator will be configured for instrumenting/profiling/debugging
    /// as appropriate. Also returns whether profiling is enabled.
    pub fn make<'v, 'b, 'e>(
        &mut self,
        module: &'v Module,
    ) -> buck2_error::Result<(Evaluator<'v, 'b, 'e>, bool)>
    where
        'a: 'b,
    {
        let mut eval = Evaluator::new(module);
        if let Some(stack_size) = self.starlark_max_callstack_size {
            eval.set_max_callstack_size(stack_size)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }
        match self.cancellation.dupe() {
            CancellationPoller::None => {}
            CancellationPoller::Context(c) => {
                eval.set_check_cancelled(Box::new(|| c.is_cancellation_requested()))
            }
            CancellationPoller::Observer(o) => {
                eval.set_check_cancelled(Box::new(move || o.is_cancellation_requested()))
            }
        }

        let is_profiling_enabled = self.profiler.initialize(&mut eval)?;
        if let Some(v) = &mut self.debugger {
            v.initialize(&mut eval)?;
        }
        Ok((eval, is_profiling_enabled))
    }

    pub fn evaluation_complete(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()> {
        self.profiler.evaluation_complete(eval)
    }
}
